{-# language BangPatterns #-}
{-# language RankNTypes #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language MagicHash #-}

module Socket.Stream.IPv4
  ( -- * Types
    Listener
  , Connection
  , Endpoint(..)
    -- * Bracketed
  , withListener
  , withAccepted
  , withConnection
  , forkAccepted
  , forkAcceptedUnmasked
    -- * Communicate
  , sendByteArray
  , sendByteArraySlice
  , sendMutableByteArray
  , sendMutableByteArraySlice
  , receiveByteArray
  , receiveBoundedByteArray
  , receiveMutableByteArray
    -- * Exceptions
  , SocketException(..)
  , Context(..)
  , Reason(..)
  ) where

import Control.Concurrent (ThreadId,threadWaitWrite,threadWaitRead)
import Control.Concurrent (forkIO,forkIOWithUnmask)
import Control.Exception (mask,onException)
import Data.Bifunctor (bimap)
import Data.Primitive (ByteArray,MutableByteArray(..))
import Data.Word (Word16)
import Foreign.C.Error (Errno(..),eAGAIN,eWOULDBLOCK,eINPROGRESS)
import Foreign.C.Types (CInt,CSize)
import GHC.Exts (RealWorld,Int(I#),shrinkMutableByteArray#)
import Socket (SocketException(..),Context(..),Reason(..))
import Socket.Debug (debug)
import Socket.IPv4 (Endpoint(..))
import System.Posix.Types (Fd)
import Net.Types (IPv4(..))

import qualified Control.Monad.Primitive as PM
import qualified Data.Primitive as PM
import qualified Linux.Socket as L
import qualified Posix.Socket as S

-- | A socket that listens for incomming connections.
newtype Listener = Listener Fd

-- | A connection-oriented stream socket.
newtype Connection = Connection Fd

withListener ::
     Endpoint
  -> (Listener -> Word16 -> IO a)
  -> IO (Either SocketException a)
withListener endpoint@Endpoint{port = specifiedPort} f = mask $ \restore -> do
  debug ("withSocket: opening listener " ++ show endpoint)
  e1 <- S.uninterruptibleSocket S.internet
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.stream)
    S.defaultProtocol
  debug ("withSocket: opened listener " ++ show endpoint)
  case e1 of
    Left err -> pure (Left (errorCode Open err))
    Right fd -> do
      e2 <- S.uninterruptibleBind fd
        (S.encodeSocketAddressInternet (endpointToSocketAddressInternet endpoint))
      debug ("withSocket: requested binding for listener " ++ show endpoint)
      case e2 of
        Left err -> do
          _ <- S.uninterruptibleClose fd
          pure (Left (errorCode Bind err))
        Right _ -> S.uninterruptibleListen fd 16 >>= \case
          -- We hardcode the listen backlog to 16. The author is unfamiliar
          -- with use cases where gains are realized from tuning this parameter.
          -- Open an issue if this causes problems for anyone.
          Left err -> do
            _ <- S.uninterruptibleClose fd
            debug "withSocket: listen failed with error code"
            pure (Left (errorCode Listen err))
          Right _ -> do 
            -- The getsockname is copied from code in Socket.Datagram.IPv4.Undestined.
            -- Consider factoring this out.
            eactualPort <- if specifiedPort == 0
              then S.uninterruptibleGetSocketName fd S.sizeofSocketAddressInternet >>= \case
                Left err -> do
                  _ <- S.uninterruptibleClose fd
                  pure (Left (errorCode GetName err))
                Right (sockAddrRequiredSz,sockAddr) -> if sockAddrRequiredSz == S.sizeofSocketAddressInternet
                  then case S.decodeSocketAddressInternet sockAddr of
                    Just S.SocketAddressInternet{port = actualPort} -> do
                      let cleanActualPort = S.networkToHostShort actualPort
                      debug ("withSocket: successfully bound listener " ++ show endpoint ++ " and got port " ++ show cleanActualPort)
                      pure (Right cleanActualPort)
                    Nothing -> do
                      _ <- S.uninterruptibleClose fd
                      pure (Left (exception GetName SocketAddressFamily))
                  else do
                    _ <- S.uninterruptibleClose fd
                    pure (Left (exception GetName SocketAddressSize))
              else pure (Right specifiedPort)
            case eactualPort of
              Left err -> pure (Left err)
              Right actualPort -> do
                a <- onException (restore (f (Listener fd) actualPort)) (S.uninterruptibleClose fd)
                S.uninterruptibleClose fd >>= \case
                  Left err -> pure (Left (errorCode Close err))
                  Right _ -> pure (Right a)

-- | Accept a connection on the listener and run the supplied callback
-- on it. This closes the connection when the callback finishes or if
-- an exception is thrown. Since this function blocks the thread until
-- the callback finishes, it is only suitable for stream socket clients
-- that handle one connection at a time. The variant 'forkAcceptedUnmasked'
-- is preferrable for servers that need to handle connections concurrently
-- (most use cases).
withAccepted ::
     Listener
  -> (Connection -> Endpoint -> IO a)
  -> IO (Either SocketException a)
withAccepted lst cb = internalAccepted
  ( \restore action -> do
    action restore
  ) lst cb

internalAccepted ::
     ((forall x. IO x -> IO x) -> ((IO a -> IO b) -> IO (Either SocketException b)) -> IO (Either SocketException c))
  -> Listener
  -> (Connection -> Endpoint -> IO a)
  -> IO (Either SocketException c)
internalAccepted wrap (Listener !lst) f = do
  threadWaitRead lst
  mask $ \restore -> do
    S.uninterruptibleAccept lst S.sizeofSocketAddressInternet >>= \case
      Left err -> pure (Left (errorCode Accept err))
      Right (sockAddrRequiredSz,sockAddr,acpt) -> if sockAddrRequiredSz == S.sizeofSocketAddressInternet
        then case S.decodeSocketAddressInternet sockAddr of
          Just sockAddrInet -> do
            let acceptedEndpoint = socketAddressInternetToEndpoint sockAddrInet
            debug ("withAccepted: successfully accepted connection from " ++ show acceptedEndpoint)
            wrap restore $ \restore' -> do
              a <- onException (restore' (f (Connection acpt) acceptedEndpoint)) (S.uninterruptibleClose acpt)
              gracefulClose acpt a
          Nothing -> do
            _ <- S.uninterruptibleClose acpt
            pure (Left (exception GetName SocketAddressFamily))
        else do
          _ <- S.uninterruptibleClose acpt
          pure (Left (exception GetName SocketAddressSize))

gracefulClose :: Fd -> a -> IO (Either SocketException a)
gracefulClose fd a = S.uninterruptibleShutdown fd S.write >>= \case
  Left err -> do
    _ <- S.uninterruptibleClose fd
    pure (Left (errorCode Shutdown err))
  Right _ -> do
    buf <- PM.newByteArray 1
    S.uninterruptibleReceiveMutableByteArray fd buf 0 1 mempty >>= \case
      Left err1 -> if err1 == eWOULDBLOCK || err1 == eAGAIN
        then do
          threadWaitRead fd
          S.uninterruptibleReceiveMutableByteArray fd buf 0 1 mempty >>= \case
            Left err -> do
              _ <- S.uninterruptibleClose fd
              pure (Left (errorCode Shutdown err))
            Right sz -> if sz == 0
              then fmap (bimap (errorCode Close) (const a)) (S.uninterruptibleClose fd)
              else do
                debug ("Socket.Stream.IPv4.gracefulClose: remote not shutdown A")
                _ <- S.uninterruptibleClose fd
                pure (Left (exception Shutdown RemoteNotShutdown))
        else do
          _ <- S.uninterruptibleClose fd
          -- Is this the right error context? It's a call
          -- to recv, but it happens while shutting down
          -- the socket.
          pure (Left (errorCode Shutdown err1))
      Right sz -> if sz == 0
        then fmap (bimap (errorCode Close) (const a)) (S.uninterruptibleClose fd)
        else do
          debug ("Socket.Stream.IPv4.gracefulClose: remote not shutdown B")
          _ <- S.uninterruptibleClose fd
          pure (Left (exception Shutdown RemoteNotShutdown))

-- | Accept a connection on the listener and run the supplied callback in
-- a new thread. Prefer 'forkAcceptedUnmasked' unless the masking state
-- needs to be preserved for the callback. Such a situation seems unlikely
-- to the author.
forkAccepted ::
     Listener
  -> (Either SocketException a -> IO ())
  -> (Connection -> Endpoint -> IO a)
  -> IO (Either SocketException ThreadId)
forkAccepted lst consumeException cb = internalAccepted
  ( \restore action -> do
    tid <- forkIO $ do
      x <- action restore
      restore (consumeException x)
    pure (Right tid)
  ) lst cb

-- | Accept a connection on the listener and run the supplied callback in
-- a new thread. The masking state is set to @Unmasked@ when running the
-- callback.
forkAcceptedUnmasked ::
     Listener
  -> (Either SocketException a -> IO ())
  -> (Connection -> Endpoint -> IO a)
  -> IO (Either SocketException ThreadId)
forkAcceptedUnmasked lst consumeException cb = internalAccepted
  ( \_ action -> do
    tid <- forkIOWithUnmask $ \unmask -> do
      x <- action unmask
      unmask (consumeException x)
    pure (Right tid)
  ) lst cb

-- | Establish a connection to a server.
withConnection ::
     Endpoint -- ^ Remote endpoint
  -> (Connection -> IO a) -- ^ Callback to consume connection
  -> IO (Either SocketException a)
withConnection !remote f = mask $ \restore -> do
  debug ("withSocket: opening connection " ++ show remote)
  e1 <- S.uninterruptibleSocket S.internet
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.stream)
    S.defaultProtocol
  debug ("withSocket: opened connection " ++ show remote)
  case e1 of
    Left err1 -> pure (Left (errorCode Open err1))
    Right fd -> do
      let sockAddr = id
            $ S.encodeSocketAddressInternet
            $ endpointToSocketAddressInternet
            $ remote
      merr <- S.uninterruptibleConnect fd sockAddr >>= \case
        Left err2 -> if err2 == eINPROGRESS
          then do
            threadWaitWrite fd
            pure Nothing
          else pure (Just (errorCode Connect err2))
        Right _ -> pure Nothing
      case merr of
        Just err -> do
          _ <- S.uninterruptibleClose fd
          pure (Left err)
        Nothing -> do
          e <- S.uninterruptibleGetSocketOption fd
            S.levelSocket S.optionError (intToCInt (PM.sizeOf (undefined :: CInt)))
          case e of
            Left err -> do
              _ <- S.uninterruptibleClose fd
              pure (Left (errorCode Option err))
            Right (sz,S.OptionValue val) -> if sz == intToCInt (PM.sizeOf (undefined :: CInt))
              then
                let err = PM.indexByteArray val 0 :: CInt in
                if err == 0
                  then do
                    a <- onException (restore (f (Connection fd))) (S.uninterruptibleClose fd)
                    gracefulClose fd a
                  else do
                    _ <- S.uninterruptibleClose fd
                    pure (Left (errorCode Connect (Errno err)))
              else do
                _ <- S.uninterruptibleClose fd
                pure (Left (exception Option OptionValueSize))

sendByteArray ::
     Connection -- ^ Connection
  -> ByteArray -- ^ Buffer (will be sliced)
  -> IO (Either SocketException ())
sendByteArray conn arr =
  sendByteArraySlice conn arr 0 (PM.sizeofByteArray arr)

sendByteArraySlice ::
     Connection -- ^ Connection
  -> ByteArray -- ^ Buffer (will be sliced)
  -> Int -- ^ Offset into payload
  -> Int -- ^ Lenth of slice into buffer
  -> IO (Either SocketException ())
sendByteArraySlice !conn !payload !off0 !len0 = go off0 len0
  where
  go !off !len = if len > 0
    then internalSend conn payload off len >>= \case
      Left e -> pure (Left e)
      Right sz' -> do
        let sz = csizeToInt sz'
        go (off + sz) (len - sz)
    else pure (Right ())

sendMutableByteArray ::
     Connection -- ^ Connection
  -> MutableByteArray RealWorld -- ^ Buffer (will be sliced)
  -> IO (Either SocketException ())
sendMutableByteArray conn arr =
  sendMutableByteArraySlice conn arr 0 =<< PM.getSizeofMutableByteArray arr

sendMutableByteArraySlice ::
     Connection -- ^ Connection
  -> MutableByteArray RealWorld -- ^ Buffer (will be sliced)
  -> Int -- ^ Offset into payload
  -> Int -- ^ Lenth of slice into buffer
  -> IO (Either SocketException ())
sendMutableByteArraySlice !conn !payload !off0 !len0 = go off0 len0
  where
  go !off !len = if len > 0
    then internalSendMutable conn payload off len >>= \case
      Left e -> pure (Left e)
      Right sz' -> do
        let sz = csizeToInt sz'
        go (off + sz) (len - sz)
    else pure (Right ())

-- The length must be greater than zero.
internalSendMutable :: 
     Connection -- ^ Connection
  -> MutableByteArray RealWorld -- ^ Buffer (will be sliced)
  -> Int -- ^ Offset into payload
  -> Int -- ^ Length of slice into buffer
  -> IO (Either SocketException CSize)
internalSendMutable (Connection !s) !payload !off !len = do
  e1 <- S.uninterruptibleSendMutableByteArray s payload
    (intToCInt off)
    (intToCSize len)
    mempty
  case e1 of
    Left err1 -> if err1 == eWOULDBLOCK || err1 == eAGAIN
      then do
        threadWaitWrite s
        e2 <- S.uninterruptibleSendMutableByteArray s payload
          (intToCInt off)
          (intToCSize len)
          mempty
        case e2 of
          Left err2 -> pure (Left (errorCode Send err2))
          Right sz -> pure (Right sz)
      else pure (Left (errorCode Send err1))
    Right sz -> pure (Right sz)

-- The length must be greater than zero.
internalSend ::
     Connection -- ^ Connection
  -> ByteArray -- ^ Buffer (will be sliced)
  -> Int -- ^ Offset into payload
  -> Int -- ^ Length of slice into buffer
  -> IO (Either SocketException CSize)
internalSend (Connection !s) !payload !off !len = do
  debug ("send: about to send chunk on stream socket, offset " ++ show off ++ " and length " ++ show len)
  e1 <- S.uninterruptibleSendByteArray s payload
    (intToCInt off)
    (intToCSize len)
    mempty
  debug "send: just sent chunk on stream socket"
  case e1 of
    Left err1 -> if err1 == eWOULDBLOCK || err1 == eAGAIN
      then do
        debug "send: waiting to for write ready on stream socket"
        threadWaitWrite s
        e2 <- S.uninterruptibleSendByteArray s payload
          (intToCInt off)
          (intToCSize len)
          mempty
        case e2 of
          Left err2 -> do
            debug "send: encountered error after sending chunk on stream socket"
            pure (Left (errorCode Send err2))
          Right sz -> pure (Right sz)
      else pure (Left (errorCode Send err1))
    Right sz -> pure (Right sz)

-- The maximum number of bytes to receive must be greater than zero.
-- The operating system guarantees us that the returned actual number
-- of bytes is less than or equal to the requested number of bytes.
-- This function does not validate that the result size is greater
-- than zero. Functions calling this must perform that check. This
-- also does not trim the buffer. The caller must do that if it is
-- necessary.
internalReceiveMaximally ::
     Connection -- ^ Connection
  -> Int -- ^ Maximum number of bytes to receive
  -> MutableByteArray RealWorld -- ^ Receive buffer
  -> Int -- ^ Offset into buffer
  -> IO (Either SocketException Int)
internalReceiveMaximally (Connection !fd) !maxSz !buf !off = do
  debug "receive: stream socket about to wait"
  threadWaitRead fd
  debug ("receive: stream socket is now readable, receiving up to " ++ show maxSz ++ " bytes at offset " ++ show off)
  e <- S.uninterruptibleReceiveMutableByteArray fd buf (intToCInt off) (intToCSize maxSz) mempty
  debug "receive: finished reading from stream socket"
  case e of
    Left err -> pure (Left (errorCode Receive err))
    Right recvSz -> pure (Right (csizeToInt recvSz))

-- | Receive exactly the given number of bytes. If the remote application
--   shuts down its end of the connection before sending the required
--   number of bytes, this returns
--   @'Left' ('SocketException' 'Receive' 'RemoteShutdown')@.
receiveByteArray ::
     Connection -- ^ Connection
  -> Int -- ^ Number of bytes to receive
  -> IO (Either SocketException ByteArray)
receiveByteArray !conn0 !total = do
  marr <- PM.newByteArray total
  go conn0 marr 0 total
  where
  go !conn !marr !off !remaining = case compare remaining 0 of
    GT -> internalReceiveMaximally conn remaining marr off >>= \case
      Left err -> pure (Left err)
      Right sz -> if sz /= 0
        then go conn marr (off + sz) (remaining - sz)
        else pure (Left (exception Receive RemoteShutdown))
    EQ -> do
      arr <- PM.unsafeFreezeByteArray marr
      pure (Right arr)
    LT -> pure (Left (exception Receive NegativeBytesRequested))

-- | Receive a number of bytes exactly equal to the size of the mutable
--   byte array. If the remote application shuts down its end of the
--   connection before sending the required number of bytes, this returns
--   @'Left' ('SocketException' 'Receive' 'RemoteShutdown')@.
receiveMutableByteArray ::
     Connection
  -> MutableByteArray RealWorld
  -> IO (Either SocketException ())
receiveMutableByteArray !conn0 !marr0 = do
  total <- PM.getSizeofMutableByteArray marr0
  go conn0 marr0 0 total
  where
  go !conn !marr !off !remaining = if remaining > 0
    then internalReceiveMaximally conn remaining marr off >>= \case
      Left err -> pure (Left err)
      Right sz -> if sz /= 0
        then go conn marr (off + sz) (remaining - sz)
        else pure (Left (exception Receive RemoteShutdown))
    else pure (Right ())

-- | Receive up to the given number of bytes. If the remote application
--   shuts down its end of the connection instead of sending any bytes,
--   this returns
--   @'Left' ('SocketException' 'Receive' 'RemoteShutdown')@.
receiveBoundedByteArray :: 
     Connection -- ^ Connection
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either SocketException ByteArray)
receiveBoundedByteArray !conn !total
  | total > 0 = do
      m <- PM.newByteArray total
      internalReceiveMaximally conn total m 0 >>= \case
        Left err -> pure (Left err) 
        Right sz -> if sz /= 0
          then do
            shrinkMutableByteArray m sz
            fmap Right (PM.unsafeFreezeByteArray m)
          else pure (Left (exception Receive RemoteShutdown))
  | total == 0 = pure (Right mempty)
  | otherwise = pure (Left (exception Receive NegativeBytesRequested))

endpointToSocketAddressInternet :: Endpoint -> S.SocketAddressInternet
endpointToSocketAddressInternet (Endpoint {address, port}) = S.SocketAddressInternet
  { port = S.hostToNetworkShort port
  , address = S.hostToNetworkLong (getIPv4 address)
  }

socketAddressInternetToEndpoint :: S.SocketAddressInternet -> Endpoint
socketAddressInternetToEndpoint (S.SocketAddressInternet {address,port}) = Endpoint
  { address = IPv4 (S.networkToHostLong address)
  , port = S.networkToHostShort port
  }

errorCode :: Context -> Errno -> SocketException
errorCode func (Errno x) = SocketException func (ErrorCode x)

exception :: Context -> Reason -> SocketException
exception func reason = SocketException func reason

intToCInt :: Int -> CInt
intToCInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

shrinkMutableByteArray :: MutableByteArray RealWorld -> Int -> IO ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  PM.primitive_ (shrinkMutableByteArray# arr sz)
