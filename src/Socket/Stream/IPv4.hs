{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

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
  , receiveMutableByteArraySlice
  , receiveMutableByteArray
    -- * Exceptions
  , SendException(..)
  , ReceiveException(..)
  , ConnectException(..)
  , ListenException(..)
  , CloseException(..)
  , Interruptibility(..)
  ) where

import Control.Concurrent (ThreadId, threadWaitRead, threadWaitWrite)
import Control.Concurrent (forkIO, forkIOWithUnmask)
import Control.Exception (mask, onException, throwIO)
import Data.Bifunctor (bimap,first)
import Data.Primitive (ByteArray, MutableByteArray(..))
import Data.Word (Word16)
import Foreign.C.Error (Errno(..), eAGAIN, eINPROGRESS, eWOULDBLOCK, ePIPE)
import Foreign.C.Error (eTIMEDOUT,eADDRNOTAVAIL,eNETUNREACH,eCONNREFUSED)
import Foreign.C.Error (eADDRINUSE)
import Foreign.C.Error (eNFILE,eMFILE,eACCES,ePERM,eCONNABORTED)
import Foreign.C.Types (CInt, CSize)
import GHC.Exts (Int(I#), RealWorld, shrinkMutableByteArray#)
import Net.Types (IPv4(..))
import Socket (SocketException(..),Interruptibility(..),Forkedness(..))
import Socket (cgetsockname,cclose)
import Socket (SocketUnrecoverableException(..))
import Socket.Stream (ConnectException(..),ListenException(..),AcceptException(..))
import Socket.Stream (SendException(..),ReceiveException(..),CloseException(..))
import Socket.Debug (debug)
import Socket.IPv4 (Endpoint(..),describeEndpoint)
import System.Posix.Types(Fd)

import qualified Control.Monad.Primitive as PM
import qualified Data.Primitive as PM
import qualified Linux.Socket as L
import qualified Posix.Socket as S
import qualified Socket as SCK

-- | A socket that listens for incomming connections.
newtype Listener = Listener Fd

-- | A connection-oriented stream socket.
newtype Connection = Connection Fd

withListener ::
     Endpoint
  -> (Listener -> Word16 -> IO a)
  -> IO (Either ListenException a)
withListener endpoint@Endpoint{port = specifiedPort} f = mask $ \restore -> do
  debug ("withSocket: opening listener " ++ describeEndpoint endpoint)
  e1 <- S.uninterruptibleSocket S.internet
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.stream)
    S.defaultProtocol
  debug ("withSocket: opened listener " ++ describeEndpoint endpoint)
  case e1 of
    Left err -> handleSocketListenException SCK.functionWithListener err
    Right fd -> do
      e2 <- S.uninterruptibleBind fd
        (S.encodeSocketAddressInternet (endpointToSocketAddressInternet endpoint))
      debug ("withSocket: requested binding for listener " ++ describeEndpoint endpoint)
      case e2 of
        Left err -> do
          _ <- S.uninterruptibleClose fd
          handleBindListenException specifiedPort SCK.functionWithListener err
        Right _ -> S.uninterruptibleListen fd 16 >>= \case
          -- We hardcode the listen backlog to 16. The author is unfamiliar
          -- with use cases where gains are realized from tuning this parameter.
          -- Open an issue if this causes problems for anyone.
          Left err -> do
            _ <- S.uninterruptibleClose fd
            debug "withSocket: listen failed with error code"
            handleBindListenException specifiedPort SCK.functionWithListener err
          Right _ -> do
            -- The getsockname is copied from code in Socket.Datagram.IPv4.Undestined.
            -- Consider factoring this out.
            actualPort <- if specifiedPort == 0
              then S.uninterruptibleGetSocketName fd S.sizeofSocketAddressInternet >>= \case
                Left err -> throwIO $ SocketUnrecoverableException
                  moduleSocketStreamIPv4
                  functionWithListener
                  [cgetsockname,describeEndpoint endpoint,describeErrorCode err]
                Right (sockAddrRequiredSz,sockAddr) -> if sockAddrRequiredSz == S.sizeofSocketAddressInternet
                  then case S.decodeSocketAddressInternet sockAddr of
                    Just S.SocketAddressInternet{port = actualPort} -> do
                      let cleanActualPort = S.networkToHostShort actualPort
                      debug ("withSocket: successfully bound listener " ++ describeEndpoint endpoint ++ " and got port " ++ show cleanActualPort)
                      pure cleanActualPort
                    Nothing -> do
                      _ <- S.uninterruptibleClose fd
                      throwIO $ SocketUnrecoverableException
                        moduleSocketStreamIPv4
                        functionWithListener
                        [cgetsockname,"non-internet socket family"]
                  else do
                    _ <- S.uninterruptibleClose fd
                    throwIO $ SocketUnrecoverableException
                      moduleSocketStreamIPv4
                      functionWithListener
                      [cgetsockname,describeEndpoint endpoint,"socket address size"]
              else pure specifiedPort
            a <- onException (restore (f (Listener fd) actualPort)) (S.uninterruptibleClose fd)
            S.uninterruptibleClose fd >>= \case
              Left err -> throwIO $ SocketUnrecoverableException
                moduleSocketStreamIPv4
                functionWithListener
                [cclose,describeEndpoint endpoint,describeErrorCode err]
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
  -> IO (Either (AcceptException 'Uninterruptible 'Unforked) a)
withAccepted lst cb = internalAccepted
  ( \restore action -> do
    fmap (first AcceptUngracefulClose) (action restore)
  ) lst cb

internalAccepted ::
     ((forall x. IO x -> IO x) -> ((IO a -> IO d) -> IO (Either CloseException d)) -> IO (Either (AcceptException 'Uninterruptible b) c))
  -> Listener
  -> (Connection -> Endpoint -> IO a)
  -> IO (Either (AcceptException 'Uninterruptible b) c)
internalAccepted wrap (Listener !lst) f = do
  threadWaitRead lst
  mask $ \restore -> do
    S.uninterruptibleAccept lst S.sizeofSocketAddressInternet >>= \case
      Left err -> handleAcceptException "withAccepted" err
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
            throwIO $ SocketUnrecoverableException
              moduleSocketStreamIPv4
              SCK.functionWithAccepted
              [SCK.cgetsockname,SCK.nonInternetSocketFamily]
        else do
          _ <- S.uninterruptibleClose acpt
          throwIO $ SocketUnrecoverableException
            moduleSocketStreamIPv4
            SCK.functionWithAccepted
            [SCK.cgetsockname,SCK.socketAddressSize]

gracefulClose :: Fd -> a -> IO (Either CloseException a)
gracefulClose fd a = S.uninterruptibleShutdown fd S.write >>= \case
  Left err -> do
    _ <- S.uninterruptibleClose fd
    -- TODO: What about ENOTCONN? Can this happen if the remote
    -- side has already closed the connection?
    throwIO $ SocketUnrecoverableException
      moduleSocketStreamIPv4
      SCK.functionGracefulClose
      [SCK.cshutdown,describeErrorCode err]
  Right _ -> do
    buf <- PM.newByteArray 1
    S.uninterruptibleReceiveMutableByteArray fd buf 0 1 mempty >>= \case
      Left err1 -> if err1 == eWOULDBLOCK || err1 == eAGAIN
        then do
          threadWaitRead fd
          S.uninterruptibleReceiveMutableByteArray fd buf 0 1 mempty >>= \case
            Left err -> do
              _ <- S.uninterruptibleClose fd
              throwIO $ SocketUnrecoverableException
                moduleSocketStreamIPv4
                SCK.functionGracefulClose
                [SCK.crecv,describeErrorCode err]
            Right sz -> if sz == 0
              then S.uninterruptibleClose fd >>= \case
                Left err -> throwIO $ SocketUnrecoverableException
                  moduleSocketStreamIPv4
                  SCK.functionGracefulClose
                  [SCK.cclose,describeErrorCode err]
                Right _ -> pure (Right a)
              else do
                debug ("Socket.Stream.IPv4.gracefulClose: remote not shutdown A")
                _ <- S.uninterruptibleClose fd
                pure (Left ClosePeerContinuedSending)
        else do
          _ <- S.uninterruptibleClose fd
          -- We threat all @recv@ errors except for the nonblocking
          -- notices as unrecoverable.
          throwIO $ SocketUnrecoverableException
            moduleSocketStreamIPv4
            SCK.functionGracefulClose
            [SCK.crecv,describeErrorCode err1]
      Right sz -> if sz == 0
        then S.uninterruptibleClose fd >>= \case
          Left err -> throwIO $ SocketUnrecoverableException
            moduleSocketStreamIPv4
            SCK.functionGracefulClose
            [SCK.cclose,describeErrorCode err]
          Right _ -> pure (Right a)
        else do
          debug ("Socket.Stream.IPv4.gracefulClose: remote not shutdown B")
          _ <- S.uninterruptibleClose fd
          pure (Left ClosePeerContinuedSending)

-- | Accept a connection on the listener and run the supplied callback in
-- a new thread. Prefer 'forkAcceptedUnmasked' unless the masking state
-- needs to be preserved for the callback. Such a situation seems unlikely
-- to the author.
forkAccepted ::
     Listener
  -> (Either CloseException a -> IO ())
  -> (Connection -> Endpoint -> IO a)
  -> IO (Either (AcceptException 'Uninterruptible 'Forked) ThreadId)
forkAccepted lst consumeException cb = internalAccepted
  ( \restore action -> do
    tid <- forkIO $ do
      x <- action restore
      restore (consumeException x)
    pure (Right tid)
  ) lst cb

-- | Accept a connection on the listener and run the supplied callback in
-- a new thread. The masking state is set to @Unmasked@ when running the
-- callback. Typically, @a@ is instantiated to @()@.
forkAcceptedUnmasked ::
     Listener
  -> (Either CloseException a -> IO ())
  -> (Connection -> Endpoint -> IO a)
  -> IO (Either (AcceptException 'Uninterruptible 'Forked) ThreadId)
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
  -> IO (Either (ConnectException 'Uninterruptible 'Unforked) a)
withConnection !remote f = mask $ \restore -> do
  debug ("withSocket: opening connection " ++ show remote)
  e1 <- S.uninterruptibleSocket S.internet
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.stream)
    S.defaultProtocol
  debug ("withSocket: opened connection " ++ show remote)
  case e1 of
    Left err -> handleSocketConnectException SCK.functionWithConnection err
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
          else pure (Just err2)
        Right _ -> pure Nothing
      case merr of
        Just err -> do
          _ <- S.uninterruptibleClose fd
          handleConnectException SCK.functionWithConnection err
        Nothing -> do
          e <- S.uninterruptibleGetSocketOption fd
            S.levelSocket S.optionError (intToCInt (PM.sizeOf (undefined :: CInt)))
          case e of
            Left err -> do
              _ <- S.uninterruptibleClose fd
              throwIO $ SocketUnrecoverableException
                moduleSocketStreamIPv4
                functionWithListener
                [SCK.cgetsockopt,describeEndpoint remote,describeErrorCode err]
            Right (sz,S.OptionValue val) -> if sz == intToCInt (PM.sizeOf (undefined :: CInt))
              then
                let err = PM.indexByteArray val 0 :: CInt in
                if err == 0
                  then do
                    a <- onException (restore (f (Connection fd))) (S.uninterruptibleClose fd)
                    fmap (first ConnectUngracefulClose) (gracefulClose fd a)
                  else do
                    _ <- S.uninterruptibleClose fd
                    handleConnectException SCK.functionWithConnection (Errno err)
              else do
                _ <- S.uninterruptibleClose fd
                throwIO $ SocketUnrecoverableException
                  moduleSocketStreamIPv4
                  functionWithListener
                  [SCK.cgetsockopt,describeEndpoint remote,connectErrorOptionValueSize]

sendByteArray ::
     Connection -- ^ Connection
  -> ByteArray -- ^ Buffer (will be sliced)
  -> IO (Either (SendException 'Uninterruptible) ())
sendByteArray conn arr =
  sendByteArraySlice conn arr 0 (PM.sizeofByteArray arr)

sendByteArraySlice ::
     Connection -- ^ Connection
  -> ByteArray -- ^ Buffer (will be sliced)
  -> Int -- ^ Offset into payload
  -> Int -- ^ Lenth of slice into buffer
  -> IO (Either (SendException 'Uninterruptible) ())
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
  -> IO (Either (SendException 'Uninterruptible) ())
sendMutableByteArray conn arr =
  sendMutableByteArraySlice conn arr 0 =<< PM.getSizeofMutableByteArray arr

sendMutableByteArraySlice ::
     Connection -- ^ Connection
  -> MutableByteArray RealWorld -- ^ Buffer (will be sliced)
  -> Int -- ^ Offset into payload
  -> Int -- ^ Lenth of slice into buffer
  -> IO (Either (SendException 'Uninterruptible) ())
sendMutableByteArraySlice !conn !payload !off0 !len0 = go off0 len0
  where
  go !off !len = if len > 0
    then internalSendMutable conn payload off len >>= \case
      Left e -> pure (Left e)
      Right sz' -> do
        let sz = csizeToInt sz'
        go (off + sz) (len - sz)
    else pure (Right ())

-- Precondition: the length must be greater than zero.
internalSendMutable ::
     Connection -- ^ Connection
  -> MutableByteArray RealWorld -- ^ Buffer (will be sliced)
  -> Int -- ^ Offset into payload
  -> Int -- ^ Length of slice into buffer
  -> IO (Either (SendException 'Uninterruptible) CSize)
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
          (S.noSignal)
        case e2 of
          Left err2 -> handleSendException functionSendMutableByteArray err2
          Right sz  -> pure (Right sz)
      else handleSendException "sendMutableByteArray" err1
    Right sz -> pure (Right sz)

-- Precondition: the length must be greater than zero.
internalSend ::
     Connection -- ^ Connection
  -> ByteArray -- ^ Buffer (will be sliced)
  -> Int -- ^ Offset into payload
  -> Int -- ^ Length of slice into buffer
  -> IO (Either (SendException 'Uninterruptible) CSize)
internalSend (Connection !s) !payload !off !len = do
  debug ("internalSend: about to send chunk on stream socket, offset " ++ show off ++ " and length " ++ show len)
  e1 <- S.uninterruptibleSendByteArray s payload
    (intToCInt off)
    (intToCSize len)
    (S.noSignal)
  debug "internalSend: just sent chunk on stream socket"
  case e1 of
    Left err1 -> if err1 == eWOULDBLOCK || err1 == eAGAIN
      then do
        debug "internalSend: waiting to for write ready on stream socket"
        threadWaitWrite s
        e2 <- S.uninterruptibleSendByteArray s payload
          (intToCInt off)
          (intToCSize len)
          (S.noSignal)
        case e2 of
          Left err2 -> do
            debug "internalSend: encountered error after sending chunk on stream socket"
            handleSendException functionSendByteArray err2
          Right sz -> pure (Right sz)
      else handleSendException functionSendByteArray err1
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
    Left err     -> pure (Left (errorCode err))
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
        else pure (Left RemoteShutdown)
    EQ -> do
      arr <- PM.unsafeFreezeByteArray marr
      pure (Right arr)
    LT -> pure (Left NegativeBytesRequested)

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
        else pure (Left RemoteShutdown)
    else pure (Right ())

-- | Receive up to the given number of bytes, using the given array and
--   starting at the given offset.
--   If the remote application shuts down its end of the connection instead of
--   sending any bytes, this returns
--   @'Left' ('SocketException' 'Receive' 'RemoteShutdown')@.
receiveMutableByteArraySlice ::
     Connection -- ^ Connection
  -> Int -- ^ Maximum number of bytes to receive
  -> MutableByteArray RealWorld -- ^ Buffer in which the data are going to be stored
  -> Int -- ^ Offset in the buffer
  -> IO (Either SocketException Int) -- ^ Either a socket exception or the number of bytes read
receiveMutableByteArraySlice !conn !total !marr !off
  | total > 0 =
      internalReceiveMaximally conn total marr off >>= \case
        Left err -> pure (Left err)
        Right sz -> if sz /= 0
          then pure (Right sz)
          else pure (Left RemoteShutdown)
  | total == 0 = pure (Right 0)
  | otherwise = pure (Left NegativeBytesRequested)

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
      receiveMutableByteArraySlice conn total m 0 >>= \case
        Left err -> pure (Left err)
        Right sz -> do
          shrinkMutableByteArray m sz
          Right <$> PM.unsafeFreezeByteArray m
  | total == 0 = pure (Right mempty)
  | otherwise = pure (Left NegativeBytesRequested)

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

intToCInt :: Int -> CInt
intToCInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

shrinkMutableByteArray :: MutableByteArray RealWorld -> Int -> IO ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  PM.primitive_ (shrinkMutableByteArray# arr sz)

errorCode :: Errno -> SocketException
errorCode (Errno x) = ErrorCode x

moduleSocketStreamIPv4 :: String
moduleSocketStreamIPv4 = "Socket.Stream.IPv4"

functionSendMutableByteArray :: String
functionSendMutableByteArray = "sendMutableByteArray"

functionSendByteArray :: String
functionSendByteArray = "sendByteArray"

functionWithListener :: String
functionWithListener = "withListener"

describeErrorCode :: Errno -> String
describeErrorCode (Errno e) = "error code " ++ show e

handleSendException :: String -> Errno -> IO (Either (SendException i) a)
{-# INLINE handleSendException #-}
handleSendException func e
  | e == ePIPE = pure (Left SendShutdown)
  | otherwise = throwIO $ SocketUnrecoverableException
      moduleSocketStreamIPv4
      func
      [describeErrorCode e]

-- These are the exceptions that can happen as a result of a
-- nonblocking @connect@ or as a result of subsequently calling
-- @getsockopt@ to get the @SO_ERROR@ after the socket is ready
-- for writes. For increased likelihood of correctness, we do
-- not distinguish between which of these error codes can show
-- up after which of those two calls. This means we are likely
-- testing for some exceptions that cannot occur as a result of
-- a particular call, but doing otherwise would be fraught with
-- uncertainty.
handleConnectException :: String -> Errno -> IO (Either (ConnectException i b) a)
handleConnectException func e
  | e == eACCES = pure (Left ConnectFirewalled)
  | e == ePERM = pure (Left ConnectFirewalled)
  | e == eNETUNREACH = pure (Left ConnectNetworkUnreachable)
  | e == eCONNREFUSED = pure (Left ConnectRefused)
  | e == eADDRNOTAVAIL = pure (Left ConnectEphemeralPortsExhausted)
  | e == eTIMEDOUT = pure (Left ConnectTimeout)
  | otherwise = throwIO $ SocketUnrecoverableException
      moduleSocketStreamIPv4
      func
      [describeErrorCode e]

-- These are the exceptions that can happen as a result
-- of calling @socket@ with the intent of using the socket
-- to open a connection (not listen for inbound connections).
handleSocketConnectException :: String -> Errno -> IO (Either (ConnectException i b) a)
handleSocketConnectException func e
  | e == eMFILE = pure (Left ConnectFileDescriptorLimit)
  | e == eNFILE = pure (Left ConnectFileDescriptorLimit)
  | otherwise = throwIO $ SocketUnrecoverableException
      moduleSocketStreamIPv4
      func
      [describeErrorCode e]

-- These are the exceptions that can happen as a result
-- of calling @socket@ with the intent of using the socket
-- to listen for inbound connections.
handleSocketListenException :: String -> Errno -> IO (Either ListenException a)
handleSocketListenException func e
  | e == eMFILE = pure (Left ListenFileDescriptorLimit)
  | e == eNFILE = pure (Left ListenFileDescriptorLimit)
  | otherwise = throwIO $ SocketUnrecoverableException
      moduleSocketStreamIPv4
      func
      [describeErrorCode e]

-- These are the exceptions that can happen as a result
-- of calling @bind@ with the intent of using the socket
-- to listen for inbound connections. This is also used
-- to clean up the error codes of @listen@. The two can
-- report some of the same error codes, and those happen
-- to be the error codes we are interested in.
--
-- NB: EACCES only happens on @bind@, not on @listen@.
handleBindListenException :: Word16 -> String -> Errno -> IO (Either ListenException a)
handleBindListenException thePort func e
  | e == eACCES = pure (Left ListenPermissionDenied)
  | e == eADDRINUSE = if thePort == 0
      then pure (Left ListenAddressInUse)
      else pure (Left ListenEphemeralPortsExhausted)
  | otherwise = throwIO $ SocketUnrecoverableException
      moduleSocketStreamIPv4
      func
      [describeErrorCode e]

-- These are the exceptions that can happen as a result
-- of calling @socket@ with the intent of using the socket
-- to open a connection (not listen for inbound connections).
handleAcceptException :: String -> Errno -> IO (Either (AcceptException i b) a)
handleAcceptException func e
  | e == eCONNABORTED = pure (Left AcceptConnectionAborted)
  | e == eMFILE = pure (Left AcceptFileDescriptorLimit)
  | e == eNFILE = pure (Left AcceptFileDescriptorLimit)
  | e == ePERM = pure (Left AcceptFirewalled)
  | otherwise = throwIO $ SocketUnrecoverableException
      moduleSocketStreamIPv4
      func
      [describeErrorCode e]

connectErrorOptionValueSize :: String
connectErrorOptionValueSize = "incorrectly sized value of SO_ERROR option"

