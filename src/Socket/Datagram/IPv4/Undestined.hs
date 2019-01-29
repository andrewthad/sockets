{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language UnboxedTuples #-}

module Socket.Datagram.IPv4.Undestined
  ( -- * Types
    Socket(..)
  , Endpoint(..)
  , Message(..)
    -- * Establish
  , withSocket
    -- * Communicate
  , send
  , receive
  , receiveMutableByteArraySlice_
  , receiveMany
  , receiveManyUnless
    -- * Exceptions
  , SocketException(..)
    -- * Examples
    -- $examples
  ) where

import Control.Concurrent (threadWaitWrite,threadWaitRead)
import Control.Exception (mask,onException)
import Data.Primitive (ByteArray,MutableByteArray(..))
import Data.Word (Word16)
import Foreign.C.Error (Errno(..),eWOULDBLOCK,eAGAIN)
import Foreign.C.Types (CInt,CSize)
import GHC.Exts (Int(I#),RealWorld,shrinkMutableByteArray#,ByteArray#,touch#)
import GHC.IO (IO(..))
import Net.Types (IPv4(..))
import Socket (SocketException(..))
import Socket.Datagram.IPv4.Undestined.Internal (Message(..),Socket(..))
import Socket.Datagram.IPv4.Undestined.Multiple (receiveMany,receiveManyUnless)
import Socket.Debug (debug)
import Socket.IPv4 (Endpoint(..))

import qualified Control.Monad.Primitive as PM
import qualified Data.Primitive as PM
import qualified Linux.Socket as L
import qualified Posix.Socket as S

-- | Open a socket and run the supplied callback on it. This closes the socket
-- when the callback finishes or when an exception is thrown. Do not return 
-- the socket from the callback. This leads to undefined behavior. If the
-- address @0.0.0.0@ is used, the socket receives on all network interfaces.
-- If the port 0 is used, an unused port is chosen by the operating system.
-- The callback provides the chosen port (or if the user specified a non-zero
-- port, the chosen port will be that value).
withSocket ::
     Endpoint -- ^ Address and port to use
  -> (Socket -> Word16 -> IO a) -- ^ Callback providing the socket and the chosen port
  -> IO (Either SocketException a)
withSocket endpoint@Endpoint{port = specifiedPort} f = mask $ \restore -> do
  debug ("withSocket: opening socket " ++ show endpoint)
  e1 <- S.uninterruptibleSocket S.internet
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.datagram)
    S.defaultProtocol
  debug ("withSocket: opened socket " ++ show endpoint)
  case e1 of
    Left err -> pure (Left (errorCode err))
    Right fd -> do
      e2 <- S.uninterruptibleBind fd
        (S.encodeSocketAddressInternet (endpointToSocketAddressInternet endpoint))
      debug ("withSocket: requested binding for " ++ show endpoint)
      case e2 of
        Left err -> do
          -- We intentionally discard any exceptions thrown by close. There is
          -- simply nothing that can be done with them.
          S.uninterruptibleErrorlessClose fd
          pure (Left (errorCode err))
        Right _ -> do
          eactualPort <- if specifiedPort == 0
            then S.uninterruptibleGetSocketName fd S.sizeofSocketAddressInternet >>= \case
              Left err -> do
                S.uninterruptibleErrorlessClose fd
                pure (Left (errorCode err))
              Right (sockAddrRequiredSz,sockAddr) -> if sockAddrRequiredSz == S.sizeofSocketAddressInternet
                then case S.decodeSocketAddressInternet sockAddr of
                  Just S.SocketAddressInternet{port = actualPort} -> do
                    let cleanPort = S.networkToHostShort actualPort
                    debug ("withSocket: successfully bound " ++ show endpoint ++ " and got port " ++ show cleanPort)
                    pure (Right cleanPort)
                  Nothing -> do
                    S.uninterruptibleErrorlessClose fd
                    pure (Left (SocketAddressFamily (-1)))
                else do
                  S.uninterruptibleErrorlessClose fd
                  pure (Left SocketAddressSize)
            else pure (Right specifiedPort)
          case eactualPort of
            Left err -> pure (Left err)
            Right actualPort -> do
              a <- onException (restore (f (Socket fd) actualPort)) (S.uninterruptibleErrorlessClose fd)
              S.uninterruptibleClose fd >>= \case
                Left err -> pure (Left (errorCode err))
                Right _ -> pure (Right a)

-- | Send a slice of a bytearray to the specified endpoint.
send ::
     Socket -- ^ Socket
  -> Endpoint -- ^ Remote IPv4 address and port
  -> ByteArray -- ^ Buffer (will be sliced)
  -> Int -- ^ Offset into payload
  -> Int -- ^ Lenth of slice into buffer
  -> IO (Either SocketException ())
send (Socket !s) !theRemote !thePayload !off !len = do
  debug ("send: about to send to " ++ show theRemote)
  e1 <- S.uninterruptibleSendToByteArray s thePayload
    (intToCInt off)
    (intToCSize len)
    mempty
    (S.encodeSocketAddressInternet (endpointToSocketAddressInternet theRemote))
  debug ("send: just sent to " ++ show theRemote)
  case e1 of
    Left err1 -> if err1 == eWOULDBLOCK || err1 == eAGAIN
      then do
        debug ("send: waiting to for write ready to send to " ++ show theRemote)
        threadWaitWrite s
        e2 <- S.uninterruptibleSendToByteArray s thePayload
          (intToCInt off)
          (intToCSize len)
          mempty
          (S.encodeSocketAddressInternet (endpointToSocketAddressInternet theRemote))
        case e2 of
          Left err2 -> do
            debug ("send: encountered error after sending")
            pure (Left (errorCode err2))
          Right sz -> if csizeToInt sz == len
            then pure (Right ())
            else pure (Left (SentMessageTruncated (csizeToInt sz)))
      else pure (Left (errorCode err1))
    Right sz -> if csizeToInt sz == len
      then do
        debug ("send: success")
        pure (Right ())
      else pure (Left (SentMessageTruncated (csizeToInt sz)))

-- | Receive a datagram into a freshly allocated bytearray.
receive ::
     Socket -- ^ Socket
  -> Int -- ^ Maximum size of datagram to receive
  -> IO (Either SocketException Message)
receive (Socket !fd) !maxSz = do
  debug "receive: about to wait"
  threadWaitRead fd
  debug "receive: socket is now readable"
  marr <- PM.newByteArray maxSz
  -- We use MSG_TRUNC so that we are able to figure out whether
  -- or not bytes were discarded. If bytes were discarded
  -- (meaning that the buffer was too small), we return an
  -- exception.
  e <- S.uninterruptibleReceiveFromMutableByteArray fd marr 0
    (intToCSize maxSz) (L.truncate) S.sizeofSocketAddressInternet
  debug "receive: finished reading from socket"
  case e of
    Left err -> pure (Left (errorCode err))
    Right (sockAddrRequiredSz,sockAddr,recvSz) -> if csizeToInt recvSz <= maxSz
      then if sockAddrRequiredSz == S.sizeofSocketAddressInternet
        then case S.decodeSocketAddressInternet sockAddr of
          Just sockAddrInet -> do
            shrinkMutableByteArray marr (csizeToInt recvSz)
            arr <- PM.unsafeFreezeByteArray marr
            pure $ Right (Message (socketAddressInternetToEndpoint sockAddrInet) arr)
          Nothing -> pure (Left (SocketAddressFamily (-1)))
        else pure (Left SocketAddressSize)
      else pure (Left (ReceivedMessageTruncated (csizeToInt recvSz)))

-- | Receive a datagram into a mutable byte array, ignoring information about
--   the remote endpoint. Returns the actual number of bytes present in the
--   datagram. Precondition: @buffer_length - offset >= max_datagram_length@.
receiveMutableByteArraySlice_ ::
     Socket -- ^ Socket
  -> MutableByteArray RealWorld -- ^ Buffer
  -> Int -- ^ Offset into buffer
  -> Int -- ^ Maximum size of datagram to receive
  -> IO (Either SocketException Int)
receiveMutableByteArraySlice_ (Socket !fd) !buf !off !maxSz = do
  threadWaitRead fd
  -- We use MSG_TRUNC so that we are able to figure out whether
  -- or not bytes were discarded. If bytes were discarded
  -- (meaning that the buffer was too small), we return an
  -- exception.
  e <- S.uninterruptibleReceiveFromMutableByteArray_ fd buf (intToCInt off) (intToCSize maxSz) (L.truncate)
  case e of
    Left err -> pure (Left (errorCode err))
    Right recvSz -> if csizeToInt recvSz <= maxSz
      then pure (Right (csizeToInt recvSz))
      else pure (Left (ReceivedMessageTruncated (csizeToInt recvSz)))

-- TODO: add receiveTimeout
-- receiveTimeout ::
--      Socket -- ^ Socket
--   -> Int -- ^ Maximum size of datagram to receive
--   -> Int -- ^ Microseconds to wait before giving up
--   -> IO (Maybe (IPv4,ByteArray))
-- receiveTimeout = error "uhoetuhntoehu"

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

errorCode :: Errno -> SocketException
errorCode (Errno x) = ErrorCode x

shrinkMutableByteArray :: MutableByteArray RealWorld -> Int -> IO ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  PM.primitive_ (shrinkMutableByteArray# arr sz)

{- $examples
 
Print every UDP packet that we receive. This terminates, closing the
socket, after receiving ten packets. This code throws any exception that
happens. This is commonly a useful behavior since most exceptions cannot
be handled gracefully.

> import qualified Data.ByteString.Char8 as BC
> import Control.Monad (replicateM_)
> import qualified Data.ByteString.Short.Internal as SB
> 
> udpStdoutServer :: IO ()
> udpStdoutServer = do
>   unhandled $ withSocket (Endpoint IPv4.loopback 0) $ \sock port -> do
>     BC.putStrLn ("Receiving datagrams on 127.0.0.1:" <> BC.pack (show port))
>     replicateM_ 10 $ do
>     DIU.Message sender (ByteArray contents) <- unhandled (DIU.receive sock 1024)
>       BC.putStrLn ("Datagram from " <> BC.pack (show sender))
>       BC.putStr (SB.fromShort (SB.SBS contents))
> 
> unhandled :: Exception e => IO (Either e a) -> IO a
> unhandled action = action >>= either throwIO pure

-}


touchByteArray :: ByteArray -> IO ()
touchByteArray (PM.ByteArray x) = touchByteArray# x

touchByteArray# :: ByteArray# -> IO ()
touchByteArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)

intToCInt :: Int -> CInt
intToCInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

