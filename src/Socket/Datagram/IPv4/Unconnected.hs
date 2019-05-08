{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language UnboxedTuples #-}

-- | Internet datagram sockets without a fixed destination.
module Socket.Datagram.IPv4.Unconnected
  ( -- * Types
    Socket(..)
  , Peer(..)
  , Message(..)
    -- * Establish
  , withSocket
    -- * Exceptions
  , SocketException(..)
    -- * Examples
    -- $examples
  ) where

import Control.Concurrent (threadWaitWrite,threadWaitRead)
import Control.Concurrent.STM (TVar)
import Control.Exception (throwIO,mask,onException)
import Data.Primitive (ByteArray,MutableByteArray(..))
import Data.Word (Word16)
import Foreign.C.Error (Errno(..),eWOULDBLOCK,eAGAIN,eACCES)
import Foreign.C.Error (eNFILE,eMFILE,eADDRINUSE)
import Foreign.C.Types (CInt,CSize)
import GHC.Exts (Int(I#),RealWorld,shrinkMutableByteArray#,ByteArray#,touch#)
import GHC.IO (IO(..))
import Net.Types (IPv4(..))
import Socket (Interruptibility(..))
import Socket (cgetsockname,Connectedness(Unconnected))
import Socket.Datagram (Socket(..),SendException(..),ReceiveException(..))
import Socket.Datagram (SocketException(..))
import Socket.Debug (debug)
import Socket.EventManager (Token)
import Socket.IPv4 (Peer(..),Message(..),describeEndpoint)
import Socket.Error (die)

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Primitive as PM
import qualified Data.Primitive as PM
import qualified Foreign.C.Error.Describe as D
import qualified Linux.Socket as L
import qualified Posix.Socket as S
import qualified Socket as SCK
import qualified Socket.EventManager as EM

-- | Open a socket and run the supplied callback on it. This closes the socket
-- when the callback finishes or when an exception is thrown. Do not return 
-- the socket from the callback. This leads to undefined behavior. If the
-- address @0.0.0.0@ is used, the socket receives on all network interfaces.
-- If the port 0 is used, an unused port is chosen by the operating system.
-- The callback provides the chosen port (or if the user specified a non-zero
-- port, the chosen port will be that value).
withSocket ::
     Peer
     -- ^ Address and port to use
  -> (Socket 'Unconnected 'SCK.IPv4 -> Word16 -> IO a)
     -- ^ Callback providing the socket and the chosen port
  -> IO (Either SocketException a)
withSocket endpoint@Peer{port = specifiedPort} f = mask $ \restore -> do
  debug ("withSocket: opening socket " ++ describeEndpoint endpoint)
  e1 <- S.uninterruptibleSocket S.internet
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.datagram)
    S.defaultProtocol
  debug ("withSocket: opened socket " ++ describeEndpoint endpoint)
  case e1 of
    Left err -> handleSocketException err
    Right fd -> do
      let !mngr = EM.manager
      EM.register mngr fd
      e2 <- S.uninterruptibleBind fd
        (S.encodeSocketAddressInternet (endpointToSocketAddressInternet endpoint))
      debug ("withSocket: requested binding for " ++ describeEndpoint endpoint)
      case e2 of
        Left err -> do
          -- We intentionally discard any exceptions thrown by close.
          -- There is simply nothing that can be done with them.
          S.uninterruptibleErrorlessClose fd
          handleBindException specifiedPort err
        Right _ -> do
          eactualPort <- if specifiedPort == 0
            then S.uninterruptibleGetSocketName fd S.sizeofSocketAddressInternet >>= \case
              Left err -> do
                S.uninterruptibleErrorlessClose fd
                die ("Socket.Datagram.IPv4.Undestined.getsockname: " ++ describeErrorCode err)
              Right (sockAddrRequiredSz,sockAddr) -> if sockAddrRequiredSz == S.sizeofSocketAddressInternet
                then case S.decodeSocketAddressInternet sockAddr of
                  Just S.SocketAddressInternet{port = actualPort} -> do
                    let cleanPort = S.networkToHostShort actualPort
                    debug ("withSocket: successfully bound " ++ describeEndpoint endpoint ++ " and got port " ++ show cleanPort)
                    pure (Right cleanPort)
                  Nothing -> do
                    S.uninterruptibleErrorlessClose fd
                    die "Socket.Datagram.IPv4.Undestined: non-internet socket family"
                else do
                  S.uninterruptibleErrorlessClose fd
                  die "Socket.Datagram.IPv4.Undestined: socket address size"
            else pure (Right specifiedPort)
          case eactualPort of
            Left err -> pure (Left err)
            Right actualPort -> do
              a <- onException (restore (f (Socket fd) actualPort)) (S.uninterruptibleErrorlessClose fd)
              S.uninterruptibleClose fd >>= \case
                Left err -> die ("Socket.Datagram.IPv4.Undestined.close: " ++ describeErrorCode err)
                Right _ -> pure (Right a)


endpointToSocketAddressInternet :: Peer -> S.SocketAddressInternet
endpointToSocketAddressInternet (Peer {address, port}) = S.SocketAddressInternet
  { port = S.hostToNetworkShort port
  , address = S.hostToNetworkLong (getIPv4 address)
  }

socketAddressInternetToEndpoint :: S.SocketAddressInternet -> Peer
socketAddressInternetToEndpoint (S.SocketAddressInternet {address,port}) = Peer
  { address = IPv4 (S.networkToHostLong address)
  , port = S.networkToHostShort port
  }

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
>   unhandled $ withSocket (Peer IPv4.loopback 0) $ \sock port -> do
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

moduleSocketDatagramIPv4Undestined :: String
moduleSocketDatagramIPv4Undestined = "Socket.Datagram.IPv4.Undestined"

functionReceive :: String
functionReceive = "receive"

functionSend :: String
functionSend = "send"

functionSendMutableByteArray :: String
functionSendMutableByteArray = "sendMutableByteArray"

functionReceiveMutableByteArray :: String
functionReceiveMutableByteArray = "receiveMutableByteArray"

functionWithSocket :: String
functionWithSocket = "withSocket"

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

handleSocketException :: Errno -> IO (Either SocketException a)
handleSocketException e
  | e == eMFILE = pure (Left SocketFileDescriptorLimit)
  | e == eNFILE = pure (Left SocketFileDescriptorLimit)
  | otherwise = die
      ("Socket.Datagram.IPv4.Undestined.socket: " ++ describeErrorCode e)

handleBindException :: Word16 -> Errno -> IO (Either SocketException a)
handleBindException !thePort !e
  | e == eACCES = pure (Left SocketPermissionDenied)
  | e == eADDRINUSE = if thePort == 0
      then pure (Left SocketAddressInUse)
      else pure (Left SocketEphemeralPortsExhausted)
  | otherwise = die
      ("Socket.Datagram.IPv4.Undestined.bind: " ++ describeErrorCode e)
