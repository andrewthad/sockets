{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language UnboxedTuples #-}
{-# language ScopedTypeVariables #-}

-- | Internet datagram sockets with a fixed destination.
module Socket.Datagram.IPv4.Connected
  ( -- * Types
    Socket(..)
  , Family(..)
  , Connectedness(..)
  , Peer(..)
  , Message(..)
    -- * Establish
  , withSocket
  , open
  , close
    -- * Exceptions
  , SocketException(..)
  , SD.ReceiveException(..)
  , SD.SendException(..)
    -- * Examples
    -- $examples
  ) where

import Control.Exception (mask,onException)
import Data.Word (Word16)
import Foreign.C.Error (Errno(..),eACCES)
import Foreign.C.Error (eNFILE,eMFILE,eADDRINUSE)
import Net.Types (IPv4(..))
import Socket (Connectedness(..),Family(..))
import Socket.Datagram (Socket(..))
import Socket.Datagram (SocketException(..))
import Socket.IPv4 (Peer(..),Message(..))
import Socket.Error (die)
import Socket.Datagram.Common (close)

import qualified Foreign.C.Error.Describe as D
import qualified Linux.Socket as L
import qualified Posix.Socket as S
import qualified Socket as SCK
import qualified Socket.EventManager as EM
import qualified Socket.Datagram as SD

-- | Unbracketed function for opening a socket. Be careful with
-- this function. Only call this in contexts where exceptions are
-- masked.
--
-- TODO: Rewrite this. Stop calling bind. Do not make the user
-- supply a local address.
open ::
     Peer
  -> Peer
  -> IO (Either SocketException (Socket 'Connected ('SCK.Internet 'SCK.V4), Word16))
open local@Peer{port = specifiedPort} !peer = do
  -- TODO: This is mostly copied from the unconnected withSocket.
  e1 <- S.uninterruptibleSocket S.internet
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.datagram)
    S.defaultProtocol
  case e1 of
    Left err -> handleSocketException err
    Right fd -> do
      let !mngr = EM.manager
      EM.register mngr fd
      e2 <- S.uninterruptibleBind fd
        (S.encodeSocketAddressInternet (endpointToSocketAddressInternet local))
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
                die ("Socket.Datagram.IPv4.Connected.getsockname: " ++ describeErrorCode err)
              Right (sockAddrRequiredSz,sockAddr) -> if sockAddrRequiredSz == S.sizeofSocketAddressInternet
                then case S.decodeSocketAddressInternet sockAddr of
                  Just S.SocketAddressInternet{port = actualPort} -> do
                    let cleanPort = S.networkToHostShort actualPort
                    pure (Right cleanPort)
                  Nothing -> do
                    S.uninterruptibleErrorlessClose fd
                    die "Socket.Datagram.IPv4.Connected: non-internet socket family"
                else do
                  S.uninterruptibleErrorlessClose fd
                  die "Socket.Datagram.IPv4.Connected: socket address size"
            else pure (Right specifiedPort)
          case eactualPort of
            Left err -> pure (Left err)
            Right actualPort -> do
              let sockAddr = id
                    $ S.encodeSocketAddressInternet
                    $ endpointToSocketAddressInternet
                    $ peer
              S.uninterruptibleConnect fd sockAddr >>= \case
                Left err -> 
                  die ("Socket.Datagram.IPv4.Connected.connect: " ++ describeErrorCode err)
                Right (_ :: ()) -> pure (Right (Socket fd, actualPort))

withSocket ::
     Peer
     -- ^ Address and port to bind to
  -> Peer
     -- ^ Peer address and port
  -> (Socket 'Connected ('SCK.Internet 'SCK.V4) -> Word16 -> IO a)
     -- ^ Callback providing the socket and the chosen port
  -> IO (Either SocketException a)
withSocket !local !peer f = mask $ \restore -> open local peer >>= \case
  Left err -> pure (Left err)
  Right (Socket fd,actualPort) -> do
    a <- onException (restore (f (Socket fd) actualPort)) (S.uninterruptibleErrorlessClose fd)
    S.uninterruptibleClose fd >>= \case
      Left err -> die ("Socket.Datagram.IPv4.Undestined.close: " ++ describeErrorCode err)
      Right _ -> pure (Right a)

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

endpointToSocketAddressInternet :: Peer -> S.SocketAddressInternet
endpointToSocketAddressInternet (Peer {address, port}) = S.SocketAddressInternet
  { port = S.hostToNetworkShort port
  , address = S.hostToNetworkLong (getIPv4 address)
  }

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
