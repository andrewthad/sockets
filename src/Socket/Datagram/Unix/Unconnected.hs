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
module Socket.Datagram.Unix.Unconnected
  ( -- * Types
    Socket(..)
  , Family(..)
  , Connectedness(..)
  , UnixAddress(..)
  , Message(..)
    -- * Establish
  , withSocket
  , open
  , close
    -- * Exceptions
  , SocketException(..)
  ) where

import Control.Exception (mask,onException)
import Foreign.C.Error (Errno(..),eACCES)
import Foreign.C.Error (eNFILE,eMFILE,eADDRINUSE)
import Socket (Connectedness(..),Family(..))
import Socket.Datagram (Socket(..))
import Socket.Datagram (SocketException(..))
import Socket.IPv4 (Message(..))
import Socket.Error (die)
import Socket.Datagram.Common (close)
import Socket.Datagram.Unix.Connected (UnixAddress(..))

import qualified Foreign.C.Error.Describe as D
import qualified Linux.Socket as L
import qualified Posix.Socket as S
import qualified Socket as SCK
import qualified Socket.EventManager as EM

open ::
     UnixAddress
     -- ^ Address and port to use
  -> IO (Either SocketException (Socket 'Unconnected 'Unix))
open (UnixAddress path) = do
  e1 <- S.uninterruptibleSocket S.unix
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.datagram)
    S.defaultProtocol
  case e1 of
    Left err -> handleSocketException err
    Right fd -> do
      let !mngr = EM.manager
      EM.register mngr fd
      let sockAddr = id
            $ S.encodeSocketAddressUnix
            $ S.SocketAddressUnix
            $ path
      e2 <- S.uninterruptibleBind fd sockAddr
      case e2 of
        Left err -> do
          -- We intentionally discard any exceptions thrown by close.
          -- There is simply nothing that can be done with them.
          S.uninterruptibleErrorlessClose fd
          handleBindException err
        Right _ -> pure (Right (Socket fd))

-- | Open a socket and run the supplied callback on it. This closes the socket
-- when the callback finishes or when an exception is thrown. Do not return 
-- the socket from the callback. This leads to undefined behavior. If the
-- address @0.0.0.0@ is used, the socket receives on all network interfaces.
-- If the port 0 is used, an unused port is chosen by the operating system.
-- The callback provides the chosen port (or if the user specified a non-zero
-- port, the chosen port will be that value).
withSocket ::
     UnixAddress
     -- ^ Address and port to use
  -> (Socket 'Unconnected ('SCK.Internet 'SCK.V4) -> IO a)
     -- ^ Callback providing the socket and the chosen port
  -> IO (Either SocketException a)
withSocket local f = mask $ \restore -> open local >>= \case
  Left err -> pure (Left err)
  Right (Socket fd) -> do
    a <- onException (restore (f (Socket fd))) (S.uninterruptibleErrorlessClose fd)
    S.uninterruptibleClose fd >>= \case
      Left err -> die ("Socket.Datagram.IPv4.Unconnected.close: " ++ describeErrorCode err)
      Right _ -> pure (Right a)

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

handleSocketException :: Errno -> IO (Either SocketException a)
handleSocketException e
  | e == eMFILE = pure (Left SocketFileDescriptorLimit)
  | e == eNFILE = pure (Left SocketFileDescriptorLimit)
  | otherwise = die
      ("Socket.Datagram.IPv4.Undestined.socket: " ++ describeErrorCode e)

handleBindException :: Errno -> IO (Either SocketException a)
handleBindException !e
  | e == eACCES = pure (Left SocketPermissionDenied)
  | e == eADDRINUSE = pure (Left SocketAddressInUse)
  | otherwise = die
      ("Socket.Datagram.IPv4.Undestined.bind: " ++ describeErrorCode e)

