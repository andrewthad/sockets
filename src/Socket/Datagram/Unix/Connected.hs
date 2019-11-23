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

-- | Unix-domain datagram sockets with a fixed destination.
module Socket.Datagram.Unix.Connected
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
  , ConnectException(..)
  , SD.ReceiveException(..)
  , SD.SendException(..)
    -- * Examples
    -- $examples
  ) where

import Control.Exception (mask,onException)
import Data.Primitive (ByteArray)
import Foreign.C.Error (Errno(..),eNOENT,ePROTOTYPE)
import Foreign.C.Error (eNFILE,eMFILE,eCONNREFUSED)
import Socket (Connectedness(..),Family(..))
import Socket.Datagram (Socket(..))
import Socket.Stream (ConnectException(..))
import Socket.Error (die)
import Socket.IPv4 (Message(..))
import Socket.Datagram.Common (close)
import Socket (Interruptibility(Uninterruptible))

import qualified Foreign.C.Error.Describe as D
import qualified Linux.Socket as L
import qualified Posix.Socket as S
import qualified Socket as SCK
import qualified Socket.EventManager as EM
import qualified Socket.Datagram as SD

newtype UnixAddress = UnixAddress ByteArray

-- | Unbracketed function for opening a socket. Be careful with
-- this function. These are UNIX-domain datagram sockets on which
-- @connect@ was called without first calling @bind@. This means
-- that they correspond neither to an entry in the filesystem nor
-- an entry in the abstract socket namespace.
open ::
     UnixAddress
  -> IO (Either (ConnectException 'Unix 'Uninterruptible) (Socket 'Connected 'SCK.Unix))
open (UnixAddress remote) = do
  -- TODO: This is somewhat copied from the internet-domain
  -- socket code
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
            $ remote
      S.uninterruptibleConnect fd sockAddr >>= \case
        Left err -> handleConnectException err
        Right (_ :: ()) -> pure (Right (Socket fd))

withSocket ::
     UnixAddress
     -- ^ Peer address (to connect to)
  -> (Socket 'Connected ('SCK.Internet 'SCK.V4) -> IO a)
     -- ^ Callback providing the socket and the chosen port
  -> IO (Either (ConnectException 'Unix 'Uninterruptible) a)
withSocket !peer f = mask $ \restore -> open peer >>= \case
  Left err -> pure (Left err)
  Right (Socket fd) -> do
    a <- onException (restore (f (Socket fd))) (S.uninterruptibleErrorlessClose fd)
    S.uninterruptibleClose fd >>= \case
      Left err -> die ("Socket.Datagram.Unix.Connected.close: " ++ describeErrorCode err)
      Right _ -> pure (Right a)

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

handleSocketException :: Errno -> IO (Either (ConnectException 'Unix i) a)
handleSocketException e
  | e == eMFILE = pure (Left ConnectFileDescriptorLimit)
  | e == eNFILE = pure (Left ConnectFileDescriptorLimit)
  | otherwise = die
      ("Socket.Datagram.Unix.Connected.socket: " ++ describeErrorCode e)

handleConnectException :: Errno -> IO (Either (ConnectException 'Unix i)  a)
handleConnectException e
  | e == eNOENT = pure (Left ConnectNoEntry)
  | e == ePROTOTYPE = pure (Left ConnectProtocolType)
  | e == eCONNREFUSED = pure (Left ConnectRefused)
  | otherwise = die
      ("Socket.Datagram.Unix.Connected.connect: " ++ describeErrorCode e)