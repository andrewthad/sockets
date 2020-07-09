{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}
{-# language UnboxedTuples #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

-- | Unix-domain @seqpacket@ sockets.
module Socket.SequencedPacket.Unix
  ( Connection(..)
  , Listener(..)
  , BindException(..)
  , openPair
  , withPair
  , listen
  , unlisten
  , unlisten_
  , withListener
  , connect
  , tryConnect
  , withConnection
  , withAccepted
  , accept
  , disconnect
  , disconnect_
  ) where

import Control.Concurrent.STM (TVar)
import Control.Exception (mask,onException)
import Data.Coerce (coerce)
import Data.Primitive (ByteArray)
import Foreign.C.Error (Errno(..),eNOENT,ePROTOTYPE,eAGAIN,eWOULDBLOCK)
import Foreign.C.Error (eNFILE,eMFILE,eCONNREFUSED,eNOTCONN)
import Foreign.C.Error (eADDRINUSE,eACCES,eCONNABORTED,ePERM)
import Socket (Connectedness(..),Family(..),SocketException(..),BindException(..))
import Socket.Datagram (Socket(..))
import Socket.Stream (ConnectException(..),CloseException(..),AcceptException(..))
import Socket.Error (die)
import Socket.IPv4 (Message(..))
import Socket.Datagram.Common (close)
import Socket (Interruptibility(Uninterruptible))
import Socket.Datagram.Unix.Connected (UnixAddress(..))
import System.Posix.Types (Fd)

import qualified Control.Concurrent.STM as STM
import qualified Data.Primitive as PM
import qualified Foreign.C.Error.Describe as D
import qualified Linux.Socket as L
import qualified Posix.Socket as S
import qualified Socket as SCK
import qualified Socket.EventManager as EM
import qualified Socket.Pair as Pair

newtype Connection = Connection Fd

newtype Listener = Listener Fd

-- | Unbracketed function for opening a connected socket pair. All warnings
-- that apply to 'open' apply to this function as well.
openPair :: IO (Either SocketException (Connection, Connection))
openPair = coerce
  @(IO (Either SocketException (Fd,Fd)))
  @(IO (Either SocketException (Connection,Connection)))
  (Pair.open S.sequencedPacket)

withPair ::
     (Connection -> Connection -> IO a)
     -- ^ Callback providing the connected datagram sockets
  -> IO (Either SocketException a)
withPair f = mask $ \restore -> openPair >>= \case
  Left err -> pure (Left err)
  Right (Connection fdA, Connection fdB) -> do
    a <- onException
      (restore (f (Connection fdA) (Connection fdB)))
      (S.uninterruptibleErrorlessClose fdA *> S.uninterruptibleErrorlessClose fdB)
    S.uninterruptibleClose fdA >>= \case
      Left err -> die ("Socket.Datagram.Unix.Connected.close: " ++ describeErrorCode err)
      Right _ -> S.uninterruptibleClose fdB >>= \case
        Left err -> die ("Socket.Datagram.Unix.Connected.close: " ++ describeErrorCode err)
        Right _ -> pure (Right a)

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

-- | Open a socket that is used to listen for inbound connections.
withListener ::
     UnixAddress -- ^ Address to bind to (path or abstract namespace name)
  -> (Listener -> IO a) -- ^ Callback
  -> IO (Either (BindException 'Unix) a)
withListener endpoint f = mask $ \restore -> do
  listen endpoint >>= \case
    Left err -> pure (Left err)
    Right sck -> do
      a <- onException (restore (f sck)) (unlisten_ sck)
      unlisten sck
      pure (Right a)

-- | Open a socket that can be used to listen for inbound connections.
-- Requirements:
--
-- * This function may only be called in contexts where exceptions
--   are masked.
-- * The caller /must/ be sure to call 'unlistener' on the resulting
--   'Listener' exactly once to close underlying file descriptor.
-- * The 'Listener' cannot be used after being given as an argument
--   to 'unlistener'.
--
-- Noncompliant use of this function leads to undefined behavior. Prefer
-- 'withListener' unless you are writing an integration with a
-- resource-management library.
listen :: UnixAddress -> IO (Either (BindException 'Unix) Listener)
listen (UnixAddress remote) = do
  e1 <- S.uninterruptibleSocket S.Unix
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.sequencedPacket)
    S.defaultProtocol
  case e1 of
    Left err -> handleSocketListenException err
    Right fd -> do
      let sockAddr = id
            $ S.encodeSocketAddressUnix
            $ S.SocketAddressUnix
            $ remote
      S.uninterruptibleBind fd sockAddr >>= \case
        Left err -> do
          S.uninterruptibleErrorlessClose fd
          handleBindListenException err
        Right _ -> S.uninterruptibleListen fd 16 >>= \case
          -- We hardcode the listen backlog to 16. The author is unfamiliar
          -- with use cases where gains are realized from tuning this parameter.
          -- Open an issue if this causes problems for anyone.
          Left err -> do
            _ <- S.uninterruptibleClose fd
            handleBindListenException err
          Right _ -> do
            -- The getsockname is copied from code in Socket.Datagram.IPv4.Undestined.
            -- Consider factoring this out.
            let !mngr = EM.manager
            EM.register mngr fd
            pure (Right (Listener fd))

-- | Open a socket and connect to a peer. Requirements:
--
-- * This function may only be called in contexts where exceptions
--   are masked.
-- * The caller /must/ be sure to call 'disconnect' or 'disconnect_'
--   on the resulting 'Connection' exactly once to close underlying
--   file descriptor.
-- * The 'Connection' cannot be used after being given as an argument
--   to 'disconnect' or 'disconnect_'.
--
-- Noncompliant use of this function leads to undefined behavior. Prefer
-- 'withConnection' unless you are writing an integration with a
-- resource-management library.
connect ::
     UnixAddress
     -- ^ Peer address
  -> IO (Either (ConnectException 'Unix 'Uninterruptible) Connection)
connect (UnixAddress !remote) = do
  e1 <- S.uninterruptibleSocket S.Unix
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.sequencedPacket)
    S.defaultProtocol
  case e1 of
    Left err -> handleConnectException err
    Right fd -> do
      let sockAddr = id
            $ S.encodeSocketAddressUnix
            $ S.SocketAddressUnix
            $ remote
      let !mngr = EM.manager
      -- TODO: I believe it is sound to make both the write and
      -- read channels start off as not ready. After all, the
      -- socket is brand new and is not connected to a peer.
      -- Consequently, there's no way we could miss events.
      EM.register mngr fd
      -- This is currently wrong. Redo this later.
      S.uninterruptibleConnect fd sockAddr >>= \case
        Left err2 -> do
          S.uninterruptibleErrorlessClose fd
          handleConnectException err2
        Right _ -> pure (Right (Connection fd))

-- | Variant of 'connect' that does not block. Returns 'Nothing' if
-- the connection cannot be established immidiately.
tryConnect ::
     UnixAddress
     -- ^ Peer address
  -> IO (Either (ConnectException 'Unix 'Uninterruptible) (Maybe Connection))
tryConnect (UnixAddress !remote) = do
  e1 <- S.uninterruptibleSocket S.Unix
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.sequencedPacket)
    S.defaultProtocol
  case e1 of
    Left err -> handleConnectException err
    Right fd -> do
      let sockAddr = id
            $ S.encodeSocketAddressUnix
            $ S.SocketAddressUnix
            $ remote
      let !mngr = EM.manager
      -- TODO: I believe it is sound to make both the write and
      -- read channels start off as not ready. After all, the
      -- socket is brand new and is not connected to a peer.
      -- Consequently, there's no way we could miss events.
      EM.register mngr fd
      -- This is currently wrong. Redo this later.
      S.uninterruptibleConnect fd sockAddr >>= \case
        Left err2 -> do
          S.uninterruptibleErrorlessClose fd
          if | err2 == eAGAIN -> pure (Right Nothing)
             | err2 == eWOULDBLOCK -> pure (Right Nothing)
             | otherwise -> handleConnectException err2
        Right _ -> pure (Right (Just (Connection fd)))

-- These are the exceptions that can happen as a result
-- of calling @socket@ with the intent of using the socket
-- to listen for inbound connections.
handleSocketListenException :: Errno -> IO (Either (BindException 'Unix) a)
handleSocketListenException e
  | e == eMFILE = pure (Left BindFileDescriptorLimit)
  | e == eNFILE = pure (Left BindFileDescriptorLimit)
  | otherwise = die ("Socket.SequencedPacket.Unix.socket: " ++ describeErrorCode e)

-- These are the exceptions that can happen as a result
-- of calling @bind@ with the intent of using the socket
-- to listen for inbound connections. This is also used
-- to clean up the error codes of @listen@. The two can
-- report some of the same error codes, and those happen
-- to be the error codes we are interested in.
--
-- NB: EACCES only happens on @bind@, not on @listen@.
handleBindListenException :: Errno -> IO (Either (BindException 'Unix) a)
handleBindListenException !e
  | e == eACCES = pure (Left BindPermissionDenied)
  | e == eADDRINUSE = pure (Left BindAddressInUse)
  | otherwise = die ("Socket.SequencedPacket.Unix.bindListen: " ++ describeErrorCode e)

handleConnectException :: Errno -> IO (Either (ConnectException 'Unix i)  a)
handleConnectException e
  | e == eNOENT = pure (Left ConnectNoEntry)
  | e == ePROTOTYPE = pure (Left ConnectProtocolType)
  | e == eCONNREFUSED = pure (Left ConnectRefused)
  | otherwise = die
      ("Socket.Datagram.Unix.Connected.connect: " ++ describeErrorCode e)

-- | Close a listener. This does not check to see whether or not
-- the operating system successfully closed the socket. It never
-- throws exceptions of any kind. This should only be preferred
-- to 'unlistener' in exception-cleanup contexts where there is
-- already an exception that will be rethrown. See the implementation
-- of 'withListener' for an example of appropriate use of both
-- 'unlistener' and 'unlistener_'.
unlisten_ :: Listener -> IO ()
unlisten_ (Listener fd) = S.uninterruptibleErrorlessClose fd

-- | Close a listener. This throws an unrecoverable exception if
--   the socket cannot be closed.
unlisten :: Listener -> IO ()
unlisten (Listener fd) = S.uninterruptibleClose fd >>= \case
  Left err -> die ("Socket.SequencedPacket.Unix.unlisten: " ++ describeErrorCode err)
  Right _ -> pure ()

-- | Establish a connection to a server.
withConnection ::
     UnixAddress
     -- ^ Peer address
  -> (Either CloseException () -> a -> IO b)
     -- ^ Callback to handle an ungraceful close. 
  -> (Connection -> IO a)
     -- ^ Callback to consume connection. Must not return the connection.
  -> IO (Either (ConnectException 'Unix 'Uninterruptible) b)
withConnection !remote g f = mask $ \restore -> do
  connect remote >>= \case
    Left err -> pure (Left err)
    Right conn -> do
      a <- onException (restore (f conn)) (disconnect_ conn)
      m <- disconnect conn
      b <- g m a
      pure (Right b)
    
-- | Close a connection gracefully, reporting a 'CloseException' when
-- the connection has to be terminated by sending a TCP reset. This
-- uses a combination of @shutdown@, @recv@, @close@ to detect when
-- resets need to be sent.
disconnect :: Connection -> IO (Either CloseException ())
disconnect (Connection fd) = gracefulCloseA fd

gracefulCloseA :: Fd -> IO (Either CloseException ())
gracefulCloseA fd = do
  let !mngr = EM.manager
  !tv <- EM.reader mngr fd
  token0 <- STM.readTVarIO tv
  S.uninterruptibleShutdown fd S.write >>= \case
    -- On Linux (not sure about others), calling shutdown
    -- on the write channel fails with with ENOTCONN if the
    -- write channel is already closed. It is common for this to
    -- happen (e.g. if the peer calls @close@ before the local
    -- process runs gracefulClose, the local operating system
    -- will have already closed the write channel). However,
    -- it does not pose a problem. We just proceed as we would
    -- have since either way we become certain that the write channel
    -- is closed.
    Left err -> if err == eNOTCONN
      then gracefulCloseB tv token0 fd
      else do
        S.uninterruptibleErrorlessClose fd
        die ("Socket.SequencedPacket.Unix.gracefulCloseA[shutdown]: " ++ describeErrorCode err)
    Right _ -> gracefulCloseB tv token0 fd

-- The second part of the shutdown function must call itself recursively
-- since we may receive false read-ready notifications at any time.
gracefulCloseB :: TVar EM.Token -> EM.Token -> Fd -> IO (Either CloseException ())
gracefulCloseB !tv !token0 !fd = do
  !buf <- PM.newByteArray 1
  S.uninterruptibleReceiveMutableByteArray fd buf 0 1 mempty >>= \case
    Left err1 -> if err1 == eWOULDBLOCK || err1 == eAGAIN
      then do
        token1 <- EM.persistentUnreadyAndWait token0 tv
        gracefulCloseB tv token1 fd
      else do
        _ <- S.uninterruptibleClose fd
        -- We treat all @recv@ errors except for the nonblocking
        -- notices as unrecoverable.
        die ("Socket.SequencedPacket.Unix.gracefulCloseB[recv]: " ++ describeErrorCode err1)
    Right sz -> if sz == 0
      then S.uninterruptibleClose fd >>= \case
        Left err -> 
          die ("Socket.SequencedPacket.Unix.gracefulCloseB[close]: " ++ describeErrorCode err)
        Right _ -> pure (Right ())
      else do
        _ <- S.uninterruptibleClose fd
        pure (Left ClosePeerContinuedSending)

-- | Close a connection. This does not check to see whether or not
-- the connection was brought down gracefully. It just calls @close@.
-- It never throws exceptions of any kind (even if @close@ fails).
-- This should only be preferred to 'disconnect' in exception-cleanup
-- contexts where there is already an exception that will be rethrown.
-- See the implementation of 'withConnection' for an example of
-- appropriate use of both 'disconnect' and 'disconnect_'.
disconnect_ :: Connection -> IO ()
disconnect_ (Connection fd) = S.uninterruptibleErrorlessClose fd

-- | Accept a connection on the listener and run the supplied callback
-- on it. This closes the connection when the callback finishes or if
-- an exception is thrown. Since this function blocks the thread until
-- the callback finishes, it is only suitable for stream socket clients
-- that handle one connection at a time. The variant 'forkAcceptedUnmasked'
-- is preferrable for servers that need to handle connections concurrently
-- (most use cases).
withAccepted ::
     Listener
  -> (Either CloseException () -> a -> IO b)
     -- ^ Callback to handle an ungraceful close. 
  -> (Connection -> IO a)
     -- ^ Callback to consume connection. Must not return the connection.
  -> IO (Either (AcceptException 'Uninterruptible) b)
withAccepted !lstn consumeException cb = do
  r <- mask $ \restore -> do
    accept lstn >>= \case
      Left e -> pure (Left e)
      Right conn -> do
        a <- onException (restore (cb conn)) (disconnect_ conn)
        e <- disconnect conn
        pure (Right (e,a))
  -- Notice that consumeException gets run in an unmasked context.
  case r of
    Left e -> pure (Left e)
    Right (e,a) -> fmap Right (consumeException e a)

-- | Listen for an inbound connection.
accept :: Listener -> IO (Either (AcceptException 'Uninterruptible) Connection)
accept (Listener !fd) = do
  -- Although this function must be called in a context where
  -- exceptions are masked, recall that EM.wait uses an STM
  -- action that might retry, meaning that this first part is
  -- still interruptible. This is a good thing in the case of
  -- this function.
  let !mngr = EM.manager
  -- The listener should already be registered, so we can just
  -- ask for the reader directly.
  !tv <- EM.reader mngr fd
  let go !oldToken = do
        waitlessAccept fd >>= \case
          Left merr -> case merr of
            Nothing -> EM.unreadyAndWait oldToken tv >>= go
            Just err -> pure (Left err)
          Right r@(Connection conn) -> do
            EM.register mngr conn
            pure (Right r)
  go =<< STM.readTVarIO tv

waitlessAccept :: Fd -> IO (Either (Maybe (AcceptException i)) Connection)
waitlessAccept !lstn = do
  -- TODO: add a variant of accept4 in posix-api that discards the peer address.
  L.uninterruptibleAccept4 lstn 128 (L.closeOnExec <> L.nonblocking) >>= \case
    Left err -> handleAcceptException err
    Right (_,_,acpt) -> pure (Right (Connection acpt))

-- These are the exceptions that can happen as a result
-- of calling @accept@.
-- TODO: There is no way a UNIX-domain connection could be firewalled.
-- Is EPERM even possible in this context?
handleAcceptException :: Errno -> IO (Either (Maybe (AcceptException i)) a)
handleAcceptException e
  | e == eAGAIN = pure (Left Nothing)
  | e == eWOULDBLOCK = pure (Left Nothing)
  | e == eCONNABORTED = pure (Left (Just AcceptConnectionAborted))
  | e == eMFILE = pure (Left (Just AcceptFileDescriptorLimit))
  | e == eNFILE = pure (Left (Just AcceptFileDescriptorLimit))
  | e == ePERM = pure (Left (Just AcceptFirewalled))
  | otherwise = die ("Socket.Stream.IPv4.accept: " ++ describeErrorCode e)
