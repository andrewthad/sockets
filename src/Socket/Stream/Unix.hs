{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Socket.Stream.Unix
  ( -- * Types
    Listener(..)
  , Connection(..)
  , UnixAddress(..)
    -- * Bracketed
  , withListener
  , withAccepted
    -- * Exceptions
  , SendException(..)
  , ReceiveException(..)
  , ConnectException(..)
  , SocketException(..)
  , AcceptException(..)
  , CloseException(..)
  , SystemdException(..)
    -- * Bracketed
  , interruptibleForkAcceptedUnmasked
    -- * Unbracketed
    -- $unbracketed
  , listen
  , unlisten
  , unlisten_
  , disconnect
  , disconnect_
  , accept
  , systemdListener
  ) where

import Control.Concurrent (ThreadId,forkIOWithUnmask)
import Control.Concurrent.STM (TVar,modifyTVar',atomically)
import Control.Exception (mask, mask_, onException)
import Data.Coerce (coerce)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK, eNOTCONN)
import Foreign.C.Error (eADDRINUSE)
import Foreign.C.Error (eNFILE,eMFILE,eACCES,ePERM,eCONNABORTED)
import Socket.Datagram.Unix.Connected (UnixAddress(..))
import Socket.Error (die)
import Socket.Stream (SocketException(..),AcceptException(..),ConnectException(..))
import Socket.Stream (Connection(..))
import Socket.Stream (SendException(..),ReceiveException(..),CloseException(..))
import System.Posix.Types (Fd(Fd))
import Socket (Interruptibility(..))
import Socket.Systemd (SystemdException(..),systemdListenerInternal)
import qualified Control.Concurrent.STM as STM
import qualified Foreign.C.Error.Describe as D
import qualified Socket.EventManager as EM
import qualified Posix.Socket as S
import qualified Linux.Socket as L
import qualified Data.Primitive as PM

-- | A socket that listens for incomming connections.
newtype Listener = Listener Fd

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
listen :: UnixAddress -> IO (Either SocketException Listener)
listen (UnixAddress path) = do
  e1 <- S.uninterruptibleSocket S.Unix
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.stream)
    S.defaultProtocol
  case e1 of
    Left err -> handleSocketListenException err
    Right fd -> do
      let sockAddr = id
            $ S.encodeSocketAddressUnix
            $ S.SocketAddressUnix
            $ path
      e2 <- S.uninterruptibleBind fd sockAddr
      case e2 of
        Left err -> do
          _ <- S.uninterruptibleClose fd
          handleBindListenException err
        Right _ -> S.uninterruptibleListen fd 16 >>= \case
          -- We hardcode the listen backlog to 16. The author is unfamiliar
          -- with use cases where gains are realized from tuning this parameter.
          -- Open an issue if this causes problems for anyone.
          Left err -> do
            _ <- S.uninterruptibleClose fd
            handleBindListenException err
          Right _ -> do
            let !mngr = EM.manager
            EM.register mngr fd
            pure (Right (Listener fd))

-- These are the exceptions that can happen as a result
-- of calling @bind@ with the intent of using the socket
-- to listen for inbound connections. This is also used
-- to clean up the error codes of @listen@. The two can
-- report some of the same error codes, and those happen
-- to be the error codes we are interested in.
--
-- NB: EACCES only happens on @bind@, not on @listen@.
handleBindListenException :: Errno -> IO (Either SocketException a)
handleBindListenException !e
  | e == eACCES = pure (Left SocketPermissionDenied)
  | e == eADDRINUSE = pure (Left SocketAddressInUse)
  | otherwise = die
      ("Socket.Stream.Unix.bindListen: " ++ describeErrorCode e)

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

-- These are the exceptions that can happen as a result
-- of calling @socket@ with the intent of using the socket
-- to listen for inbound connections.
handleSocketListenException :: Errno -> IO (Either SocketException a)
handleSocketListenException e@(Errno n)
  | e == eMFILE = pure (Left SocketFileDescriptorLimit)
  | e == eNFILE = pure (Left SocketFileDescriptorLimit)
  | otherwise = die
      ("Socket.Stream.Unix.listen: " ++ D.string e ++ " (" ++ show n ++ ")")

-- | Open a socket that is used to listen for inbound connections.
withListener ::
     UnixAddress
  -> (Listener -> IO a)
  -> IO (Either SocketException a)
withListener !endpoint f = mask $ \restore -> do
  listen endpoint >>= \case
    Left err -> pure (Left err)
    Right sck -> do
      a <- onException
        (restore (f sck))
        (unlisten_ sck)
      unlisten sck
      pure (Right a)

-- | Close a listener. This throws an unrecoverable exception if
--   the socket cannot be closed.
unlisten :: Listener -> IO ()
unlisten (Listener fd) = S.uninterruptibleClose fd >>= \case
  Left _ -> die "Socket.Stream.Unix.unlisten"
  Right _ -> pure ()

-- | Close a listener. This does not check to see whether or not
-- the operating system successfully closed the socket. It never
-- throws exceptions of any kind. This should only be preferred
-- to 'unlistener' in exception-cleanup contexts where there is
-- already an exception that will be rethrown. See the implementation
-- of 'withListener' for an example of appropriate use of both
-- 'unlistener' and 'unlistener_'.
unlisten_ :: Listener -> IO ()
unlisten_ (Listener fd) = S.uninterruptibleErrorlessClose fd

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

-- We use the maybe to mean that the user needs to wait again.
waitlessAccept :: Fd -> IO (Either (Maybe (AcceptException i)) Connection)
waitlessAccept lstn = do
  L.uninterruptibleAccept4 lstn 200 (L.closeOnExec <> L.nonblocking) >>= \case
    Left err -> handleAcceptException err
    Right (_,_,acpt) -> pure (Right (Connection acpt))

-- These are the exceptions that can happen as a result
-- of calling @accept@.
handleAcceptException :: Errno -> IO (Either (Maybe (AcceptException i)) a)
handleAcceptException e
  | e == eAGAIN = pure (Left Nothing)
  | e == eWOULDBLOCK = pure (Left Nothing)
  | e == eCONNABORTED = pure (Left (Just AcceptConnectionAborted))
  | e == eMFILE = pure (Left (Just AcceptFileDescriptorLimit))
  | e == eNFILE = pure (Left (Just AcceptFileDescriptorLimit))
  | e == ePERM = pure (Left (Just AcceptFirewalled))
  | otherwise = die ("Socket.Stream.IPv4.accept: " ++ describeErrorCode e)

-- | Close a connection gracefully, reporting a 'CloseException' when
-- the connection has to be terminated by sending a TCP reset. This
-- uses a combination of @shutdown@, @recv@, @close@ to detect when
-- resets need to be sent.
disconnect :: Connection -> IO (Either CloseException ())
disconnect (Connection fd) = gracefulCloseA fd

gracefulCloseA :: Fd -> IO (Either CloseException ())
gracefulCloseA fd = do
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
      then gracefulCloseB fd
      else do
        S.uninterruptibleErrorlessClose fd
        die "Socket.Stream.Unix.gracefulCloseA"
    Right _ -> gracefulCloseB fd

gracefulCloseB :: Fd -> IO (Either CloseException ())
gracefulCloseB !fd = do
  !buf <- PM.newByteArray 1
  -- We do not actually want to remove the bytes from the
  -- receive buffer, so we use MSG_PEEK. We are certain
  -- to send a reset when a CloseException is reported.
  -- Retrospective: Why is MSG_PEEK important? Who cares if the
  -- bytes get eaten? The receive buffer is about to get axed anyway.
  S.uninterruptibleReceiveMutableByteArray fd buf 0 1 S.peek >>= \case
    Left err1 -> if err1 == eWOULDBLOCK || err1 == eAGAIN || err1 == eNOTCONN
      then do
        _ <- S.uninterruptibleClose fd
        pure (Right ())
      else do
        _ <- S.uninterruptibleClose fd
        -- We treat all @recv@ errors except for the nonblocking
        -- notices as unrecoverable.
        die "Socket.Stream.Unix.gracefulCloseB"
    Right sz -> if sz == 0
      then S.uninterruptibleClose fd >>= \case
        Left _ -> die "Socket.Stream.Unix.gracefulCloseB"
        Right _ -> pure (Right ())
      else do
        _ <- S.uninterruptibleClose fd
        pure (Left ClosePeerContinuedSending)

-- | Close a connection. This does not check to see whether or not
-- the connection was brought down gracefully. It just calls @close@
-- and is likely to cause a TCP reset to be sent. It never
-- throws exceptions of any kind (even if @close@ fails).
-- This should only be preferred
-- to 'disconnect' in exception-cleanup contexts where there is
-- already an exception that will be rethrown. See the implementation
-- of 'withConnection' for an example of appropriate use of both
-- 'disconnect' and 'disconnect_'.
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

-- | Accept a connection on the listener and run the supplied callback in
-- a new thread. The masking state is set to @Unmasked@ when running the
-- callback. Typically, @a@ is instantiated to @()@.
interruptibleForkAcceptedUnmasked ::
     TVar Int
     -- ^ Connection counter. Incremented when connection
     --   is accepted. Decremented after connection is closed.
  -> TVar Bool
     -- ^ Interrupted. If this becomes 'True' give up and return
     --   @'Left' 'AcceptInterrupted'@.
  -> Listener
     -- ^ Connection listener
  -> (Either CloseException () -> a -> IO ())
     -- ^ Callback to handle an ungraceful close. This must not
     --   throw an exception.
  -> (Connection -> IO a)
     -- ^ Callback to consume connection. Must not return the connection.
  -> IO (Either (AcceptException 'Interruptible) ThreadId)
interruptibleForkAcceptedUnmasked !counter !abandon !lstn consumeException cb =
  mask_ $ interruptibleAcceptCounting counter abandon lstn >>= \case
    Left e -> pure (Left e)
    Right conn -> fmap Right $ forkIOWithUnmask $ \unmask -> do
      a <- onException
        (unmask (cb conn))
        (disconnect_ conn *> atomically (modifyTVar' counter (subtract 1)))
      e <- disconnect conn
      r <- unmask (consumeException e a)
      atomically (modifyTVar' counter (subtract 1))
      pure r

-- Only used internally
interruptibleAcceptCounting :: 
     TVar Int
  -> TVar Bool
  -> Listener
  -> IO (Either (AcceptException 'Interruptible) Connection)
interruptibleAcceptCounting !counter !abandon (Listener !fd) = do
  -- TODO: pull these out of the loop
  let !mngr = EM.manager
  tv <- EM.reader mngr fd
  token <- EM.interruptibleWaitCounting counter abandon tv
  if EM.isInterrupt token
    then pure (Left AcceptInterrupted)
    else waitlessAccept fd >>= \case
      Left merr -> case merr of
        Nothing -> do
          EM.unready token tv
          -- Decrement the connection counter if the notification
          -- from epoll was a false alarm.
          atomically (modifyTVar' counter (subtract 1))
          interruptibleAcceptCounting counter abandon (Listener fd)
        Just err -> pure (Left err)
      Right r@(Connection conn) -> do
        EM.register mngr conn
        pure (Right r)

-- | Retrieve a listener that systemd has passed to the process. This
-- may only be called once. There is no bracketed variant
-- of this function because the listener is expected to remain open for
-- the remainder of the application.
--
-- There are several reasons this function may return @Left@:
--
-- * @sd_listen_fds@ returned more than one file descriptor
-- * @sd_is_socket@ found that the file descriptor was not a socket or
--   that it was a socket that was not in listening mode.
systemdListener :: IO (Either SystemdException Listener)
systemdListener = coerce (systemdListenerInternal S.Unix)
