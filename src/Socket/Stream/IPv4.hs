{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Socket.Stream.IPv4
  ( -- * Types
    Listener
  , Connection
  , Peer(..)
    -- * Bracketed
  , withListener
  , withAccepted
  , withConnection
  -- , interruptibleWithConnection
  , forkAccepted
  , forkAcceptedUnmasked
  , interruptibleForkAcceptedUnmasked
    -- * Exceptions
  , SendException(..)
  , ReceiveException(..)
  , ConnectException(..)
  , SocketException(..)
  , AcceptException(..)
  , CloseException(..)
  , Interruptibility(..)
    -- * Unbracketed
    -- $unbracketed
  , listen
  , unlisten
  , unlisten_
  , connect
  -- , interruptibleConnect
  , disconnect
  , disconnect_
  , accept
  , interruptibleAccept
  ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent (forkIO, forkIOWithUnmask)
import Control.Exception (mask, mask_, onException, throwIO)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM (TVar,modifyTVar')
import Data.Word (Word16)
import Foreign.C.Error (Errno(..), eAGAIN, eINPROGRESS, eWOULDBLOCK, eNOTCONN)
import Foreign.C.Error (eADDRINUSE)
import Foreign.C.Error (eNFILE,eMFILE,eACCES,ePERM,eCONNABORTED)
import Foreign.C.Error (eTIMEDOUT,eADDRNOTAVAIL,eNETUNREACH,eCONNREFUSED)
import Foreign.C.Types (CInt)
import Net.Types (IPv4(..))
import Socket (Interruptibility(..))
import Socket (SocketUnrecoverableException(..))
import Socket (cgetsockname,cclose)
import Socket.Error (die)
import Socket.Debug (debug)
import Socket.IPv4 (Peer(..),describeEndpoint)
import Socket.Stream (ConnectException(..),SocketException(..),AcceptException(..))
import Socket.Stream (SendException(..),ReceiveException(..),CloseException(..))
import Socket.Stream (Connection(..))
import System.Posix.Types(Fd)

import qualified Control.Concurrent.STM as STM
import qualified Data.Primitive as PM
import qualified Foreign.C.Error.Describe as D
import qualified Linux.Socket as L
import qualified Posix.Socket as S
import qualified Socket as SCK
import qualified Socket.EventManager as EM

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
listen :: Peer -> IO (Either SocketException (Listener, Word16))
listen endpoint@Peer{port = specifiedPort} = do
  debug ("listen: opening listen " ++ describeEndpoint endpoint)
  e1 <- S.uninterruptibleSocket S.internet
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.stream)
    S.defaultProtocol
  debug ("listen: opened listen " ++ describeEndpoint endpoint)
  case e1 of
    Left err -> handleSocketListenException SCK.functionWithListener err
    Right fd -> do
      e2 <- S.uninterruptibleBind fd
        (S.encodeSocketAddressInternet (endpointToSocketAddressInternet endpoint))
      debug ("listen: requested binding for listen " ++ describeEndpoint endpoint)
      case e2 of
        Left err -> do
          _ <- S.uninterruptibleClose fd
          handleBindListenException specifiedPort err
        Right _ -> S.uninterruptibleListen fd 16 >>= \case
          -- We hardcode the listen backlog to 16. The author is unfamiliar
          -- with use cases where gains are realized from tuning this parameter.
          -- Open an issue if this causes problems for anyone.
          Left err -> do
            _ <- S.uninterruptibleClose fd
            debug "listen: listen failed with error code"
            handleBindListenException specifiedPort err
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
                      debug ("listen: successfully bound listen " ++ describeEndpoint endpoint ++ " and got port " ++ show cleanActualPort)
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
            let !mngr = EM.manager
            debug ("listen: registering fd " ++ show fd)
            EM.register mngr fd
            pure (Right (Listener fd, actualPort))

-- | Close a listener. This throws an unrecoverable exception if
--   the socket cannot be closed.
unlisten :: Listener -> IO ()
unlisten (Listener fd) = S.uninterruptibleClose fd >>= \case
  Left err -> throwIO $ SocketUnrecoverableException
    moduleSocketStreamIPv4
    functionWithListener
    [cclose,describeErrorCode err]
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

withListener ::
     Peer
  -> (Listener -> Word16 -> IO a)
  -> IO (Either SocketException a)
withListener endpoint f = mask $ \restore -> do
  listen endpoint >>= \case
    Left err -> pure (Left err)
    Right (sck, actualPort) -> do
      a <- onException
        (restore (f sck actualPort))
        (unlisten_ sck)
      unlisten sck
      pure (Right a)

-- | Listen for an inbound connection.
accept :: Listener -> IO (Either (AcceptException 'Uninterruptible) (Connection,Peer))
accept (Listener !fd) = do
  -- Although this function must be called in a context where
  -- exceptions are masked, recall that EM.wait uses an STM
  -- action that might retry, meaning that this first part is
  -- still interruptible. This is a good thing in the case of
  -- this function.
  debug ("accept: about to create manager, fd=" ++ show fd)
  let !mngr = EM.manager
  debug ("accept: about to get reader, fd=" ++ show fd)
  -- The listener should already be registered, so we can just
  -- ask for the reader directly.
  !tv <- EM.reader mngr fd
  let go !oldToken = do
        debug ("accept: calling waitlessAccept for " ++ show fd)
        waitlessAccept fd >>= \case
          Left merr -> case merr of
            Nothing -> EM.unreadyAndWait oldToken tv >>= go
            Just err -> pure (Left err)
          Right r@(Connection conn,_) -> do
            debug ("accept: waitlessAccept succeeded for " ++ show fd)
            EM.register mngr conn
            pure (Right r)
  go =<< STM.readTVarIO tv

-- | Listen for an inbound connection. Can be interrupted by an
-- STM-style interrupt.
interruptibleAccept ::
     TVar Bool
     -- ^ Interrupted. If this becomes 'True' give up and return
     --   @'Left' 'AcceptInterrupted'@.
  -> Listener
  -> IO (Either (AcceptException 'Interruptible) (Connection,Peer))
interruptibleAccept !abandon (Listener fd) = do
  -- TODO: pull these out of the loop
  let !mngr = EM.manager
  tv <- EM.reader mngr fd
  token <- EM.interruptibleWait abandon tv
  if EM.isInterrupt token
    then pure (Left AcceptInterrupted)
    else waitlessAccept fd >>= \case
      Left merr -> case merr of
        Nothing -> do
          EM.unready token tv
          interruptibleAccept abandon (Listener fd)
        Just err -> pure (Left err)
      Right r@(Connection conn,_) -> do
        EM.register mngr conn
        pure (Right r)

-- Only used internally
interruptibleAcceptCounting :: 
     TVar Int
  -> TVar Bool
  -> Listener
  -> IO (Either (AcceptException 'Interruptible) (Connection,Peer))
interruptibleAcceptCounting !counter !abandon (Listener !fd) = do
  -- TODO: pull these out of the loop
  let !mngr = EM.manager
  tv <- EM.reader mngr fd
  token <- EM.interruptibleWait abandon tv
  if EM.isInterrupt token
    then pure (Left AcceptInterrupted)
    else waitlessAccept fd >>= \case
      Left merr -> case merr of
        Nothing -> do
          EM.unready token tv
          interruptibleAcceptCounting counter abandon (Listener fd)
        Just err -> pure (Left err)
      Right r@(Connection conn,_) -> do
        EM.register mngr conn
        pure (Right r)

-- We use the maybe to mean that the user needs to wait again.
waitlessAccept :: Fd -> IO (Either (Maybe (AcceptException i)) (Connection,Peer))
waitlessAccept lstn = do
  L.uninterruptibleAccept4 lstn S.sizeofSocketAddressInternet (L.closeOnExec <> L.nonblocking) >>= \case
    Left err -> handleAcceptException err
    Right (sockAddrRequiredSz,sockAddr,acpt) -> if sockAddrRequiredSz == S.sizeofSocketAddressInternet
      then case S.decodeSocketAddressInternet sockAddr of
        Just sockAddrInet -> do
          let !acceptedEndpoint = socketAddressInternetToEndpoint sockAddrInet
          debug ("internalAccepted: successfully accepted connection from " ++ show acceptedEndpoint)
          pure (Right (Connection acpt, acceptedEndpoint))
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
        _ <- S.uninterruptibleClose fd
        -- TODO: What about ENOTCONN? Can this happen if the remote
        -- side has already closed the connection?
        throwIO $ SocketUnrecoverableException
          moduleSocketStreamIPv4
          SCK.functionGracefulClose
          [SCK.cshutdown,describeErrorCode err]
    Right _ -> gracefulCloseB tv token0 fd

-- The second part of the shutdown function must call itself recursively
-- since we may receive false read-ready notifications at any time.
gracefulCloseB :: TVar EM.Token -> EM.Token -> Fd -> IO (Either CloseException ())
gracefulCloseB !tv !token0 !fd = do
  !buf <- PM.newByteArray 1
  -- We do not actually want to remove the bytes from the
  -- receive buffer, so we use MSG_PEEK. We are be certain
  -- to send a reset when a CloseException is reported.
  S.uninterruptibleReceiveMutableByteArray fd buf 0 1 S.peek >>= \case
    Left err1 -> if err1 == eWOULDBLOCK || err1 == eAGAIN
      then do
        token1 <- EM.persistentUnreadyAndWait token0 tv
        gracefulCloseB tv token1 fd
      else do
        _ <- S.uninterruptibleClose fd
        -- We treat all @recv@ errors except for the nonblocking
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
        Right _ -> pure (Right ())
      else do
        debug ("Socket.Stream.IPv4.gracefulClose: remote not shutdown B")
        _ <- S.uninterruptibleClose fd
        pure (Left ClosePeerContinuedSending)

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
  -> (Connection -> Peer -> IO a)
     -- ^ Callback to consume connection. Must not return the connection.
  -> IO (Either (AcceptException 'Uninterruptible) b)
withAccepted lstn@(Listener lstnFd) consumeException cb = do
  debug ("withAccepted: fd " ++ show lstnFd)
  r <- mask $ \restore -> do
    accept lstn >>= \case
      Left e -> pure (Left e)
      Right (conn, endpoint) -> do
        a <- onException (restore (cb conn endpoint)) (disconnect_ conn)
        e <- disconnect conn
        pure (Right (e,a))
  -- Notice that consumeException gets run in an unmasked context.
  case r of
    Left e -> pure (Left e)
    Right (e,a) -> fmap Right (consumeException e a)

-- | Accept a connection on the listener and run the supplied callback in
-- a new thread. Prefer 'forkAcceptedUnmasked' unless the masking state
-- needs to be preserved for the callback. Such a situation seems unlikely
-- to the author.
forkAccepted ::
     Listener
  -> (Either CloseException () -> a -> IO ())
     -- ^ Callback to handle an ungraceful close. 
  -> (Connection -> Peer -> IO a)
     -- ^ Callback to consume connection. Must not return the connection.
  -> IO (Either (AcceptException 'Uninterruptible) ThreadId)
forkAccepted lstn consumeException cb =
  mask $ \restore -> accept lstn >>= \case
    Left e -> pure (Left e)
    Right (conn, endpoint) -> fmap Right $ forkIO $ do
      a <- onException (restore (cb conn endpoint)) (disconnect_ conn)
      e <- disconnect conn
      restore (consumeException e a)

-- | Accept a connection on the listener and run the supplied callback in
-- a new thread. The masking state is set to @Unmasked@ when running the
-- callback. Typically, @a@ is instantiated to @()@.
forkAcceptedUnmasked ::
     Listener
  -> (Either CloseException () -> a -> IO ())
     -- ^ Callback to handle an ungraceful close. 
  -> (Connection -> Peer -> IO a)
     -- ^ Callback to consume connection. Must not return the connection.
  -> IO (Either (AcceptException 'Uninterruptible) ThreadId)
forkAcceptedUnmasked lstn consumeException cb =
  mask_ $ accept lstn >>= \case
    Left e -> pure (Left e)
    Right (conn, endpoint) -> fmap Right $ forkIOWithUnmask $ \unmask -> do
      a <- onException (unmask (cb conn endpoint)) (disconnect_ conn)
      e <- disconnect conn
      unmask (consumeException e a)

-- | Accept a connection on the listener and run the supplied callback in
-- a new thread. The masking state is set to @Unmasked@ when running the
-- callback. Typically, @a@ is instantiated to @()@.
--
-- ==== __Discussion__
--
-- Why is the @counter@ argument present? At first, it seems
-- like this is something that the API consumer should implement on
-- top of this library. The argument for the inclusion of the counter
-- is has two parts: (1) clients supporting graceful termination
-- always need these semantics and (2) these semantics cannot
-- be provided without building in @counter@ as a @TVar@.
--
-- 1. Clients supporting graceful termination always need these
--    semantics. To gracefully bring down a server that has been
--    accepting connections with a forking function, an application
--    must wait for all active connections to finish. Since all
--    connections run on separate threads, this can only be
--    accomplished by a concurrency primitive. The straightforward
--    solution is to wrap a counter with either @MVar@ or @TVar@.
--    To complete graceful termination, the application must
--    block until the counter reaches zero.
-- 2. These semantics cannot be provided without building in
--    @counter@ as a @TVar@. When @abandon@ becomes @True@,
--    graceful termination begins. From this point onward, if at
--    any point the counter reaches zero, the application consuming
--    this API will complete termination. Consequently, we need
--    the guarantee that the counter does not increment after
--    the @abandon@ transaction completes. If it did increment
--    in this forbidden way (e.g. if it was incremented some
--    unspecified amount of time after a connection was accepted),
--    there would be a race condition in which the application
--    may terminate without giving the newly accepted connection
--    a chance to finish. Fortunately, @STM@ gives us the
--    composable transaction we need to get this guarantee.
--    To wait for an inbound connection, we use:
--
--    > (isReady,deregister) <- threadWaitReadSTM fd
--    > shouldReceive <- atomically $ do
--    >   readTVar abandon >>= \case
--    >     True -> do
--    >       isReady
--    >       modifyTVar' counter (+1)
--    >       pure True
--    >     False -> pure False
--
--    This eliminates the window for the race condition. If a
--    connection is accepted, the counter is guaranteed to
--    be incremented _before_ @abandon@ becomes @True@.
--    However, this code would be more simple and would perform
--    better if GHC's event manager used TVar instead of STM.
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
     -- ^ Callback to handle an ungraceful close. 
  -> (Connection -> Peer -> IO a)
     -- ^ Callback to consume connection. Must not return the connection.
  -> IO (Either (AcceptException 'Interruptible) ThreadId)
interruptibleForkAcceptedUnmasked !counter !abandon !lstn consumeException cb =
  mask_ $ interruptibleAcceptCounting counter abandon lstn >>= \case
    Left e -> do
      case e of
        AcceptInterrupted -> pure ()
        _ -> atomically (modifyTVar' counter (subtract 1))
      pure (Left e)
    Right (conn, endpoint) -> fmap Right $ forkIOWithUnmask $ \unmask -> do
      a <- onException
        (unmask (cb conn endpoint))
        (disconnect_ conn *> atomically (modifyTVar' counter (subtract 1)))
      e <- disconnect conn
      r <- unmask (consumeException e a)
      atomically (modifyTVar' counter (subtract 1))
      pure r

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
     Peer
     -- ^ Remote endpoint
  -> IO (Either (ConnectException 'Uninterruptible) Connection)
connect !remote = do
  beforeEstablishment remote >>= \case
    Left err -> pure (Left err)
    Right (fd,sockAddr) -> do
      let !mngr = EM.manager
      -- TODO: I believe it is sound to make both the write and
      -- read channels start off as not ready. After all, the
      -- socket is brand new and is not connected to a peer.
      -- Consequently, there's no way we could miss events.
      EM.register mngr fd
      tv <- EM.writer mngr fd
      debug ("connect: about to connect, fd=" ++ show fd)
      token0 <- STM.readTVarIO tv
      S.uninterruptibleConnect fd sockAddr >>= \case
        Left err2 -> if err2 == eINPROGRESS
          then do
            debug ("connect: EINPROGRESS, fd=" ++ show fd)
            -- When we receive EINPROGRESS, we have high confidence
            -- the that the socket is not yet ready (keeping in mind
            -- that it could have somehow become ready right after
            -- C's connect returned). Immidiately diving into
            -- afterEstablishment would result in a syscall to
            -- getsockopt. But what a waste that would be since it
            -- would almost certainly return EAGAIN. So, instead,
            -- we unready the write channel (taking the usual
            -- precautions) and then await readiness.
            token1 <- EM.unreadyAndWait token0 tv
            afterEstablishment tv token1 fd
          else do
            debug ("connect: failed, fd=" ++ show fd)
            S.uninterruptibleErrorlessClose fd
            handleConnectException SCK.functionWithConnection err2
        Right _ -> do
          debug ("connect: succeeded immidiately, fd=" ++ show fd)
          afterEstablishment tv token0 fd

-- | Variant of 'connect' that is interruptible using STM-style interrupts.
-- interruptibleConnect ::
--      TVar Bool
--      -- ^ Interrupted. If this becomes 'True', give up and return
--      --   @'Left' 'AcceptInterrupted'@.
--   -> Peer
--      -- ^ Remote endpoint
--   -> IO (Either (ConnectException 'Interruptible) Connection)
-- interruptibleConnect !abandon !remote = do
--   beforeEstablishment remote >>= \case
--     Left err -> pure (Left err)
--     Right (fd,sockAddr) -> S.uninterruptibleConnect fd sockAddr >>= \case
--       Left err2 -> if err2 == eINPROGRESS
--         then do
--           interruptibleWaitWrite abandon fd >>= \case
--             True -> afterEstablishment fd
--             False -> pure (Left ConnectInterrupted)
--         else do
--           S.uninterruptibleErrorlessClose fd
--           handleConnectException SCK.functionWithConnection err2
--       Right _ -> afterEstablishment fd

-- Internal function called by both connect and interruptibleConnect
-- before the connection is established. Creates the socket and prepares
-- the sockaddr.
beforeEstablishment :: Peer -> IO (Either (ConnectException i) (Fd,S.SocketAddress))
{-# INLINE beforeEstablishment #-}
beforeEstablishment !remote = do
  debug ("beforeEstablishment: opening connection " ++ show remote)
  e1 <- S.uninterruptibleSocket S.internet
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.stream)
    S.defaultProtocol
  debug ("beforeEstablishment: opened connection " ++ show remote)
  case e1 of
    Left err -> handleSocketConnectException SCK.functionWithConnection err
    Right fd -> do
      let sockAddr = id
            $ S.encodeSocketAddressInternet
            $ endpointToSocketAddressInternet
            $ remote
      pure (Right (fd,sockAddr))

-- Internal function called by both connect and interruptibleConnect
-- after the connection is established.
afterEstablishment ::
     TVar EM.Token -- token tvar 
  -> EM.Token -- old token
  -> Fd
  -> IO (Either (ConnectException i) Connection)
afterEstablishment !tv !oldToken !fd = do
  debug ("afterEstablishment: finished waiting, fd=" ++ show fd)
  e <- S.uninterruptibleGetSocketOption fd
    S.levelSocket S.optionError (intToCInt (PM.sizeOf (undefined :: CInt)))
  case e of
    Left err -> do
      S.uninterruptibleErrorlessClose fd
      throwIO $ SocketUnrecoverableException
        moduleSocketStreamIPv4
        functionWithListener
        [SCK.cgetsockopt,describeErrorCode err]
    Right (sz,S.OptionValue val) -> if sz == intToCInt (PM.sizeOf (undefined :: CInt))
      then
        let err = PM.indexByteArray val 0 :: CInt in
        if | err == 0 -> do
               debug ("afterEstablishment: connection established, fd=" ++ show fd)
               pure (Right (Connection fd))
           | Errno err == eAGAIN || Errno err == eWOULDBLOCK -> do
               debug ("afterEstablishment: not ready yet, unreadying token and waiting, fd=" ++ show fd)
               EM.unready oldToken tv
               newToken <- EM.wait tv
               afterEstablishment tv newToken fd
           | otherwise -> do
               S.uninterruptibleErrorlessClose fd
               handleConnectException SCK.functionWithConnection (Errno err)
      else do
        S.uninterruptibleErrorlessClose fd
        throwIO $ SocketUnrecoverableException
          moduleSocketStreamIPv4
          functionWithListener
          [SCK.cgetsockopt,connectErrorOptionValueSize]

-- | Close a connection gracefully, reporting a 'CloseException' when
-- the connection has to be terminated by sending a TCP reset. This
-- uses a combination of @shutdown@, @recv@, @close@ to detect when
-- resets need to be sent.
disconnect :: Connection -> IO (Either CloseException ())
disconnect (Connection fd) = gracefulCloseA fd

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

-- | Establish a connection to a server.
withConnection ::
     Peer
     -- ^ Remote endpoint
  -> (Either CloseException () -> a -> IO b)
     -- ^ Callback to handle an ungraceful close. 
  -> (Connection -> IO a)
     -- ^ Callback to consume connection. Must not return the connection.
  -> IO (Either (ConnectException 'Uninterruptible) b)
withConnection !remote g f = mask $ \restore -> do
  connect remote >>= \case
    Left err -> pure (Left err)
    Right conn -> do
      a <- onException (restore (f conn)) (disconnect_ conn)
      m <- disconnect conn
      b <- g m a
      pure (Right b)
    
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

intToCInt :: Int -> CInt
intToCInt = fromIntegral

moduleSocketStreamIPv4 :: String
moduleSocketStreamIPv4 = "Socket.Stream.IPv4"

functionWithListener :: String
functionWithListener = "withListener"

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

-- These are the exceptions that can happen as a result of a
-- nonblocking @connect@ or as a result of subsequently calling
-- @getsockopt@ to get the @SO_ERROR@ after the socket is ready
-- for writes. For increased likelihood of correctness, we do
-- not distinguish between which of these error codes can show
-- up after which of those two calls. This means we are likely
-- testing for some exceptions that cannot occur as a result of
-- a particular call, but doing otherwise would be fraught with
-- uncertainty.
handleConnectException :: String -> Errno -> IO (Either (ConnectException i) a)
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
handleSocketConnectException :: String -> Errno -> IO (Either (ConnectException i) a)
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
handleSocketListenException :: String -> Errno -> IO (Either SocketException a)
handleSocketListenException func e
  | e == eMFILE = pure (Left SocketFileDescriptorLimit)
  | e == eNFILE = pure (Left SocketFileDescriptorLimit)
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
handleBindListenException ::
     Word16
  -> Errno
  -> IO (Either SocketException a)
handleBindListenException !thePort !e
  | e == eACCES = pure (Left SocketPermissionDenied)
  | e == eADDRINUSE = if thePort == 0
      then pure (Left SocketAddressInUse)
      else pure (Left SocketEphemeralPortsExhausted)
  | otherwise = die
      ("Socket.Stream.IPv4.bindListen: " ++ describeErrorCode e)

-- These are the exceptions that can happen as a result
-- of calling @socket@ with the intent of using the socket
-- to open a connection (not listen for inbound connections).
handleAcceptException :: Errno -> IO (Either (Maybe (AcceptException i)) a)
handleAcceptException e
  | e == eAGAIN = pure (Left Nothing)
  | e == eWOULDBLOCK = pure (Left Nothing)
  | e == eCONNABORTED = pure (Left (Just AcceptConnectionAborted))
  | e == eMFILE = pure (Left (Just AcceptFileDescriptorLimit))
  | e == eNFILE = pure (Left (Just AcceptFileDescriptorLimit))
  | e == ePERM = pure (Left (Just AcceptFirewalled))
  | otherwise = die ("Socket.Stream.IPv4.accept: " ++ describeErrorCode e)

connectErrorOptionValueSize :: String
connectErrorOptionValueSize = "incorrectly sized value of SO_ERROR option"

{- $unbracketed
 
Provided here are the unbracketed functions for the creation and destruction
of listeners, outbound connections, and inbound connections. These functions
come with pretty serious requirements:

* They may only be called in contexts where exceptions are masked.
* The caller /must/ be sure to call the destruction function every
  'Listener' or 'Connection' exactly once to close underlying file
  descriptor.
* The 'Listener' or 'Connection' cannot be used after being given
  as an argument to the destruction function.
-}

