module Socket.Stream.Unix
  ( -- * Types
    Listener(..)
  , Connection(..)
  , Peer(..)
    -- * Unbracketed
    -- $unbracketed
  , listen
  , unlisten
  , unlisten_
  , connect
  , systemdListener
  , disconnect
  , disconnect_
  , accept
  , interruptibleAccept
  ) where


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
      -- TODO: shave off an allocation by building the sockaddr in C.
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

