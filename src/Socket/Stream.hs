module Socket.Stream
  ( -- * Types
    Listener
  , Connection
    -- * Establish
  , listener
  , withListener
  , accept
  , accept_
  , withAccepted
  , forkAccepted
  , forkAcceptedUnmasked
  , connect
  , disconnect
  , unsubscribe
    -- * Communicate
  , send
  , receive
  ) where

newtype Listener = Listener Fd
newtype Connection = Connection Fd

listener :: IO Listener

withListener :: (Listener -> IO a) -> IO a
withListener = bracket listener unsubcribe

accept :: Listener -> IO (Address,Connection)
accept_ :: Listener -> IO Connection

-- | Accept a connection on the listener and run the supplied callback
-- on it. This closes the connection when the callback finishes or if
-- an exception is thrown. Since this function blocks the thread until
-- the callback finishes, it is only suitable for stream socket clients
-- that handle one connection at a time. The variant 'forkAcceptedUnmasked'
-- is typically preferrable.
withAccepted :: Listener -> (Connection -> Address -> IO a) -> IO a
withAccepted x consume = do
  internalWaitForConnection x
  bracket
    (internalAcceptConnection x)
    (disconnect . snd)
    (uncurry consume)

-- | Accept a connection on the listener and run the supplied callback in
-- a new thread. Prefer 'forkAcceptedUnmasked' unless the masking state
-- needs to be preserved for the callback. Such a situation seems unlikely
-- to the author.
forkAccepted :: Listener -> (Connection -> Address -> IO a) -> IO ThreadId
forkAccepted x consume = do
  internalWaitForConnection x
  mask $ \restore -> forkIO $ do
    (address,conn) <- internalAcceptConnection x
    onException (restore (consume address conn)) (disconnect conn)
    disconnect conn

-- | Accept a connection on the listener and run the supplied callback in
-- a new thread. The masking state is set to @Unmasked@ when running the
-- callback.
forkAcceptedUnmasked :: Listener -> (Connection -> Address -> IO a) -> IO ThreadId
forkAcceptedUnmasked x consume = do
  internalWaitForConnection x
  mask_ $ forkIOWithUnmask $ \unmask -> do
    (address,conn) <- internalAcceptConnection x
    onException (unmask (consume address conn)) (disconnect conn)
    disconnect conn

internalWaitForConnection :: Listener -> IO ()
internalWaitForConnection _ = do
  fail "write internalWaitForConnection"

internalAcceptConnection :: Listener -> IO (Address,Connection)
internalAcceptConnection _ = fail "write unsafeAcceptConnection"

connect :: Address -> IO Connection

withConnection :: (Connection -> IO a) -> IO a
withConnection = bracket connect disconnect

send :: Connection -> ByteArray -> IO ()
receive :: Connection -> IO (Maybe ByteArray)

disconnect :: Connection -> IO ()
unsubcribe :: Listener -> IO ()

