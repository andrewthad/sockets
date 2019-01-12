module Socket.Stream
  ( -- * Types
    Acceptor
  , Connection
    -- * Establish
  , listen
  , accept
  , connect
  , disconnect
  , unsubscribe
    -- * Communicate
  , send
  , receive
  ) where

newtype Acceptor = Acceptor Fd
newtype Connection = Connection Fd

listen :: IO Acceptor
accept :: Acceptor -> IO (Address,Connection)
accept_ :: Acceptor -> IO Connection
connect :: Address -> IO Connection

send :: Connection -> ByteArray -> IO ()
receive :: Connection -> IO (Maybe ByteArray)

disconnect :: Connection -> IO ()
unsubcribe :: Acceptor -> IO ()

