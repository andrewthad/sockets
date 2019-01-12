module Socket.Datagram.Undestined
  ( -- * Types
  , Socket
    -- * Establish
  , establish
  , close
    -- * Communicate
  , send
  , receive
  , receiveTimeout
  ) where

newtype Socket = Socket Fd

establish :: IO Socket
close :: Socket -> IO ()

send :: Socket -> Address -> ByteArray -> IO ()
receive :: Socket -> IO (Address,ByteArray)
receiveTimeout ::
     Socket
  -> Int -- ^ Microseconds to wait before giving up
  -> IO (Maybe (Address,ByteArray))


