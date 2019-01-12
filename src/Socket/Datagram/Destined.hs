module Socket.Datagram.Destined
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

establish :: Address -> IO Socket
close :: Socket -> IO ()

send :: Socket -> ByteArray -> IO ()
receive :: Socket -> IO ByteArray
receiveTimeout ::
     Socket
  -> Int -- ^ Microseconds to wait before giving up
  -> IO (Maybe ByteArray)

