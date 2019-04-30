module Socket.Stream.Uninterruptible.Addr
  ( send
  , receiveExactly
  -- , receiveChunk
  -- , receiveBetween
  ) where

-- | Send a slice of a buffer. If needed, this calls POSIX @send@ repeatedly
--   until the entire contents of the buffer slice have been sent.
send ::
     Connection -- ^ Connection
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Length of buffer
  -> IO (Either (SendException 'Uninterruptible) ())
send _ _ _ = _

-- | Receive a number of bytes exactly equal to the length of the
--   buffer slice. If the remote application shuts down its end of the
--   connection before sending the required number of bytes, this returns
--   @'Left' 'ReceiveShutdown'@.
receiveExactly ::
     Connection -- ^ Connection
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Length of buffer
  -> IO (Either (ReceiveException 'Uninterruptible) ())
receiveExactly _ _ _ = _
