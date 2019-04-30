{-# language BangPatterns #-}
{-# language DataKinds #-}

module Socket.Stream.Interruptible.MutableBytes
  ( send
  , receiveExactly
  , receiveChunk
  , receiveBetween
  ) where

import Data.Bytes.Types (MutableBytes)
import GHC.Exts (RealWorld)
import Control.Concurrent.STM (TVar)
import Socket.Stream (Connection,ReceiveException,SendException)
import Socket (Interruptibility(Interruptible))

import qualified Socket.Stream.Interruptible.MutableBytes.Send as Send
import qualified Socket.Stream.Interruptible.MutableBytes.Receive as Receive

-- | Send a slice of a buffer. If needed, this calls POSIX @send@ repeatedly
--   until the entire contents of the buffer slice have been sent.
send ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Connection -- ^ Connection
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (SendException 'Interruptible) ())
{-# inline send #-}
send = Send.send

-- | Receive a number of bytes exactly equal to the length of the
--   buffer slice. If needed, this may call @recv@ repeatedly until
--   the requested number of bytes have been received.
receiveExactly ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (ReceiveException 'Interruptible) ())
{-# inline receiveExactly #-}
receiveExactly = Receive.receiveExactly

-- | Receive a number of bytes exactly equal to the length of the slice. This
-- only makes multiple calls to POSIX @recv@ if EAGAIN is returned. It makes at
-- most one @recv@ call that successfully fills the buffer.
receiveChunk ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (ReceiveException 'Interruptible) Int)
{-# inline receiveChunk #-}
receiveChunk = Receive.receiveChunk

-- | Receive a number of bytes that is at least the minimum size
--   and is at most the length of the slice. If needed, this may
--   call @recv@ repeatedly until the minimum requested number of
--   bytes have been received.
receiveBetween ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> Int
     -- ^ Minimum number of bytes to receive, must be less than or equal
     --   to the length of the slice.
  -> IO (Either (ReceiveException 'Interruptible) Int)
{-# inline receiveBetween #-}
receiveBetween = Receive.receiveBetween
