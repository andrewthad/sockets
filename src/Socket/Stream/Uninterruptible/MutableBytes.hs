{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}

module Socket.Stream.Uninterruptible.MutableBytes
  ( -- * Send
    send
  , sendMany
    -- * Receive
  , receiveExactly
  , receiveOnce
  , receiveBetween
  ) where

import Data.Bytes.Types (MutableBytes)
import Data.Primitive (MutableByteArray)
import Data.Primitive.Unlifted.Array (UnliftedArray(..))
import GHC.Exts (RealWorld,proxy#)
import Socket.Stream (Connection,ReceiveException,SendException)
import Socket (Interruptibility(Uninterruptible))

import qualified Socket.Stream.Uninterruptible.Bytes as SSUB
import qualified Socket.Stream.Uninterruptible.MutableBytes.Send as Send
import qualified Socket.Stream.Uninterruptible.MutableBytes.Receive as Receive

-- | Send a slice of a buffer. If needed, this calls POSIX @send@ repeatedly
--   until the entire contents of the buffer slice have been sent.
send ::
     Connection -- ^ Connection
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (SendException 'Uninterruptible) ())
{-# inline send #-}
send = Send.send proxy#

-- | Send many buffers with vectored I/O. This uses @sendmsg@ to send
--   multiple chunks at a time. It may call @sendmsg@ more than once
--   if there is not enough space in the operating system send buffer
--   to house all the chunks at the same time.
sendMany ::
     Connection -- ^ Connection
  -> UnliftedArray (MutableByteArray RealWorld) -- ^ Byte arrays
  -> IO (Either (SendException 'Uninterruptible) ())
{-# inline sendMany #-}
sendMany conn (UnliftedArray bufs) =
  -- Effectively, what we are doing here is unsafely coercing the
  -- buffer from mutable to immutable. This ends up being sound
  -- in this context because SSUB.sendMany does not preserve any
  -- referenced to its argument byte arrays.
  SSUB.sendMany conn (UnliftedArray bufs)

-- | Receive a number of bytes exactly equal to the length of the
--   buffer slice. If needed, this may call @recv@ repeatedly until
--   the requested number of bytes have been received.
receiveExactly ::
     Connection -- ^ Connection
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (ReceiveException 'Uninterruptible) ())
{-# inline receiveExactly #-}
receiveExactly = Receive.receiveExactly proxy#

-- | Receive a number of bytes exactly equal to the length of the slice. This
-- only makes multiple calls to POSIX @recv@ if EAGAIN is returned. It makes at
-- most one @recv@ call that successfully fills the buffer.
receiveOnce ::
     Connection -- ^ Connection
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (ReceiveException 'Uninterruptible) Int)
{-# inline receiveOnce #-}
receiveOnce = Receive.receiveOnce proxy#

-- | Receive a number of bytes that is at least the minimum size
--   and is at most the length of the slice. If needed, this may
--   call @recv@ repeatedly until the minimum requested number of
--   bytes have been received.
receiveBetween ::
     Connection -- ^ Connection
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> Int
     -- ^ Minimum number of bytes to receive, must be less than or equal
     --   to the length of the slice.
  -> IO (Either (ReceiveException 'Uninterruptible) Int)
{-# inline receiveBetween #-}
receiveBetween = Receive.receiveBetween proxy#
