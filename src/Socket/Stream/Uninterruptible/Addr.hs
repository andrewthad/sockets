{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}

module Socket.Stream.Uninterruptible.Addr
  ( send
  , receiveExactly
  , receiveOnce
  , receiveBetween
  ) where

import Data.Primitive.Addr (Addr)
import GHC.Exts (proxy#)
import Socket.Stream (Connection,ReceiveException,SendException)
import Socket (Interruptibility(Uninterruptible))
import Socket.AddrLength (AddrLength(..))

import qualified Socket.Stream.Uninterruptible.Addr.Send as Send
import qualified Socket.Stream.Uninterruptible.Addr.Receive as Receive

-- | Send an exact number of bytes starting from a given address. If needed,
--   this calls POSIX @send@ repeatedly until the requested number of bytes
--   has been sent.
send ::
     Connection -- ^ Connection
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Number of bytes to send
  -> IO (Either (SendException 'Uninterruptible) ())
{-# inline send #-}
send !conn !addr !len = Send.send proxy# conn (AddrLength addr len)

-- | Receive the requested number of bytes into memory beginning at
--   the specified address. If needed, this may call @recv@ repeatedly until
--   the requested number of bytes have been received.
receiveExactly ::
     Connection -- ^ Connection
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Exact number of bytes to receive
  -> IO (Either (ReceiveException 'Uninterruptible) ())
{-# inline receiveExactly #-}
receiveExactly !conn !addr !len =
  Receive.receiveExactly proxy# conn (AddrLength addr len)

-- | Receive at most the specified number of bytes. This
-- only makes multiple calls to POSIX @recv@ if EAGAIN is returned. It makes at
-- most one @recv@ call that successfully fills the buffer.
receiveOnce ::
     Connection -- ^ Connection
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Uninterruptible) Int)
{-# inline receiveOnce #-}
receiveOnce conn addr len =
  Receive.receiveOnce proxy# conn (AddrLength addr len)

-- | Receive a number of bytes that is between the inclusive lower and
--   upper bounds. If needed, this may call @recv@ repeatedly until the
--   minimum requested number of bytes have been received.
receiveBetween ::
     Connection -- ^ Connection
  -> Addr -- ^ Start of buffer
  -> Int
     -- ^ Minimum number of bytes to receive
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Uninterruptible) Int)
{-# inline receiveBetween #-}
receiveBetween conn addr minLen maxLen =
  Receive.receiveBetween proxy# conn (AddrLength addr maxLen) minLen
