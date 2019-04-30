{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}

module Socket.Stream.Uninterruptible.Bytes
  ( send
  , receiveExactly
  , receiveOnce
  , receiveBetween
  ) where

import Data.Primitive (ByteArray)
import Data.Bytes.Types (MutableBytes(..),Bytes(..))
import GHC.Exts (proxy#)
import Socket.Stream (Connection,ReceiveException,SendException)
import Socket (Interruptibility(Uninterruptible))

import qualified Data.Primitive as PM
import qualified Socket.Stream.Uninterruptible.Bytes.Send as Send
import qualified Socket.Stream.Uninterruptible.MutableBytes.Receive as Receive

-- | Send a slice of a buffer. If needed, this calls POSIX @send@ repeatedly
--   until the entire contents of the buffer slice have been sent.
send ::
     Connection -- ^ Connection
  -> Bytes -- ^ Slice of a buffer
  -> IO (Either (SendException 'Uninterruptible) ())
{-# inline send #-}
send = Send.send proxy#

-- | Receive a number of bytes exactly equal to the length of the
--   buffer slice. If needed, this may call @recv@ repeatedly until
--   the requested number of bytes have been received.
receiveExactly ::
     Connection -- ^ Connection
  -> Int -- ^ Exact number of bytes to receive
  -> IO (Either (ReceiveException 'Uninterruptible) ByteArray)
{-# inline receiveExactly #-}
receiveExactly !conn !n = do
  !marr <- PM.newByteArray n
  Receive.receiveExactly proxy# conn (MutableBytes marr 0 n) >>= \case
    Left err -> pure (Left err)
    Right _ -> do
      !arr <- PM.unsafeFreezeByteArray marr
      pure $! Right $! arr

-- | Receive at most the specified number of bytes. This
-- only makes multiple calls to POSIX @recv@ if EAGAIN is returned. It makes at
-- most one @recv@ call that successfully fills the buffer.
receiveOnce ::
     Connection -- ^ Connection
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Uninterruptible) ByteArray)
{-# inline receiveOnce #-}
receiveOnce !conn !n = do
  !marr0 <- PM.newByteArray n
  Receive.receiveOnce proxy# conn (MutableBytes marr0 0 n) >>= \case
    Left err -> pure (Left err)
    Right sz -> do
      marr1 <- PM.resizeMutableByteArray marr0 sz
      !arr <- PM.unsafeFreezeByteArray marr1
      pure $! Right $! arr

-- | Receive a number of bytes that is between the inclusive lower and
--   upper bounds. If needed, this may call @recv@ repeatedly until the
--   minimum requested number of bytes have been received.
receiveBetween ::
     Connection -- ^ Connection
  -> Int -- ^ Minimum number of bytes to receive
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Uninterruptible) ByteArray)
{-# inline receiveBetween #-}
receiveBetween !conn !minLen !maxLen = do
  !marr0 <- PM.newByteArray maxLen
  Receive.receiveBetween proxy# conn (MutableBytes marr0 0 maxLen) minLen >>= \case
    Left err -> pure (Left err)
    Right sz -> do
      marr1 <- PM.resizeMutableByteArray marr0 sz
      !arr <- PM.unsafeFreezeByteArray marr1
      pure $! Right $! arr

