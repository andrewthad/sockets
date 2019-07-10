{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}

module Socket.Stream.Uninterruptible.ByteString
  ( send
  , receiveExactly
  , receiveOnce
  , receiveBetween
  ) where

import Data.ByteString (ByteString)
import Data.Bytes.Types (UnmanagedBytes(..))
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Primitive.Addr (Addr(..))
import Data.Bytes.Types (MutableBytes(..))
import GHC.Exts (Ptr(Ptr),proxy#)
import Socket.Stream (Connection,ReceiveException,SendException)
import Socket (Interruptibility(Uninterruptible))
import Socket.Interop (fromPinned)

import qualified Data.Primitive as PM
import qualified Socket.Stream.Uninterruptible.Addr.Send as Send
import qualified Socket.Stream.Uninterruptible.MutableBytes.Receive as Receive

-- | Send a slice of a buffer. If needed, this calls POSIX @send@ repeatedly
--   until the entire contents of the buffer slice have been sent.
send ::
     Connection -- ^ Connection
  -> ByteString -- ^ Slice of a buffer
  -> IO (Either (SendException 'Uninterruptible) ())
{-# inline send #-}
send !conn !bs = unsafeUseAsCStringLen bs
  (\(Ptr addr#,len) -> Send.send proxy# conn (UnmanagedBytes (Addr addr#) len))

-- | Receive a number of bytes exactly equal to the length of the
--   buffer slice. If needed, this may call @recv@ repeatedly until
--   the requested number of bytes have been received.
receiveExactly ::
     Connection -- ^ Connection
  -> Int -- ^ Exact number of bytes to receive
  -> IO (Either (ReceiveException 'Uninterruptible) ByteString)
{-# inline receiveExactly #-}
receiveExactly !conn !n = do
  !marr <- PM.newPinnedByteArray n
  Receive.receiveExactly proxy# conn (MutableBytes marr 0 n) >>= \case
    Left err -> pure (Left err)
    Right (_ :: ()) -> pure $! Right $! fromPinned marr 0 n

-- | Receive at most the specified number of bytes. This
-- only makes multiple calls to POSIX @recv@ if EAGAIN is returned. It makes at
-- most one @recv@ call that successfully fills the buffer.
receiveOnce ::
     Connection -- ^ Connection
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Uninterruptible) ByteString)
{-# inline receiveOnce #-}
receiveOnce !conn !n = do
  !marr0 <- PM.newPinnedByteArray n
  Receive.receiveOnce proxy# conn (MutableBytes marr0 0 n) >>= \case
    Left err -> pure (Left err)
    Right sz -> pure $! Right $! fromPinned marr0 0 sz

-- | Receive a number of bytes that is between the inclusive lower and
--   upper bounds. If needed, this may call @recv@ repeatedly until the
--   minimum requested number of bytes have been received.
receiveBetween ::
     Connection -- ^ Connection
  -> Int -- ^ Minimum number of bytes to receive
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Uninterruptible) ByteString)
{-# inline receiveBetween #-}
receiveBetween !conn !minLen !maxLen = do
  !marr0 <- PM.newPinnedByteArray maxLen
  Receive.receiveBetween proxy# conn (MutableBytes marr0 0 maxLen) minLen >>= \case
    Left err -> pure (Left err)
    Right sz -> pure $! Right $! fromPinned marr0 0 sz
