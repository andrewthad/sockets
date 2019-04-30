{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language DataKinds #-}
{-# language MagicHash #-}

module Socket.Stream.Uninterruptible.ByteString
  ( send
  , receiveExactly
  , receiveOnce
  , receiveBetween
  ) where

import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ByteString.Internal (ByteString(PS))
import Data.Primitive (MutableByteArray(..),Addr(..))
import Data.Bytes.Types (MutableBytes(..))
import GHC.Exts (Ptr(Ptr),RealWorld,byteArrayContents#,unsafeCoerce#,proxy#)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr),ForeignPtrContents(PlainPtr))
import Socket.Stream (Connection,ReceiveException,SendException)
import Socket.AddrLength (AddrLength(..))
import Socket (Interruptibility(Uninterruptible))

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
  (\(Ptr addr#,len) -> Send.send proxy# conn (AddrLength (Addr addr#) len))

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
    Right _ -> pure $! Right $! fromManaged marr 0 n

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
    Right sz -> do
      marr1 <- PM.resizeMutableByteArray marr0 sz
      pure $! Right $! fromManaged marr1 0 sz

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
    Right sz -> do
      marr1 <- PM.resizeMutableByteArray marr0 sz
      pure $! Right $! fromManaged marr1 0 sz

fromManaged :: MutableByteArray RealWorld -> Int -> Int -> ByteString
{-# inline fromManaged #-}
fromManaged (MutableByteArray marr#) off len =
  PS (ForeignPtr (byteArrayContents# (unsafeCoerce# marr#)) (PlainPtr marr#)) off len

