{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language DataKinds #-}
{-# language MagicHash #-}

module Socket.Stream.Interruptible.ByteString
  ( send
  , receiveExactly
  , receiveOnce
  , receiveBetween
  ) where

import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ByteString.Internal (ByteString(PS))
import Data.Primitive (MutableByteArray(..),Addr(..))
import Data.Bytes.Types (MutableBytes(..))
import Control.Concurrent.STM (TVar)
import GHC.Exts (Ptr(Ptr),RealWorld,byteArrayContents#,unsafeCoerce#)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr),ForeignPtrContents(PlainPtr))
import Socket.Stream (Connection,ReceiveException,SendException)
import Socket.AddrLength (AddrLength(..))
import Socket (Interruptibility(Interruptible))

import qualified Data.Primitive as PM
import qualified Socket.Stream.Interruptible.Addr.Send as Send
import qualified Socket.Stream.Interruptible.MutableBytes.Receive as Receive

-- | Send a slice of a buffer. If needed, this calls POSIX @send@ repeatedly
--   until the entire contents of the buffer slice have been sent.
send ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Connection -- ^ Connection
  -> ByteString -- ^ Slice of a buffer
  -> IO (Either (SendException 'Interruptible) ())
{-# inline send #-}
send !tv !conn !bs = unsafeUseAsCStringLen bs
  (\(Ptr addr#,len) -> Send.send tv conn (AddrLength (Addr addr#) len))

-- | Receive a number of bytes exactly equal to the length of the
--   buffer slice. If needed, this may call @recv@ repeatedly until
--   the requested number of bytes have been received.
receiveExactly ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> Int -- ^ Exact number of bytes to receive
  -> IO (Either (ReceiveException 'Interruptible) ByteString)
{-# inline receiveExactly #-}
receiveExactly !tv !conn !n = do
  !marr <- PM.newPinnedByteArray n
  Receive.receiveExactly tv conn (MutableBytes marr 0 n) >>= \case
    Left err -> pure (Left err)
    Right _ -> pure $! Right $! fromManaged marr 0 n

-- | Receive at most the specified number of bytes. This
-- only makes multiple calls to POSIX @recv@ if EAGAIN is returned. It makes at
-- most one @recv@ call that successfully fills the buffer.
receiveOnce ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Interruptible) ByteString)
{-# inline receiveOnce #-}
receiveOnce !tv !conn !n = do
  !marr0 <- PM.newPinnedByteArray n
  Receive.receiveOnce tv conn (MutableBytes marr0 0 n) >>= \case
    Left err -> pure (Left err)
    Right sz -> do
      marr1 <- PM.resizeMutableByteArray marr0 sz
      pure $! Right $! fromManaged marr1 0 sz

-- | Receive a number of bytes that is between the inclusive lower and
--   upper bounds. If needed, this may call @recv@ repeatedly until the
--   minimum requested number of bytes have been received.
receiveBetween ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> Int -- ^ Minimum number of bytes to receive
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Interruptible) ByteString)
{-# inline receiveBetween #-}
receiveBetween !tv !conn !minLen !maxLen = do
  !marr0 <- PM.newPinnedByteArray maxLen
  Receive.receiveBetween tv conn (MutableBytes marr0 0 maxLen) minLen >>= \case
    Left err -> pure (Left err)
    Right sz -> do
      marr1 <- PM.resizeMutableByteArray marr0 sz
      pure $! Right $! fromManaged marr1 0 sz

fromManaged :: MutableByteArray RealWorld -> Int -> Int -> ByteString
{-# inline fromManaged #-}
fromManaged (MutableByteArray marr#) off len =
  PS (ForeignPtr (byteArrayContents# (unsafeCoerce# marr#)) (PlainPtr marr#)) off len
