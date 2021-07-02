{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language DataKinds #-}

module Socket.Stream.Interruptible.Bytes
  ( send
  , receiveExactly
  , receiveOnce
  , receiveOncePrepended
  , receiveBetween
  ) where

import Data.Primitive (ByteArray)
import Data.Bytes.Types (MutableBytes(..),Bytes(..))
import Control.Concurrent.STM (TVar)
import Socket.Stream (Connection,ReceiveException,SendException)
import Socket (Interruptibility(Interruptible))

import qualified Data.Primitive as PM
import qualified Data.Bytes as Bytes
import qualified Socket.Stream.Interruptible.Bytes.Send as Send
import qualified Socket.Stream.Interruptible.MutableBytes.Receive as Receive

-- | Send a slice of a buffer. If needed, this calls POSIX @send@ repeatedly
--   until the entire contents of the buffer slice have been sent.
send ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Connection -- ^ Connection
  -> Bytes -- ^ Slice of a buffer
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
  -> Int -- ^ Exact number of bytes to receive
  -> IO (Either (ReceiveException 'Interruptible) ByteArray)
{-# inline receiveExactly #-}
receiveExactly !tv !conn !n = do
  !marr <- PM.newByteArray n
  Receive.receiveExactly tv conn (MutableBytes marr 0 n) >>= \case
    Left err -> pure (Left err)
    Right _ -> do
      !arr <- PM.unsafeFreezeByteArray marr
      pure $! Right $! arr

-- | Receive at most the specified number of bytes. This
-- only makes multiple calls to POSIX @recv@ if EAGAIN is returned. It makes at
-- most one @recv@ call that successfully fills the buffer.
receiveOnce ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Interruptible) ByteArray)
{-# inline receiveOnce #-}
receiveOnce !tv !conn !n = do
  !marr0 <- PM.newByteArray n
  Receive.receiveOnce tv conn (MutableBytes marr0 0 n) >>= \case
    Left err -> pure (Left err)
    Right sz -> do
      marr1 <- PM.resizeMutableByteArray marr0 sz
      !arr <- PM.unsafeFreezeByteArray marr1
      pure $! Right $! arr

-- | Variant of 'receiveOnce' that returns prepends the received bytes
-- with a prefix. This can be used to attach feed leftovers back in when
-- processing line-oriented protocols.
receiveOncePrepended ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> Bytes -- ^ Prefix to prepend to the received bytes
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Interruptible) ByteArray)
{-# inline receiveOncePrepended #-}
receiveOncePrepended !tv !conn !prefix !n = do
  let !ix0 = Bytes.length prefix
  !marr0 <- PM.newByteArray (n + ix0)
  Bytes.unsafeCopy marr0 0 prefix
  Receive.receiveOnce tv conn (MutableBytes marr0 ix0 n) >>= \case
    Left err -> pure (Left err)
    Right sz -> do
      marr1 <- PM.resizeMutableByteArray marr0 (sz + ix0)
      !arr <- PM.unsafeFreezeByteArray marr1
      pure $! Right $! arr

-- | Receive a number of bytes that is between the inclusive lower and
--   upper bounds. If needed, this may call @recv@ repeatedly until the
--   minimum requested number of bytes have been received.
receiveBetween ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> Int -- ^ Minimum number of bytes to receive
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Interruptible) ByteArray)
{-# inline receiveBetween #-}
receiveBetween !tv !conn !minLen !maxLen = do
  !marr0 <- PM.newByteArray maxLen
  Receive.receiveBetween tv conn (MutableBytes marr0 0 maxLen) minLen >>= \case
    Left err -> pure (Left err)
    Right sz -> do
      marr1 <- PM.resizeMutableByteArray marr0 sz
      !arr <- PM.unsafeFreezeByteArray marr1
      pure $! Right $! arr
