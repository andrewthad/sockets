{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language TypeApplications #-}

-- | Communicate over a connection using immutable byte arrays.
-- Reception functions return 'ByteArray' instead of 'Bytes' since
-- this result always takes up the entirity of a 'ByteArray'. The
-- 'Bytes' would have redundant information since the offset would
-- be zero and the length would be the length of the 'ByteArray'
-- payload.
module Socket.Stream.Uninterruptible.Bytes
  ( -- * Send
    send
    -- * Receive
  , receiveExactly
  , receiveOnce
  , receiveBetween
  ) where

import Data.Bytes.Types (MutableBytes(..),Bytes(..))
import Data.Primitive (ByteArray)
import GHC.Exts (proxy#)
import Socket (Interruptibility(Uninterruptible))
import Socket.Stream (Connection,ReceiveException,SendException)

import qualified Data.Primitive as PM
import qualified Socket.Stream.Uninterruptible.Bytes.Send as Send
import qualified Socket.Stream.Uninterruptible.MutableBytes.Receive as Receive

-- Used for sendMany
import Control.Monad (when)
import Socket.Stream (SendException(..),Connection(..))
import Data.Primitive.Unlifted.Array (UnliftedArray)
import Socket.EventManager (Token)
import Control.Concurrent.STM (TVar)
import System.Posix.Types (Fd)
import Foreign.C.Types (CSize)
import Socket.Error (die)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK, ePIPE, eCONNRESET)
import Socket.Debug (whenDebugging)
import qualified Foreign.C.Error.Describe as D
import qualified Socket.EventManager as EM
import qualified Data.Primitive.Unlifted.Array as PM
import qualified Posix.Socket as S

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

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

