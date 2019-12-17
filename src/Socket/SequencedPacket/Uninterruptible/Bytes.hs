{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}

module Socket.SequencedPacket.Uninterruptible.Bytes
  ( send
  , trySend
  , receive
  , tryReceive
  ) where

import Socket.SequencedPacket.Unix (Connection(..))
import Data.Primitive (ByteArray)
import Data.Bytes.Types (Bytes,MutableBytes(MutableBytes))
import GHC.Exts (proxy#)
import Socket (Interruptibility(Uninterruptible))
import Socket.SequencedPacket (SendException,ReceiveException)
import Socket.SequencedPacket.Unix (Connection(..))

import qualified Data.Primitive as PM
import qualified Socket.SequencedPacket.Uninterruptible.Bytes.Send as Send
import qualified Socket.SequencedPacket.Uninterruptible.MutableBytes.Receive as Receive

-- Sadly, SEQPACKET sockets are poorly documented and, in Linux,
-- behave inconsisently depending on whether the socket is
-- internet-domain or unix-domain. More thorough coverage of
-- this is found at https://stackoverflow.com/q/3595684
--
-- Since internet-domain SEQPACKET sockets are rare and unsupported
-- by most hardware, we only support unix-domain sockets. On Linux,
-- we can do this by just reusing the send and receive functions for
-- datagram sockets.

-- | Send a message over a connection.
send ::
     Connection -- ^ Connection
  -> Bytes -- ^ Slice of a buffer
  -> IO (Either (SendException 'Uninterruptible) ())
send (Connection !sock) !buf = Send.send proxy# sock buf

-- | Variant of 'send' that never blocks. If the datagram cannot
-- immidiately be copied to the send queue, returns @False@.
trySend ::
     Connection -- ^ Connection
  -> Bytes -- ^ Slice of a buffer
  -> IO (Either (SendException 'Uninterruptible) Bool)
trySend (Connection !sock) !buf = Send.attemptSend sock buf

-- | Receive a message.
receive ::
     Connection -- ^ Connection
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Uninterruptible) ByteArray)
{-# inline receive #-}
receive (Connection conn) !n = do
  !marr0 <- PM.newByteArray n
  Receive.receive proxy# conn (MutableBytes marr0 0 n) >>= \case
    Left err -> pure (Left err)
    Right sz -> do
      marr1 <- PM.resizeMutableByteArray marr0 sz
      !arr <- PM.unsafeFreezeByteArray marr1
      pure $! Right $! arr

-- | Receive a message. The message may be truncated if the size
-- of it is larger than the maximum allowed size.
tryReceive ::
     Connection -- ^ Connection
  -> Int -- ^ Maximum message size
  -> IO (Either (ReceiveException 'Uninterruptible) (Maybe ByteArray))
tryReceive (Connection !conn) !n = do
  !marr0 <- PM.newByteArray n
  Receive.receiveAttempt conn (MutableBytes marr0 0 n) >>= \case
    Left err -> pure (Left err)
    Right Nothing -> pure (Right Nothing)
    Right (Just sz) -> do
      marr1 <- PM.resizeMutableByteArray marr0 sz
      !arr <- PM.unsafeFreezeByteArray marr1
      pure $! Right $! Just $! arr


