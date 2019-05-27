{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}

module Socket.Datagram.Uninterruptible.Bytes
  ( -- * Send
    send
  , sendToIPv4
  , -- * Receive
    receive
  , receiveFromIPv4
    -- * Receive Many
  , receiveMany
  , receiveManyFromIPv4
    -- * Types
  , Message(..)
  , Peer(..)
  , ReceiveException(..)
    -- * Slabs
  , newSlab
  , newSlabIPv4
  ) where

import Data.Bytes.Types (Bytes,MutableBytes(..))
import Data.Primitive (ByteArray,SmallArray)
import Data.Primitive.Unlifted.Array (UnliftedArray)
import Data.Primitive.PrimArray.Offset (MutablePrimArrayOffset(..))
import GHC.Exts (proxy#)
import Socket (Connectedness(..),Family(..),Version(..),Interruptibility(Uninterruptible))
import Socket.Address (posixToIPv4Peer)
import Socket.Datagram (Socket(..),SendException,ReceiveException(..))
import Socket.IPv4 (Peer(..),Message(..),Slab(..),freezeSlab)
import Socket.IPv4 (newSlabIPv4)
import Socket.Discard (newSlab)

import qualified Data.Primitive as PM
import qualified Socket.Discard
import qualified Socket.Datagram.Uninterruptible.Bytes.Send.Connected as CS
import qualified Socket.Datagram.Uninterruptible.Bytes.Send.IPv4 as V4S
import qualified Socket.Datagram.Uninterruptible.MutableBytes.Many as MM
import qualified Socket.Datagram.Uninterruptible.MutableBytes.Receive.Connected as CR
import qualified Socket.Datagram.Uninterruptible.MutableBytes.Receive.IPv4 as V4R

-- | Send a datagram using a socket with a pre-designated peer. This
-- refers to a datagram socket for which POSIX @connect@ has locked
-- down communication to an individual peer.
send ::
     Socket 'Connected a -- ^ Socket with designated peer
  -> Bytes -- ^ Slice of a buffer
  -> IO (Either (SendException 'Uninterruptible) ())
send (Socket !sock) !buf =
  CS.send proxy# () sock buf

sendToIPv4 ::
     Socket 'Unconnected ('Internet 'V4) -- ^ IPv4 socket without designated peer
  -> Peer -- ^ Destination
  -> Bytes -- ^ Slice of a buffer
  -> IO (Either (SendException 'Uninterruptible) ())
sendToIPv4 (Socket !sock) !dst !buf =
  V4S.send proxy# dst sock buf

-- | Receive a datagram, discarding the peer address. This can be used with
-- datagram sockets of any family. It is usable with both connected and
-- unconnected datagram sockets.
receive ::
     Socket c a -- ^ Socket
  -> Int -- ^ Maximum datagram size
  -> IO (Either (ReceiveException 'Uninterruptible) ByteArray)
receive (Socket !sock) !maxSz = do
  buf <- PM.newByteArray maxSz
  CR.receive proxy# sock (MutableBytes buf 0 maxSz) () >>= \case
    Right sz -> do
      r <- PM.resizeMutableByteArray buf sz >>= PM.unsafeFreezeByteArray
      pure (Right r)
    Left err -> pure (Left err)

receiveFromIPv4 ::
     Socket 'Unconnected ('Internet 'V4) -- ^ IPv4 socket without designated peer
  -> Int -- ^ Maximum datagram size
  -> IO (Either (ReceiveException 'Uninterruptible) Message)
receiveFromIPv4 (Socket !sock) !maxSz = do
  buf <- PM.newByteArray maxSz
  addr <- PM.newPrimArray 1
  V4R.receive proxy# sock (MutableBytes buf 0 maxSz) (MutablePrimArrayOffset addr 0) >>= \case
    Right size -> do
      r <- PM.resizeMutableByteArray buf size >>= PM.unsafeFreezeByteArray
      posixAddr <- PM.readPrimArray addr 0
      pure (Right (Message (posixToIPv4Peer posixAddr) r))
    Left err -> pure (Left err)

receiveManyFromIPv4 ::
     Socket 'Unconnected ('Internet 'V4) -- ^ Socket
  -> Socket.IPv4.Slab -- ^ Buffers for reception
  -> IO (Either (ReceiveException 'Uninterruptible) (SmallArray Message))
receiveManyFromIPv4 sock slab = do
  MM.receiveManyFromIPv4 sock slab >>= \case
    Left err -> pure (Left err)
    Right n -> do
      arr <- Socket.IPv4.freezeSlab slab n
      pure (Right arr)

receiveMany ::
     Socket 'Unconnected ('Internet 'V4) -- ^ Socket
  -> Socket.Discard.Slab -- ^ Buffers for reception
  -> IO (Either (ReceiveException 'Uninterruptible) (UnliftedArray ByteArray))
receiveMany sock slab = do
  MM.receiveMany sock slab >>= \case
    Left err -> pure (Left err)
    Right n -> do
      arr <- Socket.Discard.freezeSlab slab n
      pure (Right arr)
