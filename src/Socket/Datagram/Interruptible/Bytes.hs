{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}

module Socket.Datagram.Interruptible.Bytes
  ( -- * Receive
    receive
  , receiveFromIPv4
    -- * Receive Many
  , receiveMany
  , receiveManyFromIPv4
  , receiveManyPinnedFromIPv4
    -- * Types
  , Message(..)
  , Peer(..)
  , ReceiveException(..)
    -- * Slabs
    -- ** Types
  , PeerlessSlab(..)
  , IPv4Slab(..)
    -- ** Functions
  , newPeerlessSlab
  , newIPv4Slab
  ) where

import Control.Concurrent.STM (TVar)
import Data.Bytes.Types (MutableBytes(..))
import Data.Primitive (ByteArray,SmallArray)
import Data.Primitive.Unlifted.Array (UnliftedArray)
import Data.Primitive.PrimArray.Offset (MutablePrimArrayOffset(..))
import Socket (Connectedness(..),Family(..),Version(..))
import Socket (Pinnedness(Pinned,Unpinned))
import Socket (Interruptibility(Interruptible))
import Socket.Address (posixToIPv4Peer)
import Socket.Datagram (Socket(..),ReceiveException(..))
import Socket.IPv4 (Peer(..),Message(..),IPv4Slab(..),freezeIPv4Slab)
import Socket.IPv4 (newIPv4Slab,replenishIPv4Slab,replenishPinnedIPv4Slab)
import Socket.Discard (PeerlessSlab(..),newPeerlessSlab)

import qualified Data.Primitive as PM
import qualified Socket.Discard
import qualified Socket as SCK
import qualified Socket.Datagram.Interruptible.MutableBytes.Many as MM
import qualified Socket.Datagram.Interruptible.MutableBytes.Receive.Connected as CR
import qualified Socket.Datagram.Interruptible.MutableBytes.Receive.IPv4 as V4R

-- | Receive a datagram, discarding the peer address. This can be used with
-- datagram sockets of any family. It is usable with both connected and
-- unconnected datagram sockets.
receive ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket c a -- ^ Socket
  -> Int -- ^ Maximum datagram size
  -> IO (Either (ReceiveException 'Interruptible) ByteArray)
receive !intr (Socket !sock) !maxSz = do
  buf <- PM.newByteArray maxSz
  CR.receive intr sock (MutableBytes buf 0 maxSz) () >>= \case
    Right sz -> do
      r <- PM.resizeMutableByteArray buf sz >>= PM.unsafeFreezeByteArray
      pure (Right r)
    Left err -> pure (Left err)

receiveFromIPv4 ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket 'Unconnected ('Internet 'V4) -- ^ IPv4 socket without designated peer
  -> Int -- ^ Maximum datagram size
  -> IO (Either (ReceiveException 'Interruptible) Message)
receiveFromIPv4 !intr (Socket !sock) !maxSz = do
  buf <- PM.newByteArray maxSz
  addr <- PM.newPrimArray 1
  V4R.receive intr sock (MutableBytes buf 0 maxSz) (MutablePrimArrayOffset addr 0) >>= \case
    Right size -> do
      r <- PM.resizeMutableByteArray buf size >>= PM.unsafeFreezeByteArray
      posixAddr <- PM.readPrimArray addr 0
      pure (Right (Message (posixToIPv4Peer posixAddr) r))
    Left err -> pure (Left err)

receiveManyFromIPv4 ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket 'Unconnected ('SCK.Internet 'SCK.V4) -- ^ Socket
  -> Socket.IPv4.IPv4Slab 'Unpinned -- ^ Buffers for reception
  -> Int -- ^ Maximum size of single datagram
  -> IO (Either (ReceiveException 'Interruptible) (SmallArray Message))
receiveManyFromIPv4 intr sock slab maxSz = do
  replenishIPv4Slab slab maxSz
  MM.receiveManyFromIPv4 intr sock slab >>= \case
    Left err -> pure (Left err)
    Right n -> do
      arr <- Socket.IPv4.freezeIPv4Slab slab n
      pure (Right arr)

receiveManyPinnedFromIPv4 ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket 'Unconnected ('SCK.Internet 'SCK.V4) -- ^ Socket
  -> Socket.IPv4.IPv4Slab 'Pinned -- ^ Buffers for reception
  -> Int -- ^ Maximum size of single datagram
  -> IO (Either (ReceiveException 'Interruptible) (SmallArray Message))
receiveManyPinnedFromIPv4 intr sock slab maxSz = do
  replenishPinnedIPv4Slab slab maxSz
  MM.receiveManyFromIPv4 intr sock slab >>= \case
    Left err -> pure (Left err)
    Right n -> do
      arr <- Socket.IPv4.freezeIPv4Slab slab n
      pure (Right arr)

receiveMany ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket 'Unconnected ('SCK.Internet 'SCK.V4) -- ^ Socket
  -> PeerlessSlab 'Unpinned -- ^ Buffers for reception
  -> Int -- ^ Maximum size of single datagram
  -> IO (Either (ReceiveException 'Interruptible) (UnliftedArray ByteArray))
receiveMany intr sock slab maxSz = do
  Socket.Discard.replenishPeerlessSlab slab maxSz
  MM.receiveMany intr sock slab >>= \case
    Left err -> pure (Left err)
    Right n -> do
      arr <- Socket.Discard.freezePeerlessSlab slab n
      pure (Right arr)
