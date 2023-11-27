{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneDeriving #-}
{-# language UnboxedTuples #-}

module Socket.Discard
  ( PeerlessSlab(..)
  , newPeerlessSlab
  , freezePeerlessSlab
  , freezePeerlessSlabAsByteString
  , replenishPeerlessSlab
  , replenishPinnedPeerlessSlab
  ) where

import Control.Monad.Primitive (primitive,primitive_)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Primitive (ByteArray(..),SmallMutableArray,SmallArray)
import Data.Primitive (MutablePrimArray,MutableByteArray)
import Data.Primitive.Unlifted.Array (MutableUnliftedArray,UnliftedArray)
import Foreign.C.Types (CInt)
import GHC.Exts (RealWorld,Int(I#))
import Socket (Pinnedness(Pinned,Unpinned))
import Socket.Error (die)
import Socket.Interop (fromPinned)
import Socket.Slab (replenishPinned,replenishUnpinned)

import qualified GHC.Exts as Exts
import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM

-- | A slab of memory for bulk datagram ingest (via @recvmmsg@).
-- Slabs are not safe for concurrent access. This variant does
-- not have space for the peer addresses.
data PeerlessSlab :: Pinnedness -> Type where
  PeerlessSlab ::
    -- Invariant: payload sizes are nondecreasing. An
    -- implication is that zero-size payloads come first.
    -- Invariant: All non-zero-size payloads match the
    -- phantom pinnedness. All zero-size payloads are
    -- unpinned.
    { sizes :: !(MutablePrimArray RealWorld CInt)
      -- ^ Buffer for returned datagram lengths
    , payloads :: !(MutableUnliftedArray RealWorld (MutableByteArray RealWorld))
      -- ^ Buffers for datagram payloads, no slicing
    } -> PeerlessSlab p

-- | Allocate a slab that is used to receive multiple datagrams at
-- the same time without receiving any information about the peer.
newPeerlessSlab ::
     Int -- ^ maximum datagrams
  -> IO (PeerlessSlab p)
newPeerlessSlab !n = if n >= 1
  then do
    sizes <- PM.newPrimArray n
    tombstone <- PM.newByteArray 0
    payloads <- PM.newUnliftedArray n tombstone
    pure PeerlessSlab{sizes,payloads}
  else die "newPeerlessSlab"

-- | Freeze the specified number of messages in-place and return
-- them. Replaces all of the frozen messages with tombstones so that
-- new buffers can be allocated before the next reception. End users
-- should not need this function.
freezePeerlessSlab ::
     PeerlessSlab p
  -> Int
  -> IO (UnliftedArray ByteArray)
freezePeerlessSlab slab n = do
  msgs <- PM.unsafeNewUnliftedArray n
  tombstone <- PM.newByteArray 0
  freezeSlabGo slab tombstone msgs (n - 1)

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "Socket.Discard: uninitialized element"

freezePeerlessSlabAsByteString ::
     PeerlessSlab 'Pinned
  -> Int
  -> IO (SmallArray ByteString)
freezePeerlessSlabAsByteString slab n = do
  msgs <- PM.newSmallArray n errorThunk
  tombstone <- PM.newByteArray 0
  freezeSlabByteStringGo slab tombstone msgs (n - 1)

freezeSlabByteStringGo ::
     PeerlessSlab 'Pinned
  -> MutableByteArray RealWorld -- tombstone
  -> SmallMutableArray RealWorld ByteString
  -> Int
  -> IO (SmallArray ByteString)
freezeSlabByteStringGo slab@PeerlessSlab{payloads,sizes} !tombstone !arr !ix = if ix > (-1)
  then do
    !size <- PM.readPrimArray sizes ix
    -- Remove the byte array from the array of payloads, freeze it, and
    -- replace it with a tombstone.
    payloadMut <- readMutableByteArrayArray payloads ix
    writeMutableByteArrayArray payloads ix tombstone
    let sz = cintToInt size
    !payload <- PM.resizeMutableByteArray payloadMut sz
    PM.writeSmallArray arr ix $! fromPinned payload 0 sz
    freezeSlabByteStringGo slab tombstone arr (ix - 1)
  else PM.unsafeFreezeSmallArray arr

freezeSlabGo ::
     PeerlessSlab p
  -> MutableByteArray RealWorld -- tombstone
  -> MutableUnliftedArray RealWorld ByteArray
  -> Int
  -> IO (UnliftedArray ByteArray)
freezeSlabGo slab@PeerlessSlab{payloads,sizes} !tombstone !arr !ix = if ix > (-1)
  then do
    !size <- PM.readPrimArray sizes ix
    -- Remove the byte array from the array of payloads, freeze it, and
    -- replace it with a tombstone.
    payloadMut <- readMutableByteArrayArray payloads ix
    writeMutableByteArrayArray payloads ix tombstone
    !payload <- PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray payloadMut (cintToInt size)
    writeByteArrayArray arr ix payload
    freezeSlabGo slab tombstone arr (ix - 1)
  else PM.unsafeFreezeUnliftedArray arr

-- | Ensure that every buffer can accomodate at least the
-- minimum number of bytes.
replenishPeerlessSlab ::
     PeerlessSlab 'Unpinned
  -> Int -- ^ Minimum Size
  -> IO () 
replenishPeerlessSlab PeerlessSlab{payloads} minSz = do
  let sz = PM.sizeofMutableUnliftedArray payloads
  replenishUnpinned payloads minSz 0 sz

replenishPinnedPeerlessSlab ::
     PeerlessSlab 'Pinned
  -> Int -- ^ Minimum Size
  -> IO () 
replenishPinnedPeerlessSlab PeerlessSlab{payloads} minSz = do
  let sz = PM.sizeofMutableUnliftedArray payloads
  replenishPinned payloads minSz 0 sz

cintToInt :: CInt -> Int
cintToInt = fromIntegral

writeMutableByteArrayArray
  :: MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- ^ destination
  -> Int -- ^ index
  -> MutableByteArray RealWorld -- ^ value
  -> IO ()
writeMutableByteArrayArray = PM.writeUnliftedArray

writeByteArrayArray
  :: MutableUnliftedArray RealWorld ByteArray -- ^ destination
  -> Int -- ^ index
  -> ByteArray -- ^ value
  -> IO ()
writeByteArrayArray = PM.writeUnliftedArray

readMutableByteArrayArray
  :: MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- ^ source
  -> Int -- ^ index
  -> IO (MutableByteArray RealWorld)
readMutableByteArrayArray = PM.readUnliftedArray
