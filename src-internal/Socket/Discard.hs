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
  ) where

import Control.Monad.Primitive (primitive,primitive_)
import Data.Primitive (ByteArray(..))
import Data.Primitive.Unlifted.Array (MutableUnliftedArray,UnliftedArray)
import Data.Primitive (MutablePrimArray,MutableByteArray)
import Data.Primitive (SmallArray,SmallMutableArray)
import Data.Word (Word16)
import Foreign.C.Types (CInt)
import GHC.Exts (RealWorld,Int(I#))
import Socket.Error (die)

import qualified GHC.Exts as Exts
import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM

-- | A slab of memory for bulk datagram ingest (via @recvmmsg@).
-- Slabs are not safe for concurrent access. This variant does
-- not have space for the peer addresses.
data PeerlessSlab = PeerlessSlab
  { sizes :: !(MutablePrimArray RealWorld CInt)
    -- ^ Buffer for returned datagram lengths
  , payloads :: !(MutableUnliftedArray RealWorld (MutableByteArray RealWorld))
    -- ^ Buffers for datagram payloads, no slicing
  }

-- | Allocate a slab that is used to receive multiple datagrams at
-- the same time without receiving any information about the peer.
newPeerlessSlab ::
     Int -- ^ maximum datagrams
  -> Int -- ^ maximum size of individual datagram 
  -> IO PeerlessSlab
newPeerlessSlab !n !m = if n >= 1 && m >= 1
  then do
    sizes <- PM.newPrimArray n
    payloads <- PM.unsafeNewUnliftedArray n
    let go !ix = if ix > (-1)
          then do
            writeMutableByteArrayArray payloads ix =<< PM.newByteArray m
            go (ix - 1)
          else pure ()
    go (n - 1)
    pure PeerlessSlab{sizes,payloads}
  else die "newSlab"

-- | Freeze the specified number of messages in-place and return
-- them Replaces all of the frozen messages with tombstones so that
-- new buffers can be allocated before the next reception. End users
-- should not need this function.
freezePeerlessSlab :: PeerlessSlab -> Int -> IO (UnliftedArray ByteArray)
freezePeerlessSlab slab n = do
  msgs <- PM.unsafeNewUnliftedArray n
  freezeSlabGo slab msgs (n - 1)

freezeSlabGo ::
     PeerlessSlab
  -> MutableUnliftedArray RealWorld ByteArray
  -> Int
  -> IO (UnliftedArray ByteArray)
freezeSlabGo slab@PeerlessSlab{payloads,sizes} !arr !ix = if ix > (-1)
  then do
    !size <- PM.readPrimArray sizes ix
    -- Remove the byte array from the array of payloads, freeze it, and
    -- replace it with a freshly allocated byte array. 
    payloadMut <- readMutableByteArrayArray payloads ix
    originalSize <- PM.getSizeofMutableByteArray payloadMut
    writeMutableByteArrayArray payloads ix =<< PM.newByteArray originalSize
    !payload <- PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray payloadMut (cintToInt size)
    writeByteArrayArray arr ix payload
    freezeSlabGo slab arr (ix - 1)
  else PM.unsafeFreezeUnliftedArray arr

cintToInt :: CInt -> Int
cintToInt = fromIntegral

writeMutableByteArrayArray
  :: MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- ^ destination
  -> Int -- ^ index
  -> MutableByteArray RealWorld -- ^ value
  -> IO ()
writeMutableByteArrayArray (PM.MutableUnliftedArray maa#) (I# i#) (PM.MutableByteArray a)
  = primitive_ (Exts.writeMutableByteArrayArray# maa# i# a)

writeByteArrayArray
  :: MutableUnliftedArray RealWorld ByteArray -- ^ destination
  -> Int -- ^ index
  -> ByteArray -- ^ value
  -> IO ()
writeByteArrayArray (PM.MutableUnliftedArray maa#) (I# i#) (PM.ByteArray a)
  = primitive_ (Exts.writeByteArrayArray# maa# i# a)

readMutableByteArrayArray
  :: MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- ^ source
  -> Int -- ^ index
  -> IO (MutableByteArray RealWorld)
readMutableByteArrayArray (PM.MutableUnliftedArray maa#) (I# i#)
  = primitive $ \s -> case Exts.readMutableByteArrayArray# maa# i# s of
      (# s', aa# #) -> (# s', PM.MutableByteArray aa# #)
