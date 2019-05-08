{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneDeriving #-}

module Socket.Discard
  ( Slab(..)
  , newSlab
  , freezeSlab
  ) where

import Data.Kind (Type)
import Data.Word (Word16)
import Net.Types (IPv4(..))
import Data.Primitive (ByteArray(..),MutableByteArray,MutableUnliftedArray)
import Data.Primitive (SmallArray,SmallMutableArray,UnliftedArray)
import Data.Primitive (MutablePrimArray)
import Foreign.C.Types (CInt)
import GHC.Exts (RealWorld)

import qualified Data.Primitive as PM

data Slab = Slab
  { sizes :: !(MutablePrimArray RealWorld CInt)
    -- ^ Buffer for returned datagram lengths
  , payloads :: !(MutableUnliftedArray RealWorld (MutableByteArray RealWorld))
    -- ^ Buffers for datagram payloads, no slicing
  }

-- | Allocate a slab that is used to receive multiple datagrams at
-- the same time.
newSlab ::
     Int -- ^ maximum datagrams
  -> Int -- ^ maximum size of individual datagram 
  -> IO Slab
newSlab n m = do
  sizes <- PM.newPrimArray n
  payloads <- PM.unsafeNewUnliftedArray n
  let go !ix = if ix > (-1)
        then do
          PM.writeUnliftedArray payloads ix =<< PM.newByteArray m
          go (ix - 1)
        else pure ()
  go (n - 1)
  pure Slab{sizes,payloads}

freezeSlab :: Slab -> Int -> IO (UnliftedArray ByteArray)
freezeSlab slab n = do
  msgs <- PM.unsafeNewUnliftedArray n
  freezeSlabGo slab msgs (n - 1)

freezeSlabGo :: Slab -> MutableUnliftedArray RealWorld ByteArray -> Int -> IO (UnliftedArray ByteArray)
freezeSlabGo slab@Slab{payloads,sizes} !arr !ix = if ix > (-1)
  then do
    !size <- PM.readPrimArray sizes ix
    -- Remove the byte array from the array of payloads, freeze it, and
    -- replace it with a freshly allocated byte array. 
    payloadMut <- PM.readUnliftedArray payloads ix
    originalSize <- PM.getSizeofMutableByteArray payloadMut
    !payload <- PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray payloadMut (cintToInt size)
    PM.writeUnliftedArray payloads ix =<< PM.newByteArray originalSize
    PM.writeUnliftedArray arr ix payload
    freezeSlabGo slab arr (ix - 1)
  else PM.unsafeFreezeUnliftedArray arr

cintToInt :: CInt -> Int
cintToInt = fromIntegral
