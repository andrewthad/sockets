{-# language BangPatterns #-}

module Socket.Slab
  ( replenishUnpinned
  , replenishPinned
  ) where

import Data.Primitive (MutableByteArray)
import Data.Primitive.Unlifted.Array (MutableUnliftedArray)
import GHC.Exts (RealWorld)

import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM

-- Notice that this only replaces buffers until we
-- reach a single buffer that is of sufficient size.
-- This works because the buffer sizes are nondecreasing.
replenishUnpinned ::
     MutableUnliftedArray RealWorld (MutableByteArray RealWorld)
  -> Int
  -> Int
  -> Int
  -> IO () 
replenishUnpinned !xs !minSz !ix !sz = if ix < sz
  then do
    p <- PM.readUnliftedArray xs ix
    pSz <- PM.getSizeofMutableByteArray p
    if pSz < minSz
      then do
        PM.writeUnliftedArray xs ix =<< PM.newByteArray minSz
        replenishUnpinned xs minSz (ix + 1) sz
      else pure ()
  else pure ()

replenishPinned ::
     MutableUnliftedArray RealWorld (MutableByteArray RealWorld)
  -> Int
  -> Int
  -> Int
  -> IO () 
replenishPinned !xs !minSz !ix !sz = if ix < sz
  then do
    p <- PM.readUnliftedArray xs ix
    pSz <- PM.getSizeofMutableByteArray p
    if pSz < minSz
      then do
        PM.writeUnliftedArray xs ix =<< PM.newPinnedByteArray minSz
        replenishPinned xs minSz (ix + 1) sz
      else pure ()
  else pure ()

