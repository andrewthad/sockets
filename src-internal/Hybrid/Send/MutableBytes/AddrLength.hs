{-# language DuplicateRecordFields #-}

module Hybrid.Send.MutableBytes.AddrLength
  ( sendOnce
  ) where

import Data.Bytes.Types (MutableBytes(..),UnmanagedBytes(..))
import Foreign.C.Error (Errno)
import Foreign.C.Types (CSize)
import GHC.Exts (RealWorld)
import System.Posix.Types (Fd)
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset(..))

import qualified Posix.Socket as S

sendOnce ::
     Fd
  -> MutableBytes RealWorld
  -> UnmanagedBytes
  -> IO (Either Errno CSize)
sendOnce fd (MutableBytes bufA offA lenA) (UnmanagedBytes bufB lenB) =
  S.uninterruptibleSendMessageB fd
    (MutableByteArrayOffset {array=bufA,offset=offA}) (intToCSize lenA)
    bufB (intToCSize lenB)
    S.noSignal

intToCSize :: Int -> CSize
intToCSize = fromIntegral
