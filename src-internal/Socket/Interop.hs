{-# language MagicHash #-}

module Socket.Interop
  ( fromPinned
  ) where

import Data.ByteString.Internal (ByteString(PS))
import Data.Primitive (MutableByteArray(..))
import GHC.Exts (RealWorld,byteArrayContents#,unsafeCoerce#)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr),ForeignPtrContents(PlainPtr))

-- Precondition: the argument buffer is pinned
fromPinned :: MutableByteArray RealWorld -> Int -> Int -> ByteString
{-# inline fromPinned #-}
fromPinned (MutableByteArray marr#) off len =
  PS (ForeignPtr (byteArrayContents# (unsafeCoerce# marr#)) (PlainPtr marr#)) off len
