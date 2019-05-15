{-# language BangPatterns #-}
{-# language DataKinds #-}

module Socket.MutableBytes.Peerless
  ( AddressBuffer
  , AddressBufferOffset
  , writeAddress
  , offsetAddress
  , receiveFromOnce
  ) where

import Prelude hiding (length)

import Data.Bytes.Types (MutableBytes(..))
import Data.Primitive.PrimArray.Offset (MutablePrimArrayOffset)
import Foreign.C.Error (Errno)
import Foreign.C.Types (CInt,CSize)
import GHC.Exts (RealWorld)
import Posix.Socket (MessageFlags,Message(Receive))
import Posix.Socket (SocketAddressInternet)
import Socket.MutableBytes (Buffer,advance,length)
import System.Posix.Types (Fd)
import qualified Posix.Socket as S

type Address = ()
type AddressBuffer = ()
type AddressBufferOffset = ()

writeAddress :: AddressBuffer -> Int -> Address -> IO ()
writeAddress _ _ _ = pure ()

offsetAddress :: AddressBuffer -> Int -> AddressBufferOffset
offsetAddress _ _ = ()

receiveFromOnce ::
     Fd
  -> MutableBytes RealWorld
  -> MessageFlags 'Receive
  -> ()
  -> IO (Either Errno CSize)
{-# inline receiveFromOnce #-}
receiveFromOnce fd (MutableBytes arr off len) flags !_ =
  S.uninterruptibleReceiveFromMutableByteArray_
    fd arr (intToCInt off) (intToCSize len) flags
   
intToCInt :: Int -> CInt
{-# inline intToCInt #-}
intToCInt = fromIntegral

intToCSize :: Int -> CSize
{-# inline intToCSize #-}
intToCSize = fromIntegral
