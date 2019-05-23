{-# language BangPatterns #-}
{-# language DataKinds #-}

module Socket.MutableBytes.SocketAddressInternet
  ( AddressBuffer
  , AddressBufferOffset
  , writeAddress
  , offsetAddress
  , receiveFromOnce
  ) where

import Prelude hiding (length)

import Data.Bytes.Types (MutableBytes(..))
import Data.Primitive.PrimArray.Offset (MutablePrimArrayOffset(..))
import Data.Primitive.ByteArray.Offset (MutableByteArrayOffset(..))
import Foreign.C.Error (Errno)
import Foreign.C.Types (CInt,CSize)
import GHC.Exts (RealWorld)
import Posix.Socket (MessageFlags,Message(Receive))
import Posix.Socket (SocketAddressInternet)
import System.Posix.Types (Fd)

import qualified Data.Primitive as PM
import qualified Posix.Socket as S

type Address = SocketAddressInternet
type AddressBuffer = PM.MutablePrimArray RealWorld SocketAddressInternet
type AddressBufferOffset = MutablePrimArrayOffset RealWorld SocketAddressInternet

writeAddress :: AddressBuffer -> Int -> SocketAddressInternet -> IO ()
{-# inline writeAddress #-}
writeAddress = PM.writePrimArray

offsetAddress :: AddressBuffer -> Int -> AddressBufferOffset
{-# inline offsetAddress #-}
offsetAddress = MutablePrimArrayOffset

receiveFromOnce ::
     Fd
  -> MutableBytes RealWorld
  -> MessageFlags 'Receive
  -> MutablePrimArrayOffset RealWorld SocketAddressInternet
  -> IO (Either Errno CSize)
{-# inline receiveFromOnce #-}
receiveFromOnce !sock (MutableBytes arr off len) !flags !addr =
  S.uninterruptibleReceiveFromInternetMutableByteArray sock
    (MutableByteArrayOffset arr off)
    (intToCSize len)
    flags addr
   
intToCSize :: Int -> CSize
{-# inline intToCSize #-}
intToCSize = fromIntegral
