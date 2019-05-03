{-# language BangPatterns #-}
{-# language DataKinds #-}

module Socket.MutableBytes
  ( Buffer
  , advance
  , length
  , sendOnce
  , receiveOnce
  , receiveFromOnce
  ) where

import Prelude hiding (length)

import Data.Bytes.Types (MutableBytes(..))
import Posix.Socket (uninterruptibleSendMutableByteArray)
import Posix.Socket (uninterruptibleReceiveMutableByteArray)
import Posix.Socket (noSignal)
import Posix.Socket (MessageFlags,Message(Receive))
import Foreign.C.Types (CInt,CSize)
import Foreign.C.Error (Errno)
import System.Posix.Types (Fd)
import GHC.Exts (RealWorld)

import qualified Posix.Socket as S

type Buffer = MutableBytes RealWorld

advance :: MutableBytes RealWorld -> Int -> MutableBytes RealWorld
{-# inline advance #-}
advance (MutableBytes arr off len) n = MutableBytes arr (off + n) (len - n)

length :: MutableBytes RealWorld -> Int
{-# inline length #-}
length (MutableBytes _ _ len) = len

sendOnce :: Fd -> MutableBytes RealWorld -> IO (Either Errno CSize)
{-# inline sendOnce #-}
sendOnce fd (MutableBytes arr off len) =
  uninterruptibleSendMutableByteArray fd arr (intToCInt off) (intToCSize len) noSignal

receiveOnce :: Fd -> MutableBytes RealWorld -> IO (Either Errno CSize)
{-# inline receiveOnce #-}
receiveOnce fd (MutableBytes arr off len) =
  uninterruptibleReceiveMutableByteArray fd arr (intToCInt off) (intToCSize len) mempty

receiveFromOnce ::
     Fd
  -> MutableBytes RealWorld
  -> MessageFlags 'Receive
  -> CInt
  -> IO (Either Errno (CInt, S.SocketAddress, CSize))
{-# inline receiveFromOnce #-}
receiveFromOnce !sock (MutableBytes arr off len) !flags !addrSz =
  S.uninterruptibleReceiveFromMutableByteArray sock arr
    (intToCInt off) (intToCSize len) flags addrSz

intToCInt :: Int -> CInt
{-# inline intToCInt #-}
intToCInt = fromIntegral

intToCSize :: Int -> CSize
{-# inline intToCSize #-}
intToCSize = fromIntegral
