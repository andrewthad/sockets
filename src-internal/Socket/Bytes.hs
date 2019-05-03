module Socket.Bytes
  ( Buffer
  , advance
  , length
  , sendOnce
  ) where

-- Note that this module does not include a function for receiving
-- bytes since we use a mutable byte array for byte reception.

import Prelude hiding (length)

import Data.Bytes.Types (Bytes(..))
import Posix.Socket (uninterruptibleSendByteArray,noSignal)
import Foreign.C.Types (CInt,CSize)
import Foreign.C.Error (Errno)
import System.Posix.Types (Fd)

type Buffer = Bytes

advance :: Bytes -> Int -> Bytes
{-# inline advance #-}
advance (Bytes arr off len) n = Bytes arr (off + n) (len - n)

length :: Bytes -> Int
{-# inline length #-}
length (Bytes _ _ len) = len

intToCInt :: Int -> CInt
{-# inline intToCInt #-}
intToCInt = fromIntegral

intToCSize :: Int -> CSize
{-# inline intToCSize #-}
intToCSize = fromIntegral

sendOnce :: Fd -> Bytes -> IO (Either Errno CSize)
{-# inline sendOnce #-}
sendOnce fd (Bytes arr off len) =
  uninterruptibleSendByteArray fd arr (intToCInt off) (intToCSize len) noSignal

