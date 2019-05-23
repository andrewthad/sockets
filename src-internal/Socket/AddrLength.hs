module Socket.AddrLength
  ( AddrLength(..)
  , Buffer
  , advance
  , length
  , sendOnce
  , receiveOnce
  ) where

import Prelude hiding (length)

import Data.Primitive (Addr)
import Posix.Socket (uninterruptibleSend,noSignal)
import Posix.Socket (uninterruptibleReceive)
import Foreign.C.Types (CSize)
import Foreign.C.Error (Errno)
import System.Posix.Types (Fd)

import qualified Data.Primitive as PM

data AddrLength = AddrLength
  {-# UNPACK #-} !Addr -- pointer to first byte
  {-# UNPACK #-} !Int -- length

type Buffer = AddrLength

advance :: AddrLength -> Int -> AddrLength
{-# inline advance #-}
advance (AddrLength addr len) n = AddrLength (PM.plusAddr addr n) (len - n)

length :: AddrLength -> Int
{-# inline length #-}
length (AddrLength _ len) = len

sendOnce :: Fd -> AddrLength -> IO (Either Errno CSize)
{-# inline sendOnce #-}
sendOnce fd (AddrLength addr len) =
  uninterruptibleSend fd addr (intToCSize len) noSignal

receiveOnce :: Fd -> AddrLength -> IO (Either Errno CSize)
{-# inline receiveOnce #-}
receiveOnce fd (AddrLength addr len) =
  uninterruptibleReceive fd addr (intToCSize len) mempty

intToCSize :: Int -> CSize
{-# inline intToCSize #-}
intToCSize = fromIntegral
