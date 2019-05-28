{-# language DuplicateRecordFields #-}

module Socket.AddrLength
  ( Buffer
  , advance
  , length
  , sendOnce
  , receiveOnce
  ) where

import Prelude hiding (length)

import Data.Bytes.Types (UnmanagedBytes(UnmanagedBytes))
import Posix.Socket (uninterruptibleSend,noSignal)
import Posix.Socket (uninterruptibleReceive)
import Foreign.C.Types (CSize)
import Foreign.C.Error (Errno)
import System.Posix.Types (Fd)

import qualified Data.Primitive.Addr as PM

type Buffer = UnmanagedBytes

advance :: UnmanagedBytes -> Int -> UnmanagedBytes
{-# inline advance #-}
advance (UnmanagedBytes addr len) n = UnmanagedBytes (PM.plusAddr addr n) (len - n)

length :: UnmanagedBytes -> Int
{-# inline length #-}
length (UnmanagedBytes _ len) = len

sendOnce :: Fd -> UnmanagedBytes -> IO (Either Errno CSize)
{-# inline sendOnce #-}
sendOnce fd (UnmanagedBytes addr len) =
  uninterruptibleSend fd addr (intToCSize len) noSignal

receiveOnce :: Fd -> UnmanagedBytes -> IO (Either Errno CSize)
{-# inline receiveOnce #-}
receiveOnce fd (UnmanagedBytes addr len) =
  uninterruptibleReceive fd addr (intToCSize len) mempty

intToCSize :: Int -> CSize
{-# inline intToCSize #-}
intToCSize = fromIntegral
