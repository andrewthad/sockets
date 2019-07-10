{-# language BangPatterns #-}
{-# language DataKinds #-}

module Socket.UnmanagedBytes.Peerless
  ( AddressBuffer
  , AddressBufferOffset
  , writeAddress
  , offsetAddress
  , receiveFromOnce
  ) where

import Prelude hiding (length)

import Data.Bytes.Types (UnmanagedBytes(..))
import Foreign.C.Error (Errno)
import Foreign.C.Types (CSize)
import Posix.Socket (MessageFlags,Message(Receive))
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
  -> UnmanagedBytes
  -> MessageFlags 'Receive
  -> ()
  -> IO (Either Errno CSize)
{-# inline receiveFromOnce #-}
receiveFromOnce fd (UnmanagedBytes arr len) flags !_ =
  S.uninterruptibleReceiveFrom_
    fd arr (intToCSize len) flags

intToCSize :: Int -> CSize
{-# inline intToCSize #-}
intToCSize = fromIntegral
