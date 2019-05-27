{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language NamedFieldPuns #-}

module Socket.Connected.MutableBytes
  ( Buffer
  , Peer
  , advance
  , length
  , send
  ) where

import Prelude hiding (length)

import Data.Bytes.Types (MutableBytes(MutableBytes))
import Socket.MutableBytes (Buffer,length,advance)
import Foreign.C.Error (Errno)
import Foreign.C.Types (CInt,CSize)
import System.Posix.Types (Fd)
import qualified Posix.Socket as S

type Peer = ()

send :: () -> Fd -> Buffer -> IO (Either Errno CSize)
send !_ !sock (MutableBytes arr off len) =
  -- No need for MSG_NOSIGNAL since this is a datagram
  -- socket, not a stream socket.
  S.uninterruptibleSendMutableByteArray sock arr
    (intToCInt off)
    (intToCSize len)
    mempty

intToCInt :: Int -> CInt
intToCInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral



