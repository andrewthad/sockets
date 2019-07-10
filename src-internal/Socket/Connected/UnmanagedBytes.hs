{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language NamedFieldPuns #-}

module Socket.Connected.UnmanagedBytes
  ( Buffer
  , Peer
  , advance
  , length
  , send
  ) where

import Prelude hiding (length)

import Data.Bytes.Types (UnmanagedBytes(UnmanagedBytes))
import Socket.AddrLength (Buffer,length,advance)
import Foreign.C.Error (Errno)
import Foreign.C.Types (CInt,CSize)
import System.Posix.Types (Fd)
import qualified Posix.Socket as S

type Peer = ()

send :: () -> Fd -> Buffer -> IO (Either Errno CSize)
send !_ !sock (UnmanagedBytes arr len) =
  -- No need for MSG_NOSIGNAL since this is a datagram
  -- socket, not a stream socket.
  S.uninterruptibleSend sock arr
    (intToCSize len)
    mempty

intToCSize :: Int -> CSize
intToCSize = fromIntegral

