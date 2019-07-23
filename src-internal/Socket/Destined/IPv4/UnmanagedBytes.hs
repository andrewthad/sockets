{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language NamedFieldPuns #-}

module Socket.Destined.IPv4.UnmanagedBytes
  ( Buffer
  , Peer
  , advance
  , length
  , send
  ) where

import Prelude hiding (length)

import Data.Bytes.Types (UnmanagedBytes(UnmanagedBytes))
import Foreign.C.Error (Errno)
import Foreign.C.Types (CSize)
import Net.Types (IPv4(..))
import Socket.IPv4 (Peer(..))
import Socket.AddrLength (advance,length)
import System.Posix.Types (Fd)
import qualified Posix.Socket as S

type Buffer = UnmanagedBytes

send :: Peer -> Fd -> UnmanagedBytes -> IO (Either Errno CSize)
send !dst !sock (UnmanagedBytes arr len) =
  S.uninterruptibleSendToInternet sock arr
    (intToCSize len)
    mempty
    (endpointToSocketAddressInternet dst)

endpointToSocketAddressInternet :: Peer -> S.SocketAddressInternet
endpointToSocketAddressInternet (Peer {address, port}) =
  S.SocketAddressInternet
    { S.port = S.hostToNetworkShort port
    , S.address = S.hostToNetworkLong (getIPv4 address)
    }

intToCSize :: Int -> CSize
intToCSize = fromIntegral

