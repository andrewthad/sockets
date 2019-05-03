{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language NamedFieldPuns #-}

module Socket.Destined.IPv4.MutableBytes
  ( Buffer
  , Peer
  , advance
  , length
  , send
  ) where

import Prelude hiding (length)

import Data.Bytes.Types (MutableBytes(..))
import Foreign.C.Error (Errno)
import Foreign.C.Types (CInt,CSize)
import GHC.Exts (RealWorld)
import Net.Types (IPv4(..))
import Posix.Socket (MessageFlags,Message(Receive))
import Socket.IPv4 (Endpoint(..))
import Socket.MutableBytes (advance,length)
import System.Posix.Types (Fd)
import qualified Posix.Socket as S

type Buffer = MutableBytes RealWorld
type Peer = Endpoint

send :: Peer -> Fd -> Buffer -> IO (Either Errno CSize)
send !dst !sock (MutableBytes arr off len) =
  S.uninterruptibleSendToInternetMutableByteArray sock arr
    (intToCInt off)
    (intToCSize len)
    mempty
    (endpointToSocketAddressInternet dst)

endpointToSocketAddressInternet :: Endpoint -> S.SocketAddressInternet
endpointToSocketAddressInternet (Endpoint {address, port}) =
  S.SocketAddressInternet
    { S.port = S.hostToNetworkShort port
    , S.address = S.hostToNetworkLong (getIPv4 address)
    }

intToCInt :: Int -> CInt
intToCInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

