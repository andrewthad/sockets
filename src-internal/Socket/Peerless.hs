module Socket.Peerless
  ( Peer
  , maxAddressSize
  , decodeAddress
  ) where

import Foreign.C.Types (CInt)
import Posix.Socket (SocketAddress)

type Peer = ()

maxAddressSize :: CInt
maxAddressSize = (-1)

decodeAddress :: CInt -> SocketAddress -> IO Peer
decodeAddress _ _ = pure ()
