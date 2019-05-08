module Socket.Peerless
  ( Peer
  , Reception
  , maxAddressSize
  , decodeAddress
  , buildReception
  , receptionPeer
  , receptionSize
  ) where

import Foreign.C.Types (CInt)
import Posix.Socket (SocketAddress)

type Peer = ()
type Reception = Int

maxAddressSize :: CInt
maxAddressSize = (-1)

decodeAddress :: CInt -> SocketAddress -> IO Peer
decodeAddress _ _ = pure ()

-- Not used
buildReception :: () -> Int -> Int
buildReception _ = id

receptionPeer :: Reception -> Peer
receptionPeer _ = ()

receptionSize :: Reception -> Int
receptionSize = id

