{-# language NamedFieldPuns #-}

module Socket.Decode.IPv4
  ( Peer
  , Reception
  , maxAddressSize
  , decodeAddress
  , buildReception
  , receptionPeer
  , receptionSize
  ) where

import Foreign.C.Types (CInt)
import Net.Types (IPv4(IPv4))
import Posix.Socket (SocketAddress)
import Socket.Error (die)
import Socket.IPv4 (Peer(..),Receipt(..))

import qualified Posix.Socket as S

type Reception = Receipt

maxAddressSize :: CInt
maxAddressSize = S.sizeofSocketAddressInternet

decodeAddress :: CInt -> SocketAddress -> IO Peer
decodeAddress reqSz sockAddr
  | reqSz == S.sizeofSocketAddressInternet =
      case S.decodeSocketAddressInternet sockAddr of
        Just sockAddrInet -> do
          pure (socketAddressInternetToEndpoint sockAddrInet)
        Nothing -> die "Socket.Decode.IPv4: non-inet family"
  | otherwise = die "Socket.Decode.IPv4: sockaddr size"

socketAddressInternetToEndpoint :: S.SocketAddressInternet -> Peer
socketAddressInternetToEndpoint (S.SocketAddressInternet {S.address,S.port}) = Peer
  { address = IPv4 (S.networkToHostLong address)
  , port = S.networkToHostShort port
  }

buildReception :: Peer -> Int -> Reception
buildReception = Receipt

receptionPeer :: Reception -> Peer
receptionPeer (Receipt{peer}) = peer

receptionSize :: Reception -> Int
receptionSize (Receipt{size}) = size
