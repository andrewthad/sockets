{-# language NamedFieldPuns #-}

module Socket.Decode.IPv4
  ( Peer
  , maxAddressSize
  , decodeAddress
  ) where

import Foreign.C.Types (CInt)
import Net.Types (IPv4(IPv4))
import Posix.Socket (SocketAddress)
import Socket.Error (die)
import Socket.IPv4 (Endpoint(..))

import qualified Posix.Socket as S

type Peer = Endpoint

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

socketAddressInternetToEndpoint :: S.SocketAddressInternet -> Endpoint
socketAddressInternetToEndpoint (S.SocketAddressInternet {S.address,S.port}) = Endpoint
  { address = IPv4 (S.networkToHostLong address)
  , port = S.networkToHostShort port
  }

