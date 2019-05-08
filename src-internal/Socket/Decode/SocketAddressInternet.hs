{-# language NamedFieldPuns #-}

module Socket.Decode.SocketAddressInternet
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
import Posix.Socket (SocketAddress,SocketAddressInternet)
import Socket.Error (die)

import qualified Posix.Socket as S

type Peer = SocketAddressInternet
type Reception = (SocketAddressInternet,Int)

maxAddressSize :: CInt
maxAddressSize = S.sizeofSocketAddressInternet

decodeAddress :: CInt -> SocketAddress -> IO Peer
decodeAddress reqSz sockAddr
  | reqSz == S.sizeofSocketAddressInternet =
      case S.decodeSocketAddressInternet sockAddr of
        Just sockAddrInet -> pure sockAddrInet
        Nothing -> die "Socket.Decode.SocketAddressInternet: non-inet family"
  | otherwise = die $ concat
      [ "Socket.Decode.SocketAddressInternet: expected sockaddr size "
      , show S.sizeofSocketAddressInternet
      , " but got size "
      , show reqSz
      ]

buildReception :: SocketAddressInternet -> Int -> Reception
buildReception = (,)

receptionPeer :: Reception -> Peer
receptionPeer = fst

receptionSize :: Reception -> Int
receptionSize = snd
