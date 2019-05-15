{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}

module Socket.Address
  ( posixToIPv4Peer
  , ipv4PeerToPosix
  ) where

import Net.Types (IPv4(..))

import qualified Posix.Socket as S
import qualified Socket.IPv4 as IPv4

posixToIPv4Peer :: S.SocketAddressInternet -> IPv4.Peer
{-# inline posixToIPv4Peer #-}
posixToIPv4Peer (S.SocketAddressInternet {address,port}) = IPv4.Peer
  { address = IPv4 (S.networkToHostLong address)
  , port = S.networkToHostShort port
  }

ipv4PeerToPosix :: IPv4.Peer -> S.SocketAddressInternet
{-# inline ipv4PeerToPosix #-}
ipv4PeerToPosix (IPv4.Peer {address, port}) = S.SocketAddressInternet
  { port = S.hostToNetworkShort port
  , address = S.hostToNetworkLong (getIPv4 address)
  }
