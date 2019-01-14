module Socket.Types
  ( Address(..)
  ) where

data Address
  = Internet4 !IPv4
  | Internet6 !IPv6
  | Unix !ByteArray

data SocketException
  = SocketException

