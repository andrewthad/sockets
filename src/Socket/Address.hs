module Socket.Address
  ( Address(..)
  ) where

data Address
  = Internet4 !IPv4
  | Internet6 !IPv6
  | Unix !ByteArray

