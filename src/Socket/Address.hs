module Socket.Address
  ( Address(..)
  ) where

-- TODO: Add Internet6. This will require improving posix-api. 
data Address
  = Internet4 !IPv4
  | Unix !ByteArray

