module Socket.AddrLength
  ( AddrLength(..)
  ) where

import Data.Primitive (Addr)

data AddrLength = AddrLength
  {-# UNPACK #-} !Addr -- pointer to first byte
  {-# UNPACK #-} !Int -- length
