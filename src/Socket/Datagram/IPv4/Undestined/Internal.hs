{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}

module Socket.Datagram.IPv4.Undestined.Internal
  ( Socket(..)
  , Message(..)
  ) where

import Socket.IPv4 (Endpoint)
import System.Posix.Types (Fd)
import Data.Primitive (ByteArray)

-- | A connectionless datagram socket that may communicate with many different
-- endpoints on a datagram-by-datagram basis.
newtype Socket = Socket Fd
  deriving stock (Eq,Ord,Show)

data Message = Message
  { remote :: {-# UNPACK #-} !Endpoint
  , payload :: !ByteArray
  } deriving stock (Eq,Show)


