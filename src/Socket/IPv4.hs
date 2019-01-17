{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}

module Socket.IPv4
  ( Endpoint(..)
  ) where

import Data.Word (Word16)
import Net.Types (IPv4(..))

-- | An endpoint for an IPv4 socket, connection, or listener.
--   Everything is in host byte order, and the user is not
--   responisble for performing any conversions.
data Endpoint = Endpoint
  { address :: !IPv4
  , port :: !Word16
  } deriving stock (Eq,Show)
