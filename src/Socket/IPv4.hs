{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}

module Socket.IPv4
  ( Endpoint(..)
  , describeEndpoint
  ) where

import Data.Word (Word16)
import Net.Types (IPv4(..))
import qualified Data.Text as T
import qualified Net.IPv4 as IPv4

-- | An endpoint for an IPv4 socket, connection, or listener.
--   Everything is in host byte order, and the user is not
--   responisble for performing any conversions.
data Endpoint = Endpoint
  { address :: !IPv4
  , port :: !Word16
  } deriving stock (Eq,Show)

-- This is used internally for debug messages and for presenting
-- unrecoverable exceptions.
describeEndpoint :: Endpoint -> String
describeEndpoint (Endpoint {address,port}) =
  T.unpack (IPv4.encode address) ++ ":" ++ show port
