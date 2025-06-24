{-# language DuplicateRecordFields #-}

module Socket.Datagram.Slab
  ( -- * Types
    D.PeerlessSlab(..)
  , I.IPv4Slab(..)
    -- * Create
  , D.newPeerlessSlab
  , I.newIPv4Slab
    -- * Freeze
  , D.freezePeerlessSlab
  , I.freezeIPv4Slab
  ) where

import qualified Socket.IPv4 as I
import qualified Socket.Discard as D
