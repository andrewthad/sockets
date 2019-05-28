{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}

module Socket.Stream.Uninterruptible.Hybrid
  ( sendMutableBytesUnmanagedBytes
  ) where

import Data.Bytes.Types (MutableBytes,UnmanagedBytes)
import GHC.Exts (RealWorld,proxy#)
import Socket (Interruptibility(Uninterruptible))
import Socket.Stream (Connection,SendException)

import qualified Socket.Stream.Uninterruptible.MutableBytes.Addr.Send as MBA

sendMutableBytesUnmanagedBytes ::
     Connection -- ^ Connection
  -> MutableBytes RealWorld -- ^ First payload
  -> UnmanagedBytes -- ^ Second payload
  -> IO (Either (SendException 'Uninterruptible) ())
sendMutableBytesUnmanagedBytes = MBA.sendBoth proxy#

