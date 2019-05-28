{-# language BangPatterns #-}
{-# language DataKinds #-}

module Socket.Stream.Interruptible.Hybrid
  ( sendMutableBytesUnmanagedBytes
  ) where

import Control.Concurrent.STM (TVar)
import Data.Bytes.Types (MutableBytes,UnmanagedBytes)
import GHC.Exts (RealWorld)
import Socket (Interruptibility(Interruptible))
import Socket.Stream (Connection,SendException)

import qualified Socket.Stream.Interruptible.MutableBytes.Addr.Send as MBA

sendMutableBytesUnmanagedBytes ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Connection -- ^ Connection
  -> MutableBytes RealWorld -- ^ First payload
  -> UnmanagedBytes -- ^ Second payload
  -> IO (Either (SendException 'Interruptible) ())
sendMutableBytesUnmanagedBytes = MBA.sendBoth
