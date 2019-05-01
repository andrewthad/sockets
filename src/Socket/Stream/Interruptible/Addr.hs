{-# language BangPatterns #-}
{-# language DataKinds #-}

module Socket.Stream.Interruptible.Addr
  ( send
  , receiveExactly
  , receiveOnce
  , receiveBetween
  ) where

import Data.Primitive (Addr)
import Control.Concurrent.STM (TVar)
import Socket.Stream (Connection,ReceiveException,SendException)
import Socket (Interruptibility(Interruptible))
import Socket.AddrLength (AddrLength(..))

import qualified Socket.Stream.Interruptible.Addr.Send as Send
import qualified Socket.Stream.Interruptible.Addr.Receive as Receive

-- | Send an exact number of bytes starting from a given address. If needed,
--   this calls POSIX @send@ repeatedly until the requested number of bytes
--   has been sent.
send ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Connection -- ^ Connection
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Number of bytes to send
  -> IO (Either (SendException 'Interruptible) ())
{-# inline send #-}
send !tv !conn !addr !len = Send.send tv conn (AddrLength addr len)

-- | Receive the requested number of bytes into memory beginning at
--   the specified address. If needed, this may call @recv@ repeatedly until
--   the requested number of bytes have been received.
receiveExactly ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Exact number of bytes to receive
  -> IO (Either (ReceiveException 'Interruptible) ())
{-# inline receiveExactly #-}
receiveExactly !tv !conn !addr !len =
  Receive.receiveExactly tv conn (AddrLength addr len)

-- | Receive at most the specified number of bytes. This
-- only makes multiple calls to POSIX @recv@ if EAGAIN is returned. It makes at
-- most one @recv@ call that successfully fills the buffer.
receiveOnce ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Interruptible) Int)
{-# inline receiveOnce #-}
receiveOnce tv conn addr len =
  Receive.receiveOnce tv conn (AddrLength addr len)

-- | Receive a number of bytes that is between the inclusive lower and
--   upper bounds. If needed, this may call @recv@ repeatedly until the
--   minimum requested number of bytes have been received.
receiveBetween ::
     TVar Bool 
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Connection -- ^ Connection
  -> Addr -- ^ Start of buffer
  -> Int
     -- ^ Minimum number of bytes to receive
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Interruptible) Int)
{-# inline receiveBetween #-}
receiveBetween tv conn addr minLen maxLen =
  Receive.receiveBetween tv conn (AddrLength addr maxLen) minLen
