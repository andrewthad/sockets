{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language UnboxedTuples #-}
module Socket.Datagram.Interruptible.MutableBytes.Many
  ( receiveMany
  , receiveManyFromIPv4
  ) where

import Control.Concurrent.STM (TVar)
import Socket (Interruptibility(Interruptible))
import Socket (Connectedness(..))
import Socket.Datagram (Socket(..),ReceiveException)
import Socket.Discard (PeerlessSlab(..))
import Socket.IPv4 (IPv4Slab(..))

import qualified Socket as SCK
import qualified Socket.Datagram.Interruptible.MutableBytes.Receive.Many.Unit as RU
import qualified Socket.Datagram.Interruptible.MutableBytes.Receive.Many.IPv4 as RV4

-- | Receive up to the specified number of datagrams into freshly allocated
--   byte arrays. When there are many datagrams present in the receive
--   buffer, this is more efficient than calling 'receive' repeatedly. This
--   is guaranteed to fill the buffer with at least one message.
--
--   The length buffer and the payload buffers arrange data in a
--   structure-of-arrays fashion. The size of the payload received
--   into @payloads[j]@ is stored at @lengths[j]@.
receiveMany :: 
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket c a
     -- ^ Socket
  -> Socket.Discard.PeerlessSlab p
     -- ^ Buffers into which sizes and payloads are received
  -> IO (Either (ReceiveException 'Interruptible) Int)
receiveMany intr (Socket fd) (Socket.Discard.PeerlessSlab{sizes,payloads}) =
  RU.receiveMany intr fd sizes () payloads

-- | Variant of 'receiveMany' that provides that source address
-- corresponding to each datagram. This introduces another array
-- to the structure-of-arrays.
receiveManyFromIPv4 :: 
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket 'Unconnected ('SCK.Internet 'SCK.V4) -- ^ Socket
  -> Socket.IPv4.IPv4Slab p
     -- ^ Buffers into which sizes, addresses, and payloads
     -- are received
  -> IO (Either (ReceiveException 'Interruptible) Int)
receiveManyFromIPv4 intr (Socket fd) (Socket.IPv4.IPv4Slab{sizes,peers,payloads}) =
  RV4.receiveMany intr fd sizes peers payloads
