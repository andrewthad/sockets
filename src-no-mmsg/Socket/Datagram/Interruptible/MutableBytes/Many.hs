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

import Control.Applicative ((<|>))
import Control.Monad.STM (STM,atomically)
import Control.Concurrent (threadWaitWrite,threadWaitRead,threadWaitReadSTM)
import Control.Concurrent.STM (TVar)
import Control.Exception (mask,onException)
import Data.Functor (($>))
import Data.Primitive (ByteArray,MutableByteArray(..),Array)
import Data.Word (Word16)
import Foreign.C.Error (Errno(..),eWOULDBLOCK,eAGAIN)
import Foreign.C.Types (CInt,CSize,CUInt)
import GHC.Exts (Int(I#),RealWorld,ByteArray#,touch#)
import GHC.IO (IO(..))
import Net.Types (IPv4(..))
import Socket (Interruptibility(Interruptible))
import Socket (Connectedness(..))
import Socket.Datagram (Socket(..),ReceiveException)
import Socket.Debug (debug)
import Socket.IPv4 (Peer(..),Message(..),Slab(..))
import Socket.Discard (Slab(..))
import System.Posix.Types (Fd)

import qualified Socket.IPv4
import qualified Socket.Discard
import qualified Socket as SCK
import qualified Control.Monad.Primitive as PM
import qualified Data.Primitive as PM
import qualified Posix.Socket as S
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
  -> Socket.Discard.Slab
     -- ^ Buffers into which sizes and payloads are received
  -> IO (Either (ReceiveException 'Interruptible) Int)
receiveMany intr (Socket fd) (Socket.Discard.Slab{sizes,payloads}) =
  RU.receiveMany intr fd sizes () payloads

-- | Variant of 'receiveMany' that provides that source address
-- corresponding to each datagram. This introduces another array
-- to the structure-of-arrays.
receiveManyFromIPv4 :: 
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket 'Unconnected ('SCK.Internet 'SCK.V4) -- ^ Socket
  -> Socket.IPv4.Slab
     -- ^ Buffers into which sizes, addresses, and payloads
     -- are received
  -> IO (Either (ReceiveException 'Interruptible) Int)
receiveManyFromIPv4 intr (Socket fd) (Socket.IPv4.Slab{sizes,peers,payloads}) =
  RV4.receiveMany intr fd sizes peers payloads
