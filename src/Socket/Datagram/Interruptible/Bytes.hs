{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}

module Socket.Datagram.Interruptible.Bytes
  ( -- * Receive
    receive
  , receiveFromIPv4
    -- * Receive Many
  , receiveMany
  , receiveManyFromIPv4
  ) where

import Data.Bytes.Types (MutableBytes)
import Socket (Connectedness(..),Family(..),Interruptibility(Interruptible))
import Control.Concurrent.STM (TVar)
import Data.Kind (Type)
import GHC.Exts (RealWorld)
import Data.Primitive (ByteArray,SmallArray,SmallMutableArray,UnliftedArray)
import Data.Bytes.Types (MutableBytes(..),Bytes(..))
import System.Posix.Types (Fd)
import Socket.IPv4 (Peer,Message(..),Receipt(..),Slab(..))
import Socket.Datagram (Socket(..),SendException,ReceiveException)
import Foreign.C.Types (CInt)

import qualified Data.Primitive as PM
import qualified Socket.IPv4
import qualified Socket.Discard
import qualified Socket as SCK
import qualified Socket.Datagram.Interruptible.MutableBytes.Many as MM
import qualified Socket.Datagram.Interruptible.MutableBytes.Receive.Connected as CR
import qualified Socket.Datagram.Interruptible.MutableBytes.Receive.IPv4 as V4R

-- | Receive a datagram, discarding the peer address. This can be used with
-- datagram sockets of any family. It is usable with both connected and
-- unconnected datagram sockets.
receive ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket c a -- ^ Socket
  -> Int -- ^ Maximum datagram size
  -> IO (Either (ReceiveException 'Interruptible) ByteArray)
receive !intr (Socket !sock) !maxSz = do
  buf <- PM.newByteArray maxSz
  CR.receive intr sock (MutableBytes buf 0 maxSz) >>= \case
    Right sz -> do
      r <- PM.resizeMutableByteArray buf sz >>= PM.unsafeFreezeByteArray
      pure (Right r)
    Left err -> pure (Left err)

receiveFromIPv4 ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket 'Unconnected 'IPv4 -- ^ IPv4 socket without designated peer
  -> Int -- ^ Maximum datagram size
  -> IO (Either (ReceiveException 'Interruptible) Message)
receiveFromIPv4 !intr (Socket !sock) !maxSz = do
  buf <- PM.newByteArray maxSz
  V4R.receive intr sock (MutableBytes buf 0 maxSz) >>= \case
    Right (Receipt{peer,size}) -> do
      r <- PM.resizeMutableByteArray buf size >>= PM.unsafeFreezeByteArray
      pure (Right (Message peer r))
    Left err -> pure (Left err)

receiveManyFromIPv4 ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket 'Unconnected 'SCK.IPv4 -- ^ Socket
  -> Socket.IPv4.Slab -- ^ Buffers for reception
  -> IO (Either (ReceiveException 'Interruptible) (SmallArray Message))
receiveManyFromIPv4 intr sock slab = do
  MM.receiveManyFromIPv4 intr sock slab >>= \case
    Left err -> pure (Left err)
    Right n -> do
      arr <- Socket.IPv4.freezeSlab slab n
      pure (Right arr)

receiveMany ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return
     -- @'Left' 'ReceiveInterrupted'@.
  -> Socket 'Unconnected 'SCK.IPv4 -- ^ Socket
  -> Socket.Discard.Slab -- ^ Buffers for reception
  -> IO (Either (ReceiveException 'Interruptible) (UnliftedArray ByteArray))
receiveMany intr sock slab = do
  MM.receiveMany intr sock slab >>= \case
    Left err -> pure (Left err)
    Right n -> do
      arr <- Socket.Discard.freezeSlab slab n
      pure (Right arr)
