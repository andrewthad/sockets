{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}

module Socket.Datagram.Interruptible.MutableBytes
  ( -- * Send
    send
  , sendToIPv4
    -- * Receive
  , receive
  , receiveFromIPv4
    -- * Receive Many
  , MM.receiveMany
  , MM.receiveManyFromIPv4
  ) where

import Control.Concurrent.STM (TVar)
import Data.Bytes.Types (MutableBytes)
import Data.Primitive.PrimArray.Offset (MutablePrimArrayOffset)
import GHC.Exts (RealWorld)
import Posix.Socket (SocketAddressInternet)
import Socket (Connectedness(..),Family(..),Version(..),Interruptibility(Interruptible))
import Socket.Datagram (Socket(..),SendException,ReceiveException)
import Socket.IPv4 (Peer)

import qualified Socket.Datagram.Interruptible.MutableBytes.Many as MM
import qualified Socket.Datagram.Interruptible.MutableBytes.Receive.Connected as CR
import qualified Socket.Datagram.Interruptible.MutableBytes.Send.Connected as CS
import qualified Socket.Datagram.Interruptible.MutableBytes.Send.IPv4 as V4S
import qualified Socket.Datagram.Interruptible.MutableBytes.Receive.IPv4 as V4R

-- | Send a datagram using a socket with a pre-designated peer. This refers
-- to a datagram socket for which POSIX @connect@ has locked down communication
-- to an individual peer.
send ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Socket 'Connected a -- ^ Socket with designated peer
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (SendException 'Interruptible) ())
send !intr (Socket !sock) !buf =
  CS.send intr () sock buf

-- | Receive a datagram, discarding the peer address. This can be used with
-- datagram sockets of any family. It is usable with both connected and
-- unconnected datagram sockets.
receive ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Socket c a -- ^ Socket
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (ReceiveException 'Interruptible) Int)
receive !intr (Socket !sock) !buf =
  CR.receive intr sock buf () >>= \case
    Right sz -> pure (Right sz)
    Left err -> pure (Left err)

sendToIPv4 ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Socket 'Unconnected ('Internet 'V4) -- ^ IPv4 socket without designated peer
  -> Peer -- ^ Destination
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (SendException 'Interruptible) ())
sendToIPv4 !intr (Socket !sock) !dst !buf =
  V4S.send intr dst sock buf

receiveFromIPv4 ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Socket 'Unconnected ('Internet 'V4) -- ^ IPv4 socket without designated peer
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> MutablePrimArrayOffset RealWorld SocketAddressInternet
     -- ^ Buffer for returned peer address
  -> IO (Either (ReceiveException 'Interruptible) Int)
receiveFromIPv4 !intr (Socket !sock) !buf !addr =
  V4R.receive intr sock buf addr

