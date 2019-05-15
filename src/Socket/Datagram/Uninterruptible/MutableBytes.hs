{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language MagicHash #-}

module Socket.Datagram.Uninterruptible.MutableBytes
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

import Data.Bytes.Types (MutableBytes)
import Data.Primitive.PrimArray.Offset (MutablePrimArrayOffset)
import GHC.Exts (RealWorld,proxy#)
import Posix.Socket (SocketAddressInternet)
import Socket (Connectedness(..),Family(..),Interruptibility(Uninterruptible))
import Socket.Datagram (Socket(..),SendException,ReceiveException)
import Socket.IPv4 (Peer,Receipt)

import qualified Socket.Datagram.Uninterruptible.MutableBytes.Many as MM
import qualified Socket.Datagram.Uninterruptible.MutableBytes.Receive.Connected as CR
import qualified Socket.Datagram.Uninterruptible.MutableBytes.Send.Connected as CS
import qualified Socket.Datagram.Uninterruptible.MutableBytes.Send.IPv4 as V4S
import qualified Socket.Datagram.Uninterruptible.MutableBytes.Receive.IPv4 as V4R

-- | Send a datagram using a socket with a pre-designated peer. This
-- refers to a datagram socket for which POSIX @connect@ has locked
-- down communication to an individual peer.
send ::
     Socket 'Connected a -- ^ Socket with designated peer
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (SendException 'Uninterruptible) ())
send (Socket !sock) !buf =
  CS.send proxy# () sock buf

-- | Receive a datagram, discarding the peer address. This can be used with
-- datagram sockets of any family. It is usable with both connected and
-- unconnected datagram sockets.
receive ::
     Socket c a -- ^ Socket
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (ReceiveException 'Uninterruptible) Int)
receive (Socket !sock) !buf =
  CR.receive proxy# sock buf () >>= \case
    Right sz -> pure (Right sz)
    Left err -> pure (Left err)

sendToIPv4 ::
     Socket 'Unconnected 'IPv4 -- ^ IPv4 socket without designated peer
  -> Peer -- ^ Destination
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (SendException 'Uninterruptible) ())
sendToIPv4 (Socket !sock) !dst !buf =
  V4S.send proxy# dst sock buf

receiveFromIPv4 ::
     Socket 'Unconnected 'IPv4 -- ^ IPv4 socket without designated peer
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> MutablePrimArrayOffset RealWorld SocketAddressInternet
     -- ^ Buffer for returned peer address
  -> IO (Either (ReceiveException 'Uninterruptible) Int)
receiveFromIPv4 (Socket !sock) !buf !addr =
  V4R.receive proxy# sock buf addr

