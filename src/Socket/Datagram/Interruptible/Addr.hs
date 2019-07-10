{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language MagicHash #-}

module Socket.Datagram.Interruptible.Addr
  ( -- * Send
    send
  , sendToIPv4
    -- * Receive
  , receive
  , receiveFromIPv4
    -- * Slabs
    -- ** Types
  , PeerlessSlab(..)
  , IPv4Slab(..)
    -- ** Functions
  , newPeerlessSlab
  , newIPv4Slab
  ) where

import Control.Concurrent.STM (TVar)
import Data.Bytes.Types (UnmanagedBytes(UnmanagedBytes))
import Data.Primitive.Addr (Addr)
import Data.Primitive.PrimArray.Offset (MutablePrimArrayOffset)
import GHC.Exts (RealWorld)
import Posix.Socket (SocketAddressInternet)
import Socket (Connectedness(..),Family(..),Version(..),Interruptibility(Interruptible))
import Socket.Datagram (Socket(..),SendException,ReceiveException)
import Socket.IPv4 (Peer,newIPv4Slab,IPv4Slab(..))
import Socket.Discard (PeerlessSlab(..),newPeerlessSlab)

import qualified Socket.Datagram.Interruptible.Addr.Receive.Connected as CR
import qualified Socket.Datagram.Interruptible.Addr.Send.Connected as CS
import qualified Socket.Datagram.Interruptible.Addr.Send.IPv4 as V4S
import qualified Socket.Datagram.Interruptible.Addr.Receive.IPv4 as V4R

-- | Send a datagram using a socket with a pre-designated peer. This
-- refers to a datagram socket for which POSIX @connect@ has locked
-- down communication to an individual peer.
send ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Socket 'Connected a -- ^ Socket with designated peer
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Number of bytes to send
  -> IO (Either (SendException 'Interruptible) ())
send !intr (Socket !sock) !buf !len =
  CS.send intr () sock (UnmanagedBytes buf len)

-- | Receive a datagram, discarding the peer address. This can be used with
-- datagram sockets of any family. It is usable with both connected and
-- unconnected datagram sockets.
receive ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Socket c a -- ^ Socket
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Interruptible) Int)
receive !intr (Socket !sock) !buf !len =
  CR.receive intr sock (UnmanagedBytes buf len) ()

sendToIPv4 ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Socket 'Unconnected ('Internet 'V4) -- ^ IPv4 socket without designated peer
  -> Peer -- ^ Destination
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Number of bytes to send
  -> IO (Either (SendException 'Interruptible) ())
sendToIPv4 !intr (Socket !sock) !dst !buf !len =
  V4S.send intr dst sock (UnmanagedBytes buf len)

receiveFromIPv4 ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Socket 'Unconnected ('Internet 'V4) -- ^ IPv4 socket without designated peer
  -> Addr -- ^ Start of buffer
  -> Int -- ^ Maximum number of bytes to receive
  -> MutablePrimArrayOffset RealWorld SocketAddressInternet
     -- ^ Buffer for returned peer address
  -> IO (Either (ReceiveException 'Interruptible) Int)
receiveFromIPv4 !intr (Socket !sock) !buf !len !addr =
  V4R.receive intr sock (UnmanagedBytes buf len) addr

