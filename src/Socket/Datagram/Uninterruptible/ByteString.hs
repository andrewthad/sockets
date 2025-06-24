{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}

module Socket.Datagram.Uninterruptible.ByteString
  ( -- * Send
    send
  , sendToIPv4
    -- * Receive
  , receive
  , receiveFromIPv4
    -- * Receive Many
  , receiveMany
    -- * Slabs
    -- ** Types
  , PeerlessSlab(..)
  , IPv4Slab(..)
    -- ** Functions
  , newPeerlessSlab
  , newIPv4Slab
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Bytes.Types (UnmanagedBytes(UnmanagedBytes))
import Data.Bytes.Types (MutableBytes(MutableBytes))
import Data.Primitive (SmallArray)
import Data.Primitive.Addr (Addr(..))
import Data.Primitive.PrimArray.Offset (MutablePrimArrayOffset)
import GHC.Exts (Ptr(Ptr),RealWorld,proxy#)
import Posix.Socket (SocketAddressInternet)
import Socket (Connectedness(..),Family(..),Version(..),Interruptibility(Uninterruptible))
import Socket (Pinnedness(Pinned))
import Socket.Datagram (Socket(..),SendException,ReceiveException)
import Socket.IPv4 (Peer,newIPv4Slab,IPv4Slab(..))
import Socket.Discard (PeerlessSlab(..),newPeerlessSlab)
import Socket.Interop (fromPinned)

import qualified Socket.Discard
import qualified Data.Primitive as PM
import qualified Socket.Datagram.Uninterruptible.MutableBytes.Receive.Connected as CR
import qualified Socket.Datagram.Uninterruptible.Addr.Send.Connected as CS
import qualified Socket.Datagram.Uninterruptible.Addr.Send.IPv4 as V4S
import qualified Socket.Datagram.Uninterruptible.MutableBytes.Receive.IPv4 as V4R
import qualified Socket.Datagram.Uninterruptible.MutableBytes.Many as MM

-- | Send a datagram using a socket with a pre-designated peer. This
-- refers to a datagram socket for which POSIX @connect@ has locked
-- down communication to an individual peer.
send ::
     Socket 'Connected a -- ^ Socket with designated peer
  -> ByteString -- ^ Slice of a buffer
  -> IO (Either (SendException 'Uninterruptible) ())
send (Socket !sock) !bs = unsafeUseAsCStringLen bs
  (\(Ptr addr#,len) ->
    CS.send proxy# () sock (UnmanagedBytes (Addr addr#) len)
  )

-- | Receive a datagram, discarding the peer address. This can be used with
-- datagram sockets of any family. It is usable with both connected and
-- unconnected datagram sockets.
receive ::
     Socket c a -- ^ Socket
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either (ReceiveException 'Uninterruptible) ByteString)
receive (Socket !sock) !len = do
  !marr <- PM.newPinnedByteArray len
  CR.receive proxy# sock (MutableBytes marr 0 len) () >>= \case
    Right sz -> pure $! Right $! fromPinned marr 0 sz
    Left err -> pure (Left err)

sendToIPv4 ::
     Socket 'Unconnected ('Internet 'V4) -- ^ IPv4 socket without designated peer
  -> Peer -- ^ Destination
  -> ByteString -- ^ Slice of a buffer
  -> IO (Either (SendException 'Uninterruptible) ())
sendToIPv4 (Socket !sock) !dst !bs = unsafeUseAsCStringLen bs
  (\(Ptr addr#,len) ->
    V4S.send proxy# dst sock (UnmanagedBytes (Addr addr#) len)
  )

receiveFromIPv4 ::
     Socket 'Unconnected ('Internet 'V4) -- ^ IPv4 socket without designated peer
  -> Int -- ^ Maximum number of bytes to receive
  -> MutablePrimArrayOffset RealWorld SocketAddressInternet
     -- ^ Buffer for returned peer address
  -> IO (Either (ReceiveException 'Uninterruptible) ByteString)
receiveFromIPv4 (Socket !sock) !len !addr = do
  !marr <- PM.newPinnedByteArray len
  V4R.receive proxy# sock (MutableBytes marr 0 len) addr >>= \case
    Left err -> pure (Left err)
    Right sz -> pure $! Right $! fromPinned marr 0 sz

-- | Receive multiple datagrams at the same time. This uses @recvmmsg@
-- on platforms that support it. This function is provided as a convenience
-- for users of the @bytestring@ library. A slightly more allocation-friendly
-- approach to receiving datagrams as bytestrings is to make use of
-- 'Socket.Datagram.Uninterruptible.Bytes.receiveManyPinned' from
-- @Socket.Datagram.Uninterruptible.Bytes@. Performing the conversion
-- from pinned 'ByteArray' to 'ByteString' while folding over the result
-- array may lead to fewer allocations depending on how the 'ByteString's
-- are used.
receiveMany ::
     Socket c a -- ^ Socket
  -> PeerlessSlab 'Pinned -- ^ Buffers for reception
  -> Int -- ^ Maximum size of single datagram
  -> IO (Either (ReceiveException 'Uninterruptible) (SmallArray ByteString))
receiveMany sock slab maxSz = do
  Socket.Discard.replenishPinnedPeerlessSlab slab maxSz
  MM.receiveMany sock slab >>= \case
    Left err -> pure (Left err)
    Right n -> do
      arr <- Socket.Discard.freezePeerlessSlabAsByteString slab n
      pure (Right arr)

