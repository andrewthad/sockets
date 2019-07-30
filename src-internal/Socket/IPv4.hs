{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneDeriving #-}
{-# language UnboxedTuples #-}

module Socket.IPv4
  ( Peer(..)
  , Message(..)
  , IPv4Slab(..)
  , SocketException(..)
  , describeEndpoint
  , freezeIPv4Slab
  , newIPv4Slab
  , replenishIPv4Slab
  , replenishPinnedIPv4Slab
  ) where

import Control.Exception (Exception)
import Control.Monad.Primitive (primitive_,primitive)
import Data.Kind (Type)
import Data.Primitive.Unlifted.Array (MutableUnliftedArray)
import Data.Primitive (ByteArray(..),MutableByteArray)
import Data.Primitive (MutablePrimArray)
import Data.Primitive (SmallArray,SmallMutableArray)
import Data.Word (Word16)
import Foreign.C.Types (CInt)
import GHC.Exts (RealWorld,Int(I#))
import Net.Types (IPv4(..))
import Socket (Pinnedness(Pinned,Unpinned))
import Socket.Error (die)
import Socket.Slab (replenishPinned,replenishUnpinned)

import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM
import qualified Data.Text as T
import qualified GHC.Exts as Exts
import qualified Net.IPv4 as IPv4
import qualified Posix.Socket as S

-- | An peer for an IPv4 socket, connection, or listener.
--   Everything is in host byte order, and the user is not
--   responsible for performing any conversions.
data Peer = Peer
  { address :: !IPv4
  , port :: !Word16
  } deriving stock (Eq,Show)

-- | A message received from a peer. The payload may be pinned
-- or unpinned depending on the reception function that produced
-- the message.
data Message = Message
  { peer :: {-# UNPACK #-} !Peer
  , payload :: !ByteArray
  } deriving stock (Eq,Show)

-- | A slab of memory for bulk datagram ingest (via @recvmmsg@). This
-- approach cuts down on allocations. Slabs are not safe for concurrent
-- access. Do not share a slab across multiple threads.
data IPv4Slab :: Pinnedness -> Type where
  IPv4Slab ::
    -- Invariant: payload sizes are nondecreasing. An
    -- implication is that zero-size payloads come first.
    -- Invariant: All non-zero-size payloads match the
    -- phantom pinnedness. All zero-size payloads are
    -- unpinned.
    { sizes :: !(MutablePrimArray RealWorld CInt)
      -- ^ Buffer for returned datagram lengths
    , peers :: !(MutablePrimArray RealWorld S.SocketAddressInternet)
      -- ^ Buffer for returned addresses
    , payloads :: !(MutableUnliftedArray RealWorld (MutableByteArray RealWorld))
      -- ^ Buffers for datagram payloads, no slicing
    } -> IPv4Slab p

-- | Allocate a slab that is used to receive multiple datagrams at
-- the same time, additionally storing the IPv4 addresses of the peers.
newIPv4Slab ::
     Int -- ^ maximum datagrams
  -> IO (IPv4Slab p)
newIPv4Slab !n = if n >= 1
  then do
    sizes <- PM.newPrimArray n
    peers <- PM.newPrimArray n
    tombstone <- PM.newByteArray 0
    payloads <- PM.newUnliftedArray n tombstone
    pure IPv4Slab{sizes,peers,payloads}
  else die "newSlabIPv4"

-- This is used internally for debug messages and for presenting
-- unrecoverable exceptions.
describeEndpoint :: Peer -> String
describeEndpoint (Peer {address,port}) =
  T.unpack (IPv4.encode address) ++ ":" ++ show port

-- | Recoverable exceptions that happen when establishing an internet-domain
-- stream listener or datagram socket.
--
-- ==== __Discussion__
--
-- The recoverable exceptions that we encounter with stream sockets (established
-- with @socket@-@bind@-@listen@) and datagram sockets (established with
-- @socket@-@bind@) are the exact same exceptions. Consequently, we reuse
-- the same type in both case. It is a little unfortunate since the name
-- @ListenException@ would be more appropriate for stream sockets. But
-- the code reuse is worth the naming quibble.
data SocketException :: Type where
  -- | The address is protected, and the user is not the superuser. This most
  --   commonly happens when trying to bind to a port below 1024. On Linux,
  --   When it is necessary to bind to such a port on Linux, consider using the
  --   <http://man7.org/linux/man-pages/man7/capabilities.7.html CAP_NET_BIND_SERVICE>
  --   capability instead of running the process as root. (@EACCES@)
  SocketPermissionDenied :: SocketException
  -- | The given address is already in use. (@EADDRINUSE@ with specified port)
  SocketAddressInUse :: SocketException
  -- | The port number was specified as zero, but upon attempting to
  --   bind to an ephemeral port, it was determined that all port numbers
  --   numbers in the ephemeral port range are currently in use.
  --   (@EADDRINUSE@ with unspecified port)
  SocketEphemeralPortsExhausted :: SocketException
  -- | A limit on the number of open file descriptors has been reached.
  --   This could be the per-process limit or the system limit.
  --   (@EMFILE@ and @ENFILE@)
  SocketFileDescriptorLimit :: SocketException

deriving stock instance Show SocketException
deriving anyclass instance Exception SocketException

-- | Freeze the specified number of messages in-place and return
-- them (along with their origin peers). Replaces all of the frozen
-- messages with tombstones so that new buffers can be allocated
-- before the next reception. End users should not need this function.
freezeIPv4Slab ::
     IPv4Slab p -- ^ The slab
  -> Int -- ^ Number of messages in the slab.
  -> IO (SmallArray Message)
freezeIPv4Slab slab n = do
  msgs <- PM.newSmallArray n errorThunk
  tombstone <- PM.newByteArray 0
  freezeSlabGo slab tombstone msgs (n - 1)

freezeSlabGo ::
     IPv4Slab p
  -> MutableByteArray RealWorld -- tombstone
  -> SmallMutableArray RealWorld Message
  -> Int
  -> IO (SmallArray Message)
freezeSlabGo slab@IPv4Slab{payloads,peers,sizes} !tombstone !arr !ix = if ix > (-1)
  then do
    !size <- PM.readPrimArray sizes ix
    !sockaddr <- PM.readPrimArray peers ix
    -- Remove the byte array from the array of payloads, freeze it, and
    -- replace it with a tombstone.
    payloadMut <- readMutableByteArrayArray payloads ix
    !payload <- PM.unsafeFreezeByteArray
      =<< PM.resizeMutableByteArray payloadMut (cintToInt size)
    writeMutableByteArrayArray payloads ix tombstone
    let !peer = sockAddrToPeer sockaddr
        !msg = Message {peer,payload}
    PM.writeSmallArray arr ix msg
    freezeSlabGo slab tombstone arr (ix - 1)
  else PM.unsafeFreezeSmallArray arr

-- | Ensure that every buffer can accomodate at least the
-- minimum number of bytes.
replenishIPv4Slab ::
     IPv4Slab 'Unpinned
  -> Int -- ^ Minimum Size
  -> IO () 
replenishIPv4Slab IPv4Slab{payloads} minSz = do
  let sz = PM.sizeofMutableUnliftedArray payloads
  replenishUnpinned payloads minSz 0 sz

replenishPinnedIPv4Slab ::
     IPv4Slab 'Pinned
  -> Int -- ^ Minimum Size
  -> IO () 
replenishPinnedIPv4Slab IPv4Slab{payloads} minSz = do
  let sz = PM.sizeofMutableUnliftedArray payloads
  replenishPinned payloads minSz 0 sz

{-# NOINLINE errorThunk #-}
errorThunk :: Message
errorThunk = error "Socket.IPv4.errorThunk"

cintToInt :: CInt -> Int
cintToInt = fromIntegral

sockAddrToPeer :: S.SocketAddressInternet -> Peer
sockAddrToPeer (S.SocketAddressInternet {address,port}) = Peer
  { address = IPv4 (S.networkToHostLong address)
  , port = S.networkToHostShort port
  }

writeMutableByteArrayArray
  :: MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- ^ destination
  -> Int -- ^ index
  -> MutableByteArray RealWorld -- ^ value
  -> IO ()
writeMutableByteArrayArray (PM.MutableUnliftedArray maa#) (I# i#) (PM.MutableByteArray a)
  = primitive_ (Exts.writeMutableByteArrayArray# maa# i# a)

readMutableByteArrayArray
  :: MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- ^ source
  -> Int -- ^ index
  -> IO (MutableByteArray RealWorld)
readMutableByteArrayArray (PM.MutableUnliftedArray maa#) (I# i#)
  = primitive $ \s -> case Exts.readMutableByteArrayArray# maa# i# s of
      (# s', aa# #) -> (# s', PM.MutableByteArray aa# #)
