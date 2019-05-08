{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneDeriving #-}

module Socket.IPv4
  ( Peer(..)
  , Message(..)
  , Receipt(..)
  , Slab(..)
  , SocketException(..)
  , describeEndpoint
  , freezeSlab
  , newSlabIPv4
  ) where

import Control.Exception (Exception)
import Data.Kind (Type)
import Data.Word (Word16)
import Net.Types (IPv4(..))
import Data.Primitive (ByteArray(..),MutableByteArray,MutableUnliftedArray)
import Data.Primitive (SmallArray,SmallMutableArray)
import Data.Primitive (MutablePrimArray)
import Foreign.C.Types (CInt)
import GHC.Exts (RealWorld)
import Socket.Error (die)

import qualified Data.Primitive as PM
import qualified Posix.Socket as S
import qualified Data.Text as T
import qualified Net.IPv4 as IPv4

-- | An peer for an IPv4 socket, connection, or listener.
--   Everything is in host byte order, and the user is not
--   responsible for performing any conversions.
data Peer = Peer
  { address :: !IPv4
  , port :: !Word16
  } deriving stock (Eq,Show)

data Message = Message
  { peer :: {-# UNPACK #-} !Peer
  , payload :: !ByteArray
  } deriving stock (Eq,Show)

data Receipt = Receipt
  { peer :: {-# UNPACK #-} !Peer
  , size :: !Int
  } deriving stock (Eq,Show)

-- | A slab of memory for bulk datagram ingest (via @recvmmsg@). This
-- approach cuts down on allocations. Slabs are not safe for concurrent
-- access. Do not share a slab across multiple threads.
data Slab = Slab
  { sizes :: !(MutablePrimArray RealWorld CInt)
    -- ^ Buffer for returned datagram lengths
  , peers :: !(MutablePrimArray RealWorld S.SocketAddressInternet)
    -- ^ Buffer for returned addresses
  , payloads :: !(MutableUnliftedArray RealWorld (MutableByteArray RealWorld))
    -- ^ Buffers for datagram payloads, no slicing
  }

-- | Allocate a slab that is used to receive multiple datagrams at
-- the same time.
newSlabIPv4 ::
     Int -- ^ maximum datagrams
  -> Int -- ^ maximum size of individual datagram 
  -> IO Slab
newSlabIPv4 !n !m = if n >= 1 && m >= 1
  then do
    sizes <- PM.newPrimArray n
    peers <- PM.newPrimArray n
    payloads <- PM.unsafeNewUnliftedArray n
    let go !ix = if ix > (-1)
          then do
            PM.writeUnliftedArray payloads ix =<< PM.newByteArray m
            go (ix - 1)
          else pure ()
    go (n - 1)
    pure Slab{sizes,peers,payloads}
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
-- The recoverable exceptions that we from stream sockets (established
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

freezeSlab :: Slab -> Int -> IO (SmallArray Message)
freezeSlab slab n = do
  msgs <- PM.newSmallArray n errorThunk
  freezeSlabGo slab msgs (n - 1)

freezeSlabGo :: Slab -> SmallMutableArray RealWorld Message -> Int -> IO (SmallArray Message)
freezeSlabGo slab@Slab{payloads,peers,sizes} !arr !ix = if ix > (-1)
  then do
    !size <- PM.readPrimArray sizes ix
    !sockaddr <- PM.readPrimArray peers ix
    -- Remove the byte array from the array of payloads, freeze it, and
    -- replace it with a freshly allocated byte array. 
    payloadMut <- PM.readUnliftedArray payloads ix
    originalSize <- PM.getSizeofMutableByteArray payloadMut
    !payload <- PM.unsafeFreezeByteArray =<< PM.resizeMutableByteArray payloadMut (cintToInt size)
    PM.writeUnliftedArray payloads ix =<< PM.newByteArray originalSize
    let !peer = sockAddrToPeer sockaddr
        !msg = Message {peer,payload}
    PM.writeSmallArray arr ix msg
    freezeSlabGo slab arr (ix - 1)
  else PM.unsafeFreezeSmallArray arr

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

