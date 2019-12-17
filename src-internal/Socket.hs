{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}

module Socket
  ( SocketUnrecoverableException(..)
  , SocketException(..)
  , BindException(..)
  , Direction(..)
  , Connectedness(..)
  , Pinnedness(..)
  , Family(..)
  , Version(..)
  , Interruptibility(..)
  , Forkedness(..)
  , cgetsockname 
  , cgetsockopt
  , cclose 
  , crecv 
  , crecvfrom
  , cshutdown
  , negativeSliceLength
  , nonInternetSocketFamily
  , functionWithAccepted
  , functionWithConnection
  , functionWithListener
  , functionWithSocket
  , functionGracefulClose
  , socketAddressSize
  ) where

import Control.Exception (Exception(..))
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt)

import qualified Data.List as L

data Direction = Send | Receive

data Connectedness = Connected | Unconnected

-- | This is used as a phantom type to distinguish
-- between pinned and unpinned memory. The distinction
-- drawn here concerns the manner in which the user
-- requested the byte array. Byte arrays resulting
-- from newPinnedByteArray# are considered 'Pinned'
-- while those resulting from newByteArray# are
-- considered 'Unpinned'. Even if the runtime decides
-- to pin a byte array requested by newByteArray#
-- (e.g. because it is larger than 3KB), it is
-- considered 'Unpinned' by this classification.
data Pinnedness = Pinned | Unpinned

data Family = Internet Version | Unix

data Version = V4 | V6

data Interruptibility = Interruptible | Uninterruptible

data Forkedness = Forked | Unforked

data SocketException :: Type where
  -- | A limit on the number of open file descriptors has been reached.
  --   This could be the per-process limit or the system limit.
  --   (@EMFILE@ and @ENFILE@)
  SocketFileDescriptorLimit :: SocketException

deriving stock instance Show SocketException
deriving stock instance Eq SocketException
deriving anyclass instance Exception SocketException

-- | Recoverable exceptions that happen when establishing stream listeners
-- or opening datagram sockets.
--
-- ==== __Discussion__
--
-- The recoverable exceptions that we encounter with stream sockets (established
-- with @socket@-@bind@-@listen@) and datagram sockets (established with
-- @socket@-@bind@) are the exact same exceptions. Consequently, we reuse
-- the same type in both case.
data BindException :: Family -> Type where
  -- | The address is protected, and the user is not the superuser. This most
  --   commonly happens when trying to bind to a port below 1024. On Linux,
  --   When it is necessary to bind to such a port on Linux, consider using the
  --   <http://man7.org/linux/man-pages/man7/capabilities.7.html CAP_NET_BIND_SERVICE>
  --   capability instead of running the process as root. (@EACCES@)
  BindPermissionDenied :: BindException f
  -- | The given address is already in use. This can also happen if the
  --   address was recently bound to and closed. As mandated by
  --   <https://tools.ietf.org/html/rfc793 RFC 793>, such a socket remains
  --   in the @TIME-WAIT@ state for a while (commonly 30 to 120 seconds),
  --   and no new socket may be bound to the address during this period.
  --   (@EADDRINUSE@ with specified port)
  BindAddressInUse :: BindException f
  -- | The port number was specified as zero, but upon attempting to
  --   bind to an ephemeral port, it was determined that all port numbers
  --   numbers in the ephemeral port range are currently in use.
  --   (@EADDRINUSE@ with unspecified port)
  BindEphemeralPortsExhausted :: BindException ('Internet v)
  -- | A limit on the number of open file descriptors has been reached.
  --   This could be the per-process limit or the system limit.
  --   (@EMFILE@ and @ENFILE@)
  BindFileDescriptorLimit :: BindException f

deriving stock instance Show (BindException f)
deriving stock instance Eq (BindException f)
deriving anyclass instance Typeable f => Exception (BindException f)


data SocketUnrecoverableException = SocketUnrecoverableException
  { modules :: String
  , function :: String
  , description :: [String]
  }
  deriving stock (Show,Eq)

instance Exception SocketUnrecoverableException where
  displayException (SocketUnrecoverableException m f d) =
    m ++ "." ++ f ++ ": [" ++ L.intercalate "," d ++ "]"

cgetsockname :: String
cgetsockname = "getsockname"

cgetsockopt :: String
cgetsockopt = "getsockopt"

cclose :: String
cclose = "getsockname"

crecv :: String
crecv = "recv"

crecvfrom :: String
crecvfrom = "recvfrom"

cshutdown :: String
cshutdown = "shutdown"

functionGracefulClose :: String
functionGracefulClose = "gracefulClose"

nonInternetSocketFamily :: String
nonInternetSocketFamily = "non-internet socket family"

negativeSliceLength :: String
negativeSliceLength = "negative slice length"

functionWithAccepted :: String
functionWithAccepted = "withAccepted"

functionWithConnection :: String
functionWithConnection = "withConnection"

functionWithListener :: String
functionWithListener = "withListener"

functionWithSocket :: String
functionWithSocket = "withSocket"

socketAddressSize :: String
socketAddressSize = "socket address size"
