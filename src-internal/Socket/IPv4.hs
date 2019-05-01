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
  ( Endpoint(..)
  , SocketException(..)
  , describeEndpoint
  ) where

import Control.Exception (Exception)
import Data.Kind (Type)
import Data.Word (Word16)
import Net.Types (IPv4(..))

import qualified Data.Text as T
import qualified Net.IPv4 as IPv4

-- | An endpoint for an IPv4 socket, connection, or listener.
--   Everything is in host byte order, and the user is not
--   responisble for performing any conversions.
data Endpoint = Endpoint
  { address :: !IPv4
  , port :: !Word16
  } deriving stock (Eq,Show)

-- This is used internally for debug messages and for presenting
-- unrecoverable exceptions.
describeEndpoint :: Endpoint -> String
describeEndpoint (Endpoint {address,port}) =
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
