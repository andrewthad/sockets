{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
module Socket.Stream
  ( SendException(..)
  , ReceiveException(..)
  , ConnectException(..)
  , ListenException(..)
  , AcceptException(..)
  , CloseException(..)
  ) where

import Socket (Direction(..),Interruptibility(..),Forkedness(..))

import Data.Kind (Type)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

-- | These can happen when setting up a listener or when establishing
--   a connection to a peer.
data ListenException :: Type where
  -- | The address is protected, and the user is not the superuser. This most
  --   commonly happens when trying to bind to a port below 1024. On Linux,
  --   When it is necessary to bind to such a port on Linux, consider using the
  --   <http://man7.org/linux/man-pages/man7/capabilities.7.html CAP_NET_BIND_SERVICE>
  --   capability instead of running the process as root. (@EACCES@)
  ListenPermissionDenied :: ListenException
  -- | The given address is already in use. (@EADDRINUSE@ with specified port)
  ListenAddressInUse :: ListenException
  -- | The port number was specified as zero, but upon attempting to
  --   bind to an ephemeral port, it was determined that all port numbers
  --   numbers in the ephemeral port range are currently in use.
  --   (@EADDRINUSE@ with unspecified port)
  ListenEphemeralPortsExhausted :: ListenException
  -- | A limit on the number of open file descriptors has been reached.
  --   This could be the per-process limit or the system limit.
  --   (@EMFILE@ and @ENFILE@)
  ListenFileDescriptorLimit :: ListenException

deriving stock instance Show ListenException
deriving anyclass instance Exception ListenException

-- | Recoverable exceptions that can occur while connecting to a peer.
-- This includes both failures while opening the socket and failures
-- while connecting to the peer.
--
-- ==== __Discussion__
--
-- In its API for connecting to a peer, this library combines the step of
-- creating a socket with the step of connecting to the peer. In other words,
-- the end user never gets access to an unconnected stream socket.
-- Consequently, the connection exceptions correspond to the @socket@
-- errors @EMFILE@ and @ENFILE@ as well as the @connect@
-- errors @ECONNREFUSED@, @EACCES@/@EPERM@, @ETIMEDOUT@, @ENETUNREACH@, and
-- @EADDRNOTAVAIL@.
--
-- Somewhat surprisingly, @EADDRINUSE@ is not included in the list of @connect@
-- error codes we recognize as recoverable. The
-- <http://man7.org/linux/man-pages/man2/connect.2.html accept man page>
-- describes @EADDRINUSE@ as "Local address is already in use". However,
-- it is unclear what this means. The caller of @connect@ does not provide
-- an internet socket address. If ephemeral ports are exhausted, @connect@
-- will error with @EADDRNOTAVAIL@. An unresolved
-- <https://stackoverflow.com/questions/43199021/how-could-connect-fail-and-set-errno-to-eaddrinuse Stack Overflow question>
-- calls into question whether or not it is actually possible for this
-- error to happen with an internet domain socket. The author has decided
-- to omit any checks for it. This means that, if it does ever happen,
-- it will cause a @SocketUnrecoverableException@ to be thrown. The Linux
-- cognoscenti are encouraged to open an issue if they have more information
-- about the circumstances under which this exception can occur.
data ConnectException :: Interruptibility -> Forkedness -> Type where
  -- | Either the connection was blocked by a local firewall rule or it
  --   was blocked because it was to a broadcast address. Sadly, these
  --   two errors are not distinguished by the Linux sockets API.
  --   (@EACCES@/@EPERM@)
  ConnectFirewalled :: ConnectException i b
  -- | A limit on the number of open file descriptors has been reached.
  --   This could be the per-process limit or the system limit.
  --   (@EMFILE@ and @ENFILE@)
  ConnectFileDescriptorLimit :: ConnectException i b
  -- | The network is unreachable. (@ENETUNREACH@)
  ConnectNetworkUnreachable :: ConnectException i b
  -- | All port numbers numbers in the ephemeral port range are currently in
  --   use. (@EADDRNOTAVAIL@)
  ConnectEphemeralPortsExhausted :: ConnectException i b
  -- | No one is listening on the remote address. (@ECONNREFUSED@)
  ConnectRefused :: ConnectException i b
  -- | Timeout while attempting connection. The server may be too busy
  --   to accept new connections. Note that stock Linux configuration has
  --   timeout at 
  --   <http://willbryant.net/overriding_the_default_linux_kernel_20_second_tcp_socket_connect_timeout appropriately 20 seconds>.
  --   Users interested in timing out more quickly are encouraged to
  --   use @registerDelay@ with the @interruptible@ variants of the
  --   connection functions in this library. (@ETIMEDOUT@)
  ConnectTimeout :: ConnectException i b
  -- | STM-style interrupt (much safer than C-style interrupt)
  ConnectInterrupted :: ConnectException 'Interruptible b
  -- | Exception occurred while closing the socket.
  ConnectUngracefulClose :: CloseException -> ConnectException i 'Unforked

deriving stock instance Show (ConnectException i f)
deriving anyclass instance (Typeable i, Typeable f) => Exception (ConnectException i f)


data CloseException :: Type where
  -- | After the local process shut down the writing channel, it
  --   was expecting the peer to do the same. However, the peer
  --   sent more data instead. If this happens, the local process
  --   does still close the socket. However, it must send a TCP
  --   reset to accomplish this since there is still unread data
  --   in the receive buffer.
  --
  --   This can happen if the peer is misbehaving or if the consumer
  --   of the @sockets@ API has incorrectly implemented a protocol
  --   living above layer 4 of the OSI model.
  ClosePeerContinuedSending :: CloseException

deriving stock instance Show CloseException
deriving anyclass instance Exception CloseException

-- | Recoverable exceptions that can occur while accepting an inbound
-- connection.
data AcceptException :: Interruptibility -> Forkedness -> Type where
  -- | The peer reset the connection before the running process
  --   accepted it. This is not typically treated as fatal. The
  --   process may continue accepting connections. (@ECONNABORTED@)
  AcceptConnectionAborted :: AcceptException i b
  -- | A limit on the number of open file descriptors has been reached.
  --   This could be the per-process limit or the system limit.
  --   (@EMFILE@ and @ENFILE@)
  AcceptFileDescriptorLimit :: AcceptException i b
  -- | Firewall rules forbid connection. (@EPERM@)
  AcceptFirewalled :: AcceptException i b
  -- | STM-style interrupt (much safer than C-style interrupt)
  AcceptInterrupted :: AcceptException 'Interruptible b
  -- | Exception occurred while closing the socket.
  AcceptUngracefulClose :: CloseException -> AcceptException i 'Unforked

deriving stock instance Show (AcceptException i f)
deriving anyclass instance (Typeable i, Typeable f) => Exception (AcceptException i f)


data SendException :: Interruptibility -> Type where
  -- | The local socket has already shutdown its writing channel.
  --   Consequently, sending is no longer possible. This can happen
  --   even if the process does not @shutdown@ the socket. If the
  --   peer decides to @close@ the connection, the local operating system
  --   will shutdown both the reading and writing channels. (@EPIPE@)
  SendShutdown :: SendException i
  -- | STM-style interrupt (much safer than C-style interrupt)
  SendInterrupted :: SendException 'Interruptible

deriving stock instance Show (SendException i)
deriving anyclass instance Typeable i => Exception (SendException i)

data ReceiveException :: Interruptibility -> Type where
  -- | The peer shutdown its writing channel.
  ReceiveShutdown :: ReceiveException i
  -- | The peer reset the connection.
  ReceiveReset :: ReceiveException i
  -- | STM-style interrupt (much safer than C-style interrupt)
  ReceiveInterrupted :: ReceiveException 'Interruptible

deriving stock instance Show (ReceiveException i)
deriving anyclass instance Typeable i => Exception (ReceiveException i)
