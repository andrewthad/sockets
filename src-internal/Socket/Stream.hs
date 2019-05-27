{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
module Socket.Stream
  ( -- * Exceptions
    SendException(..)
  , ReceiveException(..)
  , ConnectException(..)
  , SocketException(..)
  , AcceptException(..)
  , CloseException(..)
    -- * Connection
  , Connection(..)
  ) where

import Socket (Family(..))
import Socket (Interruptibility(..))
import Socket.IPv4 (SocketException(..))
import System.Posix.Types (Fd)

import Data.Kind (Type)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

-- | A connection-oriented stream socket.
newtype Connection = Connection Fd

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
--
-- Although both the POSIX spec and the linux man page list @ENETUNREACH@
-- as an error code that is possible for any type of socket, this library
-- author does not believe that this error code can happen when calling
-- @connect@ on a unix domain socket. Open an issue if this in incorrect.
data ConnectException :: Family -> Interruptibility -> Type where
  -- | Either the connection was blocked by a local firewall rule or it
  --   was blocked because it was to a broadcast address. Sadly, these
  --   two errors are not distinguished by the Linux sockets API.
  --   (@EACCES@/@EPERM@)
  ConnectFirewalled :: ConnectException d i
  -- | A limit on the number of open file descriptors has been reached.
  --   This could be the per-process limit or the system limit.
  --   (@EMFILE@ and @ENFILE@)
  ConnectFileDescriptorLimit :: ConnectException d i
  -- | The network is unreachable. (@ENETUNREACH@)
  ConnectNetworkUnreachable :: ConnectException ('Internet v) i
  -- | No valid routing table entry matches the destination address.
  --   (@EHOSTUNREACH@)
  ConnectHostUnreachable :: ConnectException ('Internet v) i
  -- | All port numbers in the ephemeral port range are currently in
  --   use. (@EADDRNOTAVAIL@).
  ConnectEphemeralPortsExhausted :: ConnectException d i
  -- | No one is listening on the remote address. (@ECONNREFUSED@)
  ConnectRefused :: ConnectException d i
  -- | Timeout while attempting connection. The server may be too busy
  --   to accept new connections. Note that stock Linux configuration has
  --   timeout at
  --   <http://willbryant.net/overriding_the_default_linux_kernel_20_second_tcp_socket_connect_timeout appropriately 20 seconds>.
  --   Users interested in timing out more quickly are encouraged to
  --   use @registerDelay@ with the @interruptible@ variants of the
  --   connection functions in this library. (@ETIMEDOUT@)
  ConnectTimeout :: ConnectException d i
  -- | Remote socket does not match the local socket type. (@EPROTOTYPE@)
  ConnectProtocolType :: ConnectException 'Unix i
  -- | STM-style interrupt (much safer than C-style interrupt)
  ConnectInterrupted :: ConnectException d 'Interruptible

deriving stock instance Eq (ConnectException d i)
deriving stock instance Ord (ConnectException d i)
deriving stock instance Show (ConnectException d i)
deriving anyclass instance (Typeable d, Typeable i) => Exception (ConnectException d i)


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

deriving stock instance Eq CloseException
deriving stock instance Ord CloseException
deriving stock instance Show CloseException
deriving anyclass instance Exception CloseException

-- | Recoverable exceptions that can occur while accepting an inbound
-- connection.
data AcceptException :: Interruptibility -> Type where
  -- | The peer reset the connection before the running process
  --   accepted it. This is not typically treated as fatal. The
  --   process may continue accepting connections. (@ECONNABORTED@)
  AcceptConnectionAborted :: AcceptException i
  -- | A limit on the number of open file descriptors has been reached.
  --   This could be the per-process limit or the system limit.
  --   (@EMFILE@ and @ENFILE@)
  AcceptFileDescriptorLimit :: AcceptException i
  -- | Firewall rules forbid connection. (@EPERM@)
  AcceptFirewalled :: AcceptException i
  -- | STM-style interrupt (much safer than C-style interrupt)
  AcceptInterrupted :: AcceptException 'Interruptible

deriving stock instance Eq (AcceptException i)
deriving stock instance Ord (AcceptException i)
deriving stock instance Show (AcceptException i)
deriving anyclass instance (Typeable i) => Exception (AcceptException i)


data SendException :: Interruptibility -> Type where
  -- | The local socket has already shutdown its writing channel.
  --   Consequently, sending is no longer possible. This can happen
  --   even if the process does not @shutdown@ the socket. If the
  --   peer decides to @close@ the connection, the local operating system
  --   will shutdown both the reading and writing channels. (@EPIPE@)
  SendShutdown :: SendException i
  -- | The peer reset the connection.
  SendReset :: SendException i
  -- | STM-style interrupt (much safer than C-style interrupt).
  -- This provides the number of bytes sent before the interrupt
  -- happened. For @sendOnce@, this will always be zero, but
  -- for @send@, it may be any non-negative number less than the
  -- number of bytes the caller intended to send.
  SendInterrupted :: !Int -> SendException 'Interruptible

deriving stock instance Eq (SendException i)
deriving stock instance Ord (SendException i)
deriving stock instance Show (SendException i)
deriving anyclass instance Typeable i => Exception (SendException i)

-- | Recoverable exceptions that can occur while receiving data on a
-- stream socket.
--
-- ==== __Discussion__
--
-- The <http://man7.org/linux/man-pages/man2/recv.2.html recv man page>
-- explicitly documents these:
--
-- * @EAGAIN@/@EAGAIN@: Not possible after using event manager to wait.
-- * @EBADF@: Prevented by this library.
-- * @ECONNREFUSED@: Not sure if this is possible. Currently treated as
--   an unrecoverable exception.
-- * @EFAULT@: Not recoverable. API consumer has misused @Addr@.
-- * @EINTR@: Prevented by this library. Unsafe FFI is not interruptible.
-- * @EINVAL@: Prevented by this library.
-- * @ENOMEM@: Not recoverable.
-- * @ENOTCONN@: Prevented by this library.
-- * @ENOTSOCK@: Prevented by this library.
--
-- The man page includes a disclaimer: "Additional errors may be generated
-- and returned from the underlying protocol modules". One such error
-- when dealing with stream sockets in @ECONNRESET@. One scenario where
-- this happens is when the process running on the peer terminates ungracefully
-- and the operating system on the peer cleans up by sending a reset.
data ReceiveException :: Interruptibility -> Type where
  -- | The peer shutdown its writing channel. (zero-length chunk)
  ReceiveShutdown :: ReceiveException i
  -- | The peer reset the connection. (@ECONNRESET@)
  ReceiveReset :: ReceiveException i
  -- | STM-style interrupt (much safer than C-style interrupt)
  ReceiveInterrupted :: ReceiveException 'Interruptible
  -- | The peer was unreachable. (@EHOSTUNREACH@)
  ReceiveHostUnreachable :: ReceiveException i

deriving stock instance Eq (ReceiveException i)
deriving stock instance Ord (ReceiveException i)
deriving stock instance Show (ReceiveException i)
deriving anyclass instance Typeable i => Exception (ReceiveException i)
