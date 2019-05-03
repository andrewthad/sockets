{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language KindSignatures #-}

module Socket
  ( SocketException(..)
  , SocketUnrecoverableException(..)
  , Direction(..)
  , Connectedness(..)
  , Family(..)
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
import Foreign.C.Types (CInt)

import qualified Data.List as L

data Direction = Send | Receive

data Connectedness = Connected | Unconnected

data Family = IPv4 | IPv6 | Unix

data Interruptibility = Interruptible | Uninterruptible

data Forkedness = Forked | Unforked

-- | Represents any unexpected behaviors that a function working on a
--   socket, connection, or listener can exhibit.
data SocketException
  = SentMessageTruncated !Int
    -- ^ The datagram did not fit in the buffer. This can happen while
    --   sending. The field is the size of the number of bytes in the
    --   datagram that were successfully copied into the send buffer.
  | ReceivedMessageTruncated !Int
    -- ^ The datagram did not fit in the buffer. This can happen while
    --   receiving. The field is the original size of the datagram that
    --   was was truncated while copying it into the buffer.
  | SocketAddressSize
    -- ^ The socket address was not the expected size. This exception
    --   indicates a bug in this library or (less likely) in the
    --   operating system.
  | SocketAddressFamily !CInt
    -- ^ The socket address had an unexpected family. This exception
    --   indicates a bug in this library or (less likely) in the
    --   operating system. The int argument is the actual family
    --   found in the socket address.
  | OptionValueSize
    -- ^ The option value was not the expected size. This exception
    --   indicates a bug in this library or (less likely) in the
    --   operating system.
  | NegativeBytesRequested
    -- ^ The user requested a negative number of bytes in a call
    --   to a receive function.
  | ReceptionAbandoned
    -- ^ This happens when the @Unless@ variant of a function is
    --   used and the @STM@ action completes before the socket is
    --   ready for a read.
  | RemoteNotShutdown
    -- ^ The remote end sent more data when it was expected to send
    --   a shutdown.
  | RemoteShutdown
    -- ^ The remote end has shutdown its side of the full-duplex
    --   connection. This can happen @receive@ is called on a
    --   stream socket. This is not necessarily a bad thing. Many
    --   protocols use shutdown to indicate that no more data
    --   is available. These protocols can be contrasted with
    --   protocols that send a length representing a number of
    --   expected bytes.
  | ErrorCode !CInt
    -- ^ Any error code from the operating system that this library does
    --   not expect or recognize. Consult your operating system manual
    --   for details about the error code.
  deriving stock (Eq,Show)
  deriving anyclass (Exception)

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
