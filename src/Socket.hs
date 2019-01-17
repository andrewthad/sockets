{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}

module Socket
  ( SocketException(..)
  , Context(..)
  , Reason(..)
  ) where

import Control.Exception (Exception)
import Foreign.C.Types (CInt)

-- | Represents any unexpected behaviors that a function working on a
--   socket, connection, or listener can exhibit.
data SocketException = SocketException
  { context :: Context
  , reason :: Reason
  }
  deriving stock (Eq,Show)
  deriving anyclass (Exception)

-- | The function that behaved unexpectedly.
data Context
  = Accept
  | Bind
  | Close
  | Connect
  | GetName
  | Listen
  | Open
  | Option
  | Receive
  | Send
  | Shutdown
  deriving stock (Eq,Show)

-- | A description of the unexpected behavior.
data Reason
  = MessageTruncated !Int !Int
    -- ^ The datagram did not fit in the buffer. This can happen while
    --   sending or receiving. Fields: buffer size, datagram size.
  | SocketAddressSize
    -- ^ The socket address was not the expected size. This exception
    --   indicates a bug in this library or (less likely) in the
    --   operating system.
  | SocketAddressFamily
    -- ^ The socket address had an unexpected family. This exception
    --   indicates a bug in this library or (less likely) in the
    --   operating system.
  | OptionValueSize
    -- ^ The option value was not the expected size. This exception
    --   indicates a bug in this library or (less likely) in the
    --   operating system.
  | NegativeBytesRequested
    -- ^ The user requested a negative number of bytes in a call
    --   to a receive function.
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
