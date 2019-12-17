{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}

module Socket.SequencedPacket
  ( ReceiveException(..)
  , SendException(..)
  ) where

import Data.Kind (Type)
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Socket (Interruptibility(..))

data ReceiveException :: Interruptibility -> Type where
  -- | The datagram did not fit in the buffer. The field is the
  --   original size of the datagram that was truncated. If
  --   this happens, the process probably needs to start using
  --   a larger receive buffer.
  ReceiveTruncated :: ReceiveException i
  -- | The peer shutdown its writing channel. (zero-length chunk)
  ReceiveShutdown :: ReceiveException i
  -- | The peer reset the connection. (@ECONNRESET@)
  ReceiveReset :: ReceiveException i
  -- | STM-style interrupt (much safer than C-style interrupt)
  -- This provides the number of bytes received before the interrupt
  -- happened. For @receiveOnce@, this will always be zero, but
  -- for @receiveExactly@ and @receiveBetween@, it may be any
  -- non-negative number less than the number of bytes the caller
  -- intended to receive.
  ReceiveInterrupted :: ReceiveException 'Interruptible

deriving stock instance Eq (ReceiveException i)
deriving stock instance Show (ReceiveException i)
deriving anyclass instance Typeable i => Exception (ReceiveException i)

data SendException :: Interruptibility -> Type where
  -- | The datagram did not fit in the buffer. The field is the
  --   number of bytes that were successfully copied into the
  --   send buffer. The datagram does still get sent when this
  --   happens.
  SendTruncated :: !Int -> SendException i
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
  SendInterrupted :: SendException 'Interruptible

deriving stock instance Eq (SendException i)
deriving stock instance Show (SendException i)
deriving anyclass instance Typeable i => Exception (SendException i)
