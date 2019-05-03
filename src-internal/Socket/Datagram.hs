{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
module Socket.Datagram
  ( SendException(..)
  , ReceiveException(..)
  , SocketException(..)
  ) where

import Socket (Interruptibility(..))
import Socket.IPv4 (SocketException(..))

import Data.Kind (Type)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

data SendException :: Interruptibility -> Type where
  -- | The datagram did not fit in the buffer. The field is the
  --   number of bytes that were successfully copied into the
  --   send buffer. The datagram does still get sent when this
  --   happens.
  SendTruncated :: !Int -> SendException i
  -- | Attempted to send to a broadcast address.
  SendBroadcasted :: SendException i
  -- | STM-style interrupt (much safer than C-style interrupt)
  SendInterrupted :: SendException 'Interruptible

deriving stock instance Show (SendException i)
deriving anyclass instance (Typeable i) => Exception (SendException i)

data ReceiveException :: Interruptibility -> Type where
  -- | The datagram did not fit in the buffer. The field is the
  --   original size of the datagram that was truncated. If
  --   this happens, the process probably needs to start using
  --   a larger receive buffer.
  ReceiveTruncated :: !Int -> ReceiveException i
  -- | STM-style interrupt (much safer than C-style interrupt)
  ReceiveInterrupted :: ReceiveException 'Interruptible

deriving stock instance Show (ReceiveException i)
deriving anyclass instance (Typeable i) => Exception (ReceiveException i)

