{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language StandaloneDeriving #-}
module Socket.Datagram
  ( DatagramException(..)
  ) where

import Socket (Direction(..),Interruptibility(..))

import Data.Kind (Type)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

data DatagramException :: Direction -> Interruptibility -> Type where
  -- | The datagram did not fit in the buffer. If this happens
  --   while sending, the field is the number of bytes in the datagram
  --   that were successfully copied into the send buffer. If this
  --   happens while receiving, the field is the original size of
  --   the datagram that was truncated. (Users who get this while
  --   receiving likely need to use a larger receive buffer.)
  Truncation :: !Int -> DatagramException d i
  -- | STM-style interrupt (much safer than C-style interrupt)
  Interrupt :: DatagramException d 'Interruptible
  -- | Attempted to send to a broadcast address.
  Broadcast :: DatagramException 'Send i

deriving stock instance Show (DatagramException d i)
deriving anyclass instance (Typeable d, Typeable i) => Exception (DatagramException d i)
