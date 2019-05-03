{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}

module Socket.Datagram.Interruptible.MutableBytes
  ( send
  , receive
  , sendToIPv4
  , receiveFromIPv4
  ) where

import Data.Bytes.Types (MutableBytes)
import Socket (Connectedness(..),Family(..),Interruptibility(Interruptible))
import Control.Concurrent.STM (TVar)
import Data.Kind (Type)
import GHC.Exts (RealWorld)
import System.Posix.Types (Fd)
import Socket.IPv4 (Endpoint)
import Socket.Datagram (SendException,ReceiveException)
import qualified Socket.Datagram.Interruptible.MutableBytes.Receive.Connected as CR
import qualified Socket.Datagram.Interruptible.MutableBytes.Send.Connected as CS
import qualified Socket.Datagram.Interruptible.MutableBytes.Send.IPv4 as V4S
import qualified Socket.Datagram.Interruptible.MutableBytes.Receive.IPv4 as V4R

newtype Socket :: Connectedness -> Family -> Type where
  Socket :: Fd -> Socket c a

-- | Send a datagram using a socket with a pre-designated peer. This refers
-- to a datagram socket for which POSIX @connect@ has locked down communication
-- to an individual peer.
send ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Socket 'Connected a -- ^ Socket with designated endpoint
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (SendException 'Interruptible) ())
send !intr (Socket !sock) !buf =
  CS.send intr () sock buf

-- | Receive a datagram, discarding the peer address. This can be used with
-- datagram sockets of any family. It is usable with both connected and
-- unconnected datagram sockets.
receive ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Socket c a -- ^ Socket
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (ReceiveException 'Interruptible) Int)
receive !intr (Socket !sock) !buf =
  CR.receive intr sock buf >>= \case
    Right ((),sz) -> pure (Right sz)
    Left err -> pure (Left err)

sendToIPv4 ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'SendInterrupted'@.
  -> Socket 'Unconnected 'IPv4 -- ^ IPv4 socket without designated endpoint
  -> Endpoint -- ^ Destination
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (SendException 'Interruptible) ())
sendToIPv4 !intr (Socket !sock) !dst !buf =
  V4S.send intr dst sock buf

receiveFromIPv4 ::
     TVar Bool
     -- ^ Interrupt. On 'True', give up and return @'Left' 'ReceiveInterrupted'@.
  -> Socket 'Unconnected 'IPv4 -- ^ IPv4 socket without designated endpoint
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either (ReceiveException 'Interruptible) (Endpoint,Int))
receiveFromIPv4 !intr (Socket !sock) !buf =
  V4R.receive intr sock buf

