{-# language DataKinds #-}

module Socket.Interruptible
  ( InterruptRep
  , Interrupt
  , Intr
  , wait
  , tokenToStreamSendException
  , tokenToStreamReceiveException
  , tokenToDatagramSendException
  , tokenToDatagramReceiveException
  , tokenToSequencedPacketSendException
  , tokenToSequencedPacketReceiveException
  ) where

import Socket (Interruptibility(Interruptible))
import Socket.EventManager (Token)
import Control.Concurrent.STM (TVar)
import GHC.Exts (RuntimeRep(LiftedRep))
import qualified Socket.EventManager as EM
import qualified Socket.Stream as Stream
import qualified Socket.Datagram as Datagram
import qualified Socket.SequencedPacket as SequencedPacket

type InterruptRep = 'LiftedRep
type Interrupt = TVar Bool
type Intr = 'Interruptible

tokenToSequencedPacketSendException :: Token -> Either (SequencedPacket.SendException 'Interruptible) ()
{-# inline tokenToSequencedPacketSendException #-}
tokenToSequencedPacketSendException t = if EM.isInterrupt t
  then Left SequencedPacket.SendInterrupted
  else Right ()

tokenToSequencedPacketReceiveException :: Token -> Either (SequencedPacket.ReceiveException 'Interruptible) ()
{-# inline tokenToSequencedPacketReceiveException #-}
tokenToSequencedPacketReceiveException t = if EM.isInterrupt t
  then Left SequencedPacket.ReceiveInterrupted
  else Right ()

tokenToStreamSendException :: Token -> Int -> Either (Stream.SendException 'Interruptible) ()
{-# inline tokenToStreamSendException #-}
tokenToStreamSendException t i = if EM.isInterrupt t
  then Left (Stream.SendInterrupted i)
  else Right ()

tokenToStreamReceiveException :: Token -> Int -> Either (Stream.ReceiveException 'Interruptible) ()
{-# inline tokenToStreamReceiveException #-}
tokenToStreamReceiveException t i = if EM.isInterrupt t
  then Left (Stream.ReceiveInterrupted i)
  else Right ()

tokenToDatagramSendException :: Token -> Either (Datagram.SendException 'Interruptible) ()
{-# inline tokenToDatagramSendException #-}
tokenToDatagramSendException t = if EM.isInterrupt t
  then Left Datagram.SendInterrupted
  else Right ()

tokenToDatagramReceiveException :: Token -> Either (Datagram.ReceiveException 'Interruptible) ()
{-# inline tokenToDatagramReceiveException #-}
tokenToDatagramReceiveException t = if EM.isInterrupt t
  then Left Datagram.ReceiveInterrupted
  else Right ()

wait :: TVar Bool -> TVar Token -> IO Token
{-# inline wait #-}
wait = EM.interruptibleWait
