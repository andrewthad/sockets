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
  ) where

import Socket (Interruptibility(Interruptible))
import Socket.EventManager (Token)
import Control.Concurrent.STM (TVar)
import GHC.Exts (RuntimeRep(LiftedRep))
import qualified Socket.EventManager as EM
import qualified Socket.Stream as Stream
import qualified Socket.Datagram as Datagram

type InterruptRep = 'LiftedRep
type Interrupt = TVar Bool
type Intr = 'Interruptible

tokenToStreamSendException :: Token -> Either (Stream.SendException 'Interruptible) ()
{-# inline tokenToStreamSendException #-}
tokenToStreamSendException t = if EM.isInterrupt t
  then Left Stream.SendInterrupted
  else Right ()

tokenToStreamReceiveException :: Token -> Either (Stream.ReceiveException 'Interruptible) ()
{-# inline tokenToStreamReceiveException #-}
tokenToStreamReceiveException t = if EM.isInterrupt t
  then Left Stream.ReceiveInterrupted
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
