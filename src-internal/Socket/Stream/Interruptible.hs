{-# language DataKinds #-}

module Socket.Stream.Interruptible
  ( Interrupt
  , Intr
  , wait
  , tokenToSendException
  , tokenToReceiveException
  ) where

import Socket (Interruptibility(Interruptible))
import Socket.Stream (SendException(SendInterrupted))
import Socket.Stream (ReceiveException(ReceiveInterrupted))
import Socket.EventManager (Token)
import Control.Concurrent.STM (TVar)
import qualified Socket.EventManager as EM

type Interrupt = TVar Bool
type Intr = 'Interruptible

tokenToSendException :: Token -> Either (SendException 'Interruptible) ()
{-# inline tokenToSendException #-}
tokenToSendException t = if EM.isInterrupt t
  then Left SendInterrupted
  else Right ()

tokenToReceiveException :: Token -> Either (ReceiveException 'Interruptible) ()
{-# inline tokenToReceiveException #-}
tokenToReceiveException t = if EM.isInterrupt t
  then Left ReceiveInterrupted
  else Right ()

wait :: TVar Bool -> TVar Token -> IO Token
{-# inline wait #-}
wait = EM.interruptibleWait
