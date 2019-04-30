{-# language DataKinds #-}

module Socket.Stream.Uninterruptible
  ( Interrupt
  , Intr
  , wait
  , tokenToSendException
  , tokenToReceiveException
  ) where

import Socket (Interruptibility(Uninterruptible))
import Socket.Stream (SendException)
import Socket.Stream (ReceiveException)
import Socket.EventManager (Token)
import Control.Concurrent.STM (TVar)
import qualified Socket.EventManager as EM

type Interrupt = ()
type Intr = 'Uninterruptible

tokenToSendException :: Token -> Either (SendException 'Uninterruptible) ()
{-# inline tokenToSendException #-}
tokenToSendException _ = Right ()

tokenToReceiveException :: Token -> Either (ReceiveException 'Uninterruptible) ()
{-# inline tokenToReceiveException #-}
tokenToReceiveException _ = Right ()

wait :: () -> TVar Token -> IO Token
{-# inline wait #-}
wait _ = EM.wait

