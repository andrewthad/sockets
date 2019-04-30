{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}

module Socket.Stream.Uninterruptible
  ( InterruptRep
  , Interrupt
  , Intr
  , wait
  , tokenToSendException
  , tokenToReceiveException
  ) where

import Control.Concurrent.STM (TVar)
import Data.Void (Void)
import GHC.Exts (RuntimeRep(TupleRep),Proxy#)
import Socket (Interruptibility(Uninterruptible))
import Socket.EventManager (Token)
import Socket.Stream (ReceiveException)
import Socket.Stream (SendException)
import qualified Socket.EventManager as EM

-- We use Proxy# instead of () to ensure that this interrupt type
-- has no runtime cost associated with it.
type InterruptRep = 'TupleRep '[]
type Interrupt = Proxy# Void
type Intr = 'Uninterruptible

tokenToSendException :: Token -> Either (SendException 'Uninterruptible) ()
{-# inline tokenToSendException #-}
tokenToSendException _ = Right ()

tokenToReceiveException :: Token -> Either (ReceiveException 'Uninterruptible) ()
{-# inline tokenToReceiveException #-}
tokenToReceiveException _ = Right ()

wait :: Proxy# Void -> TVar Token -> IO Token
{-# inline wait #-}
wait !_ = EM.wait

