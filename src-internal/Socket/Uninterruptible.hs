{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}

module Socket.Uninterruptible
  ( InterruptRep
  , Interrupt
  , Intr
  , wait
  , tokenToStreamSendException
  , tokenToStreamReceiveException
  , tokenToDatagramSendException
  , tokenToDatagramReceiveException
  ) where

import Control.Concurrent.STM (TVar)
import Data.Void (Void)
import GHC.Exts (RuntimeRep(TupleRep),Proxy#)
import Socket (Interruptibility(Uninterruptible))
import Socket.EventManager (Token)
import qualified Socket.EventManager as EM
import qualified Socket.Stream as Stream
import qualified Socket.Datagram as Datagram

-- We use Proxy# instead of () to ensure that this interrupt type
-- has no runtime cost associated with it.
type InterruptRep = 'TupleRep '[]
type Interrupt = Proxy# Void
type Intr = 'Uninterruptible

tokenToStreamSendException :: Token -> Either (Stream.SendException 'Uninterruptible) ()
{-# inline tokenToStreamSendException #-}
tokenToStreamSendException _ = Right ()

tokenToStreamReceiveException :: Token -> Either (Stream.ReceiveException 'Uninterruptible) ()
{-# inline tokenToStreamReceiveException #-}
tokenToStreamReceiveException _ = Right ()

tokenToDatagramSendException :: Token -> Either (Datagram.SendException 'Uninterruptible) ()
{-# inline tokenToDatagramSendException #-}
tokenToDatagramSendException _ = Right ()

tokenToDatagramReceiveException :: Token -> Either (Datagram.ReceiveException 'Uninterruptible) ()
{-# inline tokenToDatagramReceiveException #-}
tokenToDatagramReceiveException _ = Right ()

wait :: Proxy# Void -> TVar Token -> IO Token
{-# inline wait #-}
wait !_ = EM.wait
