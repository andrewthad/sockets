{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}

module Stream.Send.Two.Indefinite
  ( sendBoth
  ) where

import Control.Concurrent.STM (TVar)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK, ePIPE, eCONNRESET)
import Foreign.C.Types (CSize)
import Socket.Error (die)
import Socket.EventManager (Token)
import Socket.Stream (SendException(..),Connection(..))
import Socket.Interrupt (Interrupt,Intr,wait,tokenToStreamSendException)
import System.Posix.Types (Fd)

import qualified Foreign.C.Error.Describe as D
import qualified Socket.EventManager as EM
import qualified Stream.Send.Two as SendBoth
import qualified Stream.Send.B as SendB
import qualified Stream.Send.Buffer.A as A
import qualified Stream.Send.Buffer.B as B

sendBoth ::
     Interrupt
  -> Connection
  -> A.Buffer
  -> B.Buffer
  -> IO (Either (SendException Intr) ())
sendBoth !intr (Connection !conn) !bufA !bufB = do
  let !mngr = EM.manager
  tv <- EM.writer mngr conn
  token0 <- wait intr tv
  case tokenToStreamSendException token0 0 of
    Left err -> pure (Left err)
    Right _ -> sendLoop intr conn tv token0 bufA bufB 0

sendLoop ::
     Interrupt -> Fd -> TVar Token -> Token
  -> A.Buffer -> B.Buffer -> Int -> IO (Either (SendException Intr) ())
sendLoop !intr !conn !tv !old !bufA !bufB !sent = if lenA > 0
  then SendBoth.sendOnce conn bufA bufB >>= \case
    Left e ->
      if | e == eAGAIN || e == eWOULDBLOCK -> do
             EM.unready old tv
             new <- wait intr tv
             case tokenToStreamSendException new sent of
               Left err -> pure (Left err)
               Right _ -> sendLoop intr conn tv new bufA bufB sent
         | e == ePIPE -> pure (Left SendShutdown)
         | e == eCONNRESET -> pure (Left SendReset)
         | otherwise -> die ("Socket.Stream.send: " ++ describeErrorCode e)
    Right sz' -> do
      let sz = csizeToInt sz'
      sendLoop intr conn tv old (A.advance bufA sz) bufB (sent + sz)
  else do
    let !sentB = sent - lenA
    sendLoopB intr conn tv old (B.advance bufB sentB) sentB
  where
  !lenA = A.length bufA

-- Switch to this loop when there is nothing left to send
-- in the first buffer.
sendLoopB ::
     Interrupt -> Fd -> TVar Token -> Token
  -> B.Buffer -> Int -> IO (Either (SendException Intr) ())
sendLoopB !intr !conn !tv !old !buf !sent = if len > 0
  then SendB.sendOnce conn buf >>= \case
    Left e ->
      if | e == eAGAIN || e == eWOULDBLOCK -> do
             EM.unready old tv
             new <- wait intr tv
             case tokenToStreamSendException new sent of
               Left err -> pure (Left err)
               Right _ -> sendLoopB intr conn tv new buf sent
         | e == ePIPE -> pure (Left SendShutdown)
         | e == eCONNRESET -> pure (Left SendReset)
         | otherwise -> die ("Socket.Stream.Send.Two.Indefinite.sendLoopB: " ++ describeErrorCode e)
    Right sz' -> do
      let sz = csizeToInt sz'
      sendLoopB intr conn tv old (B.advance buf sz) (sent + sz)
  else if len == 0
    then pure (Right ())
    else die "Socket.Stream.Send.Two.Indefinite.sendLoopB: negative slice length"
  where
  !len = B.length buf

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"
