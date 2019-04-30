{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}

module Socket.Stream.Indefinite
  ( send
  ) where

import Control.Concurrent.STM (TVar)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK, ePIPE, eCONNRESET)
import Foreign.C.Types (CSize)
import Socket (SocketUnrecoverableException(..))
import Socket.Error (die)
import Socket.EventManager (Token)
import Socket.Stream (SendException(..),Connection(..))
import Stream.Interrupt (Interrupt,Intr,wait,tokenToSendException)
import Stream.Send (Buffer)
import System.Posix.Types (Fd)

import qualified Foreign.C.Error.Describe as D
import qualified Socket as SCK
import qualified Socket.EventManager as EM
import qualified Socket.Stream.Backpack.Send as Buffer

-- Send the entirely of the buffer, making repeated calls to
-- POSIX @send@ if necessary.
send :: Interrupt -> Connection -> Buffer -> IO (Either (SendException Intr) ())
send !intr (Connection !conn) !buf0 = do
  let !mngr = EM.manager
  tv <- EM.writer mngr conn
  token0 <- wait intr tv
  case tokenToSendException token0 of
    Left err -> pure (Left err)
    Right _ -> sendLoop intr conn tv token0 buf0

sendLoop :: Interrupt -> Fd -> TVar Token -> Token -> Buffer -> IO (Either (SendException Intr) ())
sendLoop !intr !conn !tv !old !buf = if len > 0
  then Buffer.sendOnce conn buf >>= \case
    Left e ->
      if | e == eAGAIN || e == eWOULDBLOCK -> do
             EM.unready old tv
             new <- wait intr tv
             case tokenToSendException new of
               Left err -> pure (Left err)
               Right _ -> sendLoop intr conn tv new buf
         | e == ePIPE -> pure (Left SendShutdown)
         | e == eCONNRESET -> pure (Left SendReset)
         | otherwise -> die $ show $ SocketUnrecoverableException
             "Socket.Stream.IPv4"
             "send"
             [describeErrorCode e]
    Right sz' -> do
      let sz = csizeToInt sz'
      sendLoop intr conn tv old (Buffer.advance buf sz)
  else if len == 0
    then pure (Right ())
    else die $ show $ SocketUnrecoverableException
      "Socket.Stream.IPv4"
      "send"
      [SCK.negativeSliceLength]
  where
  !len = Buffer.length buf

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"


