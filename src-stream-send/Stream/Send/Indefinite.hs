{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}

module Stream.Send.Indefinite
  ( send
  ) where

import Control.Concurrent.STM (TVar)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK, ePIPE, eCONNRESET)
import Foreign.C.Types (CSize)
import Socket.Error (die)
import Socket.EventManager (Token)
import Socket.Stream (SendException(..),Connection(..))
import Stream.Buffer (Buffer)
import Stream.Interrupt (Interrupt,Intr,wait,tokenToSendException)
import System.Posix.Types (Fd)

import qualified Foreign.C.Error.Describe as D
import qualified Socket.EventManager as EM
import qualified Stream.Buffer as Buffer
import qualified Stream.Send as Send

-- Send the entirely of the buffer, making repeated calls to
-- POSIX @send@ if necessary.
send :: Interrupt -> Connection -> Buffer -> IO (Either (SendException Intr) ())
send !intr (Connection !conn) !buf = do
  let !mngr = EM.manager
  tv <- EM.writer mngr conn
  token0 <- wait intr tv
  case tokenToSendException token0 of
    Left err -> pure (Left err)
    Right _ -> sendLoop intr conn tv token0 buf

sendLoop :: Interrupt -> Fd -> TVar Token -> Token -> Buffer -> IO (Either (SendException Intr) ())
sendLoop !intr !conn !tv !old !buf = if len > 0
  then Send.sendOnce conn buf >>= \case
    Left e ->
      if | e == eAGAIN || e == eWOULDBLOCK -> do
             EM.unready old tv
             new <- wait intr tv
             case tokenToSendException new of
               Left err -> pure (Left err)
               Right _ -> sendLoop intr conn tv new buf
         | e == ePIPE -> pure (Left SendShutdown)
         | e == eCONNRESET -> pure (Left SendReset)
         | otherwise -> die ("Socket.Stream.IPv4.send: " ++ describeErrorCode e)
    Right sz' -> do
      let sz = csizeToInt sz'
      sendLoop intr conn tv old (Buffer.advance buf sz)
  else if len == 0
    then pure (Right ())
    else die "Socket.Stream.IPv4.send: negative slice length"
  where
  !len = Buffer.length buf

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"


