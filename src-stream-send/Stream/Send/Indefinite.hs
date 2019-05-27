{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}

module Stream.Send.Indefinite
  ( send
  , sendOnce
  ) where

import Control.Concurrent.STM (TVar)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK, ePIPE, eCONNRESET)
import Foreign.C.Types (CSize)
import Socket.Error (die)
import Socket.EventManager (Token)
import Socket.Stream (SendException(..),Connection(..))
import Socket.Buffer (Buffer)
import Socket.Interrupt (Interrupt,Intr,wait,tokenToStreamSendException)
import System.Posix.Types (Fd)

import qualified Foreign.C.Error.Describe as D
import qualified Socket.EventManager as EM
import qualified Socket.Buffer as Buffer
import qualified Stream.Send as Send

-- Send the entirely of the buffer, making repeated calls to
-- POSIX @send@ if necessary. This is used for stream sockets.
send :: Interrupt -> Connection -> Buffer -> IO (Either (SendException Intr) ())
send !intr (Connection !conn) !buf = do
  let !mngr = EM.manager
  tv <- EM.writer mngr conn
  token0 <- wait intr tv
  case tokenToStreamSendException token0 0 of
    Left err -> pure (Left err)
    Right _ -> sendLoop intr conn tv token0 buf 0

sendLoop ::
     Interrupt -> Fd -> TVar Token -> Token
  -> Buffer -> Int -> IO (Either (SendException Intr) ())
sendLoop !intr !conn !tv !old !buf !sent = if len > 0
  then Send.sendOnce conn buf >>= \case
    Left e ->
      if | e == eAGAIN || e == eWOULDBLOCK -> do
             EM.unready old tv
             new <- wait intr tv
             case tokenToStreamSendException new sent of
               Left err -> pure (Left err)
               Right _ -> sendLoop intr conn tv new buf sent
         | e == ePIPE -> pure (Left SendShutdown)
         | e == eCONNRESET -> pure (Left SendReset)
         | otherwise -> die ("Socket.Stream.send: " ++ describeErrorCode e)
    Right sz' -> do
      let sz = csizeToInt sz'
      sendLoop intr conn tv old (Buffer.advance buf sz) (sent + sz)
  else if len == 0
    then pure (Right ())
    else die "Socket.Stream.send: negative slice length"
  where
  !len = Buffer.length buf

-- TODO: sendOnce and send (along with their recursive helper
-- functions) are extremely similar. Maybe there is a way to
-- factor out something they have in common.
sendOnce :: Interrupt -> Connection -> Buffer -> IO (Either (SendException Intr) Int)
sendOnce !intr (Connection conn) !buf = do
  let !mngr = EM.manager
  tv <- EM.writer mngr conn
  token0 <- wait intr tv
  case tokenToStreamSendException token0 0 of
    Left err -> pure (Left err)
    Right _ -> sendOnceLoop intr conn tv token0 buf

sendOnceLoop ::
     Interrupt -> Fd -> TVar Token -> Token
  -> Buffer -> IO (Either (SendException Intr) Int)
sendOnceLoop !intr !conn !tv !old !buf = if len > 0
  then Send.sendOnce conn buf >>= \case
    Left e ->
      if | e == eAGAIN || e == eWOULDBLOCK -> do
             EM.unready old tv
             new <- wait intr tv
             case tokenToStreamSendException new 0 of
               Left err -> pure (Left err)
               Right _ -> sendOnceLoop intr conn tv new buf
         | e == ePIPE -> pure (Left SendShutdown)
         | e == eCONNRESET -> pure (Left SendReset)
         | otherwise -> die ("Socket.Stream.send: " ++ describeErrorCode e)
    Right sz' -> pure $! Right $! csizeToInt sz'
  else if len == 0
    then pure (Right 0)
    else die "Socket.Stream.send: negative slice length"
  where
  !len = Buffer.length buf

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"


