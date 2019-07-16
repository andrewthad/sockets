{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}

module Datagram.Send.Indefinite
  ( send
  ) where

import Control.Concurrent.STM (TVar)
import Datagram.Send (Peer)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK, eACCES)
import Foreign.C.Error (eCONNREFUSED)
import Foreign.C.Types (CSize)
import Socket.Error (die)
import Socket.EventManager (Token)
import Socket.Datagram (SendException(..))
import Socket.Buffer (Buffer)
import Socket.Interrupt (Interrupt,Intr,wait,tokenToDatagramSendException)
import System.Posix.Types (Fd)

import qualified Foreign.C.Error.Describe as D
import qualified Socket.EventManager as EM
import qualified Socket.Buffer as Buffer
import qualified Datagram.Send as Send

-- Send the entirely of the buffer, making a single call to
-- POSIX @send@. This is used for datagram sockets. We cannot use a
-- Socket newtype here since destined and undestined sockets
-- use different newtypes.
send ::
     Interrupt
  -> Peer
  -> Fd
  -> Buffer
  -> IO (Either (SendException Intr) ())
send !intr !dst !sock !buf = do
  let !mngr = EM.manager
  tv <- EM.writer mngr sock
  token0 <- wait intr tv
  case tokenToDatagramSendException token0 of
    Left err -> pure (Left err)
    Right _ -> sendLoop intr dst sock tv token0 buf

sendLoop ::
     Interrupt -> Peer -> Fd -> TVar Token -> Token
  -> Buffer -> IO (Either (SendException Intr) ())
sendLoop !intr !dst !sock !tv !old !buf =
  Send.send dst sock buf >>= \case
    Left e ->
      if | e == eAGAIN || e == eWOULDBLOCK -> do
             EM.unready old tv
             new <- wait intr tv
             case tokenToDatagramSendException new of
               Left err -> pure (Left err)
               Right _ -> sendLoop intr dst sock tv new buf
         | e == eACCES -> pure (Left SendBroadcasted)
         | e == eCONNREFUSED -> pure (Left SendConnectionRefused)
         | otherwise -> die ("Socket.Datagram.send: " ++ describeErrorCode e)
    Right sz -> if csizeToInt sz == Buffer.length buf
      then pure $! Right ()
      else pure $! Left $! SendTruncated $! csizeToInt sz

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"
