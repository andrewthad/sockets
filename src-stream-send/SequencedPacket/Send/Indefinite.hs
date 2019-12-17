{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}

module SequencedPacket.Send.Indefinite
  ( send
  , attemptSend
  ) where

import Control.Concurrent.STM (TVar)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK)
import Foreign.C.Error (eCONNRESET,ePIPE)
import Foreign.C.Types (CSize)
import Socket.Error (die)
import Socket.EventManager (Token)
import Socket.SequencedPacket (SendException(..))
import Socket.Buffer (Buffer)
import Socket.Interrupt (Interrupt,Intr,wait,tokenToSequencedPacketSendException)
import System.Posix.Types (Fd)

import qualified Foreign.C.Error.Describe as D
import qualified Socket.EventManager as EM
import qualified Socket.Buffer as Buffer
import qualified Stream.Send as Send

-- Send the entirely of the buffer, making a single call to
-- POSIX @send@. This is used for datagram sockets. We cannot use a
-- Socket newtype here since destined and undestined sockets
-- use different newtypes.
send ::
     Interrupt
  -> Fd
  -> Buffer
  -> IO (Either (SendException Intr) ())
send !intr !sock !buf = do
  let !mngr = EM.manager
  tv <- EM.writer mngr sock
  token0 <- wait intr tv
  case tokenToSequencedPacketSendException token0 of
    Left err -> pure (Left err)
    Right _ -> sendLoop intr sock tv token0 buf

-- Never blocks.
attemptSend ::
     Fd
  -> Buffer
  -> IO (Either (SendException Intr) Bool)
attemptSend !sock !buf = Send.sendOnce sock buf >>= \case
  Left e ->
    if | e == eAGAIN || e == eWOULDBLOCK -> pure (Right False)
       | e == ePIPE -> pure (Left SendShutdown)
       | e == eCONNRESET -> pure (Left SendReset)
       | otherwise -> die ("Socket.SequencedPacket.send: " ++ describeErrorCode e)
  Right sz -> if csizeToInt sz == Buffer.length buf
    then pure $! Right True
    else pure $! Left $! SendTruncated $! csizeToInt sz

sendLoop ::
     Interrupt -> Fd -> TVar Token -> Token
  -> Buffer -> IO (Either (SendException Intr) ())
sendLoop !intr !sock !tv !old !buf =
  Send.sendOnce sock buf >>= \case
    Left e ->
      if | e == eAGAIN || e == eWOULDBLOCK -> do
             EM.unready old tv
             new <- wait intr tv
             case tokenToSequencedPacketSendException new of
               Left err -> pure (Left err)
               Right _ -> sendLoop intr sock tv new buf
         | e == ePIPE -> pure (Left SendShutdown)
         | e == eCONNRESET -> pure (Left SendReset)
         | otherwise -> die ("Socket.SequencedPacket.send: " ++ describeErrorCode e)
    Right sz -> if csizeToInt sz == Buffer.length buf
      then pure $! Right ()
      else pure $! Left $! SendTruncated $! csizeToInt sz

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

