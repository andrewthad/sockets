{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}

module SequencedPacket.Receive.Indefinite
  ( receive
  , receiveAttempt
  ) where

import Control.Concurrent.STM (TVar)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK, eCONNRESET)
import Foreign.C.Types (CSize)
import Socket.Error (die)
import Socket.EventManager (Token)
import Socket.SequencedPacket (ReceiveException(..))
import Socket.Buffer (Buffer)
import Socket.Interrupt (Interrupt,Intr,wait,tokenToSequencedPacketReceiveException)
import System.Posix.Types (Fd)

import qualified Foreign.C.Error.Describe as D
import qualified Socket.EventManager as EM
import qualified Socket.Buffer as Buffer
import qualified Stream.Receive as Receive
import qualified Linux.Socket as L

receive ::
     Interrupt
  -> Fd
  -> Buffer
  -> IO (Either (ReceiveException Intr) Int)
receive !intr !sock !buf = do
  let !mngr = EM.manager
  tv <- EM.reader mngr sock
  token0 <- wait intr tv
  case tokenToSequencedPacketReceiveException token0 of
    Left err -> pure (Left err)
    Right _ -> receiveLoop intr tv token0 sock buf

receiveAttempt ::
     Fd -- ^ Socket
  -> Buffer -- ^ Buffer
  -> IO (Either (ReceiveException Intr) (Maybe Int))
{-# inline receiveAttempt #-}
receiveAttempt !fd !buf = do
  e <- Receive.receiveOnce fd buf
  case e of
    Left err ->
      if | err == eWOULDBLOCK || err == eAGAIN -> pure (Right Nothing)
         | err == eCONNRESET -> pure (Left ReceiveReset)
         | otherwise -> die $ concat
             [ "Socket.SequencedPacket.receive: " 
             , describeErrorCode err
             ]
    Right recvSz -> do
      let !recvSzInt = csizeToInt recvSz
      -- TODO: This is sort of a hack, but it mostly works. We should
      -- use MSG_TRUNC, but I am too lazy to fix this at the moment.
      if recvSzInt == Buffer.length buf
        then pure (Left ReceiveTruncated)
        else pure (Right (Just recvSzInt))

receiveLoop ::
     Interrupt
  -> TVar Token
  -> Token
  -> Fd -- ^ Socket
  -> Buffer -- ^ Buffer
  -> IO (Either (ReceiveException Intr) Int)
receiveLoop !intr !tv !token0 !fd !buf = receiveAttempt fd buf >>= \case
  Left err -> pure (Left err)
  Right m -> case m of
    Nothing -> do
      EM.unready token0 tv
      token1 <- wait intr tv
      case tokenToSequencedPacketReceiveException token1 of
        Left err -> pure (Left err)
        Right _ -> receiveLoop intr tv token1 fd buf
    Just !r -> pure (Right r)

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

