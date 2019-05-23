{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}

module Datagram.Receive.Indefinite
  ( receive
  , receiveLoop
  , receiveAttempt
  ) where

import Control.Concurrent.STM (TVar)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK)
import Foreign.C.Types (CSize)
import Socket.Error (die)
import Socket.EventManager (Token)
import Socket.Datagram (ReceiveException(..))
import Socket.Debug (debug)
import Socket.Buffer (Buffer)
import Socket.Interrupt (Interrupt,Intr,wait,tokenToDatagramReceiveException)
import System.Posix.Types (Fd)

import qualified Foreign.C.Error.Describe as D
import qualified Socket.EventManager as EM
import qualified Socket.Buffer as Buffer
import qualified Datagram.Receive as Receive
import qualified Linux.Socket as L

-- Send the entirely of the buffer, making a single call to
-- POSIX @send@. This is used for datagram sockets. We cannot use a
-- Socket newtype here since connected and unconnected sockets
-- apply Socket to different types.
receive ::
     Interrupt
  -> Fd
  -> Buffer
  -> Receive.AddressBufferOffset
  -> IO (Either (ReceiveException Intr) Int)
receive !intr !sock !buf !addrBuf = do
  let !mngr = EM.manager
  tv <- EM.reader mngr sock
  token0 <- wait intr tv
  case tokenToDatagramReceiveException token0 of
    Left err -> pure (Left err)
    Right _ -> receiveLoop intr tv token0 sock buf addrBuf

receiveAttempt ::
     Fd -- ^ Socket
  -> Buffer -- ^ Buffer
  -> Receive.AddressBufferOffset -- ^ Buffer for the address
  -> IO (Either (ReceiveException Intr) (Maybe Int))
{-# inline receiveAttempt #-}
receiveAttempt !fd !buf !addrBuf = do
  -- We use MSG_TRUNC so that we are able to figure out
  -- whether or not bytes were discarded. If bytes were
  -- discarded (meaning that the buffer was too small),
  -- we return an exception.
  e <- Receive.receiveFromOnce fd buf L.truncate addrBuf
  case e of
    Left err -> if err == eWOULDBLOCK || err == eAGAIN
      then pure (Right Nothing)
      else die $ concat
        [ "Socket.Datagram.receive: " 
        , describeErrorCode err
        ]
    Right recvSz -> do
      let !recvSzInt = csizeToInt recvSz
      if recvSzInt <= Buffer.length buf
        then pure (Right (Just recvSzInt))
        else pure (Left (ReceiveTruncated recvSzInt))

receiveLoop ::
     Interrupt
  -> TVar Token
  -> Token
  -> Fd -- ^ Socket
  -> Buffer -- ^ Buffer
  -> Receive.AddressBufferOffset
  -> IO (Either (ReceiveException Intr) Int)
receiveLoop !intr !tv !token0 !fd !buf !addrBuf = receiveAttempt fd buf addrBuf >>= \case
  Left err -> pure (Left err)
  Right m -> case m of
    Nothing -> do
      EM.unready token0 tv
      token1 <- wait intr tv
      receiveLoop intr tv token1 fd buf addrBuf
    Just !r -> pure (Right r)

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

