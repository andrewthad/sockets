{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}

module Datagram.Receive.Indefinite
  ( receive
  ) where

import Control.Concurrent.STM (TVar)
import Datagram.DecodeAddress (Peer,maxAddressSize,decodeAddress)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK)
import Foreign.C.Types (CSize)
import Socket.Error (die)
import Socket.EventManager (Token)
import Socket.Datagram (ReceiveException(..))
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
-- Socket newtype here since destined and undestined sockets
-- use different newtypes.
receive :: Interrupt -> Fd -> Buffer -> IO (Either (ReceiveException Intr) (Peer,Int))
receive !intr !sock !buf = do
  let !mngr = EM.manager
  tv <- EM.reader mngr sock
  token0 <- wait intr tv
  case tokenToDatagramReceiveException token0 of
    Left err -> pure (Left err)
    Right _ -> receiveLoop intr tv token0 sock buf

receiveLoop ::
     Interrupt
  -> TVar Token
  -> Token
  -> Fd -- ^ Socket
  -> Buffer -- ^ Buffer
  -> IO (Either (ReceiveException Intr) (Peer,Int))
receiveLoop !intr !tv !token0 !fd !buf = do
  -- We use MSG_TRUNC so that we are able to figure out
  -- whether or not bytes were discarded. If bytes were
  -- discarded (meaning that the buffer was too small),
  -- we return an exception.
  e <- Receive.receiveFromOnce fd buf L.truncate maxAddressSize
  case e of
    Left err -> if err == eWOULDBLOCK || err == eAGAIN
      then do
        EM.unready token0 tv
        token1 <- wait intr tv
        receiveLoop intr tv token1 fd buf
      else die $ concat
        [ "Socket.Datagram.receive: " 
        , describeErrorCode err
        ]
    Right (sockAddrRequiredSz,sockAddr,recvSz) -> do
      peer <- decodeAddress sockAddrRequiredSz sockAddr
      let !recvSzInt = csizeToInt recvSz
      if recvSzInt <= Buffer.length buf
        then pure $ Right (peer,recvSzInt)
        else pure (Left (ReceiveTruncated recvSzInt))

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

