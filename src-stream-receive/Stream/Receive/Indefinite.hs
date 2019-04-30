{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}

module Stream.Receive.Indefinite
  ( receiveExactly
  , receiveChunk
  , receiveBetween
  ) where

import Control.Monad (when)
import Control.Concurrent.STM (TVar)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK, eCONNRESET)
import Foreign.C.Types (CSize)
import Socket.Error (die)
import Socket.EventManager (Token)
import Socket.Stream (ReceiveException(..),Connection(..))
import Stream.Buffer (Buffer)
import Stream.Interrupt (Interrupt,Intr,wait,tokenToReceiveException)
import System.Posix.Types (Fd)

import qualified Foreign.C.Error.Describe as D
import qualified Socket.EventManager as EM
import qualified Stream.Buffer as Buffer
import qualified Stream.Receive as Receive

-- Receive exactly the specified number of bytes, making repeated calls to
-- POSIX @recv@ if necessary.
receiveExactly :: Interrupt -> Connection -> Buffer -> IO (Either (ReceiveException Intr) ())
receiveExactly !intr (Connection !conn) !buf = do
  let !mngr = EM.manager
  !tv <- EM.writer mngr conn
  e <- receiveLoop intr conn tv buf (Buffer.length buf) 0
  case e of 
    Left err -> pure (Left err)
    -- Discard the total since it is known to be equal to the
    -- requested number of bytes.
    Right _ -> pure (Right ())

-- Receive a chunk of data from the socket. This may make multiple calls
-- to POSIX @recv@ if EAGAIN is returned. It makes at most one call that
-- successfully fills the buffer.
receiveChunk :: Interrupt -> Connection -> Buffer -> IO (Either (ReceiveException Intr) Int)
receiveChunk !intr (Connection !conn) !buf = do
  let !mngr = EM.manager
  !tv <- EM.writer mngr conn
  receiveLoop intr conn tv buf 1 0

-- Receive a number of bytes bounded on the upper end by the buffer length
-- and on the lower end by the integer argument. This will make repeated
-- calls to POSIX @recv@ until the lower bound is reached.
receiveBetween :: Interrupt -> Connection -> Buffer -> Int -> IO (Either (ReceiveException Intr) Int)
receiveBetween !intr (Connection !conn) !buf !minLen
  | Buffer.length buf > 0 = if minLen >= 0 && minLen <= Buffer.length buf
      then do
        let !mngr = EM.manager
        !tv <- EM.writer mngr conn
        receiveLoop intr conn tv buf minLen 0
      else die "Socket.Stream.IPv4.receive: negative slice length"
  | Buffer.length buf == 0 && minLen == 0 = pure (Right 0)
  | otherwise = die "Socket.Stream.IPv4.receive: negative slice length"

receiveLoop ::
     Interrupt -> Fd -> TVar Token -> Buffer
  -> Int -> Int -> IO (Either (ReceiveException Intr) Int)
receiveLoop !intr !conn !tv !buf !minLen !total
  | minLen <= 0 = pure $! Right $! total
  | otherwise = do
      let !maxLen = Buffer.length buf
      !token <- wait intr tv
      case tokenToReceiveException token of
        Left err -> pure (Left err)
        Right _ -> Receive.receiveOnce conn buf >>= \case
          Left err ->
            if | err == eAGAIN || err == eWOULDBLOCK -> do
                   EM.persistentUnready token tv
                   receiveLoop intr conn tv buf minLen total
               | err == eCONNRESET -> pure (Left ReceiveReset)
               | otherwise -> die ("Socket.Stream.IPv4.receive: " ++ describeErrorCode err)
          Right recvSzCInt -> if recvSzCInt /= 0
            then do
              let recvSz = csizeToInt recvSzCInt
              -- This only works because of a guarantee that epoll makes in
              -- the man page in FAQ question 9.
              when (recvSz < maxLen) $ do
                EM.unready token tv
              receiveLoop intr conn tv (Buffer.advance buf recvSz) (minLen - recvSz) (total + recvSz)
            else pure (Left ReceiveShutdown)

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"


