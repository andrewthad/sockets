{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language UnboxedTuples #-}

module Stream.Receive.Indefinite
  ( receiveExactly
  , receiveOnce
  , receiveBetween
  ) where

import Control.Monad (when)
import Control.Concurrent.STM (TVar)
import Foreign.C.Error (Errno(..), eAGAIN, eWOULDBLOCK, eCONNRESET, eHOSTUNREACH)
import Foreign.C.Types (CSize)
import GHC.Exts (RealWorld,State#,Int(I#),Int#)
import GHC.IO (IO(IO))
import Socket.Error (die)
import Socket.EventManager (Token)
import Socket.Stream (ReceiveException(..),Connection(..))
import Socket.Buffer (Buffer)
import Socket.Interrupt (Interrupt,Intr,wait,tokenToStreamReceiveException)
import System.Posix.Types (Fd)

import qualified Foreign.C.Error.Describe as D
import qualified Socket.EventManager as EM
import qualified Socket.Buffer as Buffer
import qualified Stream.Receive as Receive

-- Receive exactly the specified number of bytes, making repeated calls to
-- POSIX @recv@ if necessary.
receiveExactly ::
     Interrupt
  -> Connection
  -> Buffer
  -> IO (Either (ReceiveException Intr) ())
receiveExactly !intr (Connection !conn) !buf = do
  let !mngr = EM.manager
  !tv <- EM.reader mngr conn
  e <- box $ receiveLoop intr conn tv buf (Buffer.length buf) 0
  case e of
    Left err -> pure (Left err)
    -- Discard the total since it is known to be equal to the
    -- requested number of bytes.
    Right _ -> pure (Right ())

-- Receive a chunk of data from the socket. This may make multiple calls
-- to POSIX @recv@ if EAGAIN is returned. It makes at most one call that
-- successfully fills the buffer.
receiveOnce ::
     Interrupt
  -> Connection
  -> Buffer
  -> IO (Either (ReceiveException Intr) Int)
receiveOnce !intr (Connection !conn) !buf = do
  let !mngr = EM.manager
  !tv <- EM.reader mngr conn
  box $ receiveLoop intr conn tv buf 1 0

-- Receive a number of bytes bounded on the upper end by the buffer length
-- and on the lower end by the integer argument. This will make repeated
-- calls to POSIX @recv@ until the lower bound is reached.
receiveBetween :: Interrupt -> Connection -> Buffer -> Int -> IO (Either (ReceiveException Intr) Int)
receiveBetween !intr (Connection !conn) !buf !minLen
  | Buffer.length buf > 0 = if minLen >= 0 && minLen <= Buffer.length buf
      then do
        let !mngr = EM.manager
        !tv <- EM.reader mngr conn
        box $ receiveLoop intr conn tv buf minLen 0
      else die "Socket.Stream.IPv4.receive: negative slice length"
  | Buffer.length buf == 0 && minLen == 0 = pure (Right 0)
  | otherwise = die "Socket.Stream.IPv4.receive: negative slice length"

type Result# = State# RealWorld -> (# State# RealWorld, (# ReceiveException Intr | Int# #) #)

unbox :: IO (Either (ReceiveException Intr) Int) -> Result#
{-# inline unbox #-}
unbox (IO f) = \s0 -> case f s0 of
  (# s1, e #) -> case e of
    Left err -> (# s1, (# err | #) #)
    Right (I# i) -> (# s1, (# | i #) #)

box :: Result# -> IO (Either (ReceiveException Intr) Int)
box f = IO $ \s0 -> case f s0 of
  (# s1, e #) -> case e of
    (# err | #) -> (# s1, Left err #)
    (# | i #) -> (# s1, Right (I# i) #)

-- We unbox the result since GHC is not very good at CPR analysis.
receiveLoop ::
     Interrupt
  -> Fd
  -> TVar Token
  -> Buffer
  -> Int
  -> Int
  -> Result#
     -- result is isomorphic to IO (Either (ReceiveException Intr) Int)
receiveLoop !intr !conn !tv !buf !minLen !total
  | minLen <= 0 = unbox (pure (Right total))
  | otherwise = unbox $ do
      let !maxLen = Buffer.length buf
      !token <- wait intr tv
      case tokenToStreamReceiveException token total of
        Left err -> pure (Left err)
        Right _ -> Receive.receiveOnce conn buf >>= \case
          Left err ->
            if | err == eAGAIN || err == eWOULDBLOCK -> do
                   EM.persistentUnready token tv
                   box $ receiveLoop intr conn tv buf minLen total
               | err == eCONNRESET -> pure (Left ReceiveReset)
               | err == eHOSTUNREACH -> pure (Left ReceiveHostUnreachable)
               | otherwise -> die ("Socket.Stream.IPv4.receive: " ++ describeErrorCode err)
          Right recvSzCInt -> if recvSzCInt /= 0
            then do
              let recvSz = csizeToInt recvSzCInt
              -- This unready only works because of a guarantee that epoll
              -- makes in the man page in FAQ question 9.
              when (recvSz < maxLen) (EM.unready token tv)
              box $ receiveLoop intr conn tv
                (Buffer.advance buf recvSz) (minLen - recvSz) (total + recvSz)
            else pure (Left ReceiveShutdown)

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"


