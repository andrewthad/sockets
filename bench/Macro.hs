{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language UnboxedTuples #-}

-- This is a benchmark designed to stress both the sockets library
-- and the GHC event manager. It opens a moderate number of datagram sockets
-- that each belong to one of two teams: A and B. There is one worker
-- thread for each socket. The thread sends a datagram to a pseudorandomly
-- detemined socket on the other team. Then, it waits to receive a datagram
-- from a socket on the other team. All worker threads repeatedly perform
-- this task forever. Once a large number N of total receives have occurred,
-- the lucky worker thread performing the Nth receives fills an MVar that
-- tells the main thread that enough work has been done. The main thread
-- prints the total number of elapsed nanoseconds and then exits. This
-- benchmark does not attempt to close the sockets before exiting.

import Control.Concurrent (forkIO)
import Control.Exception (Exception)
import Control.Exception (throwIO)
import Control.Monad (forever,forM_,when)
import Data.Primitive (PrimArray,MutablePrimArray(..))
import Data.Primitive.MVar (MVar)
import Data.Word (Word16)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Exts (RealWorld,Int(I#))
import GHC.IO (IO(..))
import Socket.Datagram.IPv4.Undestined (Endpoint(..))
import System.Entropy (getEntropy)

import qualified Data.ByteString as B
import qualified Data.Primitive as PM
import qualified Data.Primitive.MVar as PM
import qualified GHC.Exts as E
import qualified Net.IPv4 as IPv4
import qualified Socket.Datagram.IPv4.Undestined as DIU

main :: IO ()
main = do
  done <- PM.newEmptyMVar
  recvCounter <- PM.newPrimArray 1
  PM.writePrimArray recvCounter 0 0
  socketsCounterA <- PM.newPrimArray 1
  PM.writePrimArray socketsCounterA 0 0
  socketsCounterB <- PM.newPrimArray 1
  PM.writePrimArray socketsCounterB 0 0
  socketsA <- PM.newPrimArray participants
  socketsB <- PM.newPrimArray participants
  socketsMVarA <- PM.newEmptyMVar
  socketsMVarB <- PM.newEmptyMVar
  forM_ (enumFromTo 0 (participants - 1)) $ \ix -> do
    forkIO $ worker ix done recvCounter socketsCounterA socketsA socketsMVarA socketsMVarB
  forM_ (enumFromTo 0 (participants - 1)) $ \ix -> do
    forkIO $ worker ix done recvCounter socketsCounterB socketsB socketsMVarB socketsMVarA
  _ <- PM.readMVar socketsMVarA
  _ <- PM.readMVar socketsMVarB
  start <- getMonotonicTimeNSec
  PM.takeMVar done
  end <- getMonotonicTimeNSec
  print (end - start)

participants :: Int
participants = 64

-- This is in units of machine words
payloadSize :: Int
payloadSize = 32

totalReceives :: Int
totalReceives = 3000000

-- The PrimArray must be of length @participants@.
worker :: 
     Int -- ^ Worker identifier
  -> MVar RealWorld () -- ^ Used to signal that enough receives have happened
  -> MutablePrimArray RealWorld Int -- ^ Counter of total receives, singleton array
  -> MutablePrimArray RealWorld Int -- ^ Counter of opened sockets, singleton array
  -> MutablePrimArray RealWorld Word16 -- ^ Ports used by local team
  -> MVar RealWorld (PrimArray Word16) -- ^ MVar for ports used by local team
  -> MVar RealWorld (PrimArray Word16) -- ^ MVar for ports used by remote team
  -> IO ()
worker !ident !done !recvCounter !counter !locals !mlocals !mremotes = do
  unhandled $ DIU.withSocket (DIU.Endpoint IPv4.loopback 0) $ \sock myPort -> do
    buf@(PM.MutablePrimArray buf#) <- PM.newPrimArray payloadSize
    seedByteString <- getEntropy (payloadSize * PM.sizeOf (undefined :: Int))
    let seedByteArray = E.fromList (B.unpack seedByteString)
    PM.copyByteArray (PM.MutableByteArray buf#) 0 seedByteArray 0 (payloadSize * PM.sizeOf (undefined :: Int))
    PM.writePrimArray locals ident myPort
    incrementWorkerCounter counter >>= \case
      True -> PM.unsafeFreezePrimArray locals >>= PM.putMVar mlocals
      False -> pure ()
    remotes <- PM.readMVar mremotes
    act sock buf remotes recvCounter done

act ::
     DIU.Socket -- Socket
  -> MutablePrimArray RealWorld Int -- Buffer for receives
  -> PrimArray Word16 -- Ports used by remote team
  -> MutablePrimArray RealWorld Int -- Receive counter, singleton array
  -> MVar RealWorld () -- Signal that we are finished
  -> IO ()
act !sock !buf@(MutablePrimArray buf#) !remotes !counter !done = forever $ do
  n <- scramble buf
  let remote = PM.indexPrimArray remotes (mod n participants)
  unhandled $ DIU.sendMutableByteArray sock (Endpoint {port = remote, address = IPv4.loopback})
    (PM.MutableByteArray buf#) 0 (payloadSize * PM.sizeOf (undefined :: Int))
  recvSz <- unhandled $ DIU.receiveMutableByteArraySlice_ sock (PM.MutableByteArray buf#) 0
    (payloadSize * PM.sizeOf (undefined :: Int))
  when (recvSz /= payloadSize * PM.sizeOf (undefined :: Int)) $ do
    fail "bad receive in act"
  incrementReceiveCounter counter >>= \case
    True -> PM.putMVar done ()
    False -> pure ()

scramble :: MutablePrimArray RealWorld Int -> IO Int
scramble arr = go 0 0x36b0b1c47d1ba5e1 0x55109de6a59394b3
  where
  go !ix !acc1 !acc2 = if ix < payloadSize
    then do
      v <- PM.readPrimArray arr ix
      PM.writePrimArray arr ix ((v + acc1) * acc2)
      go (ix + 1) acc2 v
    else pure (acc1 + acc2)

-- Returns true if the value of the counter reached the total
-- number of participants.
incrementWorkerCounter :: MutablePrimArray RealWorld Int -> IO Bool
incrementWorkerCounter (MutablePrimArray arr) = IO $ \s0 -> case E.fetchAddIntArray# arr 0# 1# s0 of
  (# s1, i #) -> (# s1, I# i == participants - 1 #)

incrementReceiveCounter :: MutablePrimArray RealWorld Int -> IO Bool
incrementReceiveCounter (MutablePrimArray arr) = IO $ \s0 -> case E.fetchAddIntArray# arr 0# 1# s0 of
  (# s1, i #) -> (# s1, I# i == totalReceives - 1 #)

unhandled :: Exception e => IO (Either e a) -> IO a
unhandled action = action >>= either throwIO pure

