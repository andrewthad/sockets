{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

{-# OPTIONS_GHC -fforce-recomp -O2 -Wall -Werror #-}

import Control.Exception (Exception,throwIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar)
import Control.Concurrent.MVar (MVar,newEmptyMVar,putMVar,takeMVar)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Primitive as PM
import qualified Net.IPv4 as IPv4
import qualified Socket.Stream.IPv4 as Stream
import qualified Socket.Stream.Uninterruptible.Bytes as Stream
import qualified System.IO as IO

main :: IO ()
main = do
  Stream.systemdListener >>= \case
    Right listener -> do
      BC8.hPutStr IO.stderr "[MAIN__] Acquired listener from systemd.\n"
      run listener
    Left _ -> do
      BC8.hPutStr IO.stderr "[MAIN__] Not running as a socket-activated service.\n"
      BC8.hPutStr IO.stderr "[MAIN__] Listening on port 9998.\n"
      Stream.listen (Stream.Peer IPv4.loopback 9998) >>= \case
        Left err -> throwIO err
        Right (listener, _) -> run listener

run :: Stream.Listener -> IO ()
run !listener = do
  terminating <- STM.newTVarIO False
  counter <- STM.newTVarIO (0 :: Int)
  done <- newEmptyMVar @()
  _ <- forkIO (listenForever done counter terminating listener)
  E.catch (takeMVar done)
    (\(e :: E.AsyncException) -> do
      IO.hFlush IO.stderr
      case e of
        E.UserInterrupt -> do
          STM.atomically $ STM.writeTVar terminating True
          STM.atomically $ do
            conns <- STM.readTVar counter
            case compare conns 0 of
              GT -> STM.retry
              EQ -> pure ()
              LT -> STM.throwSTM NegativeActiveConnections
          takeMVar done
        _ -> E.throwIO e
    )

data NegativeActiveConnections = NegativeActiveConnections
  deriving stock (Show,Eq)
  deriving anyclass (Exception)

countBytes :: Stream.Connection -> IO Int
countBytes !conn = do
  let go !acc = Stream.receiveOnce conn (16384 - 16) >>= \case
        Left err -> case err of
          Stream.ReceiveShutdown -> pure acc
          -- TODO: log exceptions
          _ -> do
            BC8.hPutStr IO.stderr "[WORKER] Client did something unexpected.\n"
            pure acc
        Right arr -> do
          let sz = PM.sizeofByteArray arr
          IO.hFlush IO.stderr
          go (acc + sz)
  n <- go 0
  IO.hFlush IO.stderr
  pure n

listenForever :: MVar () -> TVar Int -> TVar Bool -> Stream.Listener -> IO ()
listenForever !done !counter !terminating !lstn = do
  BC8.hPutStr IO.stderr "[LISTEN] Waiting for inbound connection.\n"
  e <- Stream.interruptibleForkAcceptedUnmasked counter terminating lstn
    (\e _ -> case e of
      Left Stream.ClosePeerContinuedSending ->
        BC8.hPutStr IO.stderr "[WORKER] Client continued sending after session had ended.\n"
      Right () ->
        BC8.hPutStr IO.stderr "[WORKER] Connection gracefully closed.\n"
    )
    (\conn (Stream.Peer _ port) -> do
      BC8.hPutStr IO.stderr (BC8.concat ["Accepted connection on port ", BC8.pack (show port), "\n"])
      total <- countBytes conn
      BC8.hPutStr IO.stderr (BC8.concat ["From peer on port ", BC8.pack (show port), ", received ", BC8.pack (show total), " bytes.\n"])
    )
  case e of
    Right _ -> listenForever done counter terminating lstn
    Left err -> case err of
      Stream.AcceptConnectionAborted -> do
        BC8.hPutStr IO.stderr "[MAIN__] Client reset the connection. Non-fatal.\n"
        listenForever done counter terminating lstn
      Stream.AcceptFileDescriptorLimit -> do
        BC8.hPutStr IO.stderr "[MAIN__] Ran out of file descriptors. Shutting down server.\n"
        putMVar done ()
      Stream.AcceptFirewalled -> do
        BC8.hPutStr IO.stderr "[MAIN__] Local firewall prohibits connection. Shutting down server.\n"
        putMVar done ()
      Stream.AcceptInterrupted -> do
        BC8.hPutStr IO.stderr "[MAIN__] User interrupt. No more connections.\n"
        putMVar done ()
  IO.hFlush IO.stderr
