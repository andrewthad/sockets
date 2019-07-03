{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

import Prelude hiding (log,length)

import Control.Exception (Exception,throwIO)
import System.Environment (getArgs)
import Data.Primitive (ByteArray(..))
import Control.Monad (replicateM_)
import Data.Bytes.Types (Bytes(..))

import qualified Socket.Datagram.IPv4.Unconnected as DIU
import qualified Socket.Datagram.Uninterruptible.Bytes as DIU
import qualified Socket.Stream.IPv4 as SI
import qualified Socket.Stream.Uninterruptible.Bytes as SI
import qualified GHC.Exts as E
import qualified Data.Primitive as PM
import qualified Data.Primitive.MVar as PM
import qualified Net.IPv4 as IPv4
import qualified Data.ByteString as B
import qualified Data.ByteString.Short.Internal as SB
import qualified Data.ByteString.Char8 as BC
import qualified System.Log.FastLogger as FL
import qualified Data.Primitive as PM

-- TODO: Use optparse-applicative to let the user choose
-- between different commands.

main :: IO ()
main = udpStdoutServer

gettysburgClient :: IO ()
gettysburgClient = do
  [port] <- getArgs
  unhandled $ SI.withConnection
    (SI.Peer IPv4.loopback (read port)) 
    throwOnCloseException $ \conn -> unhandled $ SI.send conn $ Bytes
      { array = gettysburgAddress 
      , offset = 0
      , length = PM.sizeofByteArray gettysburgAddress
      }

-- This waits until a single connection is established. It then dumps out
-- anything it receives to stdout. When the remote application shuts down
-- its end, this shuts down its end in return and then terminates.
tcpStdoutServer :: IO ()
tcpStdoutServer = do
  FL.withFastLogger (FL.LogStdout 2048) $ \log -> do
    unhandled $ SI.withListener (SI.Peer IPv4.loopback 0) $ \listener port -> do
      log (FL.toLogStr ("Listening on 127.0.0.1:" <> BC.pack (show port) <> "\n============================\n"))
      unhandled $ SI.withAccepted listener throwOnCloseException $ \conn _ -> do
        let go = SI.receiveOnce conn 512 >>= \case
              Left SI.ReceiveShutdown -> pure ()
              Left e -> throwIO e
              Right (PM.ByteArray arr) -> do
                log (FL.toLogStr (SB.fromShort (SB.SBS arr)))
                go
        go

-- Print every UDP packet that we receive. This terminates, closing the
-- socket, after receiving ten packets.
udpStdoutServer :: IO ()
udpStdoutServer = do
  unhandled $ DIU.withSocket (DIU.Peer IPv4.loopback 0) $ \sock port -> do
    BC.putStrLn ("Receiving datagrams on 127.0.0.1:" <> BC.pack (show port))
    replicateM_ 10 $ do
      DIU.Message remote (ByteArray payload) <- unhandled (DIU.receiveFromIPv4 sock 1024)
      BC.putStrLn ("Datagram from " <> BC.pack (show remote))
      BC.putStr (SB.fromShort (SB.SBS payload))

unhandled :: Exception e => IO (Either e a) -> IO a
unhandled action = action >>= either throwIO pure

throwOnCloseException :: Either SI.CloseException () -> () -> IO ()
throwOnCloseException e () = either throwIO pure e

gettysburgAddress :: PM.ByteArray
gettysburgAddress = PM.ByteArray arr
  where
  !(SB.SBS arr) = SB.toShort $ mconcat
    [ "Four score and seven years ago our fathers brought forth on this "
    , "continent, a new nation, conceived in Liberty, and dedicated to "
    , "the proposition that all men are created equal. Now we are engaged "
    , "in a great civil war, testing whether that nation, or any nation so "
    , "conceived and so dedicated, can long endure. We are met on a great "
    , "battle-field of that war. We have come to dedicate a portion of that "
    , "field, as a final resting place for those who here gave their lives "
    , "that that nation might live. It is altogether fitting and proper that "
    , "we should do this."
    ]
