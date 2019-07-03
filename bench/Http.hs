{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Data.Bytes.Types (Bytes(..),MutableBytes(..))
import Data.Char (ord)
import Data.Primitive
import Data.Word
import GHC.Exts (RealWorld)
import Socket.Stream.IPv4 (Connection)
import System.Environment
import System.IO (stderr)
import Text.Read (readMaybe)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Primitive as PM
import qualified GHC.Exts as E
import qualified Net.IPv4 as IPv4
import qualified Socket.Stream.IPv4 as S
import qualified Socket.Stream.Uninterruptible.MutableBytes as Mutable
import qualified Socket.Stream.Uninterruptible.Bytes as Immutable

main :: IO ()
main = do
  portStr <- lookupEnv "PORT"
  let port = maybe 8888 id (readMaybe =<< portStr)
  err <- S.withListener (S.Endpoint{S.address=IPv4.any,S.port}) $ \lstn _ -> do
    let go = do
          e <- S.forkAcceptedUnmasked lstn
            (\e () -> either handleCloseException pure e)
            (\conn _ -> do
              buf <- PM.newByteArray (2048 - 16)
              echoStage1 conn buf
            )
          case e of
            Left err -> pure err
            Right _ -> go
    go
  either throwIO throwIO err

echoStage1 :: Connection -> MutableByteArray RealWorld -> IO ()
echoStage1 !conn !buffer = do
  Mutable.receiveBetween conn (MutableBytes buffer 0 bufSize) 4 >>= \case
    Left err -> case err of
      -- A shutdown right here is expected for http clients that do not
      -- reuse connections.
      S.ReceiveShutdown -> pure ()
      -- For some reason, wrk resets the connections when it finishes
      -- instead of shutting them down gracefully.
      S.ReceiveReset -> pure ()
      -- _ -> BC.hPutStrLn stderr (BC.pack (show err))
    Right n -> echoStage2 conn buffer n

-- precondition: total >= 4
echoStage2 :: Connection -> MutableByteArray RealWorld -> Int -> IO ()
echoStage2 !sock !buffer !total = do
  (w4 :: Word8) <- PM.readByteArray buffer (total - 1)
  (w3 :: Word8) <- PM.readByteArray buffer (total - 2)
  (w2 :: Word8) <- PM.readByteArray buffer (total - 3)
  (w1 :: Word8) <- PM.readByteArray buffer (total - 4)
  if w1 == 13 && w2 == 10 && w3 == 13 && w4 == 10
    then Immutable.send sock (unsliced httpResponse) >>= \case
      Left err -> BC.hPutStrLn stderr (BC.pack (show err))
      Right _ -> echoStage1 sock buffer
    else do
      let remaining = bufSize - total
      if remaining > 0
        then Mutable.receiveOnce sock (MutableBytes buffer total remaining) >>= \case
          Left err -> BC.hPutStrLn stderr (BC.pack (show err))
          Right n -> echoStage2 sock buffer (total + n)
        else BC.hPutStrLn stderr "Buffer exhausted"

unsliced :: ByteArray -> Bytes
unsliced arr = Bytes arr 0 (PM.sizeofByteArray arr)

httpResponse :: ByteArray
httpResponse = E.fromList $ map (fromIntegral . ord)
  "HTTP/1.1 200 OK\r\n\
  \Content-Type: text/html; charset=UTF-8\r\n\
  \Content-Length: 500\r\n\
  \Connection: Keep-Alive\r\n\
  \\r\n"
  ++
  replicate 500 (48 :: Word8)

bufSize :: Int
bufSize = 2048 - 16

handleCloseException :: S.CloseException -> IO ()
handleCloseException S.ClosePeerContinuedSending =
  BC.hPutStrLn stderr "Exception: Peer continued sending\n"
