{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language DeriveAnyClass #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

import Control.Concurrent.Async (concurrently)
import Control.Monad (replicateM_)
import Control.Exception (Exception)
import Control.Exception (throwIO)
import Control.Monad.ST (runST)
import Data.Bool (bool)
import Data.Primitive (ByteArray)
import Data.Word (Word16,Word8)
import GHC.Exts (RealWorld)
import System.Exit (exitFailure)
import System.IO (stderr,hPutStrLn)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Primitive as PM
import qualified Data.Primitive.MVar as PM
import qualified GHC.Exts as E
import qualified Net.IPv4 as IPv4
import qualified Socket.Datagram.IPv4.Spoof as DIS
import qualified Socket.Datagram.IPv4.Undestined as DIU
import qualified Socket.Stream.IPv4 as SI

main :: IO ()
main = do
  canSpoof <- DIS.withSocket (const (pure ())) >>= \case
    Right () -> pure True
    Left e -> case e of
      DIS.SocketPermissionDenied -> pure False
      DIS.SocketFileDescriptorLimit -> do
        hPutStrLn stderr "All ephemeral ports are in use. Terminating."
        exitFailure
  defaultMain (tests canSpoof)

tests :: Bool -> TestTree
tests canSpoof = testGroup "socket"
  [ testGroup "datagram"
    [ testGroup "ipv4"
      [ testGroup "undestined"
        [ testCase "A" testDatagramUndestinedA
        , testCase "B" testDatagramUndestinedB
        , testCase "C" testDatagramUndestinedC
        ]
      , testGroup "spoof" $ if canSpoof
          then
            [ testCase "A" testDatagramSpoofA
            , testCase "B" testDatagramSpoofB
            ]
          else []
      ]
    ]
  , testGroup "stream"
    [ testGroup "ipv4"
      [ testCase "A" testStreamA
      , testGroup "B"
        [ testCase "1MB" (testStreamB 1)
        , testCase "4MB" (testStreamB 4)
        , testCase "32MB" (testStreamB 32)
        ]
      ]
    ]
  ]

unhandled :: Exception e => IO (Either e a) -> IO a
unhandled action = action >>= either throwIO pure

unhandledClose :: Either SI.CloseException () -> a -> IO a
unhandledClose m a = case m of
  Right () -> pure a
  Left e -> throwIO e

data MagicByteMismatch = MagicByteMismatch
  deriving stock (Show,Eq)
  deriving anyclass (Exception)

data NegativeByteCount = NegativeByteCount
  deriving stock (Show,Eq)
  deriving anyclass (Exception)

testDatagramUndestinedA :: Assertion
testDatagramUndestinedA = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  (port,received) <- concurrently (sender m) (receiver m)
  received @=? DIU.Message (DIU.Endpoint IPv4.loopback port) message
  where
  message = E.fromList [0,1,2,3] :: ByteArray
  sz = PM.sizeofByteArray message
  sender :: PM.MVar RealWorld Word16 -> IO Word16
  sender m = unhandled $ DIU.withSocket (DIU.Endpoint IPv4.loopback 0) $ \sock srcPort -> do
    dstPort <- PM.takeMVar m
    unhandled $ DIU.send sock (DIU.Endpoint IPv4.loopback dstPort) message 0 sz
    pure srcPort
  receiver :: PM.MVar RealWorld Word16 -> IO DIU.Message
  receiver m = unhandled $ DIU.withSocket (DIU.Endpoint IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    unhandled $ DIU.receiveByteArray sock sz

testDatagramUndestinedB :: Assertion
testDatagramUndestinedB = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  (n :: PM.MVar RealWorld ()) <- PM.newEmptyMVar
  (port,received) <- concurrently (sender m n) (receiver m n)
  received @=?
    ( DIU.Message (DIU.Endpoint IPv4.loopback port) message1
    , DIU.Message (DIU.Endpoint IPv4.loopback port) message2
    )
  where
  message1 = E.fromList [0,1,2,3] :: ByteArray
  message2 = E.fromList [4,5,6,8,9,10] :: ByteArray
  sz1 = PM.sizeofByteArray message1
  sz2 = PM.sizeofByteArray message2
  sender :: PM.MVar RealWorld Word16 -> PM.MVar RealWorld () -> IO Word16
  sender m n = unhandled $ DIU.withSocket (DIU.Endpoint IPv4.loopback 0) $ \sock srcPort -> do
    dstPort <- PM.takeMVar m
    unhandled $ DIU.send sock (DIU.Endpoint IPv4.loopback dstPort) message1 0 sz1
    unhandled $ DIU.send sock (DIU.Endpoint IPv4.loopback dstPort) message2 0 sz2
    PM.putMVar n ()
    pure srcPort
  receiver :: PM.MVar RealWorld Word16 -> PM.MVar RealWorld () -> IO (DIU.Message,DIU.Message)
  receiver m n = unhandled $ DIU.withSocket (DIU.Endpoint IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    PM.takeMVar n
    msgs <- unhandled $ DIU.receiveMany sock 3 (max sz1 sz2)
    if PM.sizeofArray msgs == 2
      then pure (PM.indexArray msgs 0, PM.indexArray msgs 1)
      else fail "received a number of messages other than 2"
      
testDatagramUndestinedC :: Assertion
testDatagramUndestinedC = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  (n :: PM.MVar RealWorld ()) <- PM.newEmptyMVar
  (port,received) <- concurrently (sender m n) (receiver m n)
  received @=?
    ( DIU.Message (DIU.Endpoint IPv4.loopback port) message1
    , DIU.Message (DIU.Endpoint IPv4.loopback port) message2
    , DIU.Message (DIU.Endpoint IPv4.loopback port) message3
    )
  where
  message1 = E.fromList (enumFromTo 0 9):: ByteArray
  message2 = E.fromList (enumFromTo 10 10) :: ByteArray
  message3 = E.fromList (enumFromTo 11 25) :: ByteArray
  sz1 = PM.sizeofByteArray message1
  sz2 = PM.sizeofByteArray message2
  sz3 = PM.sizeofByteArray message3
  sender :: PM.MVar RealWorld Word16 -> PM.MVar RealWorld () -> IO Word16
  sender m n = unhandled $ DIU.withSocket (DIU.Endpoint IPv4.loopback 0) $ \sock srcPort -> do
    dstPort <- PM.takeMVar m
    unhandled $ DIU.send sock (DIU.Endpoint IPv4.loopback dstPort) message1 0 sz1
    unhandled $ DIU.send sock (DIU.Endpoint IPv4.loopback dstPort) message2 0 sz2
    unhandled $ DIU.send sock (DIU.Endpoint IPv4.loopback dstPort) message3 0 sz3
    PM.putMVar n ()
    pure srcPort
  receiver :: PM.MVar RealWorld Word16 -> PM.MVar RealWorld () -> IO (DIU.Message,DIU.Message,DIU.Message)
  receiver m n = unhandled $ DIU.withSocket (DIU.Endpoint IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    PM.takeMVar n
    msgsX <- unhandled $ DIU.receiveMany sock 2 (max sz1 sz2)
    (msg1,msg2) <- if PM.sizeofArray msgsX == 2
      then pure (PM.indexArray msgsX 0, PM.indexArray msgsX 1)
      else fail "received a number of messages other than 2"
    msgsY <- unhandled $ DIU.receiveMany sock 2 sz3
    msg3 <- if PM.sizeofArray msgsY == 1
      then pure (PM.indexArray msgsY 0)
      else fail "received a number of messages other than 2"
    pure (msg1,msg2,msg3)
      

-- This test involves a made up protocol that goes like this:
-- The sender always starts by sending the length of the rest
-- of the payload as a native-endian encoded machine-sized int.
-- (This could only ever work for a machine that is communicating
-- with itself). Then, it sends a bytearray of that specified
-- length. Then, both ends are expected to shutdown their sides
-- of the connection.
testStreamA :: Assertion
testStreamA = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  ((),received) <- concurrently (sender m) (receiver m)
  received @=? message
  where
  message = E.fromList (enumFromTo 0 (100 :: Word8)) :: ByteArray
  sz = PM.sizeofByteArray message
  szb = runST $ do
    marr <- PM.newByteArray (PM.sizeOf (undefined :: Int))
    PM.writeByteArray marr 0 sz
    PM.unsafeFreezeByteArray marr
  sender :: PM.MVar RealWorld Word16 -> IO ()
  sender m = do
    dstPort <- PM.takeMVar m
    unhandled $ SI.withConnection (DIU.Endpoint IPv4.loopback dstPort) unhandledClose $ \conn -> do
      unhandled $ SI.sendByteArray conn szb
      unhandled $ SI.sendByteArray conn message
  receiver :: PM.MVar RealWorld Word16 -> IO ByteArray
  receiver m = unhandled $ SI.withListener (SI.Endpoint IPv4.loopback 0) $ \listener port -> do
    PM.putMVar m port
    unhandled $ SI.withAccepted listener unhandledClose $ \conn _ -> do
      serializedSize <- unhandled $ SI.receiveByteArray conn (PM.sizeOf (undefined :: Int))
      let theSize = PM.indexByteArray serializedSize 0 :: Int
      result <- unhandled $ SI.receiveByteArray conn theSize
      pure result

-- The sender sends a large amount of traffic that may exceed
-- the size of the operating system's TCP send buffer. The 
-- amount is configurable because the test suite wants to
-- check this for several values.
testStreamB :: Int -> Assertion
testStreamB megabytes = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  ((),()) <- concurrently (sender m) (receiver m)
  pure ()
  where
  message = E.fromList (replicate (32 * 1024) magicByte) :: ByteArray
  chunkSize = PM.sizeofByteArray message
  sender :: PM.MVar RealWorld Word16 -> IO ()
  sender m = do
    dstPort <- PM.takeMVar m
    unhandled $ SI.withConnection (DIU.Endpoint IPv4.loopback dstPort) unhandledClose $ \conn -> do
      replicateM_ (32 * megabytes) $ unhandled $ SI.sendByteArray conn message
  receiver :: PM.MVar RealWorld Word16 -> IO ()
  receiver m = unhandled $ SI.withListener (SI.Endpoint IPv4.loopback 0) $ \listener port -> do
    PM.putMVar m port
    unhandled $ SI.withAccepted listener unhandledClose $ \conn _ -> do
      buffer <- PM.newByteArray chunkSize
      let receiveLoop !remaining
            | remaining > 0 = do
                let recvSize = min remaining chunkSize
                PM.setByteArray buffer 0 chunkSize (0 :: Word8)
                bytesReceived <- unhandled (SI.receiveBoundedMutableByteArraySlice conn recvSize buffer 0)
                verifyClientSendBytes buffer bytesReceived >>= \case
                  True -> receiveLoop (remaining - bytesReceived)
                  False -> throwIO MagicByteMismatch
            | remaining == 0 = pure ()
            | otherwise = throwIO NegativeByteCount
      receiveLoop (32 * megabytes * chunkSize)
      pure ()

magicByte :: Word8
magicByte = 0xFA

verifyClientSendBytes :: PM.MutableByteArray RealWorld -> Int -> IO Bool
verifyClientSendBytes arr len = go (len - 1)
  where
  go !ix = if ix >= 0
    then do
      w <- PM.readByteArray arr ix
      if w == magicByte then go (ix - 1) else pure False
    else pure True

-- Here, the sender spoofs its ip address and port.
testDatagramSpoofA :: Assertion
testDatagramSpoofA = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  ((),received) <- concurrently (sender m) (receiver m)
  received @=? DIU.Message
    (DIU.Endpoint (IPv4.fromOctets 8 7 6 5) 60000)
    payload
  where
  sz = 16
  payload = E.fromList (enumFromTo (0 :: Word8) (fromIntegral sz - 1))
  sender :: PM.MVar RealWorld Word16 -> IO ()
  sender m = unhandled $ DIS.withSocket $ \sock -> do
    dstPort <- PM.takeMVar m
    marr <- PM.newByteArray sz
    PM.copyByteArray marr 0 payload 0 sz
    unhandled $ DIS.sendMutableByteArray sock
      (DIU.Endpoint (IPv4.fromOctets 8 7 6 5) 60000)
      (DIU.Endpoint IPv4.loopback dstPort)
      marr 0 sz
  receiver :: PM.MVar RealWorld Word16 -> IO DIU.Message
  receiver m = unhandled $ DIU.withSocket (DIU.Endpoint IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    unhandled $ DIU.receiveByteArray sock 500
      
-- Here, the sender spoofs its ip address and port twice, picking a
-- different port each time.
testDatagramSpoofB :: Assertion
testDatagramSpoofB = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  ((),received) <- concurrently (sender m) (receiver m)
  received @=?
    ( DIU.Message
      (DIU.Endpoint (IPv4.fromOctets 8 7 6 5) 60000)
      payloadA
    , DIU.Message
      (DIU.Endpoint (IPv4.fromOctets 9 8 7 6) 59999)
      payloadB
    )
  where
  sz = 16
  payloadA = E.fromList (enumFromTo (1 :: Word8) (fromIntegral sz))
  payloadB = E.fromList (enumFromTo (2 :: Word8) (fromIntegral sz + 1))
  sender :: PM.MVar RealWorld Word16 -> IO ()
  sender m = unhandled $ DIS.withSocket $ \sock -> do
    dstPort <- PM.takeMVar m
    marrA <- PM.newByteArray sz
    marrB <- PM.newByteArray sz
    PM.copyByteArray marrA 0 payloadA 0 sz
    PM.copyByteArray marrB 0 payloadB 0 sz
    unhandled $ DIS.sendMutableByteArray sock
      (DIU.Endpoint (IPv4.fromOctets 8 7 6 5) 60000)
      (DIU.Endpoint IPv4.loopback dstPort)
      marrA 0 sz
    unhandled $ DIS.sendMutableByteArray sock
      (DIU.Endpoint (IPv4.fromOctets 9 8 7 6) 59999)
      (DIU.Endpoint IPv4.loopback dstPort)
      marrB 0 sz
  receiver :: PM.MVar RealWorld Word16 -> IO (DIU.Message,DIU.Message)
  receiver m = unhandled $ DIU.withSocket (DIU.Endpoint IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    msg1 <- unhandled $ DIU.receiveByteArray sock 500
    msg2 <- unhandled $ DIU.receiveByteArray sock 500
    return (msg1,msg2)
