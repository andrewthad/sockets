{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language DeriveAnyClass #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

import Control.Applicative (liftA3)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently)
import Control.Monad (replicateM_)
import Control.Exception (Exception)
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import Data.Bytes.Types (Bytes(..),MutableBytes(..))
import Data.Primitive (ByteArray)
import Data.Word (Word16,Word8)
import GHC.Exts (RealWorld)
import System.Exit (exitFailure)
import System.IO (stderr,hPutStrLn)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as B
import qualified Data.Primitive as PM
import qualified Data.Primitive.MVar as PM
import qualified GHC.Exts as E
import qualified Net.IPv4 as IPv4
import qualified Socket.Datagram.IPv4.Spoof as DIS
import qualified Socket.Datagram.IPv4.Unconnected as DIU
import qualified Socket.Datagram.Uninterruptible.Bytes as DUB
import qualified Socket.Stream.IPv4 as SI
import qualified Socket.Stream.Uninterruptible.Bytes as UB
import qualified Socket.Stream.Uninterruptible.MutableBytes as UMB
import qualified Socket.Stream.Uninterruptible.ByteString as UBS

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
        , testCase "D" testDatagramUndestinedD
        , testCase "E" testDatagramUndestinedE
        , testCase "F" testDatagramUndestinedF
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
      , testCase "C" testStreamC
      , testCase "D" testStreamD
      , testCase "E" testStreamE
      ]
    ]
  ]

unsliced :: ByteArray -> Bytes
unsliced arr = Bytes arr 0 (PM.sizeofByteArray arr)

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
  received @=? DIU.Message (DIU.Peer IPv4.loopback port) message
  where
  message = E.fromList [0,1,2,3] :: ByteArray
  sz = PM.sizeofByteArray message
  sender :: PM.MVar RealWorld Word16 -> IO Word16
  sender m = unhandled $ DIU.withSocket (DUB.Peer IPv4.loopback 0) $ \sock srcPort -> do
    dstPort <- PM.takeMVar m
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message)
    pure srcPort
  receiver :: PM.MVar RealWorld Word16 -> IO DIU.Message
  receiver m = unhandled $ DIU.withSocket (DIU.Peer IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    unhandled $ DUB.receiveFromIPv4 sock sz

testDatagramUndestinedB :: Assertion
testDatagramUndestinedB = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  (n :: PM.MVar RealWorld ()) <- PM.newEmptyMVar
  ((),received) <- concurrently (sender m n) (receiver m n)
  let expected = (message1,message2)
  expected @=? received
  where
  message1 = E.fromList [0..3] :: ByteArray
  message2 = E.fromList [4..11] :: ByteArray
  sz1 = PM.sizeofByteArray message1
  sz2 = PM.sizeofByteArray message2
  sender :: PM.MVar RealWorld Word16 -> PM.MVar RealWorld () -> IO ()
  sender m n = unhandled $ DIU.withSocket (DIU.Peer IPv4.loopback 0) $ \sock _ -> do
    dstPort <- PM.takeMVar m
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message1)
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message2)
    PM.putMVar n ()
  receiver :: PM.MVar RealWorld Word16 -> PM.MVar RealWorld () -> IO (ByteArray,ByteArray)
  receiver m n = unhandled $ DIU.withSocket (DIU.Peer IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    PM.takeMVar n
    slab <- DUB.newSlab 2 (max sz1 sz2)
    msgs <- unhandled $ DUB.receiveMany sock slab
    let msgCount = PM.sizeofUnliftedArray msgs
    if msgCount == 2
      then pure (PM.indexUnliftedArray msgs 0, PM.indexUnliftedArray msgs 1)
      else fail ("received a number of messages other than 2: " ++ show msgCount)
      
testDatagramUndestinedC :: Assertion
testDatagramUndestinedC = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  (n :: PM.MVar RealWorld ()) <- PM.newEmptyMVar
  (port,received) <- concurrently (sender m n) (receiver m n)
  let expected = 
        ( DIU.Message (DIU.Peer IPv4.loopback port) message1
        , DIU.Message (DIU.Peer IPv4.loopback port) message2
        , DIU.Message (DIU.Peer IPv4.loopback port) message3
        , DIU.Message (DIU.Peer IPv4.loopback port) message4
        )
  expected @=? received
  where
  message1 = E.fromList (enumFromTo 0 9):: ByteArray
  message2 = E.fromList (enumFromTo 10 12) :: ByteArray
  message3 = E.fromList (enumFromTo 13 27) :: ByteArray
  message4 = E.fromList (enumFromTo 28 31) :: ByteArray
  sz1 = PM.sizeofByteArray message1
  sz2 = PM.sizeofByteArray message2
  sz3 = PM.sizeofByteArray message3
  sz4 = PM.sizeofByteArray message4
  sender :: PM.MVar RealWorld Word16 -> PM.MVar RealWorld () -> IO Word16
  sender m n = unhandled $ DIU.withSocket (DIU.Peer IPv4.loopback 0) $ \sock srcPort -> do
    dstPort <- PM.takeMVar m
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message1)
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message2)
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message3)
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message4)
    PM.putMVar n ()
    pure srcPort
  receiver :: PM.MVar RealWorld Word16 -> PM.MVar RealWorld ()
           -> IO (DIU.Message,DIU.Message,DIU.Message,DIU.Message)
  receiver m n = unhandled $ DIU.withSocket (DIU.Peer IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    PM.takeMVar n
    slab <- DUB.newSlabIPv4 3 (max sz1 (max sz2 (max sz3 sz4)))
    msgsX <- unhandled $ DUB.receiveManyFromIPv4 sock slab
    let msgCountX = PM.sizeofSmallArray msgsX
    (msg1,msg2,msg3) <- if PM.sizeofSmallArray msgsX == 3
      then pure (PM.indexSmallArray msgsX 0, PM.indexSmallArray msgsX 1, PM.indexSmallArray msgsX 2)
      else fail $ "received a number of messages other than 3: " ++ show msgCountX
    msgsY <- unhandled $ DUB.receiveManyFromIPv4 sock slab
    let msgCountY = PM.sizeofSmallArray msgsY
    msg4 <- if msgCountY == 1
      then pure (PM.indexSmallArray msgsY 0)
      else fail $ "received a number of messages other than 1: " ++ show msgCountY
    pure (msg1,msg2,msg3,msg4)

testDatagramUndestinedD :: Assertion
testDatagramUndestinedD = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  ((),received) <- concurrently (sender m) (receiver m)
  let expected = (message1,message2,message3)
  expected @=? received
  where
  message1 = E.fromList (enumFromTo 0 15):: ByteArray
  message2 = E.fromList (enumFromTo 16 18) :: ByteArray
  message3 = E.fromList (enumFromTo 19 23) :: ByteArray
  sz1 = PM.sizeofByteArray message1
  sz2 = PM.sizeofByteArray message2
  sz3 = PM.sizeofByteArray message3
  sender :: PM.MVar RealWorld Word16 -> IO ()
  sender m = unhandled $ DIU.withSocket (DUB.Peer IPv4.loopback 0) $ \sock _ -> do
    dstPort <- PM.takeMVar m
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message1)
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message2)
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message3)
  receiver :: PM.MVar RealWorld Word16 -> IO (ByteArray,ByteArray,ByteArray)
  receiver m = unhandled $ DIU.withSocket (DIU.Peer IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    liftA3 (,,)
      (unhandled $ DUB.receive sock sz1)
      (unhandled $ DUB.receive sock sz2)
      (unhandled $ DUB.receive sock sz3)

testDatagramUndestinedE :: Assertion
testDatagramUndestinedE = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  (n :: PM.MVar RealWorld ()) <- PM.newEmptyMVar
  ((),received) <- concurrently (sender m n) (receiver m n)
  let expected = (message1,message2,message3)
  expected @=? received
  where
  message1 = E.fromList (enumFromTo 0 9):: ByteArray
  message2 = E.fromList (enumFromTo 10 14) :: ByteArray
  message3 = E.fromList (enumFromTo 15 21) :: ByteArray
  sz1 = PM.sizeofByteArray message1
  sz2 = PM.sizeofByteArray message2
  sz3 = PM.sizeofByteArray message3
  sender :: PM.MVar RealWorld Word16 -> PM.MVar RealWorld () -> IO ()
  sender !m !n = unhandled $ DIU.withSocket (DUB.Peer IPv4.loopback 0) $ \sock _ -> do
    dstPort <- PM.takeMVar m
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message1)
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message2)
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message3)
    PM.putMVar n ()
  receiver :: PM.MVar RealWorld Word16 -> PM.MVar RealWorld () -> IO (ByteArray,ByteArray,ByteArray)
  receiver m n = unhandled $ DIU.withSocket (DIU.Peer IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    PM.takeMVar n
    slab <- DUB.newSlab 1 (max sz1 (max sz2 sz3))
    msgsX <- unhandled $ DUB.receiveMany sock slab
    when (PM.sizeofUnliftedArray msgsX /= 1) $ fail "more than one message for X"
    msgsY <- unhandled $ DUB.receiveMany sock slab
    when (PM.sizeofUnliftedArray msgsX /= 1) $ fail "more than one message for Y"
    msgsZ <- unhandled $ DUB.receiveMany sock slab
    when (PM.sizeofUnliftedArray msgsX /= 1) $ fail "more than one message for Z"
    pure (PM.indexUnliftedArray msgsX 0,PM.indexUnliftedArray msgsY 0,PM.indexUnliftedArray msgsZ 0)

testDatagramUndestinedF :: Assertion
testDatagramUndestinedF = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  ((),received) <- concurrently (sender m) (receiver m)
  let expected = (Left (DUB.ReceiveTruncated sz))
  expected @=? received
  where
  message = E.fromList (enumFromTo 0 11):: ByteArray
  sz = PM.sizeofByteArray message
  sender !m = unhandled $ DIU.withSocket (DUB.Peer IPv4.loopback 0) $ \sock _ -> do
    dstPort <- PM.takeMVar m
    unhandled $ DUB.sendToIPv4 sock (DIU.Peer IPv4.loopback dstPort) (unsliced message)
  receiver m = unhandled $ DIU.withSocket (DIU.Peer IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    slab <- DUB.newSlab 1 (sz - 1)
    DUB.receiveMany sock slab

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
    unhandled $ SI.withConnection (DIU.Peer IPv4.loopback dstPort) unhandledClose $ \conn -> do
      unhandled $ UB.send conn (unsliced szb)
      unhandled $ UB.send conn (unsliced message)
  receiver :: PM.MVar RealWorld Word16 -> IO ByteArray
  receiver m = unhandled $ SI.withListener (SI.Peer IPv4.loopback 0) $ \listener port -> do
    PM.putMVar m port
    unhandled $ SI.withAccepted listener unhandledClose $ \conn _ -> do
      serializedSize <- unhandled $ UB.receiveExactly conn (PM.sizeOf (undefined :: Int))
      let theSize = PM.indexByteArray serializedSize 0 :: Int
      result <- unhandled $ UB.receiveExactly conn theSize
      pure result

testStreamC :: Assertion
testStreamC = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  ((),received) <- concurrently (sender m) (receiver m)
  received @=? message
  where
  message = B.pack (enumFromTo 0 (100 :: Word8)) :: ByteString
  sender :: PM.MVar RealWorld Word16 -> IO ()
  sender m = do
    dstPort <- PM.takeMVar m
    unhandled $ SI.withConnection (DIU.Peer IPv4.loopback dstPort) unhandledClose $ \conn -> do
      unhandled $ UBS.send conn message
  receiver :: PM.MVar RealWorld Word16 -> IO ByteString
  receiver m = unhandled $ SI.withListener (SI.Peer IPv4.loopback 0) $ \listener port -> do
    PM.putMVar m port
    unhandled $ SI.withAccepted listener unhandledClose $ \conn _ -> do
      unhandled $ UBS.receiveExactly conn (B.length message)

-- This is intended to test creating sockets after other sockets have been
-- closed.
testStreamD :: Assertion
testStreamD = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  ((),()) <- concurrently (server m) (client m)
  pure ()
  where
  message = E.fromList (replicate 50000 (0 :: Word8)) :: ByteArray
  totalConns = 20
  simultaneousClients = 5
  client :: PM.MVar RealWorld Word16 -> IO ()
  client m = do
    dstPort <- PM.takeMVar m
    counter <- PM.newEmptyMVar
    replicateM_ simultaneousClients $ forkIO $ do
      replicateM_ (div totalConns simultaneousClients) $ do
        _ <- unhandled $ SI.withConnection (DIU.Peer IPv4.loopback dstPort) unhandledClose $ \conn -> do
          let go = UB.send conn (unsliced message) >>= \case
                Left SI.SendShutdown -> pure ()
                Left e -> throwIO e
                Right () -> go
          go
        pure ()
      PM.putMVar counter ()
    replicateM_ simultaneousClients $ PM.takeMVar counter
  server :: PM.MVar RealWorld Word16 -> IO ()
  server m = unhandled $ SI.withListener (SI.Peer IPv4.loopback 0) $ \listener port -> do
    PM.putMVar m port
    counter <- PM.newEmptyMVar
    replicateM_ totalConns $ do
      SI.forkAcceptedUnmasked listener
        (\e () -> case e of
          Left SI.ClosePeerContinuedSending -> PM.putMVar counter ()
          Right () -> fail "testStreamD: unexpected behavior"
        )
        (\conn _ -> do
          _ <- unhandled $ UB.receiveExactly conn 1
          pure ()
        )
    replicateM_ totalConns $ PM.takeMVar counter


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
    unhandled $ SI.withConnection (DIU.Peer IPv4.loopback dstPort) unhandledClose $ \conn -> do
      replicateM_ (32 * megabytes) $ unhandled $ UB.send conn (unsliced message)
  receiver :: PM.MVar RealWorld Word16 -> IO ()
  receiver m = unhandled $ SI.withListener (SI.Peer IPv4.loopback 0) $ \listener port -> do
    PM.putMVar m port
    unhandled $ SI.withAccepted listener unhandledClose $ \conn _ -> do
      buffer <- PM.newByteArray chunkSize
      let receiveLoop !remaining
            | remaining > 0 = do
                let recvSize = min remaining chunkSize
                PM.setByteArray buffer 0 chunkSize (0 :: Word8)
                bytesReceived <- unhandled
                  (UMB.receiveOnce conn (MutableBytes buffer 0 recvSize))
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
    (DIU.Peer (IPv4.fromOctets 8 7 6 5) 60000)
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
      (DIU.Peer (IPv4.fromOctets 8 7 6 5) 60000)
      (DIU.Peer IPv4.loopback dstPort)
      marr 0 sz
  receiver :: PM.MVar RealWorld Word16 -> IO DIU.Message
  receiver m = unhandled $ DIU.withSocket (DIU.Peer IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    unhandled $ DUB.receiveFromIPv4 sock 500
      
-- Here, the sender spoofs its ip address and port twice, picking a
-- different port each time.
testDatagramSpoofB :: Assertion
testDatagramSpoofB = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  ((),received) <- concurrently (sender m) (receiver m)
  received @=?
    ( DIU.Message
      (DIU.Peer (IPv4.fromOctets 8 7 6 5) 60000)
      payloadA
    , DIU.Message
      (DIU.Peer (IPv4.fromOctets 9 8 7 6) 59999)
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
      (DIU.Peer (IPv4.fromOctets 8 7 6 5) 60000)
      (DIU.Peer IPv4.loopback dstPort)
      marrA 0 sz
    unhandled $ DIS.sendMutableByteArray sock
      (DIU.Peer (IPv4.fromOctets 9 8 7 6) 59999)
      (DIU.Peer IPv4.loopback dstPort)
      marrB 0 sz
  receiver :: PM.MVar RealWorld Word16 -> IO (DIU.Message,DIU.Message)
  receiver m = unhandled $ DIU.withSocket (DIU.Peer IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    msg1 <- unhandled $ DUB.receiveFromIPv4 sock 500
    msg2 <- unhandled $ DUB.receiveFromIPv4 sock 500
    return (msg1,msg2)

testStreamE :: Assertion
testStreamE = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  ((),received) <- concurrently (sender m) (receiver m)
  received @=? (mconcat [messageA, messageB, messageC, messageD])
  where
  messageA = E.fromList [0..17] :: ByteArray
  messageB = E.fromList [18..92] :: ByteArray
  messageC = E.fromList [93..182] :: ByteArray
  messageD = E.fromList [183..255] :: ByteArray
  sender :: PM.MVar RealWorld Word16 -> IO ()
  sender m = do
    dstPort <- PM.takeMVar m
    unhandled $ SI.withConnection (DIU.Peer IPv4.loopback dstPort) unhandledClose $ \conn -> do
      unhandled $ UB.send conn (unsliced messageA)
      unhandled $ UB.send conn (unsliced messageB)
      unhandled $ UB.send conn (unsliced messageC)
      unhandled $ UB.send conn (unsliced messageD)
  receiver :: PM.MVar RealWorld Word16 -> IO ByteArray
  receiver m = unhandled $ SI.withListener (SI.Peer IPv4.loopback 0) $ \listener port -> do
    PM.putMVar m port
    unhandled $ SI.withAccepted listener unhandledClose $ \conn _ -> do
      marr <- PM.newByteArray 256
      x <- unhandled $ UMB.receiveBetween conn (MutableBytes marr 0 60) 20
      y <- unhandled $ UMB.receiveBetween conn (MutableBytes marr x 150) 100
      unhandled $ UMB.receiveExactly conn (MutableBytes marr (x + y) (256 - (x + y)))
      PM.unsafeFreezeByteArray marr
