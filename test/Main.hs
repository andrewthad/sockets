{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}

import Control.Concurrent (threadWaitRead)
import Control.Concurrent.Async (concurrently)
import Control.Exception (Exception)
import Control.Exception (throwIO)
import Control.Monad.ST (runST)
import Data.Primitive (ByteArray)
import Data.Word (Word16,Word8)
import Foreign.C.Error (Errno,errnoToIOError)
import Foreign.C.Types (CInt,CSize)
import GHC.Exts (RealWorld)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Socket.Datagram.IPv4.Undestined as DIU
import qualified Socket.Stream.IPv4 as SI
import qualified GHC.Exts as E
import qualified Data.Primitive as PM
import qualified Data.Primitive.MVar as PM
import qualified Net.IPv4 as IPv4

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "socket"
  [ testGroup "datagram"
    [ testGroup "ipv4"
      [ testGroup "undestined"
        [ testCase "A" testDatagramUndestinedA
        ]
      ]
    ]
  , testGroup "stream"
    [ testGroup "ipv4"
      [ testCase "A" testStreamA
      ]
    ]
  ]

unhandled :: Exception e => IO (Either e a) -> IO a
unhandled action = action >>= either throwIO pure

testDatagramUndestinedA :: Assertion
testDatagramUndestinedA = do
  (m :: PM.MVar RealWorld Word16) <- PM.newEmptyMVar
  (port,received) <- concurrently (sender m) (receiver m)
  received @=? (DIU.Endpoint IPv4.loopback port, message)
  where
  message = E.fromList [0,1,2,3] :: ByteArray
  sz = PM.sizeofByteArray message
  sender :: PM.MVar RealWorld Word16 -> IO Word16
  sender m = unhandled $ DIU.withSocket (DIU.Endpoint IPv4.loopback 0) $ \sock srcPort -> do
    dstPort <- PM.takeMVar m
    unhandled $ DIU.send sock (DIU.Endpoint IPv4.loopback dstPort) message 0 sz
    pure srcPort
  receiver :: PM.MVar RealWorld Word16 -> IO (DIU.Endpoint,ByteArray)
  receiver m = unhandled $ DIU.withSocket (DIU.Endpoint IPv4.loopback 0) $ \sock port -> do
    PM.putMVar m port
    unhandled $ DIU.receive sock sz

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
    unhandled $ SI.withConnection (DIU.Endpoint IPv4.loopback dstPort) $ \conn -> do
      unhandled $ SI.sendByteArray conn szb
      unhandled $ SI.sendByteArray conn message
  receiver :: PM.MVar RealWorld Word16 -> IO ByteArray
  receiver m = unhandled $ SI.withListener (SI.Endpoint IPv4.loopback 0) $ \listener port -> do
    PM.putMVar m port
    unhandled $ SI.withAccepted listener $ \conn _ -> do
      serializedSize <- unhandled $ SI.receiveByteArray conn (PM.sizeOf (undefined :: Int))
      let theSize = PM.indexByteArray serializedSize 0 :: Int
      result <- unhandled $ SI.receiveByteArray conn theSize
      pure result


