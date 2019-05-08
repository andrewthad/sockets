{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Datagram.Receive.Many.Indefinite
  ( receiveMany
  ) where

import Data.Bytes.Types (MutableBytes(..))
import Data.Primitive (MutablePrimArray)
import Data.Primitive (PrimArray,UnliftedArray,MutableByteArray)
import Data.Primitive (MutableUnliftedArray)
import Data.Word (Word8)
import Datagram.Receive.Many.EndpointArray (MutableEndpointArray)
import Datagram.DecodeAddress (receptionPeer,receptionSize)
import Foreign.C.Types (CInt)
import GHC.Exts (RealWorld)
import Socket.Debug (debug,whenDebugging)
import Socket.Datagram (ReceiveException)
import Socket.Datagram.Instantiate (receiveLoop,receiveAttempt)
import Socket.Interrupt (Intr,Interrupt)
import Socket.Interrupt (wait,tokenToDatagramReceiveException)
import System.Posix.Types (Fd)

import qualified Datagram.Receive.Many.EndpointArray as A
import qualified Socket.EventManager as EM
import qualified Data.Primitive as PM

receiveMany :: 
     Interrupt
  -> Fd -- ^ Socket
  -> MutablePrimArray RealWorld CInt
  -> MutableEndpointArray
  -> MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- ^ Buffers
  -> IO (Either (ReceiveException Intr) Int)
receiveMany !intr !sock !lens !ends !bufs = do
  let !mngr = EM.manager
      !lenBufs = PM.sizeofMutableUnliftedArray bufs
  if lenBufs > 0
    then do
      tv <- EM.reader mngr sock
      token0 <- wait intr tv
      case tokenToDatagramReceiveException token0 of
        Left err -> pure (Left err)
        Right _ -> do
          buf0 <- PM.readUnliftedArray bufs 0
          sz0 <- PM.getSizeofMutableByteArray buf0
          whenDebugging $ do
            (byte0 :: Word8) <- if sz0 > 0
              then PM.readByteArray buf0 0
              else pure 0
            debug $ "receiveMany: datagram 0 pre-run first byte: " ++ show byte0
          receiveLoop intr tv token0 sock (MutableBytes buf0 0 sz0) >>= \case
            Left err -> pure (Left err)
            Right r0 -> do
              -- What a shame that this must be converted from Int to CInt
              -- after just having been converted the other direction in
              -- receiveLoop. Oh well.
              let peer0 = receptionPeer r0
                  recvSz0 = receptionSize r0
              PM.writePrimArray lens 0 (intToCInt recvSz0)
              A.write ends 0 peer0
              whenDebugging $ do
                (byte0 :: Word8) <- if recvSz0 > 0
                  then PM.readByteArray buf0 0
                  else pure 0
                debug $ "receiveMany: datagram 0 received " ++ show recvSz0
                     ++ " bytes into " ++ show sz0 ++ "-byte buffer (first byte: "
                     ++ show byte0 ++ ")"
              let go !ix = if ix < lenBufs
                    then do
                      buf <- PM.readUnliftedArray bufs ix
                      sz <- PM.getSizeofMutableByteArray buf
                      receiveAttempt sock (MutableBytes buf 0 sz) >>= \case
                        Left err -> pure (Left err)
                        Right Nothing -> pure (Right ix)
                        Right (Just r) -> do
                          let peer = receptionPeer r
                              recvSz = receptionSize r
                          PM.writePrimArray lens ix (intToCInt recvSz)
                          A.write ends ix peer
                          whenDebugging $ do
                            (byte0 :: Word8) <- if recvSz > 0
                              then PM.readByteArray buf 0
                              else pure 0
                            debug $ "receiveMany: datagram " ++ show ix ++ " received "
                                 ++ show recvSz ++ " bytes into " ++ show sz
                                 ++ "-byte buffer (first byte: " ++ show byte0 ++ ")"
                          go (ix + 1)
                    else pure (Right ix)
              go 1
    else do
      -- TODO: If the interrupt has fired, do we still want
      -- to succeed like this? Probably not. We will need to
      -- add a function to Socket.Interrupt for checking the
      -- interrupt without waiting.
      pure (Right 0)

intToCInt :: Int -> CInt
{-# inline intToCInt #-}
intToCInt = fromIntegral
