{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Datagram.Receive.Many.Indefinite
  ( receiveMany
  ) where

import Control.Monad.Primitive (primitive)
import Data.Bytes.Types (MutableBytes(..))
import Data.Primitive (MutablePrimArray)
import Data.Primitive (PrimArray,UnliftedArray,MutableByteArray)
import Data.Primitive (MutableUnliftedArray)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import GHC.Exts (RealWorld,Int(I#))
import Socket.Debug (debug,whenDebugging)
import Socket.Datagram (ReceiveException)
import Socket.Datagram.Instantiate (receiveLoop,receiveAttempt)
import Socket.Interrupt (Intr,Interrupt)
import Socket.Interrupt (wait,tokenToDatagramReceiveException)
import System.Posix.Types (Fd)

import qualified Datagram.Receive as Receive
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Socket.EventManager as EM

-- Invariant: Buffer lengths are all equal, and their size is
-- greater than zero.
-- Implementation note: We only check for the interrupt on the
-- first datagram reception. After that, we keep going (without
-- checking for the interrupt) until we hit EAGAIN or until we
-- have received the maximum number of datagrams that the slab
-- can hold.
receiveMany :: 
     Interrupt
  -> Fd -- ^ Socket
  -> MutablePrimArray RealWorld CInt
  -> Receive.AddressBuffer
  -> MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- ^ Buffers
  -> IO (Either (ReceiveException Intr) Int)
receiveMany !intr !sock !lens !addrs !bufs = do
  let !mngr = EM.manager
      !lenBufs = PM.sizeofMutableUnliftedArray bufs
  tv <- EM.reader mngr sock
  token0 <- wait intr tv
  case tokenToDatagramReceiveException token0 of
    Left err -> pure (Left err)
    Right _ -> do
      buf0 <- readMutableByteArrayArray bufs 0
      sz0 <- PM.getSizeofMutableByteArray buf0
      whenDebugging $ do
        (byte0 :: Word8) <- if sz0 > 0
          then PM.readByteArray buf0 0
          else pure 0
        debug $ "receiveMany: datagram 0 pre-run first byte: " ++ show byte0
      receiveLoop intr tv token0 sock (MutableBytes buf0 0 sz0) (Receive.offsetAddress addrs 0) >>= \case
        Left err -> pure (Left err)
        Right recvSz0 -> do
          -- What a shame that this must be converted from Int to CInt
          -- after just having been converted the other direction in
          -- receiveLoop. Oh well.
          PM.writePrimArray lens 0 (intToCInt recvSz0)
          whenDebugging $ do
            (byte0 :: Word8) <- if recvSz0 > 0
              then PM.readByteArray buf0 0
              else pure 0
            debug $ "receiveMany: datagram 0 received " ++ show recvSz0
                 ++ " bytes into " ++ show sz0 ++ "-byte buffer (first byte: "
                 ++ show byte0 ++ ")"
          let go !ix = if ix < lenBufs
                then do
                  buf <- readMutableByteArrayArray bufs ix
                  sz <- PM.getSizeofMutableByteArray buf
                  whenDebugging $ do
                    (byte0 :: Word8) <- if sz > 0
                      then PM.readByteArray buf 0
                      else pure 0
                    let addr0 = PM.mutableByteArrayContents buf
                    len0 <- PM.readPrimArray lens ix
                    debug $ "receiveMany: datagram " ++ show ix ++ " pre-run [first_byte="
                         ++ show byte0 ++ "][length=" ++ show len0 ++ "][buf_addr="
                         ++ show addr0 ++ "]"
                  receiveAttempt sock (MutableBytes buf 0 sz) (Receive.offsetAddress addrs ix) >>= \case
                    Left err -> pure (Left err)
                    Right m -> case m of
                      Nothing -> pure (Right ix)
                      Just recvSz -> do
                        PM.writePrimArray lens ix (intToCInt recvSz)
                        whenDebugging $ do
                          (byte0 :: Word8) <- if recvSz > 0
                            then PM.readByteArray buf 0
                            else pure 0
                          debug $ "receiveMany: datagram " ++ show ix ++ " received "
                               ++ show recvSz ++ " bytes into " ++ show sz
                               ++ "-byte buffer [first_byte=" ++ show byte0 ++ "]"
                        go (ix + 1)
                else pure (Right ix)
          go 1

intToCInt :: Int -> CInt
{-# inline intToCInt #-}
intToCInt = fromIntegral

readMutableByteArrayArray
  :: MutableUnliftedArray RealWorld (MutableByteArray RealWorld) -- ^ source
  -> Int -- ^ index
  -> IO (MutableByteArray RealWorld)
readMutableByteArrayArray (PM.MutableUnliftedArray maa#) (I# i#)
  = primitive $ \s -> case Exts.readMutableByteArrayArray# maa# i# s of
      (# s', aa# #) -> (# s', PM.MutableByteArray aa# #)
