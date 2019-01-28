{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language UnboxedTuples #-}
module Socket.Datagram.IPv4.Undestined.Multiple
  ( receiveMany
  ) where

import Control.Concurrent (threadWaitWrite,threadWaitRead)
import Control.Exception (mask,onException)
import Data.Primitive (ByteArray,MutableByteArray(..),Array)
import Data.Word (Word16)
import Foreign.C.Error (Errno(..),eWOULDBLOCK,eAGAIN)
import Foreign.C.Types (CInt,CSize,CUInt)
import GHC.Exts (Int(I#),RealWorld,shrinkMutableByteArray#,ByteArray#,touch#)
import GHC.IO (IO(..))
import Net.Types (IPv4(..))
import Socket (SocketException(..))
import Socket.Datagram.IPv4.Undestined.Internal (Message(..),Socket(..))
import Socket.Debug (debug)
import Socket.IPv4 (Endpoint(..))
import System.Posix.Types (Fd)

import qualified Control.Monad.Primitive as PM
import qualified Data.Primitive as PM
import qualified Linux.Socket as L
import qualified Posix.Socket as S

-- | Receive up to the specified number of datagrams into freshly allocated
--   byte arrays. When there are many datagrams present on the receive
--   buffer, this is more efficient than calling 'receive' repeatedly. The
--   array is guaranteed to have at least one message.
--
--   The byte arrays in the resulting messages are always pinned.
receiveMany ::
     Socket -- ^ Socket
  -> Int -- ^ Maximum number of datagrams to receive
  -> Int -- ^ Maximum size of each datagram to receive
  -> IO (Either SocketException (Array Message))
receiveMany = receiveManyNative

receiveManyNative :: Socket -> Int -> Int -> IO (Either SocketException (Array Message))
receiveManyNative (Socket !fd) !maxDatagrams !maxSz = do
  threadWaitRead fd
  L.uninterruptibleReceiveMultipleMessageB fd S.sizeofSocketAddressInternet (intToCSize maxSz) (intToCUInt maxDatagrams) L.truncate >>= \case
    Left err -> pure (Left (errorCode err))
    Right (saneSockAddrs,sockAddrs,greatestMsgSz,msgs) -> if saneSockAddrs == 0
      then if cuintToInt greatestMsgSz > maxSz
        then pure (Left (ReceivedMessageTruncated (cuintToInt greatestMsgSz)))
        else do
          let len = PM.sizeofUnliftedArray msgs
          let sockaddrBase = PM.byteArrayContents sockAddrs
          finalMsgs <- PM.newArray len errorThunk
          let go !ix = if ix >= 0
                then S.indexSocketAddressInternet sockaddrBase ix >>= \case
                  Left fam -> do
                    touchByteArray sockAddrs
                    pure (Left (SocketAddressFamily fam))
                  Right sockAddrInet -> do
                    touchByteArray sockAddrs
                    let !msg = Message
                          (socketAddressInternetToEndpoint sockAddrInet)
                          (PM.indexUnliftedArray msgs ix)
                    PM.writeArray finalMsgs ix msg
                    go (ix - 1)
                else do
                  touchByteArray sockAddrs
                  fmap Right (PM.unsafeFreezeArray finalMsgs)
          go (len - 1)
      else pure (Left SocketAddressSize)

-- Used internally in arrays
errorThunk :: a
errorThunk = error "Socket.Datagram.IPv4.Undestined: uninitialized element"

touchByteArray :: ByteArray -> IO ()
touchByteArray (PM.ByteArray x) = touchByteArray# x

touchByteArray# :: ByteArray# -> IO ()
touchByteArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)

socketAddressInternetToEndpoint :: S.SocketAddressInternet -> Endpoint
socketAddressInternetToEndpoint (S.SocketAddressInternet {address,port}) = Endpoint
  { address = IPv4 (S.networkToHostLong address)
  , port = S.networkToHostShort port
  }

cuintToInt :: CUInt -> Int
cuintToInt = fromIntegral

errorCode :: Errno -> SocketException
errorCode (Errno x) = ErrorCode x

intToCUInt :: Int -> CUInt
intToCUInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

