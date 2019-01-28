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
receiveMany = receiveManyShim

-- Although this is a shim for recvmmsg, it is still better than calling
-- receive repeatedly since it avoids unneeded calls to the event
-- manager. This is guaranteed to return at least one message.
--
-- This function is currently unused. It is being left here so that,
-- when cross-platform compatibility is someday handled, this will
-- be available for platforms that do not provide recvmmsg.
receiveManyShim :: Socket -> Int -> Int -> IO (Either SocketException (Array Message))
receiveManyShim (Socket !fd) !maxDatagrams !maxSz = do
  debug "receiveMany: about to wait"
  threadWaitRead fd
  debug "receiveMany: socket is now readable"
  msgs <- PM.newArray maxDatagrams errorThunk
  -- We use MSG_TRUNC so that we are able to figure out whether
  -- or not bytes were discarded. If bytes were discarded
  -- (meaning that the buffer was too small), we return an
  -- exception.
  let go !ix = if ix < maxDatagrams
        then do
          -- This does not need to allocate pinned memory for
          -- the call to recvfrom to work correctly. It allocates
          -- pinned memory so that its behavior is consistent with
          -- that of receiveManyNative.
          marr <- PM.newPinnedByteArray maxSz
          e <- S.uninterruptibleReceiveFromMutableByteArray fd marr 0
            (intToCSize maxSz) (L.truncate) S.sizeofSocketAddressInternet
          case e of
            Left err -> if err == eWOULDBLOCK || err == eAGAIN
              then do
                r <- PM.freezeArray msgs 0 ix
                pure (Right r)
              else pure (Left (errorCode err))
            Right (sockAddrRequiredSz,sockAddr,recvSz) -> if csizeToInt recvSz <= maxSz
              then if sockAddrRequiredSz == S.sizeofSocketAddressInternet
                then case S.decodeSocketAddressInternet sockAddr of
                  Just sockAddrInet -> do
                    shrinkMutableByteArray marr (csizeToInt recvSz)
                    arr <- PM.unsafeFreezeByteArray marr
                    let !msg = Message (socketAddressInternetToEndpoint sockAddrInet) arr
                    PM.writeArray msgs ix msg
                    go (ix + 1)
                  Nothing -> pure (Left (SocketAddressFamily (-1)))
                else pure (Left SocketAddressSize)
              else pure (Left (ReceivedMessageTruncated (csizeToInt recvSz)))
        else do
          r <- PM.unsafeFreezeArray msgs
          pure (Right r)
  go 0

-- Used internally in arrays
errorThunk :: a
errorThunk = error "Socket.Datagram.IPv4.Undestined: uninitialized element"

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

socketAddressInternetToEndpoint :: S.SocketAddressInternet -> Endpoint
socketAddressInternetToEndpoint (S.SocketAddressInternet {address,port}) = Endpoint
  { address = IPv4 (S.networkToHostLong address)
  , port = S.networkToHostShort port
  }

shrinkMutableByteArray :: MutableByteArray RealWorld -> Int -> IO ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  PM.primitive_ (shrinkMutableByteArray# arr sz)

intToCSize :: Int -> CSize
intToCSize = fromIntegral

errorCode :: Errno -> SocketException
errorCode (Errno x) = ErrorCode x

