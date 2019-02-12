{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language UnboxedTuples #-}

-- | Internet datagram sockets without a fixed destination.
-- The user may spoof the source address and may specify the
-- packet ID. An application must have @CAP_NET_RAW@ or be
-- running as root to use the functions in this module.
module Socket.Datagram.IPv4.Spoof
  ( -- * Types
    Socket(..)
  , Endpoint(..)
  , Message(..)
    -- * Establish
  , withSocket
    -- * Communicate
  , sendMutableByteArray
    -- * Exceptions
  , SocketException(..)
    -- * Examples
    -- $examples
  ) where

import Control.Concurrent (threadWaitWrite,threadWaitRead)
import Control.Exception (mask,onException)
import Data.Bits (unsafeShiftL,unsafeShiftR,complement,(.&.))
import Data.Monoid (Endo(..))
import Data.Primitive (ByteArray,MutableByteArray(..))
import Data.Word (Word16,Word8,Word64,Word32)
import Foreign.C.Error (Errno(..),eWOULDBLOCK,eAGAIN)
import Foreign.C.Types (CInt,CSize)
import GHC.Exts (Int(I#),RealWorld,shrinkMutableByteArray#,ByteArray#,touch#)
import GHC.IO (IO(..))
import Net.Types (IPv4(..))
import Socket (SocketException(..))
import Socket.Datagram.IPv4.Undestined.Internal (Message(..))
import Socket.Debug (debug,whenDebugging)
import Socket.IPv4 (Endpoint(..))
import System.Posix.Types (Fd)
import Numeric (showHex)
import Text.Printf (printf)

import qualified Control.Monad.Primitive as PM
import qualified Data.Primitive as PM
import qualified Linux.Socket as L
import qualified Posix.Socket as S
import qualified GHC.Exts as E

-- | A socket that send datagrams with spoofed source IP addresses.
-- It cannot receive datagrams.
newtype Socket = Socket Fd
  deriving stock (Eq,Ord,Show)

-- | Open a socket and run the supplied callback on it. This closes the socket
-- when the callback finishes or when an exception is thrown. Do not return 
-- the socket from the callback. This leads to undefined behavior. The user
-- cannot specify an endpoint since the socket cannot receive traffic.
withSocket ::
     (Socket -> IO a) -- ^ Callback providing the socket
  -> IO (Either SocketException a)
withSocket f = mask $ \restore -> do
  debug "withSocket: opening raw socket"
  e1 <- S.uninterruptibleSocket S.internet
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) S.raw)
    S.rawProtocol
  debug "withSocket: opened raw socket"
  case e1 of
    Left err -> pure (Left (errorCode err))
    Right fd -> do
      a <- onException (restore (f (Socket fd))) (S.uninterruptibleErrorlessClose fd)
      S.uninterruptibleClose fd >>= \case
        Left err -> pure (Left (errorCode err))
        Right _ -> pure (Right a)

-- | Send a slice of a bytearray to the specified endpoint.
sendMutableByteArray ::
     Socket -- ^ Socket
  -> Endpoint -- ^ Spoofed source address and port
  -> Endpoint -- ^ Remote IPv4 address and port
  -> MutableByteArray RealWorld -- ^ Buffer (will be sliced)
  -> Int -- ^ Offset into payload
  -> Int -- ^ Lenth of slice into buffer
  -> IO (Either SocketException ())
sendMutableByteArray (Socket !s) !theSource !theRemote !thePayload !off !len = do
  let ipHeaderSz = cintToInt L.sizeofIpHeader
  let totalHeaderSz = cintToInt (L.sizeofIpHeader + L.sizeofUdpHeader)
  let totalPacketSz = len + totalHeaderSz
  -- Why do we add one to the size? This extra byte at the end will always
  -- be zeroed out. It makes UDP checksum calculation a little easier,
  -- since we can now pull out Word16s until we reach the end. If the
  -- original length was even, this extra byte ends up unused by the
  -- checksum. But, if the original length was odd, it does get used.
  buf <- PM.newPinnedByteArray (totalPacketSz + 1)
  PM.setByteArray buf 0 (totalPacketSz + 1) (0 :: Word8)
  let addr = PM.mutableByteArrayContents buf
  L.pokeIpHeaderVersionIhl addr (4 * 16 + 5)
  L.pokeIpHeaderTypeOfService addr 0
  -- TODO: Actually check the length.
  -- NB: The packet length must be in network byte order.
  -- Expermentally, it seems that it does not.
  L.pokeIpHeaderTotalLength addr (S.hostToNetworkShort (intToWord16 totalPacketSz))
  L.pokeIpHeaderIdentifier addr 0
  L.pokeIpHeaderFragmentOffset addr 0
  L.pokeIpHeaderTimeToLive addr 64
  L.pokeIpHeaderProtocol addr (cintToWord8 (S.getProtocol S.udp))
  -- The linux kernel fills in the ip header checksum for us.
  L.pokeIpHeaderChecksum addr 0
  let src = S.hostToNetworkLong (getIPv4 (address theSource))
  L.pokeIpHeaderSourceAddress addr src
  let dst = S.hostToNetworkLong (getIPv4 (address theRemote))
  L.pokeIpHeaderDestinationAddress addr dst
  let udpAddr = PM.plusAddr addr ipHeaderSz
  L.pokeUdpHeaderSourcePort udpAddr (S.hostToNetworkShort (port theSource))
  L.pokeUdpHeaderDestinationPort udpAddr (S.hostToNetworkShort (port theRemote))
  let udpLen = cintToInt L.sizeofUdpHeader + len
  L.pokeUdpHeaderLength udpAddr (S.hostToNetworkShort (intToWord16 udpLen))
  PM.copyMutableByteArray buf totalHeaderSz thePayload off len
  L.pokeUdpHeaderChecksum udpAddr . S.hostToNetworkShort =<< udpChecksum src dst buf ipHeaderSz udpLen
  touchMutableByteArray buf
  debug ("spoof send mutable: about to send to " ++ show theRemote)
  whenDebugging $ do
    d <- PM.newByteArray totalPacketSz
    PM.copyMutableByteArray d 0 buf 0 totalPacketSz
    x <- PM.unsafeFreezeByteArray d
    debug ("raw packet: " ++ (foldMap (printf "%.2x ") (E.toList x)))
  e1 <- S.uninterruptibleSendToMutableByteArray s buf 0 (intToCSize totalPacketSz)
    mempty
    (S.encodeSocketAddressInternet (endpointToSocketAddressInternet theRemote))
  debug ("spoof send mutable: just sent to " ++ show theRemote)
  case e1 of
    Left err1 -> if err1 == eWOULDBLOCK || err1 == eAGAIN
      then do
        debug ("send mutable: waiting to for write ready to send to " ++ show theRemote)
        threadWaitWrite s
        e2 <- S.uninterruptibleSendToMutableByteArray s buf
          (intToCInt off)
          (intToCSize len)
          mempty
          (S.encodeSocketAddressInternet (endpointToSocketAddressInternet theRemote))
        case e2 of
          Left err2 -> do
            debug ("send mutable: encountered error after sending")
            pure (Left (errorCode err2))
          Right sz -> if csizeToInt sz == totalPacketSz
            then pure (Right ())
            else pure (Left (SentMessageTruncated (csizeToInt sz)))
      else do
        debug "spoof send mutable: sent on first try but got error code" 
        pure (Left (errorCode err1))
    Right sz -> if csizeToInt sz == totalPacketSz
      then do
        debug ("send mutable: success")
        pure (Right ())
      else pure (Left (SentMessageTruncated (csizeToInt sz)))

-- Precondition: the mutable byte array must have an extra zeroed out
-- byte at the end. That is, at arr[offset+length], there exists a
-- zero byte. The offset must divide two evenly.
udpChecksum ::
     Word32 -- source (network byte order)
  -> Word32 -- dest (network byte order)
  -> MutableByteArray RealWorld -- payload
  -> Int -- offset (start of the udp header)
  -> Int -- length (udp header size plus payload size) 
  -> IO Word16
udpChecksum src dst payload off len = do
  let sum0 = word16ToWord64 (S.hostToNetworkShort (word32ToWord16 src))
  debug ("udp checksum source lower: " ++ printf "%.8X" sum0)
  let sum1 = sum0 + word16ToWord64 (S.hostToNetworkShort (word32ToWord16 (unsafeShiftR src 16)))
  debug ("udp checksum source lower+upper: " ++ printf "%.8X" sum1)
  let sum2 = sum1 + word16ToWord64 (S.hostToNetworkShort (word32ToWord16 dst))
      sum3 = sum2 + word16ToWord64 (S.hostToNetworkShort (word32ToWord16 (unsafeShiftR dst 16)))
  debug ("udp checksum source+dest lower+upper: " ++ printf "%.8X" sum3)
  let sum4 = sum3 + word16ToWord64 (cintToWord16 (S.getProtocol S.udp))
      sum5 = sum4 + word16ToWord64 (intToWord16 len)
  debug ("udp checksum pseudoheader without carries: " ++ printf "%.8X" sum5)
  let halfLen = unsafeShiftR (len + off) 1
  debug ("udp checksum start offset: " ++ show (unsafeShiftR off 1))
  debug ("udp checksum last offset: " ++ show halfLen)
  let go :: Int -> Word64 -> IO Word64
      go !ix !acc = if ix < halfLen
        then do
          w16 <- PM.readByteArray payload ix :: IO Word16
          debug ("udp checksum payload iteration " ++ show ix ++ ": " ++ printf "%.8X" acc)
          go (ix + 1) (word16ToWord64 (S.hostToNetworkShort w16) + acc)
        else pure acc
  r <- go (unsafeShiftR off 1) sum5
  -- We assume that the upper 16 bits in this 64-bit word are zeroes.
  -- There is no way for a datagram to be long enough to start to
  -- fill the bits beyond 48.
  pure (word64ToWord16 (complement ((r .&. 0xFFFF) + (unsafeShiftR r 16 .&. 0xFFFF) + (unsafeShiftR r 32))))

endpointToSocketAddressInternet :: Endpoint -> S.SocketAddressInternet
endpointToSocketAddressInternet (Endpoint {address, port}) = S.SocketAddressInternet
  { port = S.hostToNetworkShort port
  , address = S.hostToNetworkLong (getIPv4 address)
  }

socketAddressInternetToEndpoint :: S.SocketAddressInternet -> Endpoint
socketAddressInternetToEndpoint (S.SocketAddressInternet {address,port}) = Endpoint
  { address = IPv4 (S.networkToHostLong address)
  , port = S.networkToHostShort port
  }

errorCode :: Errno -> SocketException
errorCode (Errno x) = ErrorCode x

shrinkMutableByteArray :: MutableByteArray RealWorld -> Int -> IO ()
shrinkMutableByteArray (MutableByteArray arr) (I# sz) =
  PM.primitive_ (shrinkMutableByteArray# arr sz)


touchByteArray :: ByteArray -> IO ()
touchByteArray (PM.ByteArray x) = touchByteArray# x

touchByteArray# :: ByteArray# -> IO ()
touchByteArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)

intToCInt :: Int -> CInt
intToCInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

csizeToInt :: CSize -> Int
csizeToInt = fromIntegral

cintToInt :: CInt -> Int
cintToInt = fromIntegral

cintToWord8 :: CInt -> Word8
cintToWord8 = fromIntegral

intToWord16 :: Int -> Word16
intToWord16 = fromIntegral

cintToWord16 :: CInt -> Word16
cintToWord16 = fromIntegral

word16ToWord64 :: Word16 -> Word64
word16ToWord64 = fromIntegral

word32ToWord64 :: Word32 -> Word64
word32ToWord64 = fromIntegral

word64ToWord16 :: Word64 -> Word16
word64ToWord16 = fromIntegral

word32ToWord16 :: Word32 -> Word16
word32ToWord16 = fromIntegral

touchMutableByteArray :: MutableByteArray RealWorld -> IO ()
touchMutableByteArray (MutableByteArray x) = touchMutableByteArray# x

touchMutableByteArray# :: E.MutableByteArray# RealWorld -> IO ()
touchMutableByteArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)
