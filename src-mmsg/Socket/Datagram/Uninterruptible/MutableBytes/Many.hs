{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language UnboxedTuples #-}
module Socket.Datagram.Uninterruptible.MutableBytes.Many
  ( receiveMany
  , receiveManyFromIPv4
  ) where

import Control.Concurrent.STM (TVar)
import Foreign.C.Error (eWOULDBLOCK,eAGAIN)
import Foreign.C.Types (CInt,CUInt)
import GHC.IO (IO(..))
import Socket (Interruptibility(Uninterruptible))
import Socket (Connectedness(..))
import Socket.Datagram (Socket(..),ReceiveException)
import Socket.IPv4 (Slab(..))
import Socket.Error (die)
import System.Posix.Types (Fd)

import qualified Data.Primitive as PM
import qualified Linux.Socket as L
import qualified Socket as SCK
import qualified Socket.Discard
import qualified Socket.EventManager as EM

-- | Receive up to the specified number of datagrams into freshly allocated
--   byte arrays. When there are many datagrams present in the receive
--   buffer, this is more efficient than calling 'receive' repeatedly. This
--   is guaranteed to fill the buffer with at least one message.
--
--   The length buffer and the payload buffers arrange data in a
--   structure-of-arrays fashion. The size of the payload received
--   into @payloads[j]@ is stored at @lengths[j]@.
receiveMany :: 
     Socket c a
     -- ^ Socket
  -> Socket.Discard.Slab
     -- ^ Buffers into which sizes and payloads are received
  -> IO (Either (ReceiveException 'Uninterruptible) Int)
receiveMany (Socket !fd) !slab@Socket.Discard.Slab{payloads} = do
  let !mngr = EM.manager
  tv <- EM.reader mngr fd
  let !sz = intToCUInt (PM.sizeofMutableUnliftedArray payloads)
  token0 <- EM.wait tv
  receiveManyGo sz tv fd slab token0

receiveManyGo ::
     CUInt
  -> TVar EM.Token 
  -> Fd
  -> Socket.Discard.Slab
  -> EM.Token
  -> IO (Either (ReceiveException 'Uninterruptible) Int)
receiveManyGo !sz !tv !fd !slab@(Socket.Discard.Slab{sizes,payloads}) !token0 = do
  e <- L.uninterruptibleReceiveMultipleMessageD fd sizes payloads sz L.truncate
  -- TODO: add truncation check
  case e of
    Left err -> if err == eAGAIN || err == eWOULDBLOCK
      then do
        EM.unready token0 tv
        token1 <- EM.wait tv
        receiveManyGo sz tv fd slab token1
      else die "receiveMany"
    Right grams -> if grams == 0
      then die "receiveMany: 0 datagrams"
      else pure $! Right $! cintToInt grams

-- | Variant of 'receiveMany' that provides that source address
-- corresponding to each datagram. This introduces another array
-- to the structure-of-arrays.
receiveManyFromIPv4 :: 
     Socket 'Unconnected 'SCK.IPv4 -- ^ Socket
  -> Socket.IPv4.Slab
     -- ^ Buffers into which sizes, addresses, and payloads
     -- are received
  -> IO (Either (ReceiveException 'Uninterruptible) Int)
receiveManyFromIPv4 (Socket fd) slab@(Socket.IPv4.Slab{payloads}) = do
  let !mngr = EM.manager
  tv <- EM.reader mngr fd
  let !sz = intToCUInt (PM.sizeofMutableUnliftedArray payloads)
  token0 <- EM.wait tv
  receiveManyFromIPv4Go sz tv fd slab token0

receiveManyFromIPv4Go ::
     CUInt
  -> TVar EM.Token 
  -> Fd
  -> Socket.IPv4.Slab
  -> EM.Token
  -> IO (Either (ReceiveException 'Uninterruptible) Int)
receiveManyFromIPv4Go !sz !tv !fd !slab@(Socket.IPv4.Slab{sizes,payloads,peers}) !token0 = do
  e <- L.uninterruptibleReceiveMultipleMessageC fd sizes peers payloads sz L.truncate
  -- TODO: add truncation check
  case e of
    Left err -> if err == eAGAIN || err == eWOULDBLOCK
      then do
        EM.unready token0 tv
        token1 <- EM.wait tv
        receiveManyFromIPv4Go sz tv fd slab token1
      else die "receiveMany"
    Right grams -> if grams == 0
      then die "receiveMany: 0 datagrams"
      else pure $! Right $! cintToInt grams

intToCUInt :: Int -> CUInt
intToCUInt = fromIntegral

cintToInt :: CInt -> Int
cintToInt = fromIntegral
