{-# language BangPatterns #-}
{-# language DataKinds #-}

module Socket.MutableBytes.Peerless
  ( Buffer
  , advance
  , length
  , receiveFromOnce
  ) where

import Prelude hiding (length)

import Data.Bytes.Types (MutableBytes(..))
import Socket.MutableBytes (Buffer,advance,length)
import Foreign.C.Types (CInt,CSize)
import Foreign.C.Error (Errno)
import Posix.Socket (MessageFlags,Message(Receive))
import System.Posix.Types (Fd)
import GHC.Exts (RealWorld)
import qualified Posix.Socket as S

receiveFromOnce ::
     Fd
  -> MutableBytes RealWorld
  -> MessageFlags 'Receive
  -> CInt
  -> IO (Either Errno (CInt, S.SocketAddress, CSize))
{-# inline receiveFromOnce #-}
receiveFromOnce fd (MutableBytes arr off len) flags _ =
  (fmap.fmap) (\sz -> (-1,S.SocketAddress mempty,sz))
  $ S.uninterruptibleReceiveFromMutableByteArray_
    fd arr (intToCInt off) (intToCSize len) flags
   
intToCInt :: Int -> CInt
{-# inline intToCInt #-}
intToCInt = fromIntegral

intToCSize :: Int -> CSize
{-# inline intToCSize #-}
intToCSize = fromIntegral
