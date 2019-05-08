{-# language NamedFieldPuns #-}

module Socket.EndpointArray.IPv4
  ( EndpointArray
  , MutableEndpointArray
  , new
  , empty
  , write
  , resize
  , freeze
  ) where

import Control.Applicative (liftA2)
import Data.Primitive (PrimArray,MutablePrimArray)
import GHC.Exts (RealWorld)
import Net.Types (IPv4)
import Socket.IPv4 (Peer(..))
import Data.Word (Word16)

import qualified Data.Primitive as PM

type EndpointArray = (PrimArray Word16,PrimArray IPv4)
type MutableEndpointArray =
  (MutablePrimArray RealWorld Word16,MutablePrimArray RealWorld IPv4)

new :: Int -> IO MutableEndpointArray
new n = liftA2 (,) (PM.newPrimArray n) (PM.newPrimArray n)

empty :: EndpointArray
empty = (mempty,mempty)

write :: MutableEndpointArray -> Int -> Peer -> IO ()
write (ports,addresses) ix Peer{port,address} = do
  PM.writePrimArray ports ix port
  PM.writePrimArray addresses ix address

resize :: MutableEndpointArray -> Int -> IO MutableEndpointArray
resize (ports,addresses) n = liftA2 (,)
  (PM.resizeMutablePrimArray ports n)
  (PM.resizeMutablePrimArray addresses n)

freeze :: MutableEndpointArray -> IO EndpointArray
freeze (ports,addresses) = liftA2 (,)
  (PM.unsafeFreezePrimArray ports)
  (PM.unsafeFreezePrimArray addresses)

