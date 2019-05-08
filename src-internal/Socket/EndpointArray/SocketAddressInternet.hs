{-# language NamedFieldPuns #-}

module Socket.EndpointArray.SocketAddressInternet
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
import Posix.Socket (SocketAddressInternet)
import Data.Word (Word16)

import qualified Data.Primitive as PM

type EndpointArray = PrimArray SocketAddressInternet
type MutableEndpointArray =
  MutablePrimArray RealWorld SocketAddressInternet

new :: Int -> IO MutableEndpointArray
new = PM.newPrimArray

empty :: EndpointArray
empty = mempty

write :: MutableEndpointArray -> Int -> SocketAddressInternet -> IO ()
write = PM.writePrimArray

resize :: MutableEndpointArray -> Int -> IO MutableEndpointArray
resize = PM.resizeMutablePrimArray

freeze :: MutableEndpointArray -> IO EndpointArray
freeze = PM.unsafeFreezePrimArray
