{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language KindSignatures #-}

module Socket
  ( SocketUnrecoverableException(..)
  , Direction(..)
  , Connectedness(..)
  , Family(..)
  , Version(..)
  , Interruptibility(..)
  , Forkedness(..)
  , cgetsockname 
  , cgetsockopt
  , cclose 
  , crecv 
  , crecvfrom
  , cshutdown
  , negativeSliceLength
  , nonInternetSocketFamily
  , functionWithAccepted
  , functionWithConnection
  , functionWithListener
  , functionWithSocket
  , functionGracefulClose
  , socketAddressSize
  ) where

import Control.Exception (Exception(..))
import Data.Kind (Type)
import Foreign.C.Types (CInt)

import qualified Data.List as L

data Direction = Send | Receive

data Connectedness = Connected | Unconnected

data Family = Internet Version | Unix

data Version = V4 | V6

data Interruptibility = Interruptible | Uninterruptible

data Forkedness = Forked | Unforked

data SocketUnrecoverableException = SocketUnrecoverableException
  { modules :: String
  , function :: String
  , description :: [String]
  }
  deriving stock (Show,Eq)

instance Exception SocketUnrecoverableException where
  displayException (SocketUnrecoverableException m f d) =
    m ++ "." ++ f ++ ": [" ++ L.intercalate "," d ++ "]"

cgetsockname :: String
cgetsockname = "getsockname"

cgetsockopt :: String
cgetsockopt = "getsockopt"

cclose :: String
cclose = "getsockname"

crecv :: String
crecv = "recv"

crecvfrom :: String
crecvfrom = "recvfrom"

cshutdown :: String
cshutdown = "shutdown"

functionGracefulClose :: String
functionGracefulClose = "gracefulClose"

nonInternetSocketFamily :: String
nonInternetSocketFamily = "non-internet socket family"

negativeSliceLength :: String
negativeSliceLength = "negative slice length"

functionWithAccepted :: String
functionWithAccepted = "withAccepted"

functionWithConnection :: String
functionWithConnection = "withConnection"

functionWithListener :: String
functionWithListener = "withListener"

functionWithSocket :: String
functionWithSocket = "withSocket"

socketAddressSize :: String
socketAddressSize = "socket address size"
