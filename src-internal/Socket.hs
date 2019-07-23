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
  , Pinnedness(..)
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

-- | This is used as a phantom type to distinguish
-- between pinned and unpinned memory. The distinction
-- drawn here concerns the manner in which the user
-- requested the byte array. Byte arrays resulting
-- from newPinnedByteArray# are considered 'Pinned'
-- while those resulting from newByteArray# are
-- considered 'Unpinned'. Even if the runtime decides
-- to pin a byte array requested by newByteArray#
-- (e.g. because it is larger than 3KB), it is
-- considered 'Unpinned' by this classification.
data Pinnedness = Pinned | Unpinned

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
