{-# language LambdaCase #-}

-- Functions like @close@ are the same regardless of whether
-- a socket is connected, unconnected, internet-domain, or
-- unix-domain.
module Socket.Datagram.Common
  ( close
  ) where

import Foreign.C.Error (Errno(..))
import Socket.Datagram (Socket(..))
import Socket.Error (die)
import qualified Posix.Socket as S
import qualified Foreign.C.Error.Describe as D

-- | Unbracketed function for closing a datagram socket. This must not
-- be used in conjunction with 'withSocket'.
close :: Socket c a -> IO ()
close (Socket fd) = S.uninterruptibleClose fd >>= \case
  Left err -> die ("Socket.Datagram.Common.close: " ++ describeErrorCode err)
  Right _ -> pure ()

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"

