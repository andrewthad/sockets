{-# language BangPatterns #-}

module Socket.Pair
  ( open
  ) where

import Foreign.C.Error (Errno(..))
import Foreign.C.Error (eNFILE,eMFILE)
import Socket (SocketException(..))
import Socket.Error (die)
import System.Posix.Types (Fd)

import qualified Foreign.C.Error.Describe as D
import qualified Linux.Socket as L
import qualified Posix.Socket as S
import qualified Socket.EventManager as EM

-- Helper for opening a connected unix-domain socket pair. This is
-- used by Socket.Datagram.Unix.Connected and by Socket.SequencedPacket.Unix.
-- It could be used by Socket.Stream.Unix if that module is ever written.
open :: S.Type -> IO (Either SocketException (Fd, Fd))
open !typ = do
  e1 <- S.uninterruptibleSocketPair S.Unix
    (L.applySocketFlags (L.closeOnExec <> L.nonblocking) typ)
    S.defaultProtocol
  case e1 of
    Left err -> handleSocketException err
    Right (fdA,fdB) -> do
      let !mngr = EM.manager
      EM.register mngr fdA
      EM.register mngr fdB
      pure (Right (fdA, fdB))

handleSocketException :: Errno -> IO (Either SocketException a)
handleSocketException e
  | e == eMFILE = pure (Left SocketFileDescriptorLimit)
  | e == eNFILE = pure (Left SocketFileDescriptorLimit)
  | otherwise = die
      ("Socket.Pair.open: " ++ describeErrorCode e)

describeErrorCode :: Errno -> String
describeErrorCode err@(Errno e) = "error code " ++ D.string err ++ " (" ++ show e ++ ")"
