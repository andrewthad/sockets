{-# language BangPatterns #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Socket.Systemd
  ( SystemdException(..)
  , systemdListenerInternal
  ) where

import Control.Exception (Exception)
import Data.Bits ((.&.))
import Foreign.C.Types (CInt)
import Foreign.C.Error (Errno(..))
import System.Posix.Types (Fd(Fd))

import qualified Posix.File as F
import qualified Socket.EventManager as EM
import qualified Posix.Socket as S
import qualified Linux.Systemd as L

-- Internal function used for inheriting either an internet
-- stream socket or a unix-domain stream socket from systemd.
systemdListenerInternal :: S.Family -> IO (Either SystemdException Fd)
{-# noinline systemdListenerInternal #-}
systemdListenerInternal !fam = L.listenFds 1 >>= \case
  Left (Errno e) -> pure (Left (SystemdErrno e))
  Right n -> case n of
    1 -> L.isSocket fd0 fam S.stream 1 >>= \case
      Left (Errno e) -> pure (Left (SystemdErrno e))
      Right r -> case r of
        0 -> pure (Left SystemdDescriptorInfo)
        _ -> F.uninterruptibleGetStatusFlags fd0 >>= \case
          Left (Errno e) -> pure (Left (SystemdFnctlErrno e))
          Right status -> if F.nonblocking .&. status == mempty
            then pure (Left SystemdBlocking)
            else do
              let !mngr = EM.manager
              EM.register mngr fd0
              pure (Right fd0)
    _ -> pure (Left (SystemdDescriptorCount n))
  where
  fd0 = Fd 3

data SystemdException
  = SystemdDescriptorCount !CInt
  | SystemdDescriptorInfo
  | SystemdBlocking
    -- ^ The socket was in blocking mode. Set @NonBlocking=True@ in the systemd
    -- service to resolve this.
  | SystemdErrno !CInt
  | SystemdFnctlErrno !CInt
  deriving stock (Show,Eq)
  deriving anyclass (Exception)
