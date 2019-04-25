{-# language MagicHash #-}

module Socket.Error
  ( die
  ) where

-- The purpose of this module is to coax GHC into producing
-- core that is easier to reason about. (As a secondary concern,
-- it is possible that it may be compiled to slightly better cmm
-- and assembly). For some reason, GHC does not like to inline @fail@,
-- so here we inline the implementation of fail into @die@ and mark
-- @die@ with an INLINE pragma. This causes @raiseIO#@ to show up
-- in GHC core at use sites of this function. And we optimizing
-- core, GHC is smart enough to figure out that expressions that
-- follow @raiseIO#@ are unreachable.
--
-- Impressively, this seemingly minor change leads to GHC being able
-- to apply additional optimizations. For example, there is some
-- needless construction of @Left@ and @Right@ that is eliminated
-- when we have @raiseIO#@ inlined properly.
import GHC.IO (IO(..))
import GHC.Exts (raiseIO#)
import Control.Exception (toException)

die :: String -> IO a
{-# inline die #-}
die _ = IO (raiseIO# (toException (userError "sockets: internal error, rebuild with verbose-errors flag to learn more")))
