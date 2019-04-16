{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}

module Socket.EventManager
  ( manager
  , register
  , reader
  , deregister
  ) where

-- Why write another event manager? GHC ships with the mio event manager,
-- but mio is burdened with backwards-compatibility concerns that are
-- antithetical to performance:
--
-- * It supports platforms that have @poll@ as their only mechanism
--   for polling events. This limits mio to using the level-triggered
--   interfaces of @epoll@ and @kqueue@.
-- * It supports multiple registrations per file descriptor. Taking
--   advantage of this feature implies that a file descriptor is shared
--   across multiple green threads. However, such sharing is dubious.
--   An application using network sockets is this way is suseptible to
--   the thundering herd problem. Making this even worse is that a stream
--   socket shared across multiple threads lacks useful behavior (unlike
--   a datagram socket).
-- * It supports arbitrary callbacks associated with each registration.
--   With network sockets, the only callback ever used is one that fills
--   a TVar or MVar. This is good since processing data inside the
--   callback could delay or hang the event manager. But, since the
--   only callback network sockets ever need is one that fills a variable,
--   there is no need to support arbitrary callbacks.
--
-- In constrast to mio, the event manager in this module:
--
-- * Supports only platforms with event notification facilities that provide
--   an edge-triggered interface.
-- * Allows at most 1 registration per file descriptor. This registration
--   always includes the read channel and the write channel.
-- * Pushes out readiness notifications using @TVar@s rather than callbacks.
-- 
-- After a user registers an file descriptor with @register@, it may call
-- @reader@ or @writer@ at any time to retrieve the @TVar Bool@ associated with
-- that describes the readiness of that channel. Because of how edge-triggered
-- event notification works, this TVar has some slightly unusual properties.
-- This is best illustrated by example:
-- 
-- TODO: This is now out of date and should probably be deleted.
-- > example :: MutableByteArray RealWorld -> IO ()
-- > example buf = do
-- >   register fd
-- >   readyVar <- reader fd
-- >   readTVarIO readyVar >>= \case
-- >     True -> do
-- >       numBytes <- uninterruptibleReceive fd buf 
-- >       -- No guarantee yet that there are bytes on the
-- >       -- receive buffer.
-- >       if numBytes == 0
-- >         then do
-- >           atomically $ do
-- >             x <- readTVar readyVar
-- >             if 
-- >           atomically (check =<< readTVar readyVar)
-- >         else ...
-- >     False -> do
-- >       -- Need to wait for more input
-- >       atomically (check =<< readTVar readyVar)
-- >       numBytes <- uninterruptibleReceive fd buf 
-- >       if numBytes == 0
-- >         then fail "Not possible"
-- >         else ...

import Control.Applicative (liftA2)
import Control.Monad.STM (atomically)
import Data.Primitive (MutableUnliftedArray(..),MutablePrimArray,ByteArray)
import Data.Primitive (PrimUnlifted,Prim)
import Data.Primitive.Unlifted.TVar (UnliftedTVar)
import Data.Word (Word64)
import Foreign.C.Error (Errno(..))
import GHC.Exts (RealWorld)
import System.Posix.Types (Fd)
import Data.Bits (countLeadingZeros,finiteBitSize,unsafeShiftL,(.|.))
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.ST (runST)
import Control.Monad (when)
import Control.Concurrent (getNumCapabilities,forkOn,rtsSupportsBoundThreads)
import GHC.Conc.Sync (yield)
import Foreign.C.Types (CInt)

import qualified Linux.Epoll as Epoll
import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Atomic as PM
import qualified Data.Primitive.Unlifted.TVar as PM

-- | Register interest in reads and writes. After registering a socket,
-- use 'reader' and 'writer' to get access to the transactional variables
-- that describe the readiness of their corresponding channels.
--
-- Precondition: There is no existing registration for this file descriptor.
register ::
     Manager -- The event manager
  -> Fd -- File descriptor
  -> IO ()
register mngr@Manager{epoll} !fd = do
  constructivelyLookupTier1 (fdToInt fd) mngr
  -- Enough space for a single registration.
  ev <- PM.newPrimArray 1
  PM.writePrimArray ev 0 $ Epoll.Event
    { Epoll.events = Epoll.input <> Epoll.output <> Epoll.edgeTriggered
    , Epoll.payload = fd
    }
  e <- Epoll.uninterruptibleControlMutablePrimArray epoll Epoll.add fd ev
  case e of
    Left (Errno code) ->
      fail $ "Socket.EventManager.register: epoll_ctl error " ++ show code
    Right () -> pure ()

-- This does not close the file descriptor. Call this function either
-- right before or right after closing the socket. It does not matter
-- which order they happen in. Be sure to mask exceptions when closing
-- the socket. It is important to ensure that an asynchronous exception
-- doesn't cause closing or deregistration to happen without the other
-- happening as well. Notice that this function does not call epoll_ctl.
-- Closing the file descriptor will cause epoll deregistration to happen.
deregister :: Manager -> Fd -> IO ()
deregister Manager{variables} !fd = do
  (readVar,writeVar) <- lookupBoth (fdToInt fd) variables
  -- It should not be necessary to batch these in the same atomically
  -- since this function should only ever be called with exceptions
  -- masked. However, we do it anyway since it might improve performance.
  -- It's difficult to test this theory.
  atomically $ do
    PM.modifyUnliftedTVar readVar incrementEventCounter
    PM.modifyUnliftedTVar writeVar incrementEventCounter

-- Deregister insterest in reads and writes.
-- Precondition: A previous call to register has been made.
-- unregister

type MUArray = MutableUnliftedArray RealWorld

data Manager = Manager
  { variables :: !(MUArray (MUArray (UnliftedTVar Token)))
  , novars :: !(MUArray (UnliftedTVar Token))
    -- An empty mutable array. This array is used to mark the absense of
    -- a tier-two array of TVars.
  , epoll :: !Fd
  }

manager :: Manager
{-# noinline manager #-}
manager = unsafePerformIO $ do
  when (not rtsSupportsBoundThreads) $ do
    fail $ "Socket.Event.manager: threaded runtime required"
  novars <- PM.unsafeNewUnliftedArray 0
  variables <- PM.newUnliftedArray 32 novars
  Epoll.uninterruptibleCreate 1 >>= \case
    Left (Errno code) ->
      fail $ "Socket.EventManager.manager: epoll_create error code " ++ show code
    Right epoll -> do
      -- Spawn a worker thread (for calling epoll_wait in the background)
      -- on each capability. Recall that since this is in a 
      -- noinline+unsafePerformIO setting, this only happens the
      -- first time the manager is accessed. These workers should
      -- continue to run forever. Nothing should be able to kill them
      -- since the thread IDs are discarded.
      capNum <- getNumCapabilities
      let go !ix = if ix > (-1)
            then do
              _ <- forkOn ix $ do
                let initSz = 1
                initArr <- PM.newPrimArray initSz
                loopManager initArr initSz epoll variables
              go (ix - 1)
            else pure ()
      go (capNum - 1)
      pure (Manager {variables,novars,epoll})

reader :: Manager -> Fd -> IO (UnliftedTVar Token)
reader Manager{variables} !fd = lookupGeneric 0 (fdToInt fd) variables

lookupBoth ::
     Int -- File descriptor
  -> MUArray (MUArray (UnliftedTVar Token))
  -> IO (UnliftedTVar Token,UnliftedTVar Token)
lookupBoth !fd !arr = do
  let (ixTier1,ixTier2) = decompose fd
  tier2 <- PM.readUnliftedArray arr ixTier1
  liftA2 (,)
    (PM.readUnliftedArray tier2 (ixTier2 * 2))
    (PM.readUnliftedArray tier2 (ixTier2 * 2 + 1))

-- The file descriptor must already be registered. Otherwise, this
-- function may look in an uninitialized tier-two array.
lookupGeneric ::
     Int -- Read: 0, Write: 1
  -> Int -- File descriptor
  -> MutableUnliftedArray RealWorld (MutableUnliftedArray RealWorld (UnliftedTVar Token))
  -> IO (UnliftedTVar Token)
lookupGeneric !rw !fd !arr = do
  let (ixTier1,ixTier2) = decompose fd
  tier2 <- PM.readUnliftedArray arr ixTier1
  PM.readUnliftedArray tier2 ((ixTier2 * 2) + rw)

constructivelyLookupTier1 ::
     Int -- File descriptor
  -> Manager
  -> IO ()
constructivelyLookupTier1 !fd Manager{variables,novars} = do
  let (ixTier1,_) = decompose fd
  varsTier2 <- PM.readUnliftedArray variables ixTier1
  if PM.sameMutableUnliftedArray varsTier2 novars
    then do
      -- We want 2 * 2^N tvars because there is a separate read and
      -- write tvar for every file descriptor.
      let !len = exp2succ ixTier1
      varsAttempt <- PM.unsafeNewUnliftedArray len
      let goVars !ix = if ix > (-1)
            then do
              PM.writeUnliftedArray varsAttempt ix
                =<< PM.newUnliftedTVarIO emptyToken
              goVars (ix - 1)
            else pure ()
      goVars (len - 1)
      -- We ignore the result of casUnliftedArray. It does not actually
      -- matter whether or not it succeeded. If it failed, some other
      -- thread must have initialized the tier 2 arrays.
      _ <- PM.casUnliftedArray variables ixTier1 novars varsAttempt
      pure ()
    else pure ()

loopManager :: 
     MutablePrimArray RealWorld (Epoll.Event 'Epoll.Response Fd)
  -> Int -- size of events buffer
  -> Fd -- epoll file descriptor
  -> MUArray (MUArray (UnliftedTVar Token)) -- tier 1 variables array
  -> IO ()
loopManager !evs0 !sz0 !epfd !tier1 = do
  (evs1, sz1) <- stepManager evs0 sz0 epfd tier1
  loopManager evs1 sz1 epfd tier1

stepManager ::
     MutablePrimArray RealWorld (Epoll.Event 'Epoll.Response Fd)
  -> Int -- size of events buffer
  -> Fd -- epoll file descriptor
  -> MUArray (MUArray (UnliftedTVar Token)) -- tier 1 variables array
  -> IO (MutablePrimArray RealWorld (Epoll.Event 'Epoll.Response Fd),Int)
     -- returns new events buffer and its size
stepManager !evs0 !sz0 !epfd !tier1 = do
  Epoll.uninterruptibleWaitMutablePrimArray epfd evs0 (intToCInt sz0) >>= \case
    Left (Errno code) -> fail $ "Socket.EventManager.stepManager: A " ++ show code
    Right len0 -> if len0 > 0
      then handleEvents evs0 (cintToInt len0) sz0 tier1
      else do
        yield
        Epoll.uninterruptibleWaitMutablePrimArray epfd evs0 (intToCInt sz0) >>= \case
          Left (Errno code) -> fail $ "Socket.EventManager.stepManager: B " ++ show code
          Right len1 -> if len1 > 0
            then handleEvents evs0 (cintToInt len1) sz0 tier1
            else do
              Epoll.waitMutablePrimArray epfd evs0 (intToCInt sz0) (-1) >>= \case
                Left (Errno code) -> fail $ "Socket.EventManager.stepManager: C " ++ show code
                Right len2 -> handleEvents evs0 (cintToInt len2) sz0 tier1

intToCInt :: Int -> CInt
intToCInt = fromIntegral

cintToInt :: CInt -> Int
cintToInt = fromIntegral

-- This should only ever be called when the number of events is
-- greater than zero. It still works fine if the number is zero,
-- but @stepManager@ shoud be behaving differently depending on
-- this value. This function is also responsible for doubling
-- the buffer size when needed. Do not reuse the argument buffer
-- after calling this function.
handleEvents ::
     MutablePrimArray RealWorld (Epoll.Event 'Epoll.Response Fd)
  -> Int -- number of events
  -> Int -- size of events buffer, always greater than or equal to number of events
  -> MUArray (MUArray (UnliftedTVar Token)) -- tier 1 variables array
  -> IO (MutablePrimArray RealWorld (Epoll.Event 'Epoll.Response Fd),Int)
     -- returns new events buffer and its size
handleEvents !evs !len !sz !vars = do
  traverseMutablePrimArray_
    ( \(Epoll.Event{Epoll.events,Epoll.payload}) -> do
      let fd = payload
      (readVar,writeVar) <- lookupBoth (fdToInt fd) vars
      when (Epoll.containsEvents events Epoll.input) $ do
        atomically $ PM.modifyUnliftedTVar readVar incrementEventCounter
      when (Epoll.containsEvents events Epoll.output) $ do
        atomically $ PM.modifyUnliftedTVar writeVar incrementEventCounter
    ) evs 0 len
  if sz == len 
    then do
      let newSz = len * 2
      newBuf <- PM.resizeMutablePrimArray evs newSz
      pure (newBuf,newSz)
    else pure (evs,sz)

traverseMutablePrimArray_ ::
     Prim a
  => (a -> IO b)
  -> MutablePrimArray RealWorld a
  -> Int -- offset
  -> Int -- end
  -> IO ()
{-# inline traverseMutablePrimArray_ #-}
traverseMutablePrimArray_ f a off len = go off where
  go !ix = if ix < len
    then (f =<< PM.readPrimArray a ix) *> go (ix + 1)
    else pure ()

-- Given an argument N, return 2^N.
exp2 :: Int -> Int
{-# INLINE exp2 #-}
exp2 n = unsafeShiftL (1 :: Int) n

-- Given an argument N, return 2^(N+1).
exp2succ :: Int -> Int
{-# INLINE exp2succ #-}
exp2succ n = unsafeShiftL (1 :: Int) (n + 1)

-- Decompose an index N into two parts, A and B.
--
-- * A = ⌊log2(N+1)⌋
-- * B = N - 2^A + 1
-- 
-- This gives the following decompositions:
--
-- * 0 => (0,0)
-- * 1 => (1,0)
-- * 2 => (1,1)
-- * 3 => (2,0)
-- * 4 => (2,1)
-- * 5 => (2,2)
-- * 6 => (2,3)
--
-- Precondition: N >= 0.
decompose :: Int -> (Int,Int)
{-# INLINE decompose #-}
decompose n =
  let !a = finiteBitSize (undefined :: Int) - countLeadingZeros (n + 1) - 1
      !b = (n + 1) - exp2 a
   in (a,b)

fdToInt :: Fd -> Int
{-# INLINE fdToInt #-}
fdToInt = fromIntegral

-- Token is an optimization of the data type:
--   data Token = Token
--     { ready :: Bool, eventCount :: Word63 }
-- Invariant: the bytearray has length 8.
-- The descriptor counter and the event counter are represented in
-- the predictable way. The readiness bit is the highest bit. Visually:
--   |XYYYYYYY|YYYYYYYY|YYYYYYYY|YYYYYYYY|YYYYYYYY|YYYYYYYY|YYYYYYYY|YYYYYYYY
-- X: readiness (1 is ready, 0 is not ready)
-- Y: event counter
-- Since a 63-bit word has so many inhabitants, we pretend that it will
-- never wrap around. In practice, an application would need to run for
-- trillions of years for overflow to happen.
newtype Token = Token ByteArray
  deriving newtype PrimUnlifted

readyBit :: Word64
readyBit = 0x8000000000000000

-- The empty token has readiness set to true.
emptyToken :: Token
emptyToken = Token $ runST $ do
  arr <- PM.newByteArray 8
  PM.writeByteArray arr 0 readyBit
  PM.unsafeFreezeByteArray arr

-- Increments the event counter. Sets readiness to true. Leaves
-- the descriptor counter alone.
incrementEventCounter :: Token -> Token
incrementEventCounter (Token arr) = Token $ runST $ do
  let (w :: Word64) = PM.indexByteArray arr 0
  marr <- PM.newByteArray 8
  PM.writeByteArray marr 0 (readyBit .|. (w + 1))
  PM.unsafeFreezeByteArray marr

-- [Notes on registration]
-- This interface requires every call to @register@ to be paired with
-- a call to @deregister@. Consumers of this API must mask asynchronous
-- exceptions at appropriate places to ensure that this happens.
--
-- Consumers of this API must also help out when they discover
-- that a channel that was thought to be ready in not actually ready.
-- These consumers need to update the TVar appropriately. But what
-- if the TVar is updated by a epoll_wait worker thread at the
-- same time that the thread using the socket tries to update it?
-- This is what the event counter (in Token) is for. The socket thread
-- should abandon its attempt to update the token if it discovers
-- that the event counter has increased.
--
-- There is a bit of trickiness to closing sockets as well.
-- What if an epoll_wait worker thread attempts to fill a TVar
-- after a file descriptor is reused? Assuming an event manager thread
-- EVM and a worker thread WRK, consider this situation:
-- * [EVM] epoll_wait: finds that FD 123 became ready for reads
-- * [WRK] close socket with FD 123, causing epoll_ctl with EPOLL_CTL_DEL
-- * [WRK] open socket, kernel reuses FD 123
-- * [WRK] register FD 123
-- * [EVM] Lookup the read TVar and attempt to set its token to have
--         readiness true.
-- The descriptor counter (in Token) is used to prevent nonsense
-- results like this. However, this actually isn't a problem since
-- readiness is understood to include false positives. That is,
-- if the token says that a descriptor is ready, it might not
-- actually be true, but if a token says that a descriptor is
-- not ready, it is definitely not ready.


-- This comment block is no longer relevant. We do not use poll anymore.
--
-- Since we use epoll's edge-triggered interface, epoll does not tell
-- us the original readiness values. To get these, we use poll. It is
-- important to register with epoll before calling poll. If we did it the other
-- way around, there would be a race condition where we could miss
-- a toggle from not ready to ready between the two calls. This could
-- happen if, between the two calls, the operating system received data from
-- the peer and stuck it in the receive buffer.
--
-- Assumption A. Crucially, the opposite situation cannot occur.
-- That is, it should not be possible for newly created socket to
-- go from ready to not ready on either the read or the write channel.
-- Since the socket should not be shared with another thread, there
-- could not have been any calls to recv/send that would cause a toggle
-- in this direction.
-- 
-- Here is a table of how we interpret all possible situations:
-- * Var: True, Poll: X ==> True. epoll_wait ran at some point
--     before or after poll and found that channel had gone from
--     being not ready to ready. The value of poll does not matter.
--     If it was False, then poll must have ran before epoll_wait.
--     If it was True, we do not know in what order they ran. Either
--     way, we can say with confidence that the channel is now ready.
--     We can say this only because of Assumption A described earlier.
-- * Var: False, Poll: X ==> X. Either epoll_wait has not run, or
--     it ran and did not detect a change in the readiness of the channel.
--     Because of Assumption A, we do not consider the possibility that
--     epoll_wait ran after poll and set the readiness
--     to False. Consequently, we use the value from Poll since that must
--     still be the current value.
-- 
-- These interpretations suggest that logical disjunction will give 
-- us the current readiness of the channel.


-- This poll code is no longer needed.
-- pollSingle :: Fd -> IO (Bool,Bool)
-- pollSingle !fd = do
--   pfds <- PM.newPrimArray 1
--   PM.writePrimArray pfds 0 $ Poll.PollFd
--     { Poll.descriptor = fd
--     , Poll.request = Poll.input <> Poll.output
--     , Poll.response = mempty
--     }
--   Poll.uninterruptiblePollMutablePrimArray pfds 0 >>= \case
--     Left (Errno e) -> fail ("Socket.EventManager.pollSingle: error code " ++ show e)
--     Right _ -> do
--       -- We actually do not care about the return value of poll.
--       -- Looking in the revents field will tell us what we care
--       -- about anyway.
--       Poll.PollFd{Poll.response} <- PM.readPrimArray pfds 0
--       pure
--         ( Poll.isSubeventOf Poll.input response
--         , Poll.isSubeventOf Poll.output response
--         )
