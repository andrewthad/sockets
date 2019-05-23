{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Socket.EventManager
  ( -- * Manager
    manager
    -- * Registration
  , register
  , reader
  , writer
    -- * Transactional Variables
  , Token
  , unready
  , wait
  , unreadyAndWait
  , persistentUnreadyAndWait
  , persistentUnready
  , interruptibleWait
  , interruptibleWaitCounting
  , isInterrupt
  ) where

import Control.Applicative (liftA2,(<|>))
import Control.Concurrent (getNumCapabilities,forkOn,rtsSupportsBoundThreads)
import Control.Concurrent.STM (TVar)
import Control.Monad (when)
import Control.Monad.STM (atomically)
import Data.Bits (countLeadingZeros,finiteBitSize,unsafeShiftL,(.|.),(.&.))
import Data.Bits (unsafeShiftR)
import Data.Primitive.Unlifted.Array (MutableUnliftedArray(..))
import Data.Primitive (MutableByteArray(..),MutablePrimArray(..))
import Data.Primitive (Prim)
import Data.Word (Word64,Word32)
import Foreign.C.Error (Errno(..))
import Foreign.C.Types (CInt)
import GHC.Conc.Sync (TVar(..),yield)
import GHC.Exts (RealWorld,Int(I#),(*#),TVar#,ArrayArray#,MutableArrayArray#)
import GHC.Exts (Any,MutableArray#,unsafeCoerce#,(==#),isTrue#,casArray#)
import GHC.IO (IO(..))
import Numeric (showIntAtBase)
import Socket.Error (die)
import Socket.Debug (debug,whenDebugging,debugging)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (Fd)

import qualified Control.Monad.STM as STM
import qualified Control.Concurrent.STM as STM
import qualified Linux.Epoll as Epoll
import qualified Control.Monad.Primitive as PM
import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM
import qualified GHC.Exts as Exts

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
-- This is best illustrated by example. The example has been removed.



-- | Register interest in reads and writes. After registering a socket,
-- use 'reader' and 'writer' to get access to the transactional variables
-- that describe the readiness of their corresponding channels. When
-- possible, register a file descriptor before doing whatever thing
-- may cause it to become ready. This is currently not important for
-- correctness (since the read and write channel optimistically start
-- out as ready). However, future optimizations may introduce
-- registration functions that let users specify if the channels
-- should start as ready or not ready.
--
-- Precondition: There is no existing registration for this file descriptor.
register ::
     Manager -- The event manager
  -> Fd -- File descriptor
  -> IO ()
register mngr@Manager{epoll} !fd = do
  (ixTier2, tier2) <- constructivelyLookupTier1 (fdToInt fd) mngr
  let ixRead = ixTier2 * 2
      ixWrite = ixRead + 1
  readVar <- readTVarArray tier2 ixRead
  writeVar <- readTVarArray tier2 ixWrite
  -- It should not be necessary to batch these in the same atomically
  -- since this function should only ever be called with exceptions
  -- masked. However, we do it anyway since it might improve performance.
  -- It's difficult to test this theory.
  atomically $ do
    STM.modifyTVar' readVar resetToken
    STM.modifyTVar' writeVar resetToken
  -- Enough space for a single registration.
  ev <- PM.newPrimArray 1
  debug ("register: registering fd " ++ show fd)
  PM.writePrimArray ev 0 $ Epoll.Event
    { Epoll.events = Epoll.input <> Epoll.output <> Epoll.edgeTriggered <> Epoll.readHangup
    , Epoll.payload = fd
    }
  e <- Epoll.uninterruptibleControlMutablePrimArray epoll Epoll.add fd ev
  case e of
    Left (Errno code) ->
      die $ "Socket.EventManager.register: epoll_ctl error " ++ show code
    Right () -> pure ()

-- HAHA: deregister has been eliminated.
-- This does not close the file descriptor. Call this function either
-- right before or right after closing the socket. It does not matter
-- which order they happen in. Be sure to mask exceptions when closing
-- the socket. It is important to ensure that an asynchronous exception
-- doesn't cause closing or deregistration to happen without the other
-- happening as well. Notice that this function does not call epoll_ctl.
-- Closing the file descriptor will cause epoll deregistration to happen.
-- deregister :: Manager -> Fd -> IO ()
-- deregister Manager{variables} !fd = do
--   (readVar,writeVar) <- lookupBoth (fdToInt fd) variables

-- Deregister insterest in reads and writes.
-- Precondition: A previous call to register has been made.
-- unregister

type MUArray = MutableUnliftedArray RealWorld

data Manager = Manager
  { variables :: !(MUArray (MUArray (TVar Token)))
  , novars :: !(MUArray (TVar Token))
    -- An empty mutable array. This array is used to mark the absense of
    -- a tier-two array of TVars.
  , epoll :: !Fd
  }

manager :: Manager
{-# noinline manager #-}
manager = unsafePerformIO $ do
  when (not rtsSupportsBoundThreads) $ do
    fail $ "Socket.Event.manager: threaded runtime required"
  !novars <- PM.unsafeNewUnliftedArray 0
  !variables <- PM.unsafeNewUnliftedArray 32
  let goX !ix = if ix >= 0
        then do
          writeMutableUnliftedArrayArray variables ix novars
          goX (ix - 1)
        else pure ()
  goX 32
  Epoll.uninterruptibleCreate1 Epoll.closeOnExec >>= \case
    Left (Errno code) ->
      die $ "Socket.EventManager.manager: epoll_create error code " ++ show code
    Right !epoll -> do
      -- Spawn a worker thread (for calling epoll_wait in the background)
      -- on each capability. Recall that since this is in a 
      -- noinline+unsafePerformIO setting, this only happens the
      -- first time the manager is accessed. These workers should
      -- continue to run forever. Nothing should be able to kill them
      -- since the thread IDs are discarded.
      capNum <- getNumCapabilities
      -- There should basically never be a non-positive number of
      -- capabilities, so we reserve this check for debugging
      -- situations.
      whenDebugging $ do
        when (capNum < 1) $ do
          die $ "Socket.EventManager.manager: non-positive number of capabilities"
      let go !ix = if ix > (-1)
            then do
              _ <- forkOn ix $ do
                let !initSz = if debugging then 1 else 8
                !initArr <- newPinnedPrimArray initSz
                loopManager initArr initSz epoll variables
              go (ix - 1)
            else pure ()
      -- In debugging mode, spawn a single event manager thread.
      go (if debugging then 0 else capNum)
      pure (Manager {variables,novars,epoll})

reader :: Manager -> Fd -> IO (TVar Token)
reader Manager{variables} !fd = lookupGeneric 0 (fdToInt fd) variables

writer :: Manager -> Fd -> IO (TVar Token)
writer Manager{variables} !fd = lookupGeneric 1 (fdToInt fd) variables

lookupBoth ::
     Int -- File descriptor
  -> MUArray (MUArray (TVar Token))
  -> IO (TVar Token,TVar Token)
lookupBoth !fd !arr = do
  let (ixTier1,ixTier2) = decompose fd
  tier2 <- readMutableUnliftedArrayArray arr ixTier1
  liftA2 (,)
    (readTVarArray tier2 (ixTier2 * 2))
    (readTVarArray tier2 (ixTier2 * 2 + 1))

-- The file descriptor must already be registered. Otherwise, this
-- function may look in an uninitialized tier-two array.
lookupGeneric ::
     Int -- Read: 0, Write: 1
  -> Int -- File descriptor
  -> MutableUnliftedArray RealWorld (MutableUnliftedArray RealWorld (TVar Token))
  -> IO (TVar Token)
lookupGeneric !rw !fd !arr = do
  let (ixTier1,ixTier2) = decompose fd
  tier2 <- readMutableUnliftedArrayArray arr ixTier1
  readTVarArray tier2 ((ixTier2 * 2) + rw)

constructivelyLookupTier1 ::
     Int -- File descriptor
  -> Manager
  -> IO (Int, MUArray (TVar Token))
constructivelyLookupTier1 !fd Manager{variables,novars} = do
  let (ixTier1,ixTier2) = decompose fd
  varsTier2 <- readMutableUnliftedArrayArray variables ixTier1
  if PM.sameMutableUnliftedArray varsTier2 novars
    then do
      -- We want 2 * 2^N tvars because there is a separate read and
      -- write tvar for every file descriptor.
      let !len = exp2succ ixTier1
      varsAttempt <- PM.unsafeNewUnliftedArray len
      let goVars !ix = if ix > (-1)
            then do
              writeTVarArray varsAttempt ix
                =<< STM.newTVarIO emptyToken
              goVars (ix - 1)
            else pure ()
      goVars (len - 1)
      -- We ignore the success of casUnliftedArray. It does not actually
      -- matter whether or not it succeeded. If it failed, some other
      -- thread must have initialized the tier 2 arrays.
      (success,tier2) <- casMutableUnliftedArrayArray variables ixTier1 novars varsAttempt
      debug ("constructivelyLookupTier1: Created tier 2 array of length " ++ show len ++ " at index " ++ show ixTier1 ++ " with success " ++ show success)
      pure (ixTier2,tier2)
    else pure (ixTier2,varsTier2)

loopManager :: 
     MutablePrimArray RealWorld (Epoll.Event 'Epoll.Response Fd)
  -> Int -- size of events buffer
  -> Fd -- epoll file descriptor
  -> MUArray (MUArray (TVar Token)) -- tier 1 variables array
  -> IO ()
loopManager !evs0 !sz0 !epfd !tier1 = do
  yield
  (!evs1, !sz1) <- stepManager evs0 sz0 epfd tier1
  loopManager evs1 sz1 epfd tier1

stepManager ::
     MutablePrimArray RealWorld (Epoll.Event 'Epoll.Response Fd)
  -> Int -- size of events buffer
  -> Fd -- epoll file descriptor
  -> MUArray (MUArray (TVar Token)) -- tier 1 variables array
  -> IO (MutablePrimArray RealWorld (Epoll.Event 'Epoll.Response Fd),Int)
     -- returns new events buffer and its size
stepManager !evs0 !sz0 !epfd !tier1 = do
  Epoll.uninterruptibleWaitMutablePrimArray epfd evs0 (intToCInt sz0) >>= \case
    Left (Errno code) -> die $ "Socket.EventManager.stepManager: A " ++ show code
    Right len0 -> if len0 > 0
      then handleEvents evs0 (cintToInt len0) sz0 tier1
      else do
        debug "stepManager: first attempt returned no events"
        yield
        Epoll.uninterruptibleWaitMutablePrimArray epfd evs0 (intToCInt sz0) >>= \case
          Left (Errno code) -> die $ "Socket.EventManager.stepManager: B " ++ show code
          Right len1 -> if len1 > 0
            then do
              debug "stepManager: second attempt succeeded"
              handleEvents evs0 (cintToInt len1) sz0 tier1
            else do
              debug "stepManager: second attempt returned no events"
              whenDebugging $ do
                actualSize <- PM.getSizeofMutablePrimArray evs0
                when (actualSize /= sz0) (die "stepManager: bad size")
              Epoll.waitMutablePrimArray epfd evs0 (intToCInt sz0) (-1) >>= \case
                Left (Errno code) -> die $ "Socket.EventManager.stepManager: C " ++ show code
                Right len2 -> if len2 > 0
                  then do
                    whenDebugging $ do
                      let !(MutablePrimArray evs0#) = evs0
                      let untypedEvs0 = MutableByteArray evs0#
                      debug ("stepManager: third attempt succeeded, len=" ++ show len2 ++ ",sz=" ++ show sz0)
                      (w0 :: Word32) <- PM.readByteArray untypedEvs0 0
                      (w1 :: Word32) <- PM.readByteArray untypedEvs0 1
                      (w2 :: Word32) <- PM.readByteArray untypedEvs0 2
                      debug $ "stepManager: element 0 raw after third attempt " ++ 
                        lpad 32 (showIntAtBase 2 binChar w0 "") ++ " " ++
                        lpad 32 (showIntAtBase 2 binChar w1 "") ++ " " ++
                        lpad 32 (showIntAtBase 2 binChar w2 "")
                      when (sz0 > 1) $ do
                        (w0a :: Word32) <- PM.readByteArray untypedEvs0 3
                        (w1a :: Word32) <- PM.readByteArray untypedEvs0 4
                        (w2a :: Word32) <- PM.readByteArray untypedEvs0 5
                        debug $ "stepManager: element 1 raw after third attempt " ++ 
                          lpad 32 (showIntAtBase 2 binChar w0a "") ++ " " ++
                          lpad 32 (showIntAtBase 2 binChar w1a "") ++ " " ++
                          lpad 32 (showIntAtBase 2 binChar w2a "")
                    handleEvents evs0 (cintToInt len2) sz0 tier1
                  else die $ "Socket.EventManager.stepManager: D"

lpad :: Int -> String -> String
lpad m xs = replicate (m - length ys) '0' ++ ys
  where ys = take m xs

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
  -> MUArray (MUArray (TVar Token)) -- tier 1 variables array
  -> IO (MutablePrimArray RealWorld (Epoll.Event 'Epoll.Response Fd),Int)
     -- returns new events buffer and its size
handleEvents !evs !len !sz !vars = do
  traverseMutablePrimArray_
    ( \(Epoll.Event{Epoll.events,Epoll.payload}) -> do
      let fd = payload
      let hasReadInclusive = Epoll.containsAnyEvents events
            (Epoll.input <> Epoll.readHangup <> Epoll.error <> Epoll.hangup)
      let hasPersistentReadInclusive = Epoll.containsAnyEvents events Epoll.readHangup
      let hasWriteInclusive = Epoll.containsAnyEvents events
            (Epoll.output <> Epoll.error <> Epoll.hangup)
      let hasRead = Epoll.containsAnyEvents events Epoll.input
      let hasReadHangup = Epoll.containsAnyEvents events Epoll.readHangup
      let hasWrite = Epoll.containsAnyEvents events Epoll.output
      let hasHangup = Epoll.containsAnyEvents events Epoll.hangup
      let hasError = Epoll.containsAnyEvents events Epoll.error
      whenDebugging $ do
        let hasPriority = Epoll.containsAnyEvents events Epoll.priority
        let Epoll.Events e = events
        debug $
          "handleEvents: fd " ++ show fd  ++
          " bitmask " ++ showIntAtBase 2 binChar e "" ++ " read [" ++ show hasRead ++
          "] write [" ++ show hasWrite ++ "] hangup [" ++ show hasHangup ++
          "] readHangup [" ++ show hasReadHangup ++
          "] error [" ++ show hasError ++
          "] priority [" ++ show hasPriority ++ "]"
      (readVar,writeVar) <- lookupBoth (fdToInt fd) vars
      when hasReadInclusive $ atomically $ do
        old <- STM.readTVar readVar
        let !new = if hasPersistentReadInclusive
              then persistentReadyToken old
              else readyToken old
        STM.writeTVar readVar new
      when hasWriteInclusive $ atomically $ STM.modifyTVar' writeVar readyToken
    ) evs 0 len
  if | len < sz -> pure (evs,sz)
     | len == sz -> do
        let newSz = sz * 2
        debug ("handleEvents: doubling size of array to " ++ show newSz)
        newBuf <- newPinnedPrimArray newSz
        pure (newBuf,newSz)
     | otherwise -> die "Socket.EventManager.handleEvents: len > sz"

binChar :: Int -> Char
binChar = \case
  0 -> '0'
  1 -> '1'
  _ -> 'x'

traverseMutablePrimArray_ ::
     Prim a
  => (a -> IO ())
  -> MutablePrimArray RealWorld a
  -> Int -- offset
  -> Int -- end
  -> IO ()
{-# inline traverseMutablePrimArray_ #-}
traverseMutablePrimArray_ f a off end = go off where
  go !ix = if ix < end
    then do
      debug ("traverseMutablePrimArray_: index " ++ show ix)
      f =<< PM.readPrimArray a ix
      go (ix + 1)
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
-- *  0 => (0,0)
-- *  1 => (1,0)
-- *  2 => (1,1)
-- *  3 => (2,0)
-- *  4 => (2,1)
-- *  5 => (2,2)
-- *  6 => (2,3)
-- *  7 => (3,0)
-- *  8 => (3,1)
-- *  9 => (3,2)
-- * 10 => (3,3)
-- * 11 => (3,4)
-- * 12 => (3,5)
-- * 13 => (3,6)
-- * 14 => (3,7)
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
--     { ready :: Bool, pready :: Bool, interrupt :: Bool, eventCount :: Word61 }
-- Invariant: the bytearray has length 8.
-- The descriptor counter and the event counter are represented in
-- the predictable way. The readiness bit is the highest bit. Visually:
--   |WXYZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ
-- W: readiness (1 is ready, 0 is not ready)
-- X: persistent readiness (only used for read channel, set by EPOLLRDHUP)
-- Y: interruptness (only used for interruptible waiting)
-- Z: event counter
--
-- Since a 62-bit word has so many inhabitants, we pretend that it will
-- never wrap around. In practice, an application would need to run for
-- trillions of years for overflow to happen.
--
-- The notion of persistent readiness is only relevant when dealing with
-- the read channel. Why does it even exist? Consider the following two
-- desirable behaviors:
-- 
-- 1. Avoid unneeded @recv@ syscalls. That is, if a @recv@ returns
--    something less that the full number of bytes requested, we want
--    to unready the token. The next time we want to @recv@, we want to wait
--    for the token to be ready before even attempting a @recv@.
-- 2. We want to be properly notification when the peer shuts down. Epoll
--    reports this as EPOLLRDHUP.
--
-- The difficulty is that, without persistent readiness, performing
-- optimization (1), can lead to a hung application. So, we introduce a
-- persistent readiness. This is set when the EPOLLRDHUP notification
-- is delivered. It is only allowed to be reset when a @recv@ returns
-- EAGAIN, not by merely not receiving enough bytes. One might ask:
-- Why should it ever need to be reset? Recall that because of the way
-- this event manager is designed, it is always possible to receive
-- a notification that was intended for a previous user of the
-- file descriptor.
newtype Token = Token Word64

readyBit :: Word64
readyBit = 0x8000000000000000

persistentReadyBits :: Word64
persistentReadyBits = 0xC000000000000000

-- Preserves persistent readiness and interruptedness
unreadyBit :: Word64
unreadyBit = 0x7FFFFFFFFFFFFFFF

eqToken :: Token -> Token -> Bool
eqToken (Token a) (Token b) = a == b

-- The empty token has readiness set to true and persistent readiness
-- set to false.
emptyToken :: Token
emptyToken = Token readyBit

interruptToken :: Token
interruptToken = Token 0x2000000000000000

isTokenReady :: Token -> Bool
isTokenReady (Token w) = unsafeShiftR w 62 /= 0

isInterrupt :: Token -> Bool
isInterrupt (Token w) = (0x2000000000000000 == w)

-- Increments the event counter. Sets readiness to true.
-- Leaves persistent readiness alone. Leaves interruptedness
-- alone.
readyToken :: Token -> Token
readyToken (Token w) = Token (readyBit .|. (w + 1))

persistentReadyToken :: Token -> Token
persistentReadyToken (Token w) = Token (persistentReadyBits .|. (w + 1))

-- Increments the event counter. Sets readiness to true. Sets
-- persistent readiness and interruptness to false.
resetToken :: Token -> Token
resetToken (Token w) = Token ((readyBit .|. (w + 1)) .&. 0x9FFFFFFFFFFFFFFF)

-- | Sets the readiness to false. Leaves the persisted readiness alone.
-- Does not affect the interruptedness. Increments the event counter.
unreadyToken :: Token -> Token
unreadyToken (Token w) = Token (unreadyBit .&. (w + 1))

-- | Sets the readiness, the persisted readiness, and the interruptedness
-- to false. Increments the event counter.
persistentUnreadyToken :: Token -> Token
persistentUnreadyToken (Token w) = Token (0x1FFFFFFFFFFFFFFF .&. (w + 1))

-- | Why does 'unready' need the previous token value. At first glance,
-- it seems that it would suffice to simply set something to false
-- and be done with it. However, this runs into a subtle race condition.
-- What if an @epoll_wait@ worker thread discovered that the file
-- descriptor was ready for reads right before 'unready' was called?
-- We take the old token value so that we can check to see if anything 
-- has changed since we last checked in. If that's the case, this
-- function aborts, leaving whatever the most recent call to
-- @epoll_wait@ had done in tact.
unready ::
     Token -- ^ Token provided by previous call to wait
  -> TVar Token -- ^ Transactional variable for readiness
  -> IO ()
unready !oldToken !tv = atomically $ do
  newToken <- STM.readTVar tv
  if eqToken oldToken newToken
    then STM.writeTVar tv $! unreadyToken oldToken
    else pure ()

persistentUnready ::
     Token -- ^ Token provided by previous call to wait
  -> TVar Token -- ^ Transactional variable for readiness
  -> IO ()
persistentUnready !oldToken !tv = atomically $ do
  newToken <- STM.readTVar tv
  if eqToken oldToken newToken
    then STM.writeTVar tv $! persistentUnreadyToken oldToken
    else pure ()

unreadyAndWait ::
     Token -- ^ Token provided by previous call to wait
  -> TVar Token -- ^ Transactional variable for readiness
  -> IO Token -- ^ New token
unreadyAndWait !oldToken !tv = do
  unready oldToken tv
  wait tv

persistentUnreadyAndWait ::
     Token -- ^ Token provided by previous call to wait
  -> TVar Token -- ^ Transactional variable for readiness
  -> IO Token -- ^ New token
persistentUnreadyAndWait !oldToken !tv = do
  persistentUnready oldToken tv
  wait tv

-- | Wait until the token indicates readiness. Keep in mind that
-- false positives are possible. When a false positive happens,
-- use 'unready' and then 'wait' again. Keep doing this until
-- the file descriptor is actually ready for reads/writes.
wait :: TVar Token -> IO Token
wait !tv = do
  !token0@(Token val) <- STM.readTVarIO tv
  debug $ "wait: initial token value " ++ (lpad 64 (showIntAtBase 2 binChar val ""))
  if isTokenReady token0
    then pure token0
    else atomically $ do
      token1 <- STM.readTVar tv
      STM.check (isTokenReady token1)
      pure token1

interruptibleWait ::
     TVar Bool -- ^ Interrupt
  -> TVar Token
  -> IO Token
interruptibleWait !interrupt !tv = do
  -- We make an effort to avoid a transaction if possible,
  -- calling readTVarIO on both variables.
  STM.readTVarIO interrupt >>= \case
    True -> pure interruptToken
    False -> do
      token0 <- STM.readTVarIO tv
      if isTokenReady token0
        then pure token0
        else do
          atomically $
            ( do STM.check =<< STM.readTVar interrupt
                 pure interruptToken
            ) <|>
            ( do token1 <- STM.readTVar tv
                 STM.check (isTokenReady token1)
                 pure token1
            )

interruptibleWaitCounting :: TVar Int -> TVar Bool -> TVar Token -> IO Token
interruptibleWaitCounting !counter !interrupt !tv = atomically $
  -- We cannot go to the same lengths to avoid a transaction as
  -- we do in interruptibleWait. Notablely, the token check and
  -- the counter increment must happen in a transaction together.
  ( do STM.check =<< STM.readTVar interrupt
       pure interruptToken
  ) <|>
  ( do token1 <- STM.readTVar tv
       STM.check (isTokenReady token1)
       STM.modifyTVar' counter (+1)
       pure token1
  )

-- Not yet present in primitive library.
newPinnedPrimArray :: forall a. Prim a
  => Int -> IO (MutablePrimArray RealWorld a)
{-# INLINE newPinnedPrimArray #-}
newPinnedPrimArray (I# n#)
  = PM.primitive (\s# -> case Exts.newPinnedByteArray# (n# *# PM.sizeOf# (undefined :: a)) s# of
      (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #))

-- This can be unsound if the result is passed to the FFI. Fortunately,
-- we do not do that with the result.
readTVarArray :: forall a.
     MutableUnliftedArray RealWorld (TVar a) -- ^ source
  -> Int -- ^ index
  -> IO (TVar a)
readTVarArray (MutableUnliftedArray maa#) (I# i#)                       
  = PM.primitive $ \s -> case Exts.readArrayArrayArray# maa# i# s of                
      (# s', aa# #) -> (# s', TVar ((unsafeCoerce# :: ArrayArray# -> TVar# RealWorld a) aa#) #)                       
readMutableUnliftedArrayArray                                                           
  :: MutableUnliftedArray RealWorld (MutableUnliftedArray RealWorld a) -- ^ source
  -> Int -- ^ index
  -> IO (MutableUnliftedArray RealWorld a)
readMutableUnliftedArrayArray (MutableUnliftedArray maa#) (I# i#)                       
  = PM.primitive $ \s -> case Exts.readArrayArrayArray# maa# i# s of                
      (# s', aa# #) -> (# s', MutableUnliftedArray ((unsafeCoerce# :: ArrayArray# -> MutableArrayArray# RealWorld) aa#) #)                       

-- See readTVarArray
writeTVarArray :: forall a.
     MutableUnliftedArray RealWorld (TVar a) -- ^ destination
  -> Int -- ^ index
  -> TVar a -- ^ value
  -> IO ()
writeTVarArray (PM.MutableUnliftedArray maa#) (I# i#) (TVar a)
  = PM.primitive_ (Exts.writeArrayArrayArray# maa# i# ((unsafeCoerce# :: TVar# RealWorld a -> ArrayArray#) a))

writeMutableUnliftedArrayArray :: forall a.
     MutableUnliftedArray RealWorld (MutableUnliftedArray RealWorld a) -- ^ source
  -> Int -- ^ index
  -> MutableUnliftedArray RealWorld a -- ^ value
  -> IO ()
writeMutableUnliftedArrayArray (PM.MutableUnliftedArray maa#) (I# i#) (MutableUnliftedArray a)
  = PM.primitive_ (Exts.writeArrayArrayArray# maa# i# ((unsafeCoerce# :: MutableArrayArray# RealWorld -> ArrayArray#) a))

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

casMutableUnliftedArrayArray ::
     MutableUnliftedArray RealWorld (MutableUnliftedArray RealWorld a) -- ^ array
  -> Int -- ^ index
  -> (MutableUnliftedArray RealWorld a) -- ^ expected old value
  -> (MutableUnliftedArray RealWorld a) -- ^ new value
  -> IO (Bool,MutableUnliftedArray RealWorld a)
{-# INLINE casMutableUnliftedArrayArray #-}
casMutableUnliftedArrayArray (MutableUnliftedArray arr#) (I# i#) (MutableUnliftedArray old) (MutableUnliftedArray new) =
  -- All of this unsafeCoercing is really nasty business. This will go away
  -- once https://github.com/ghc-proposals/ghc-proposals/pull/203 happens.
  -- Also, this is unsound if the result is immidiately consumed by
  -- the FFI.
  IO $ \s0 ->
    let !uold = (unsafeCoerce# :: MutableArrayArray# RealWorld -> Any) old
        !unew = (unsafeCoerce# :: MutableArrayArray# RealWorld -> Any) new
     in case casArray# ((unsafeCoerce# :: MutableArrayArray# RealWorld -> MutableArray# RealWorld Any) arr#) i# uold unew s0 of
          (# s1, n, ur #) -> (# s1, (isTrue# (n ==# 0# ),MutableUnliftedArray ((unsafeCoerce# :: Any -> MutableArrayArray# RealWorld) ur)) #)

