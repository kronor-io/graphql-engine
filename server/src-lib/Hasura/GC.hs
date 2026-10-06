-- safety for unsafePerformIO below
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Hasura.GC
  ( ourIdleGC,
    requestHeapShrink,
    shrinkHeapOnRequest,
  )
where

import Control.Concurrent.Extended qualified as C
import Control.Concurrent.STM qualified as STM
import Data.SerializableBlob qualified as SB
import GHC.Stats
import Hasura.Logging
import Hasura.Prelude
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performMajorGC, performMinorGC)

-- | The RTS's idle GC doesn't work for us:
--
--    - when `-I` is too low it may fire continuously causing scary high CPU
--      when idle among other issues (see #2565)
--    - when we set it higher it won't run at all leading to memory being
--      retained when idle (especially noticeable when users are benchmarking and
--      see memory stay high after finishing). In the theoretical worst case
--      there is such low haskell heap pressure that we never run finalizers to
--      free the foreign data from e.g. libpq.
--    - as of GHC 8.10.2 we have access to `-Iw`, but those two knobs still
--      don’t give us a guarantee that a major GC will always run at some
--      minumum frequency (e.g. for finalizers)
--
-- ...so we hack together our own using GHC.Stats, which should have
-- insignificant runtime overhead.
--
-- NOTE: as always the cost of a major GC (forced here, or initiated by the RTS)
-- with the default copying collector is proportional to live (non-garbage)
-- heap data. Tune parameters here to balance: more frequent GC pauses vs.
-- prompt cleanup of foreign data (which does not exert GC pressure).
--
-- NOTE: larger nursery size (+RTS -A) may help us run more finalizers during
-- cheaper minor GCs, before they are promoted, making it feasible (maybe) to
-- run this with longer interval parameters.
ourIdleGC ::
  Logger Hasura ->
  -- | Run a major GC when we've been "idle" for idleInterval
  DiffTime ->
  -- | ...as long as it has been > minGCInterval time since the last major GC
  DiffTime ->
  -- | Additionally, if it has been > maxNoGCInterval time, force a GC regardless.
  DiffTime ->
  IO void
ourIdleGC (Logger logger) idleInterval minGCInterval maxNoGCInterval =
  startTimer >>= go 0 0 False
  where
    go gcs_prev major_gcs_prev lastIterationPerformedGC timerSinceLastMajorGC = do
      timeSinceLastGC <- timerSinceLastMajorGC
      when (timeSinceLastGC < minGCInterval) $ do
        -- no need to check idle until we've passed the minGCInterval:
        C.sleep (minGCInterval - timeSinceLastGC)

      RTSStats {gcs, major_gcs} <- getRTSStats
      -- We use minor GCs as a proxy for "activity", which seems to work
      -- well-enough (in tests it stays stable for a few seconds when we're
      -- logically "idle" and otherwise increments quickly)
      let areIdle = gcs == gcs_prev
          areOverdue = timeSinceLastGC > maxNoGCInterval

      if
        -- a major GC was run since last iteration (cool!), reset timer:
        | major_gcs > major_gcs_prev -> do
            startTimer >>= go gcs major_gcs False

        -- we are idle and its a good time to do a GC, or we're overdue and must run a GC:
        | areIdle || areOverdue -> do
            -- If we performed a GC last time and nothing was promoted meantime
            -- (minor GCs are the same) running a cheaper minor GC should
            -- suffice to perform any new due finalizers:
            if lastIterationPerformedGC && areIdle
              then do
                performMinorGC
                startTimer >>= go (gcs + 1) major_gcs True
              else do
                when (areOverdue && not areIdle)
                  $ logger
                  $ UnstructuredLog LevelInfo
                  $ "Overdue for a major GC: forcing one even though we don't appear to be idle"
                performMajorGC
                startTimer >>= go (gcs + 1) (major_gcs + 1) True

        -- else keep the timer running, waiting for us to go idle:
        | otherwise -> do
            C.sleep idleInterval
            go gcs major_gcs False timerSinceLastMajorGC

-- | Ask 'shrinkHeapOnRequest' to hand the memory left over from a schema cache
-- rebuild back to the OS. Cheap and idempotent: requests made while a shrink
-- is pending or running coalesce into one.
requestHeapShrink :: IO ()
requestHeapShrink = STM.atomically $ STM.writeTVar heapShrinkRequested True

heapShrinkRequested :: STM.TVar Bool
{-# NOINLINE heapShrinkRequested #-}
heapShrinkRequested = unsafePerformIO $ STM.newTVarIO False

-- | Return the memory a schema cache rebuild leaves behind.
--
-- While a rebuild runs, the old and the new schema cache are both live, so the
-- heap grows to fit both. Once the old one is dropped the RTS should give the
-- surplus back, but it only does so gradually: the memory it keeps after a
-- major GC is scaled by @-F@, and that factor decays (at the rate set by
-- @-Fd@) only across consecutive major GCs that were not triggered by
-- allocation. Under live traffic most major GCs are allocation-triggered and
-- reset the decay, so after a metadata apply the heap stays sized for the
-- rebuild's peak indefinitely: roughly 4x the live data instead of 2x.
--
-- So after each schema cache swap, run spaced major GCs ourselves until the
-- RTS stops returning memory (or 'maxGCs' is reached). Each one is a full
-- stop-the-world collection, hence the spacing.
shrinkHeapOnRequest ::
  Logger Hasura ->
  -- | Pause between consecutive forced major GCs
  DiffTime ->
  -- | Upper bound on forced major GCs per shrink
  Int ->
  IO void
shrinkHeapOnRequest (Logger logger) spacing maxGCs = forever do
  STM.atomically do
    STM.readTVar heapShrinkRequested >>= STM.check
    STM.writeTVar heapShrinkRequested False
  before <- memInUse
  after <- go maxGCs before
  logger
    $ UnstructuredLog LevelInfo
    $ SB.fromText
    $ "Heap shrink after schema cache update: "
    <> tshow (before `div` mib)
    <> " MiB -> "
    <> tshow (after `div` mib)
    <> " MiB in use"
  where
    mib = 1024 * 1024
    memInUse = gcdetails_mem_in_use_bytes . gc <$> getRTSStats
    go 0 current = pure current
    go n current = do
      C.sleep spacing
      performMajorGC
      next <- memInUse
      -- The RTS returns whole megablocks only; once a GC frees less than a
      -- couple of them the decay has bottomed out.
      if next + 2 * mib > current
        then pure next
        else go (n - 1) next
