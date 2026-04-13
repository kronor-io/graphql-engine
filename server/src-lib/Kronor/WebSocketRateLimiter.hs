{-# LANGUAGE NumericUnderscores #-}

-- | Per-connection WebSocket message rate limiter using a sliding window counter.
--
-- Each WebSocket connection gets its own 'RateLimiterState' (an IORef).
-- On each message received, 'checkRateLimit' is called. If the number of
-- messages in the current window exceeds the configured limit, it returns
-- 'RateLimitExceeded'. Otherwise it records the message and returns
-- 'RateLimitOk'.
module Kronor.WebSocketRateLimiter
  ( RateLimiterState,
    RateLimitResult (..),
    RateLimitConfig (..),
    newRateLimiterState,
    checkRateLimit,
  )
where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import GHC.Clock (getMonotonicTimeNSec)
import Hasura.Prelude

-- | Configuration for the rate limiter.
data RateLimitConfig = RateLimitConfig
  { -- | Maximum number of messages allowed per window.
    rlcMaxMessages :: !Int,
    -- | Window size in seconds.
    rlcWindowSeconds :: !Int
  }

-- | Internal state for a single connection's rate limiter.
data WindowState = WindowState
  { -- | Start of the current window in nanoseconds (monotonic clock).
    wsWindowStart :: !Word64,
    -- | Number of messages received in the current window.
    wsMessageCount :: !Int
  }

-- | Opaque handle to per-connection rate limiter state.
newtype RateLimiterState = RateLimiterState (IORef WindowState)

-- | Result of a rate limit check.
data RateLimitResult
  = RateLimitOk
  | RateLimitExceeded

-- | Create a new rate limiter state for a connection.
newRateLimiterState :: IO RateLimiterState
newRateLimiterState = do
  now <- getMonotonicTimeNSec
  RateLimiterState <$> newIORef (WindowState now 0)

-- | Check whether a message should be allowed or rejected.
-- If the current window has expired, resets the counter.
-- Returns 'RateLimitExceeded' if the message count exceeds the limit.
checkRateLimit :: RateLimitConfig -> RateLimiterState -> IO RateLimitResult
checkRateLimit RateLimitConfig {..} (RateLimiterState ref) = do
  now <- getMonotonicTimeNSec
  atomicModifyIORef' ref $ \ws ->
    let windowNs = fromIntegral rlcWindowSeconds * 1_000_000_000
        elapsed = now - wsWindowStart ws
     in if elapsed >= windowNs
          then -- Window expired, start a new one with this message counted
            (WindowState now 1, RateLimitOk)
          else
            let newCount = wsMessageCount ws + 1
             in if newCount > rlcMaxMessages
                  then (ws, RateLimitExceeded)
                  else (ws {wsMessageCount = newCount}, RateLimitOk)
