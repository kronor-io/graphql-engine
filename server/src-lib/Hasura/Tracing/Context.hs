{-# LANGUAGE OverloadedRecordDot #-}
module Hasura.Tracing.Context
  ( TraceContext (..),
    TraceMetadata,
    SpanStatus (..),
    toLoggableFields,
  )
where

import Data.Aeson qualified as J
import Data.Char qualified
import Hasura.Prelude
import Hasura.Tracing.Sampling
import Hasura.Tracing.TraceId
import Hasura.Tracing.TraceState (TraceState)
import OpenTelemetry.Trace.Core qualified as OpenTelemetry
import OpenTelemetry.Trace.Id as OpenTelemetry (Base (..), spanIdBaseEncodedText, traceIdBaseEncodedText)
import OpenTelemetry.Context.ThreadLocal qualified as OpenTelemetry
import OpenTelemetry.Context qualified as OpenTelemetry
import Data.Text qualified as T

-- | The status of a span, corresponding to the OpenTelemetry span status.
-- https://opentelemetry.io/docs/specs/otel/trace/api/#set-status
--
-- Spans are 'SpanStatusUnset' by default. Set to 'SpanStatusError' (with an
-- optional description) when the span represents a failed operation.
data SpanStatus
  = SpanStatusUnset
  | SpanStatusError Text
  deriving (Eq, Show)

-- | The status of a span, corresponding to the OpenTelemetry span status.
-- https://opentelemetry.io/docs/specs/otel/trace/api/#set-status
--
-- Spans are 'SpanStatusUnset' by default. Set to 'SpanStatusError' (with an
-- optional description) when the span represents a failed operation.
data SpanStatus
  = SpanStatusUnset
  | SpanStatusError Text
  deriving (Eq, Show)

-- | Any additional human-readable key-value pairs relevant to the execution of
-- a span.
--
-- When the Open Telemetry exporter is in use these become attributes. Where
-- possible and appropriate, consider using key names from the documented OT
-- semantic conventions here:
-- https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/
-- This can serve to document the metadata, even for users not using open telemetry.
--
-- We may make this type more closely align with the OT data model in the future
-- (e.g. supporting int, etc)
type TraceMetadata = [(Text, Text)]

-- | A trace context records the current active trace, the active span
-- within that trace, and the span's parent, unless the current span
-- is the root. This is like a call stack.
data TraceContext = TraceContext
  { tcTracer :: OpenTelemetry.Tracer,
    tcCurrentTrace :: TraceId,
    tcCurrentSpan :: SpanId,
    tcCurrentParent :: Maybe SpanId,
    tcSamplingState :: SamplingState,
    -- Optional vendor-specific trace identification information across different distributed tracing systems.
    -- It's used for the W3C Trace Context only https://www.w3.org/TR/trace-context/#tracestate-header
    tcStateState :: TraceState
  }

toLoggableFields :: TraceContext -> IO J.Value
toLoggableFields _ = do
      mSpan <- OpenTelemetry.lookupSpan <$> OpenTelemetry.getContext
      case mSpan of
        Nothing -> return $ J.object []
        Just sp -> do
          ctxt <- OpenTelemetry.getSpanContext sp

          let tId = hexToDec (T.takeEnd 16 (traceIdBaseEncodedText Base16 ctxt.traceId))
          let sId = hexToDec (spanIdBaseEncodedText Base16 ctxt.spanId)
          return $ J.object
                  [ ("trace_id", J.String (tshow tId))
                  , ("span_id", J.String (tshow sId))
                  ]
  where
    -- Convert a hex string to a decimal number
    hexToDec :: Text -> Integer
    hexToDec = foldr (\c s -> s * 16 + c) 0 . reverse . map (fromIntegral . Data.Char.digitToInt) . T.unpack

-- Should this be here? This implicitly ties Tracing to the name of fields in HTTP headers.
-- instance J.ToJSON TraceContext where
--   toJSON TraceContext {..} =
--     let idFields =
--           [ "trace_id" .= bsToTxt (traceIdToHex tcCurrentTrace),
--             "span_id" .= bsToTxt (spanIdToHex tcCurrentSpan)
--           ]
--         samplingFieldMaybe =
--           samplingStateToHeader @Text tcSamplingState <&> \t ->
--             "sampling_state" .= t
--      in J.object $ idFields ++ maybeToList samplingFieldMaybe
