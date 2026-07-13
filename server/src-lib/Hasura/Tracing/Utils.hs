-- | This module contains a collection of utility functions we use with tracing
-- throughout the codebase, but that are not a core part of the library. If we
-- were to move tracing to a separate library, those functions should be kept
-- here in the core engine code.
module Hasura.Tracing.Utils
  ( traceHTTPRequest,
    attachSourceConfigAttributes,
    composedPropagator,
  )
where

import Control.Lens
import Data.CaseInsensitive qualified as CI
import Data.String
import Data.Text.Extended (toTxt)
import Hasura.Prelude
import Hasura.RQL.Types.SourceConfiguration (HasSourceConfiguration (..))
import Hasura.Tracing.Class
import Hasura.Tracing.Context
import Hasura.Tracing.Propagator (HttpPropagator)
import Hasura.Tracing.Propagator.B3 (b3TraceContextPropagator)
import Hasura.Tracing.Propagator.W3CTraceContext (w3cTraceContextPropagator)
import Hasura.Tracing.TraceId (SpanKind (SKClient))
import Network.HTTP.Client.Transformable qualified as HTTP
import OpenTelemetry.Context.ThreadLocal qualified as OpenTelemetry
import OpenTelemetry.Propagator qualified as Propagator
import OpenTelemetry.Trace.Core qualified as OpenTelemetry

-- | Wrap the execution of an HTTP request in a span in the current
-- trace. Despite its name, this function does not start a new trace, and the
-- span will therefore not be recorded if the surrounding context isn't traced
-- (see 'spanWith').
--
-- Additionally, this function adds metadata regarding the request to the
-- created span, and injects the trace context into the HTTP header.
traceHTTPRequest ::
  (MonadIO m, MonadTrace m) =>
  HttpPropagator ->
  -- | http request that needs to be made
  HTTP.Request ->
  -- | a function that takes the traced request and executes it
  (HTTP.Request -> m a) ->
  m a
traceHTTPRequest _propagator req f = do
  let method = bsToTxt (view HTTP.method req)
      uri = view HTTP.url req
  newSpan (method <> " " <> uri) SKClient do
    maybeTraceContext <- currentContext
    case maybeTraceContext of
      Nothing -> f req
      Just traceContext -> do
        -- Propagate the live OpenTelemetry context (real trace/span ids), not
        -- the hasura TraceContext (kronor fixes its ids). hs-opentelemetry
        -- 1.0.0.0's inject writes into a TextMap, so project that back to headers.
        let otelPropagator = OpenTelemetry.getTracerProviderPropagators $ OpenTelemetry.getTracerTracerProvider $ tcTracer traceContext
            reqBytes = HTTP.getReqSize req
        otelContext <- liftIO OpenTelemetry.getContext
        injected <- liftIO $ Propagator.inject otelPropagator otelContext Propagator.emptyTextMap
        let headers = [(CI.mk (txtToBs k), txtToBs v) | (k, v) <- Propagator.textMapToList injected]
        attachMetadata [
            ("http.request.body.size", fromString (show reqBytes))
          , ("http.request.method", method)
          , ("http.request.uri", uri)
          , ("span.type", "http")
          , ("span.kind", "client")
          ]
        f $ over HTTP.headers (headers <>) req

attachSourceConfigAttributes :: forall b m. (HasSourceConfiguration b, MonadTrace m) => SourceConfig b -> m ()
attachSourceConfigAttributes sourceConfig = do
  let backendSourceKind = sourceConfigBackendSourceKind @b sourceConfig
  attachMetadata [("source.kind", toTxt $ backendSourceKind)]

-- | Propagator composition of Trace Context and ZipKin B3.
composedPropagator :: HttpPropagator
composedPropagator = b3TraceContextPropagator <> w3cTraceContextPropagator
