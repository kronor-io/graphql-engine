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
import Data.String
import Data.Text.Extended (toTxt)
import Hasura.Prelude
import Hasura.RQL.Types.SourceConfiguration (HasSourceConfiguration (..))
import Hasura.Tracing.Class
import Hasura.Tracing.Context
import Hasura.Tracing.Propagator (HttpPropagator)
import Hasura.Tracing.TraceId (SpanKind (SKClient))
import Network.HTTP.Client.Transformable qualified as HTTP
import OpenTelemetry.Trace.Core qualified as OpenTelemetry
import OpenTelemetry.Context.ThreadLocal qualified as OpenTelemetry
import OpenTelemetry.Propagator qualified as Propagator

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
        let propagator = OpenTelemetry.getTracerProviderPropagators $ OpenTelemetry.getTracerTracerProvider $ tcTracer traceContext
        let reqBytes = HTTP.getReqSize req
        context <- liftIO OpenTelemetry.getContext
        headers <- Propagator.inject propagator context []
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
