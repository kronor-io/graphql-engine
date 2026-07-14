{-# LANGUAGE OverloadedRecordDot #-}

module Kronor.OpenTelemetryReporter
  ( openTelemetryReporter,
    initializeTracer,
    shutdownTracer,
  )
where

import Data.Bifunctor qualified
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Hasura.Prelude
import Hasura.Tracing.Reporter (Reporter (..))
import OpenTelemetry.Context qualified as OpenTelemetry
import OpenTelemetry.Context.ThreadLocal qualified as OpenTelemetry
import OpenTelemetry.Resource
import OpenTelemetry.Trace as OpenTelemetry

openTelemetryReporter :: OpenTelemetry.Tracer -> Reporter
openTelemetryReporter tracer = Reporter \_context spanName _spanKind getMetadata _getStatus action -> do
  threadContext <- OpenTelemetry.getContext

  (parent, theSpan) <- liftIO do
    s <-
      OpenTelemetry.createSpanWithoutCallStack
        tracer
        threadContext
        spanName
        OpenTelemetry.defaultSpanArguments

    OpenTelemetry.adjustContext (OpenTelemetry.insertSpan s)
    pure (OpenTelemetry.lookupSpan threadContext, s)

  result <- action

  liftIO do
    metadata <- getMetadata
    OpenTelemetry.addAttributes theSpan $ HashMap.fromList $ Data.Bifunctor.second OpenTelemetry.toAttribute <$> metadata
    OpenTelemetry.endSpan theSpan Nothing
    OpenTelemetry.adjustContext $ \ctx ->
      maybe (OpenTelemetry.removeSpan ctx) (`OpenTelemetry.insertSpan` ctx) parent

  return result

initializeTracer :: Env.Environment -> IO (OpenTelemetry.Tracer, OpenTelemetry.TracerProvider)
initializeTracer env = do
  (processors, options) <- OpenTelemetry.getTracerProviderInitializationOptions
  ddTags <- detectDatadog env

  -- Keep the SDK's default id-generator/sampler/limits/propagators, but
  -- replace the auto-detected process resources with just our Datadog tags.
  let optionsMinusProcessData =
        options
          { tracerProviderOptionsResources = materializeResources (toResource ddTags)
          }

  provider <- createTracerProvider processors optionsMinusProcessData
  setGlobalTracerProvider provider
  return (makeTracer provider "graphql-engine" tracerOptions, provider)

shutdownTracer :: OpenTelemetry.TracerProvider -> IO ()
shutdownTracer provider = void (shutdownTracerProvider provider Nothing)

data DatadogTags = DatadogTags
  { ddEnv :: Maybe Text,
    ddVersion :: Maybe Text,
    ddService :: Maybe Text
  }

detectDatadog :: Env.Environment -> IO DatadogTags
detectDatadog env = do
  let ddEnv = T.pack <$> (Env.lookupEnv env "DD_ENV" <|> Env.lookupEnv env "OTEL_SERVICE_NAME")
      ddVersion = T.pack <$> Env.lookupEnv env "DD_VERSION"
      ddService = T.pack <$> Env.lookupEnv env "DD_SERVICE"
  return DatadogTags {..}

instance ToResource DatadogTags where
  toResource :: DatadogTags -> Resource
  toResource dd =
    mkResource
      [ "env" .=? dd.ddEnv,
        "version" .=? dd.ddVersion,
        "service.name" .=? dd.ddService,
        "service.version" .=? dd.ddVersion
      ]