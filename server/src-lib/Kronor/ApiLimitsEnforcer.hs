{-# LANGUAGE OverloadedRecordDot #-}

module Kronor.ApiLimitsEnforcer (checkGQLExecution, checkGQLBatchedReqs, askGraphqlOperationLimit) where

import Control.Concurrent.STM qualified as STM
import Data.Map qualified as Map
import Data.Time.Clock.Units qualified as Clock
import Data.UUID qualified as UUID
import Hasura.Base.Error
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as Protocol
import Hasura.Prelude
import Hasura.RQL.Types.ApiLimit qualified as Limits
import Hasura.RQL.Types.SchemaCache
import Hasura.Server.Limits qualified as Limits
import Hasura.Server.Types qualified as HGE
import Hasura.Session
import Kronor.TokenValidator (HasInvalidTokens (..))
import Language.GraphQL.Draft.Syntax qualified as G
import System.Timeout.Lifted (timeout)

checkGQLExecution ::
  ( MonadError QErr m,
    HasInvalidTokens m,
    MonadIO m
  ) =>
  Hasura.Session.UserInfo ->
  SchemaCache ->
  Protocol.GQLReq Protocol.GQLExecDoc ->
  m ()
checkGQLExecution info sc req = do
  invalidTokensRef <- askInvalidTokens
  invalidTokens <- liftIO $ STM.atomically $ STM.readTVar invalidTokensRef

  case Hasura.Session.getSessionVariableValue "x-hasura-token-id" info._uiSession of
    Just token ->
      case UUID.fromText token of
        Nothing -> do
          throw400 InvalidParams "Invalid token"
        Just uuidToken -> do
          when (uuidToken `elem` invalidTokens) $ do
            throw400 InvalidParams "Invalid token"
    _ -> pure ()

  let Limits.ApiLimit _ _ _ _ _ disabledLimits = sc.scApiLimits
  unless disabledLimits $ do
    query <- Protocol.getSingleOperation req
    enforceNodeLimits info sc query
    enforceDepthLimits info sc query

checkGQLBatchedReqs ::
  Monad m =>
  UserInfo ->
  HGE.RequestId ->
  [Protocol.GQLReq Protocol.GQLQueryText] ->
  SchemaCache ->
  m (Either QErr ())
checkGQLBatchedReqs userInfo _requestId reqs sc = runExceptT $ do
  let Limits.ApiLimit _ _ _ _ mbatchLimit disabledLimits = sc.scApiLimits

  unless disabledLimits $ do
    case mbatchLimit of
      Nothing -> pure ()
      Just (Limits.Limit (Limits.MaxBatchSize globalMax) perRoleMax) -> do
        let totalReqs = length reqs

        case Map.lookup userInfo._uiRole perRoleMax of
          Nothing -> do
            when (globalMax < totalReqs) $
              throw429 BadRequest "too many batched requests in a single request"
          Just (Limits.MaxBatchSize roleMax) ->
            when (roleMax < totalReqs) $
              throw429 BadRequest "too many batched requests in a single request"

enforceNodeLimits ::
  MonadError QErr m =>
  UserInfo ->
  SchemaCache ->
  Protocol.SingleOperation ->
  m ()
enforceNodeLimits userInfo sc query = do
  let Limits.ApiLimit _ _ mnodeLimit _ _ disabledLimits = sc.scApiLimits
  let totalNodes =
        query
          & G._todSelectionSet
          & countSelectionFields

  unless disabledLimits $ do
    case mnodeLimit of
      Nothing -> pure ()
      Just (Limits.Limit (Limits.MaxNodes globalMax) perRoleMax) -> do
        case Map.lookup userInfo._uiRole perRoleMax of
          Nothing ->
            when (globalMax < totalNodes) $
              throw429 BadRequest "too many nodes in a single query"
          Just (Limits.MaxNodes roleMax) ->
            when (roleMax < totalNodes) $
              throw429 BadRequest "too many nodes in a single query"
  where
    countSelectionFields :: [G.Selection G.NoFragments G.Name] -> Int
    countSelectionFields = \case
      [] -> 0
      (G.SelectionField (G.Field {_fName = n}) : xs)
        | isIntrospectionFieldName n ->
            -- instrospection queries are exempt from node limits
            countSelectionFields xs
      (G.SelectionField (G.Field {_fSelectionSet = []}) : xs) ->
        countSelectionFields xs
      (G.SelectionField (G.Field {_fSelectionSet = nested}) : xs) ->
        1 + countSelectionFields nested + countSelectionFields xs
      (G.SelectionInlineFragment frag : xs) ->
        countSelectionFields xs + countSelectionFields (G._ifSelectionSet frag)

enforceDepthLimits ::
  MonadError QErr m =>
  UserInfo ->
  SchemaCache ->
  Protocol.SingleOperation ->
  m ()
enforceDepthLimits userInfo sc query = do
  let Limits.ApiLimit _ mDepthLimit _ _ _ disabledLimits = sc.scApiLimits
  let totalNodes =
        query
          & G._todSelectionSet
          & countDepths [0]
          & maximum

  unless disabledLimits $ do
    case mDepthLimit of
      Nothing -> pure ()
      Just (Limits.Limit (Limits.MaxDepth globalMax) perRoleMax) -> do
        case Map.lookup userInfo._uiRole perRoleMax of
          Nothing ->
            when (globalMax < totalNodes) $
              throw429 BadRequest "node depth limit exceeded"
          Just (Limits.MaxDepth roleMax) ->
            when (roleMax < totalNodes) $
              throw429 BadRequest "node depth limit exceeded"
  where
    countDepths :: [Int] -> [G.Selection G.NoFragments G.Name] -> [Int]
    countDepths acc = \case
      [] -> acc
      (G.SelectionField (G.Field {_fName = n}) : xs)
        | isIntrospectionFieldName n ->
            -- introspection queries are exempt from depth limits
            countDepths acc xs
      (G.SelectionField (G.Field {_fSelectionSet = []}) : xs) ->
        countDepths acc xs
      (G.SelectionField (G.Field {_fSelectionSet = nested}) : xs) ->
        let innerDepth = 1 + (maximum (countDepths [0] nested))
         in countDepths (innerDepth : acc) xs
      (G.SelectionInlineFragment frag : xs) ->
        let innerDepth = 1 + (maximum (countDepths [0] (G._ifSelectionSet frag)))
         in countDepths (innerDepth : acc) xs

askGraphqlOperationLimit ::
  Monad m =>
  HGE.RequestId ->
  UserInfo ->
  Limits.ApiLimit ->
  m Limits.ResourceLimits
askGraphqlOperationLimit _requestId userInfo apiLimit = do
  let Limits.ApiLimit _ _ _ mTimeLimit _ disabledLimits = apiLimit
  if disabledLimits
    then do
      pure $ Limits.ResourceLimits id
    else do
      case mTimeLimit of
        Nothing -> do
          pure $ Limits.ResourceLimits id
        Just (Limits.Limit (Limits.MaxTime globalMax) perRoleMax) -> do
          case Map.lookup userInfo._uiRole perRoleMax of
            Nothing -> do
              pure $ Limits.ResourceLimits $ \action -> do
                res <- timeout (fromInteger $ Clock.diffTimeToMicroSeconds (Clock.toDiffTime globalMax)) action
                case res of
                  Nothing -> do
                    let err = err500 (CustomCode "time-limit-exceeded") "operation timed out"
                    throwError err
                  Just a -> do
                    pure a
            Just (Limits.MaxTime roleMax) -> do
              pure $ Limits.ResourceLimits $ \action -> do
                res <- timeout (fromInteger $ Clock.diffTimeToMicroSeconds (Clock.toDiffTime roleMax)) action
                case res of
                  Nothing -> do
                    let err = err500 (CustomCode "time-limit-exceeded") "operation timed out"
                    throwError err
                  Just a -> do
                    pure a

isIntrospectionFieldName :: G.Name -> Bool
isIntrospectionFieldName name =
  name
    `elem` [ G.unsafeMkName "__schema",
             G.unsafeMkName "__type",
             G.unsafeMkName "__typename"
           ]
