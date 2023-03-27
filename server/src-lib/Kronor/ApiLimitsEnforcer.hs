{-# LANGUAGE OverloadedRecordDot #-}

module Kronor.ApiLimitsEnforcer (checkGQLExecution, checkGQLBatchedReqs) where

import Data.HashMap.Strict.InsOrd qualified as OMap
import GHC.Records qualified
import Hasura.Base.Error
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as Protocol
import Hasura.Prelude
import Hasura.RQL.Types.ApiLimit qualified as Limits
import Hasura.Session (RoleName)
import Language.GraphQL.Draft.Syntax qualified as G

checkGQLExecution ::
  ( MonadError QErr m,
    GHC.Records.HasField "_uiRole" userInfo RoleName,
    GHC.Records.HasField "scApiLimits" schemaCache Limits.ApiLimit
  ) =>
  userInfo ->
  schemaCache ->
  Protocol.GQLReq Protocol.GQLExecDoc ->
  m ()
checkGQLExecution userInfo sc req = do
  query <- Protocol.getSingleOperation req
  enforceNodeLimits userInfo sc query
  enforceDepthLimits userInfo sc query

checkGQLBatchedReqs ::
  ( GHC.Records.HasField "_uiRole" userSession RoleName,
    GHC.Records.HasField "scApiLimits" schemaCache Limits.ApiLimit,
    Foldable t,
    Monad m
  ) =>
  userSession ->
  requestId ->
  t a ->
  schemaCache ->
  m (Either QErr ())
checkGQLBatchedReqs userInfo _requestId reqs sc = runExceptT $ do
  let Limits.ApiLimit _ _ _ _ mbatchLimit disabledLimits = sc.scApiLimits

  unless disabledLimits $ do
    case mbatchLimit of
      Nothing -> pure ()
      Just (Limits.Limit (Limits.MaxBatchSize globalMax) perRoleMax) -> do
        let totalReqs = length reqs

        case OMap.lookup userInfo._uiRole perRoleMax of
          Nothing -> do
            when (globalMax < totalReqs) $
              throw429 BadRequest "too many batched requests in a single request"
          Just (Limits.MaxBatchSize roleMax) ->
            when (roleMax < totalReqs) $
              throw429 BadRequest "too many batched requests in a single request"

enforceNodeLimits ::
  ( MonadError QErr m,
    GHC.Records.HasField "_uiRole" userInfo RoleName,
    GHC.Records.HasField "scApiLimits" schemaCache Limits.ApiLimit
  ) =>
  userInfo ->
  schemaCache ->
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
        case OMap.lookup userInfo._uiRole perRoleMax of
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
      (G.SelectionField (G.Field {_fSelectionSet = []}) : xs) ->
        countSelectionFields xs
      (G.SelectionField (G.Field {_fSelectionSet = nested}) : xs) ->
        1 + countSelectionFields nested + countSelectionFields xs
      (G.SelectionInlineFragment frag : xs) ->
        countSelectionFields xs + countSelectionFields (G._ifSelectionSet frag)

enforceDepthLimits ::
  ( MonadError QErr m,
    GHC.Records.HasField "_uiRole" userInfo RoleName,
    GHC.Records.HasField "scApiLimits" schemaCache Limits.ApiLimit
  ) =>
  userInfo ->
  schemaCache ->
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
        case OMap.lookup userInfo._uiRole perRoleMax of
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
      (G.SelectionField (G.Field {_fSelectionSet = []}) : xs) ->
        countDepths acc xs
      (G.SelectionField (G.Field {_fSelectionSet = nested}) : xs) ->
        let innerDepth = 1 + (maximum (countDepths [0] nested))
         in countDepths (innerDepth : acc) xs
      (G.SelectionInlineFragment frag : xs) ->
        let innerDepth = 1 + (maximum (countDepths [0] (G._ifSelectionSet frag)))
         in countDepths (innerDepth : acc) xs