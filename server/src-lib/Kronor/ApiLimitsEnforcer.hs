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
  let Limits.ApiLimit _ _ mnodeLimit _ _ disabledLimits = sc.scApiLimits
  query <- Protocol.getSingleOperation req
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