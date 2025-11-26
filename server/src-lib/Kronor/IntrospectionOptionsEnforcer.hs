{-# LANGUAGE OverloadedRecordDot #-}

module Kronor.IntrospectionOptionsEnforcer (executeIntrospection) where

import Data.Aeson.Ordered qualified as JO
import Hasura.Base.Error
import Hasura.GraphQL.Execute
   ( ExecutionStep (..),
   )
import Hasura.Prelude
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.Session
import Kronor.TokenValidator (HasInvalidTokens (..))

executeIntrospection :: Monad m =>
    UserInfo ->
    JO.Value ->
    SetGraphqlIntrospectionOptions ->
    m (Either QErr ExecutionStep)
executeIntrospection ui introspectionQuery introspectionOptions = runExceptT $ do
  if ui._uiRole `elem` introspectionOptions._idrDisabledForRoles
    then throw401 "Introspection disabled"
    else pure $ ExecStepRaw introspectionQuery
