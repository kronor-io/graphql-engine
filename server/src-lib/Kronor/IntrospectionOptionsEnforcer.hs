{-# LANGUAGE OverloadedRecordDot #-}

module Kronor.IntrospectionOptionsEnforcer (executeIntrospection) where

import Data.Aeson.Ordered qualified as JO
import Hasura.Base.Error
import Hasura.GraphQL.Execute
   ( ExecutionStep (..),
   )
import Hasura.Prelude
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.Authentication.Role (adminRoleName)
import Hasura.Authentication.User (UserInfo (..))

executeIntrospection :: Monad m =>
    UserInfo ->
    JO.Value ->
    SetGraphqlIntrospectionOptions ->
    m (Either QErr ExecutionStep)
executeIntrospection ui introspectionQuery introspectionOptions = runExceptT $ do
  if ui._uiRole `elem` introspectionOptions._idrDisabledForRoles && ui._uiFallbackRole /= Just adminRoleName
    then throw401 "Introspection disabled"
    else pure $ ExecStepRaw introspectionQuery
