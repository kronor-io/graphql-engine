{-# LANGUAGE OverloadedRecordDot #-}

module Kronor.IntrospectionOptionsEnforcer (executeIntrospection) where

import Hasura.Base.Error
import Hasura.GraphQL.Execute
   ( ExecutionStep (..),
   )
import Hasura.Prelude
import Hasura.RQL.IR.Root (RFRawPayload (..))
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.Authentication.Role (adminRoleName)
import Hasura.Authentication.User (UserInfo (..))

executeIntrospection :: Monad m =>
    UserInfo ->
    RFRawPayload ->
    SetGraphqlIntrospectionOptions ->
    m (Either QErr ExecutionStep)
executeIntrospection ui introspectionQuery = \case
    SetGraphqlIntrospectionOptions_Disabled disabledRoles ->
        runExceptT $ do
          let disabled = ui._uiRole `elem` disabledRoles
                      && ui._uiFallbackRole /= Just adminRoleName
          if disabled
            then throw401 "Introspection disabled"
            else pure $ ExecStepRaw (irEncJSON introspectionQuery)

    SetGraphqlIntrospectionOptions_Enabled enabledRoles ->
        runExceptT $ do
          let enabled = ui._uiRole `elem` enabledRoles
                      || ui._uiFallbackRole == Just adminRoleName
          if enabled
            then pure $ ExecStepRaw (irEncJSON introspectionQuery)
            else throw401 "Introspection disabled"
