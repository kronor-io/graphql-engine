module Hasura.RQL.Types.GraphqlSchemaIntrospection
  ( SetGraphqlIntrospectionOptions (..), emptySetGraphqlIntrospectionOptions
  )
where

import Autodocodec (HasCodec (codec))
import Autodocodec.Extended (hashSetCodec)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON, omitNothingFields)
import Data.HashSet qualified as Set
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as J
import Hasura.Authentication.Role (RoleName)
import Hasura.Prelude
import Autodocodec qualified as AC

data SetGraphqlIntrospectionOptions =
      SetGraphqlIntrospectionOptions_Disabled (Set.HashSet RoleName)
    | SetGraphqlIntrospectionOptions_Enabled (Set.HashSet RoleName)
  deriving (Show, Eq, Generic)

instance NFData SetGraphqlIntrospectionOptions

instance Hashable SetGraphqlIntrospectionOptions

instance HasCodec SetGraphqlIntrospectionOptions where
  codec = AC.dimapCodec dec enc
              $ AC.disjointEitherCodec disabledForRolesCodec enabledForRolesCodec
      where
          dec (Left n) = SetGraphqlIntrospectionOptions_Disabled n
          dec (Right n) = SetGraphqlIntrospectionOptions_Enabled n

          enc (SetGraphqlIntrospectionOptions_Disabled n) = Left n
          enc (SetGraphqlIntrospectionOptions_Enabled n) = Right n

          disabledForRolesCodec = AC.object  "SetGraphqlIntrospectionOptions"
                                      $ AC.requiredFieldWith "disabled_for_roles" hashSetCodec "disable introspection for these roles"
          enabledForRolesCodec = AC.object  "SetGraphqlIntrospectionOptions"
                                      $ AC.requiredFieldWith "enabled_for_roles" hashSetCodec "enable introspection for these roles"

instance FromJSON SetGraphqlIntrospectionOptions where
  parseJSON = J.withObject "SetGraphqlIntrospectionOptions" \o ->
    case J.toList o of
      [("disabled_for_roles", n)] -> SetGraphqlIntrospectionOptions_Disabled <$> J.parseJSON n
      [("enabled_for_roles", n)] -> SetGraphqlIntrospectionOptions_Enabled <$> J.parseJSON n
      _ -> fail "Invalid SetGraphqlIntrospectionOptions. Formats include: { disabled_for_roles: [string] }, { enabled_for_roles: [string] }"


emptySetGraphqlIntrospectionOptions :: SetGraphqlIntrospectionOptions
emptySetGraphqlIntrospectionOptions = SetGraphqlIntrospectionOptions_Disabled mempty

instance ToJSON SetGraphqlIntrospectionOptions where
  toJSON (SetGraphqlIntrospectionOptions_Disabled n) =
    J.object [ "disabled_for_roles" J..= n ]
  toJSON (SetGraphqlIntrospectionOptions_Enabled n) =
    J.object [ "enabled_for_roles" J..= n ]

