-- | Fields whose arguments, selection set and result don't depend on their
-- name, as data.
module Hasura.GraphQL.Schema.NamedField
  ( NamedField (..),
    namedFieldParser,
    subselectionNamedField,
    lazyFieldParser,
  )
where

import Hasura.GraphQL.Parser.Class (MonadParse)
import Hasura.GraphQL.Parser.Internal.Parser qualified as IP
import Hasura.GraphQL.Schema.Parser
  ( FieldParser,
    InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.Prelude
import Hasura.RQL.Types.Metadata.Object (MetadataObjId)
import Language.GraphQL.Draft.Syntax qualified as G

-- | A field that parses the same way whatever its name, but for the name in
-- its definition and errors: e.g. the field that selects from a table, which
-- the table's root field and the array relationships to the table share. Its
-- parts (arguments, selection set) are built once, and a field made from it
-- with 'namedFieldParser' only adds the name and description.
data NamedField n a = NamedField
  { nfDefinition :: G.Name -> Maybe G.Description -> P.Definition P.FieldInfo,
    nfParse :: G.Name -> G.Field G.NoFragments P.Variable -> n a
  }

instance (Functor n) => Functor (NamedField n) where
  fmap f NamedField {..} = NamedField nfDefinition \name field -> f <$> nfParse name field

-- | The field with the given name and description.
namedFieldParser :: G.Name -> Maybe G.Description -> NamedField n a -> FieldParser n a
namedFieldParser name description NamedField {..} = IP.FieldParser (nfDefinition name description) (nfParse name)

-- | The 'NamedField' of @'P.subselection' _ _ arguments body@, with the given
-- origin ('P.setFieldParserOrigin'), that builds its result from the parsed
-- arguments and selection set.
subselectionNamedField ::
  (MonadParse n) =>
  MetadataObjId ->
  InputFieldsParser n a ->
  Parser 'Output n b ->
  (a -> b -> c) ->
  NamedField n c
subselectionNamedField origin arguments body build =
  NamedField
    { nfDefinition = \name description ->
        case IP.subselectionDefinition name description arguments body of
          P.Definition name' description' _ directives info -> P.Definition name' description' (Just origin) directives info,
      nfParse = \name field -> do
        (_, _, parsedArguments, parsedBody) <- IP.rawSubselectionParse argumentNames name arguments body field
        pure $ build parsedArguments parsedBody
    }
  where
    -- lazy, so that making the field doesn't build its arguments' parsers: see
    -- Note [Building parsers lazily] in Control.Monad.Memoize
    argumentNames = IP.selectionArgumentNames arguments

-- | A field parser with the given name, description and origin, whose type and
-- parser are those of the given field parser, which is only evaluated when they
-- are needed: so that making a root field doesn't build the parsers of what it
-- selects. See Note [Building parsers lazily] in Control.Monad.Memoize.
lazyFieldParser :: G.Name -> Maybe G.Description -> MetadataObjId -> FieldParser n a -> FieldParser n a
lazyFieldParser name description origin parser =
  IP.FieldParser
    (P.Definition name description (Just origin) [] info)
    (\field -> IP.fParser parser field)
  where
    info = case IP.fDefinition parser of P.Definition _ _ _ _ i -> i
