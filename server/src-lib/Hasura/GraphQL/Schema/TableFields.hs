-- | Input objects with a field per field of a table or logical model (boolean
-- expressions, order by expressions) that parse their fields from the schema
-- cache rather than with a field parser per field.
--
-- See Note [Data-driven table input objects].
module Hasura.GraphQL.Schema.TableFields
  ( TableFieldEntries,
    tableFieldEntries,
    TableObject (..),
    tableObject,
    TrailingField (..),
    columnFieldName,
    optionalFieldDefinition,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Map.Strict qualified as Map
import Data.Traversable (mapAccumL)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Word (Word16)
import Hasura.GraphQL.Schema.Parser
  ( Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.Prelude
import Hasura.RQL.Types.Column (structuredColumnInfoName)
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

{- Note [Data-driven table input objects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every role's schema has a boolean expression and an order by expression for
every table it can select from, with a field for most of the table's fields.
Building an 'InputFieldsParser' for each of them costs a few hundred bytes per
(role, table, field), which adds up to megabytes across roles.

What these fields parse into comes from the table's fields and the role's
permissions, which are already in the schema cache, so these objects parse
their fields from those instead. For each of the table's fields, in the order
of the table's field info map (which is the order 'tableSelectFields' gives
them, and so the order of the fields of the object), the object keeps a
'Word16': either 'absentField', for a field that isn't part of the object, or
the index of the field's entry. An entry holds what parsing the field needs
beyond the table's 'FieldInfo': for a column, the parser of its type (the
comparison expression in a boolean expression, the ordering operator in an
order by expression), which all the columns of the type share; for a
relationship, its name and the target's parser, which is memoized like any
other parser; and so on.

The object can also have fields that don't correspond to fields of the table
(e.g. _and, _or and _not), which come after the table's fields: see
'TrailingField'.

To parse an object, its parser walks the table's fields, looks up the ones that
are part of the object in the given object, then looks up the trailing fields,
and parses the given fields in that order, the way 'P.fieldOptional' does. So
the parsed fields, their order and the errors are those of an object built with
a field parser per field.

The field definitions are only needed for introspection, so they are a thunk,
which nothing evaluates unless the role's schema is introspected; see Note
[Building role parsers lazily] in Hasura.GraphQL.Schema.
-}

-- | For each of the fields of a table, in the order of its field info map, the
-- index of its entry or 'absentField'; and the entries.
type TableFieldEntries e = (U.Vector Word16, V.Vector e)

absentField :: Word16
absentField = maxBound

-- | Assign entries to the fields of a table, given in the order of its field
-- info map: 'Nothing' for a field that isn't part of the object, an entry
-- that the fields with the same key share, or an entry of the field's own.
--
-- The result is fully evaluated, except for the contents of the entries, which
-- may be knot-tied parsers.
tableFieldEntries :: forall k e. (Ord k) => [Maybe (Either (k, e) e)] -> TableFieldEntries e
tableFieldEntries fields =
  let ((_, _, entries), codes) = mapAccumL assign (mempty, 0, []) fields
      !codesVector = U.fromList codes
      !entriesVector = V.fromList $ reverse entries
   in V.foldr seq () entriesVector `seq` (codesVector, entriesVector)
  where
    assign ::
      (Map.Map k Word16, Word16, [e]) ->
      Maybe (Either (k, e) e) ->
      ((Map.Map k Word16, Word16, [e]), Word16)
    assign acc@(byKey, next, entries) = \case
      Nothing -> (acc, absentField)
      Just (Left (key, entry))
        | Just code <- Map.lookup key byKey -> (acc, code)
        | otherwise -> ((Map.insert key next byKey, next + 1, entry : entries), next)
      Just (Right entry) -> ((byKey, next + 1, entry : entries), next)

-- | An input object with a field per field of a table that is part of it,
-- followed by some trailing fields. See Note [Data-driven table input
-- objects].
data TableObject b e t n x = TableObject
  { toName :: G.Name,
    toDescription :: Maybe G.Description,
    -- | the fields of the table (or logical model)
    toFieldInfoMap :: FieldInfoMap (FieldInfo b),
    -- | the entries of the fields that are part of the object, from
    -- 'tableFieldEntries'
    toEntries :: TableFieldEntries e,
    -- | the name of a field of the object
    toFieldName :: FieldInfo b -> e -> G.Name,
    -- | the definition of a field of the object
    toFieldDefinition :: FieldInfo b -> e -> G.Name -> P.Definition P.InputFieldInfo,
    -- | parse the given value of a field of the object, the way
    -- 'P.fieldOptional' does
    toParseField :: FieldInfo b -> e -> G.Name -> P.InputValue P.Variable -> n (Maybe x),
    -- | the fields after the table's ones, each of which only exists if
    -- 'toTrailingField' returns it
    toTrailing :: [t],
    toTrailingField :: t -> Maybe (TrailingField n x)
  }

-- | A field of a table input object that comes after the table's fields.
data TrailingField n x = TrailingField
  { tfName :: G.Name,
    -- | lazy: only needed for introspection
    tfDefinition :: ~(P.Definition P.InputFieldInfo),
    -- | parse the given value of the field, the way 'P.fieldOptional' does
    tfParse :: P.InputValue P.Variable -> n (Maybe x)
  }

tableObject :: forall b e t n x. (P.MonadParse n) => TableObject b e t n x -> Parser 'Input n [x]
{-# INLINE tableObject #-}
tableObject TableObject {..} =
  P.objectWith toName toDescription definitions parseFields
  where
    (codes, entries) = toEntries

    -- the definitions of the fields, only evaluated for introspection
    definitions =
      HashMap.foldr
        ( \fieldInfo continue !i ->
            let code = codes U.! i
                entry = entries V.! fromIntegral code
             in if code == absentField
                  then continue (i + 1)
                  else toFieldDefinition fieldInfo entry (toFieldName fieldInfo entry) : continue (i + 1)
        )
        (const [tfDefinition field | Just field <- toTrailingField <$> toTrailing])
        toFieldInfoMap
        (0 :: Int)

    parseFields input = do
      -- the given fields of the table, in order, and how many there are
      let Walk _ givenCount givenReversed = HashMap.foldl' (walk input) (Walk 0 0 []) toFieldInfoMap
          given = reverse givenReversed
          givenTrailing =
            [ (field, value)
            | Just field <- toTrailingField <$> toTrailing,
              Just value <- [HashMap.lookup (tfName field) input]
            ]
      -- Any other given field isn't a field of the object: reject it the way
      -- 'P.object' does.
      when (givenCount + length givenTrailing /= HashMap.size input) do
        let known = HashSet.fromList $ map (tfName . fst) givenTrailing ++ [name | Given _ _ name _ <- given]
        P.checkInputObjectFields toName (`HashSet.member` known) input
      tableFields <- for given \(Given fieldInfo entry name value) -> toParseField fieldInfo entry name value
      trailingFields <- for givenTrailing \(field, value) -> tfParse field value
      pure $ catMaybes tableFields ++ catMaybes trailingFields

    -- One step of the walk over the table's fields that finds the given ones.
    -- A strict left fold, so that a parse only allocates for the given fields.
    walk input (Walk i count acc) fieldInfo
      | code == absentField = Walk (i + 1) count acc
      | Just value <- HashMap.lookup name input = Walk (i + 1) (count + 1) (Given fieldInfo entry name value : acc)
      | otherwise = Walk (i + 1) count acc
      where
        code = U.unsafeIndex codes i
        entry = V.unsafeIndex entries (fromIntegral code)
        name = toFieldName fieldInfo entry

-- | The state of the walk over a table's fields in 'tableObject': the index
-- of the next field, the number of given fields found, and the given fields
-- found, in reverse order.
data Walk b e = Walk !Int !Int [Given b e]

-- | A field of a table given in a table input object, with its entry, name
-- and value.
data Given b e = Given (FieldInfo b) e G.Name (P.InputValue P.Variable)

-- | The definition of a field of an input object that 'P.fieldOptional'
-- would make.
optionalFieldDefinition :: ('Input P.<: k) => G.Name -> Parser k n a -> P.Definition P.InputFieldInfo
optionalFieldDefinition fieldName parser =
  P.Definition fieldName Nothing Nothing [] $ P.InputFieldInfo (P.nullableType $ P.pType parser) Nothing

-- | The name of the field of a column. Only meant for columns: the name of any
-- other field is the empty name.
columnFieldName :: FieldInfo b -> G.Name
columnFieldName = \case
  FIColumn columnInfo -> structuredColumnInfoName columnInfo
  fieldInfo -> fromMaybe (G.unsafeMkName "") $ fieldInfoGraphQLName fieldInfo
