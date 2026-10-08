-- | Input objects with a field per field of a table (boolean expressions,
-- order by expressions) that parse the table's columns from the schema cache
-- rather than with a field parser per column.
--
-- See Note [Data-driven table input objects].
module Hasura.GraphQL.Schema.TableFields
  ( TableFieldEntry (..),
    TableFieldEntries,
    tableFieldEntries,
    tableFieldsObject,
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
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.Prelude
import Hasura.RQL.Types.Column
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

{- Note [Data-driven table input objects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every role's schema has a boolean expression and an order by expression for
every table it can select from, and most of their fields are columns. Building
an 'InputFieldsParser' for each of them costs a few hundred bytes per (role,
table, column), which adds up to megabytes across roles.

A table's columns and the role's permissions are already in the schema cache,
though, so these objects parse their columns from those instead. For each of
the table's fields, in the order of the table's field info map (which is the
order 'tableSelectFields' gives them, and so the order of the fields of the
object), the object keeps a 'Word16': either 'absentField', for a field that
isn't part of the object, or the index of the field's 'TableFieldEntry'.
Columns share the entry of their type (in a boolean expression, the comparison
expression of the type; in an order by expression, the ordering operator);
every other field (relationships, computed fields, nested objects) gets an
entry with its own field parser, as before.

To parse an object, its parser walks the table's fields, looks up the ones that
are part of the object in the given object, and parses them in order: a column
with its entry's parser, the way 'P.fieldOptional' does, any other field with
its field parser. The fields that don't belong to the table (e.g. _and, _or and
_not) still have their own field parsers, and come after the table's fields.
So the parsed fields, their order and the errors are those of an object built
with a field parser per field.

The field definitions are only needed for introspection, so they are a thunk,
which nothing evaluates unless the role's schema is introspected; see Note
[Building role parsers lazily] in Hasura.GraphQL.Schema.
-}

-- | How a field of a table is parsed by a table input object, see Note
-- [Data-driven table input objects].
data TableFieldEntry c o
  = -- | A column, parsed with the parser shared by the columns of its type.
    ColumnEntry c
  | -- | Any other field, parsed by its own field parser.
    OtherEntry o

-- | For each of the fields of a table, in the order of its field info map, the
-- index of its entry or 'absentField'; and the entries.
type TableFieldEntries c o = (U.Vector Word16, V.Vector (TableFieldEntry c o))

absentField :: Word16
absentField = maxBound

-- | Assign entries to the fields of a table, given in the order of its field
-- info map: 'Nothing' for a field that isn't part of the object, a column's
-- type and parser, or another field's parser. Columns of the same type share
-- an entry.
--
-- The result is fully evaluated, except for the parsers, which may be
-- knot-tied.
tableFieldEntries :: forall k c o. (Ord k) => [Maybe (Either (k, c) o)] -> TableFieldEntries c o
tableFieldEntries fields =
  let ((_, _, entries), codes) = mapAccumL assign (mempty, 0, []) fields
      !codesVector = U.fromList codes
      !entriesVector = V.fromList $ reverse entries
   in V.foldr seq () entriesVector `seq` (codesVector, entriesVector)
  where
    assign ::
      (Map.Map k Word16, Word16, [TableFieldEntry c o]) ->
      Maybe (Either (k, c) o) ->
      ((Map.Map k Word16, Word16, [TableFieldEntry c o]), Word16)
    assign acc@(byType, next, entries) = \case
      Nothing -> (acc, absentField)
      Just (Left (columnType, parser))
        | Just code <- Map.lookup columnType byType -> (acc, code)
        | otherwise -> ((Map.insert columnType next byType, next + 1, ColumnEntry parser : entries), next)
      Just (Right parser) -> ((byType, next + 1, OtherEntry parser : entries), next)

-- | An input object with a field per field of a table that is part of the
-- object, followed by some other fields. See Note [Data-driven table input
-- objects].
tableFieldsObject ::
  forall b k n c x r y.
  (P.MonadParse n, 'Input P.<: k) =>
  G.Name ->
  Maybe G.Description ->
  FieldInfoMap (FieldInfo b) ->
  -- | the entries of the table's fields, from 'tableFieldEntries'
  TableFieldEntries (Parser k n c) (InputFieldsParser n (Maybe x)) ->
  -- | the parsed field of a column, if any
  (ColumnInfo b -> c -> Maybe x) ->
  -- | the fields that come after the table's
  InputFieldsParser n r ->
  -- | the result, from the parsed fields of the table and the other fields
  ([x] -> r -> y) ->
  Parser 'Input n y
{-# INLINE tableFieldsObject #-}
tableFieldsObject name description fieldInfoMap (codes, entries) mkColumnField otherFields mkResult =
  P.objectWith name description definitions parseFields
  where
    -- Folds over the table's fields that are part of the object, in order. A
    -- fold rather than a list, so that GHC can't float the list out of the
    -- parser and keep it alive with the schema.
    foldFields :: (FieldInfo b -> TableFieldEntry (Parser k n c) (InputFieldsParser n (Maybe x)) -> a -> a) -> a -> a
    foldFields f z =
      HashMap.foldr
        ( \fieldInfo continue !i ->
            let code = codes U.! i
             in if code == absentField
                  then continue (i + 1)
                  else f fieldInfo (entries V.! fromIntegral code) (continue (i + 1))
        )
        (const z)
        fieldInfoMap
        (0 :: Int)

    definitions =
      foldFields
        ( \fieldInfo entry rest -> case (entry, fieldInfo) of
            (ColumnEntry parser, FIColumn (SCIScalarColumn columnInfo)) ->
              P.Definition (ciName columnInfo) Nothing Nothing [] (P.InputFieldInfo (P.nullableType $ P.pType parser) Nothing) : rest
            (OtherEntry parser, _) -> P.ifDefinitions parser ++ rest
            _ -> rest
        )
        (P.ifDefinitions otherFields)

    parseFields input = do
      -- the fields of the table that are given, in order
      let given =
            foldFields
              ( \fieldInfo entry rest -> case (entry, fieldInfo) of
                  (ColumnEntry parser, FIColumn (SCIScalarColumn columnInfo))
                    | Just value <- HashMap.lookup (ciName columnInfo) input ->
                        GivenColumn parser columnInfo value : rest
                  (OtherEntry parser, _)
                    | any (\d -> HashMap.member (P.dName d) input) (P.ifDefinitions parser) ->
                        GivenOther parser : rest
                  _ -> rest
              )
              []
          givenOthers = filter (\d -> HashMap.member (P.dName d) input) (P.ifDefinitions otherFields)
      -- Any other given field isn't a field of the object: reject it the way
      -- 'P.object' does.
      when (length given + length givenOthers /= HashMap.size input) do
        let known = HashSet.fromList $ map P.dName givenOthers ++ concatMap givenNames given
        P.checkInputObjectFields name (`HashSet.member` known) input
      tableFields <- for given \case
        GivenColumn parser columnInfo value ->
          mkColumnField columnInfo <$> P.parseOptionalField (ciName columnInfo) parser value
        GivenOther parser -> P.ifParser parser input
      mkResult (catMaybes tableFields) <$> P.ifParser otherFields input

    givenNames = \case
      GivenColumn _ columnInfo _ -> [ciName columnInfo]
      GivenOther parser -> map P.dName $ P.ifDefinitions parser

-- | A field of a table given in a table input object.
data GivenField b k n c x
  = GivenColumn (Parser k n c) (ColumnInfo b) (P.InputValue P.Variable)
  | GivenOther (InputFieldsParser n (Maybe x))
