{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.GraphQL.Schema.BoolExp
  ( AggregationPredicatesSchema (..),
    AggregationPredicateField (..),
    sharedComparisonExps,
    tableBoolExp,
    logicalModelBoolExp,
    mkBoolOperator,
    equalityOperators,
    comparisonOperators,
  )
where

import Data.Has (getter)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text.Casing (GQLNameIdentifier)
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.Base.Error (throw500)
import Hasura.Function.Cache
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.TableFields
import Hasura.GraphQL.Schema.Typename
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))
import Hasura.LogicalModel.Common
import Hasura.LogicalModel.Types (LogicalModelName (..))
import Hasura.Name qualified as Name
import Hasura.NativeQuery.Cache (NativeQueryInfo (_nqiReturns))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G
import Type.Reflection

-- | Backends implement this type class to specify the schema of
-- aggregation predicates.
--
-- The default implementation results in a parser that does not parse anything.
--
-- The scope of this class is local to the function 'boolExp'. In particular,
-- methods in `class BackendSchema` and `type MonadBuildSchema` should *NOT*
-- include this class as a constraint.
class AggregationPredicatesSchema (b :: BackendType) where
  -- | The fields of a table's boolean expression for aggregation predicates,
  -- in order.
  aggregationPredicateFields ::
    forall r m n.
    (MonadBuildSourceSchema b r m n) =>
    TableInfo b ->
    SchemaT r m [AggregationPredicateField b n]

-- Overlapping instance for backends that do not implement Aggregation Predicates.
instance {-# OVERLAPPABLE #-} (AggregationPredicates b ~ Const Void) => AggregationPredicatesSchema (b :: BackendType) where
  aggregationPredicateFields ::
    forall r m n.
    (MonadBuildSourceSchema b r m n) =>
    TableInfo b ->
    SchemaT r m [AggregationPredicateField b n]
  aggregationPredicateFields _ = pure []

-- | A field of a boolean expression for aggregation predicates (e.g. over an
-- array relationship).
data AggregationPredicateField b n = AggregationPredicateField
  { apfName :: G.Name,
    -- | The parser of the field's value, if the field exists. Lazy: it may be
    -- knot-tied, and whether the field exists is only known once its parser is
    -- built.
    apfParser :: ~(Maybe (Parser 'Input n (AggregationPredicates b (UnpreparedValue b))))
  }

{- Note [Sharing comparison expressions between roles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The comparison expression of a column type (e.g. `String_comparison_exp`)
doesn't depend on the role: 'comparisonExps' only reads the source and the
schema options. Yet every role built its own, for every type its boolean
expressions use. So 'Hasura.GraphQL.Schema.buildGQLContext' builds them once,
for every column type of every source, with a role that has no permissions,
and puts them in the 'SchemaContext' ('scSharedComparisons'); every role uses
those. A type that isn't there (or a source whose shared comparison expressions
failed to build) falls back to the role's own 'comparisonExps'.

They are the same parsers, with the same type definitions, as the ones a role
would build, so the schema doesn't change.
-}

-- | The comparison expression of a column type: the one shared by all roles,
-- or the role's own. See Note [Sharing comparison expressions between roles].
sharedComparisonExps ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  ColumnType b ->
  SchemaT r m (Parser 'Input n [ComparisonExp b])
sharedComparisonExps columnType = do
  sourceInfo :: SourceInfo b <- asks getter
  shared <- retrieve scSharedComparisons
  let sharedComparison = do
        SharedSourceComparisons comparisons <- AB.unpackAnyBackend @b =<< HashMap.lookup (_siName sourceInfo) shared
        HashMap.lookup columnType comparisons
  maybe (comparisonExps @b columnType) pure sharedComparison

-- | How a field of a table or logical model is parsed in a boolean expression,
-- beside its 'FieldInfo'. See Note [Data-driven table input objects] in
-- Hasura.GraphQL.Schema.TableFields.
--
-- The parsers are lazy: they may be knot-tied.
data BoolExpEntry b n
  = -- | a scalar column, with the comparison expression of its type, which
    -- the columns of that type share
    BEColumn ~(Parser 'Input n [ComparisonExp b])
  | -- | a relationship, with its name, target, and the target's boolean
    -- expression
    BERelationship G.Name (RelationshipTarget b) ~(Parser 'Input n (AnnBoolExp b (UnpreparedValue b)))
  | -- | a computed field that returns a scalar, with its name and the
    -- comparison expression of the scalar's type
    BEComputedScalar G.Name ~(Parser 'Input n [ComparisonExp b])
  | -- | a computed field that returns rows of a table, with its name and the
    -- table's boolean expression
    BEComputedTable G.Name ~(Parser 'Input n (AnnBoolExp b (UnpreparedValue b)))
  | -- | a nested object column, with its name and the boolean expression of
    -- its logical model
    BENestedObject G.Name ~(Parser 'Input n (AnnBoolExp b (UnpreparedValue b)))

-- | The target of a relationship, whose select permissions filter it.
data RelationshipTarget b
  = TargetTable (TableInfo b)
  | TargetLogicalModel (LogicalModelInfo b)

-- | The fields of a boolean expression after the fields of the table.
data BoolExpTrailing b n
  = BTOr
  | BTAnd
  | BTNot
  | BTAggregationPredicates (AggregationPredicateField b n)

-- | The connectives, which every boolean expression has.
connectives :: [BoolExpTrailing b n]
connectives = [BTOr, BTAnd, BTNot]

-- |
-- > input type_bool_exp {
-- >   _or: [type_bool_exp!]
-- >   _and: [type_bool_exp!]
-- >   _not: type_bool_exp
-- >   column: type_comparison_exp
-- >   ...
-- > }
--
-- The boolean expression of a table or logical model, from its fields. See
-- Note [Data-driven table input objects] in Hasura.GraphQL.Schema.TableFields.
boolExpObject ::
  forall b r m n.
  (MonadBuildSchema b r m n, AggregationPredicatesSchema b) =>
  G.Name ->
  G.Description ->
  FieldInfoMap (FieldInfo b) ->
  -- | whether a field can be part of the object
  (FieldInfo b -> Bool) ->
  Maybe (SelPermInfo b) ->
  [AggregationPredicateField b n] ->
  -- | the boolean expression itself, for the connectives; it is knot-tied, so
  -- it must not be forced here
  Parser 'Input n (AnnBoolExp b (UnpreparedValue b)) ->
  SchemaT r m (Parser 'Input n (AnnBoolExp b (UnpreparedValue b)))
boolExpObject name description fieldInfoMap includeField selectPermissions aggregationPredicates recur = do
  roleName <- retrieve scRole
  -- Strict, so that nothing the entries were built from stays alive.
  !entries <-
    fmap tableFieldEntries $ for (HashMap.elems fieldInfoMap) \fieldInfo ->
      if includeField fieldInfo
        then boolExpEntry selectPermissions fieldInfo
        else pure Nothing
  let trailing = case aggregationPredicates of
        [] -> connectives
        _ -> connectives ++ map BTAggregationPredicates aggregationPredicates
  pure
    $ BoolAnd
    <$> tableObject
      TableObject
        { toName = name,
          toDescription = Just description,
          toFieldInfoMap = fieldInfoMap,
          toEntries = entries,
          toFieldName = \fieldInfo -> \case
            BEColumn _ -> columnFieldName fieldInfo
            BERelationship fieldName _ _ -> fieldName
            BEComputedScalar fieldName _ -> fieldName
            BEComputedTable fieldName _ -> fieldName
            BENestedObject fieldName _ -> fieldName,
          toFieldDefinition = \_ entry fieldName -> case entry of
            BEColumn parser -> optionalFieldDefinition fieldName parser
            BERelationship _ _ parser -> optionalFieldDefinition fieldName parser
            BEComputedScalar _ parser -> optionalFieldDefinition fieldName parser
            BEComputedTable _ parser -> optionalFieldDefinition fieldName parser
            BENestedObject _ parser -> optionalFieldDefinition fieldName parser,
          toParseField = \fieldInfo entry fieldName value -> case (entry, fieldInfo) of
            (BEColumn parser, FIColumn (SCIScalarColumn columnInfo)) -> do
              let redactionExp = fromMaybe NoRedaction $ flip getRedactionExprForColumn (ciColumn columnInfo) =<< selectPermissions
              Just . BoolField . AVColumn columnInfo redactionExp <$> P.parseOptionalField fieldName parser value
            (BERelationship _ target parser, FIRelationship relationshipInfo) -> do
              let permissions =
                    (fmap . fmap) partialSQLExpToUnpreparedValue
                      $ maybe annBoolExpTrue spiFilter
                      $ case target of
                        TargetTable tableInfo -> tableSelectPermissions roleName tableInfo
                        TargetLogicalModel logicalModelInfo -> getSelPermInfoForLogicalModel roleName logicalModelInfo
              Just . BoolField . AVRelationship relationshipInfo . RelationshipFilters permissions <$> P.parseOptionalField fieldName parser value
            (BEComputedScalar _ parser, FIComputedField computedFieldInfo) -> do
              let redactionExp = fromMaybe NoRedaction $ flip getRedactionExprForComputedField (_cfiName computedFieldInfo) =<< selectPermissions
              Just . BoolField . computedFieldBoolExp computedFieldInfo . CFBEScalar redactionExp <$> P.parseOptionalField fieldName parser value
            (BEComputedTable _ parser, FIComputedField computedFieldInfo@ComputedFieldInfo {_cfiReturnType})
              | ReturnsTable table <- computedFieldReturnType @b _cfiReturnType ->
                  Just . BoolField . computedFieldBoolExp computedFieldInfo . CFBETable table <$> P.parseOptionalField fieldName parser value
            (BENestedObject _ parser, FIColumn (SCIObjectColumn nestedObjectInfo)) ->
              Just . BoolField . AVNestedObject nestedObjectInfo <$> P.parseOptionalField fieldName parser value
            _ -> pure Nothing,
          toTrailing = trailing,
          toTrailingField = \case
            BTOr -> Just $ connective Name.__or (P.list recur) BoolOr
            BTAnd -> Just $ connective Name.__and (P.list recur) BoolAnd
            BTNot -> Just $ connective Name.__not recur BoolNot
            BTAggregationPredicates AggregationPredicateField {apfName, apfParser} -> do
              parser <- apfParser
              pure
                TrailingField
                  { tfName = apfName,
                    tfDefinition = optionalFieldDefinition apfName parser,
                    tfParse = fmap (Just . BoolField . AVAggregationPredicates) . P.parseOptionalField apfName parser
                  }
        }
  where
    connective :: G.Name -> Parser 'Input n a -> (a -> AnnBoolExp b (UnpreparedValue b)) -> TrailingField n (AnnBoolExp b (UnpreparedValue b))
    connective fieldName parser f =
      TrailingField
        { tfName = fieldName,
          tfDefinition = optionalFieldDefinition fieldName parser,
          tfParse = fmap (Just . f) . P.parseOptionalField fieldName parser
        }

    computedFieldBoolExp :: ComputedFieldInfo b -> ComputedFieldBoolExp b (UnpreparedValue b) -> AnnBoolExpFld b (UnpreparedValue b)
    computedFieldBoolExp ComputedFieldInfo {..} =
      let ComputedFieldFunction {..} = _cfiFunction
          functionArgs =
            flip FunctionArgsExp mempty
              $ fromComputedFieldImplicitArguments @b UVSession _cffComputedFieldImplicitArgs
       in AVComputedField . AnnComputedFieldBoolExp _cfiXComputedFieldInfo _cfiName _cffName functionArgs

-- | The entry of a field of a table or logical model in its boolean
-- expression, if it has one.
boolExpEntry ::
  forall b r m n.
  (MonadBuildSchema b r m n, AggregationPredicatesSchema b) =>
  Maybe (SelPermInfo b) ->
  FieldInfo b ->
  SchemaT r m (Maybe (Either (ColumnType b, BoolExpEntry b n) (BoolExpEntry b n)))
boolExpEntry selectPermissions fieldInfo = runMaybeT do
  _ <- hoistMaybe selectPermissions
  fieldName <- hoistMaybe $ fieldInfoGraphQLName fieldInfo
  case fieldInfo of
    -- field_name: field_type_comparison_exp
    FIColumn (SCIScalarColumn columnInfo) ->
      Left . (ciType columnInfo,) . BEColumn <$> lift (sharedComparisonExps @b (ciType columnInfo))
    FIColumn (SCIObjectColumn NestedObjectInfo {..}) -> do
      SourceInfo {..} <- asks getter
      logicalModelInfo <-
        HashMap.lookup _noiType _siLogicalModels
          `onNothing` throw500 ("Logical model " <> _noiType <<> " not found in source " <>> _siName)
      Right . BENestedObject fieldName <$> lift (logicalModelBoolExp logicalModelInfo)
    FIColumn (SCIArrayColumn _) -> empty -- TODO(dmoverton)
    -- field_name: field_type_bool_exp
    FIRelationship relationshipInfo -> do
      case riTarget relationshipInfo of
        RelTargetNativeQuery nativeQueryName -> do
          logicalModelInfo <- _nqiReturns <$> askNativeQueryInfo nativeQueryName
          Right . BERelationship fieldName (TargetLogicalModel logicalModelInfo) <$> lift (logicalModelBoolExp logicalModelInfo)
        RelTargetTable remoteTable -> do
          remoteTableInfo <- askTableInfo remoteTable
          Right . BERelationship fieldName (TargetTable remoteTableInfo) <$> lift (tableBoolExp remoteTableInfo)
    FIComputedField ComputedFieldInfo {..} -> do
      let ComputedFieldFunction {..} = _cfiFunction
      -- For a computed field to qualify in boolean expression it shouldn't have any input arguments
      case toList _cffInputArgs of
        [] ->
          case computedFieldReturnType @b _cfiReturnType of
            ReturnsScalar scalarType ->
              Right . BEComputedScalar fieldName <$> lift (sharedComparisonExps @b (ColumnScalar scalarType))
            ReturnsTable table -> do
              info <- askTableInfo table
              Right . BEComputedTable fieldName <$> lift (tableBoolExp info)
            ReturnsOthers -> hoistMaybe Nothing
        _ -> hoistMaybe Nothing
    -- Using remote relationship fields in boolean expressions is not supported.
    FIRemoteRelationship _ -> empty

-- | Boolean expression for logical models
logicalModelBoolExp ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b
  ) =>
  LogicalModelInfo b ->
  SchemaT r m (Parser 'Input n (AnnBoolExp b (UnpreparedValue b)))
logicalModelBoolExp logicalModel = do
  roleName <- retrieve scRole
  sourceInfo :: SourceInfo b <- asks getter
  let name = getLogicalModelName (_lmiName logicalModel)
      gqlName = mkTableBoolExpTypeName (C.fromCustomName name)
      selectPermissions = getSelPermInfoForLogicalModel roleName logicalModel
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
      typeName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableBoolExpTypeName gqlName
      description =
        G.Description
          $ "Boolean expression to filter rows from the logical model for "
          <> name
          <<> ". All fields are combined with a logical 'AND'."
  P.memoizeOn 'logicalModelBoolExp (_siName sourceInfo, name) do
    -- Logical models can't have relationships to other entities, so they
    -- don't have aggregation predicates.
    recur <- logicalModelBoolExp logicalModel
    boolExpObject typeName description (logicalModelFieldsToFieldInfo $ _lmiFields logicalModel) (const True) selectPermissions [] recur

-- | Booleans expressions for tables
--
-- See Note [Data-driven table input objects] in Hasura.GraphQL.Schema.TableFields.
tableBoolExp ::
  forall b r m n.
  (MonadBuildSchema b r m n, AggregationPredicatesSchema b) =>
  TableInfo b ->
  SchemaT r m (Parser 'Input n (AnnBoolExp b (UnpreparedValue b)))
tableBoolExp tableInfo = do
  roleName <- retrieve scRole
  let selectPermissions = tableSelectPermissions roleName tableInfo
  gqlName <- getTableIdentifierName tableInfo
  fieldInfos <- tableSelectFields tableInfo
  let description =
        G.Description
          $ "Boolean expression to filter rows from the table "
          <> tableInfoName tableInfo
          <<> ". All fields are combined with a logical 'AND'."
  sourceInfo :: SourceInfo b <- asks getter
  P.memoizeOn 'tableBoolExp (_siName sourceInfo, tableInfoName tableInfo) do
    let customization = _siCustomization sourceInfo
        tCase = _rscNamingConvention customization
        mkTypename = runMkTypename $ _rscTypeNames customization
        name = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableBoolExpTypeName gqlName
        selectable = HashSet.fromList $ fieldInfoName <$> fieldInfos
    aggregationPredicates <- aggregationPredicateFields tableInfo
    recur <- tableBoolExp tableInfo
    boolExpObject
      name
      description
      (_tciFieldInfoMap $ _tiCoreInfo tableInfo)
      ((`HashSet.member` selectable) . fieldInfoName)
      selectPermissions
      aggregationPredicates
      recur

{- Note [Nullability in comparison operators]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In comparisonExps, we hardcode most operators with `Nullability False` when
calling `column`, which might seem a bit sketchy. Shouldn’t the nullability
depend on the nullability of the underlying Postgres column?

No. If we did that, then we would allow boolean expressions like this:

    delete_users(where: {status: {eq: null}})

which in turn would generate a SQL query along the lines of:

    DELETE FROM users WHERE users.status = NULL

but `= NULL` might not do what they expect. For instance, on Postgres, it always
evaluates to False!

Even operators for which `null` is a valid value must be careful in their
implementation. An explicit `null` must always be handled explicitly! If,
instead, an explicit null is ignored:

    foo <- fmap join $ fieldOptional "_foo_level" $ nullable int

then

       delete_users(where: {_foo_level: null})
    => delete_users(where: {})
    => delete_users()

Now we’ve gone and deleted every user in the database. Whoops! Hopefully the
user had backups!

In most cases, as mentioned above, we avoid this problem by making the column
value non-nullable (which is correct, since we never treat a null value as a SQL
NULL), then creating the field using 'fieldOptional'. This creates a parser that
rejects nulls, but won’t be called at all if the field is not specified, which
is permitted by the GraphQL specification. See Note [The value of omitted
fields] in Hasura.GraphQL.Parser.Internal.Parser for more details.

Additionally, it is worth nothing that the `column` parser *does* handle
explicit nulls, by creating a Null column value.

But... the story doesn't end there. Some of our users WANT this peculiar
behaviour. For instance, they want to be able to express the following:

    query($isVerified: Boolean) {
      users(where: {_isVerified: {_eq: $isVerified}}) {
        name
      }
    }

    $isVerified is True  -> return users who are verified
    $isVerified is False -> return users who aren't
    $isVerified is null  -> return all users

In the future, we will likely introduce a separate group of operators that do
implement this particular behaviour explicitly; but for now we have an option that
reverts to the previous behaviour.

To do so, we have to treat explicit nulls as implicit one: this is what the
'nullable' combinator does: it treats an explicit null as if the field has never
been called at all.
-}

-- This is temporary, and should be removed as soon as possible.
mkBoolOperator ::
  (P.IsParse n, 'Input P.<: k) =>
  -- | Naming convention for the field
  NamingCase ->
  -- | shall this be collapsed to True when null is given?
  Options.DangerouslyCollapseBooleans ->
  -- | name of this operator
  GQLNameIdentifier ->
  -- | optional description
  Maybe G.Description ->
  -- | parser for the underlying value
  Parser k n a ->
  InputFieldsParser n (Maybe a)
mkBoolOperator tCase Options.DangerouslyCollapseBooleans name desc = fmap join . P.fieldOptional (applyFieldNameCaseIdentifier tCase name) desc . P.nullable
mkBoolOperator tCase Options.Don'tDangerouslyCollapseBooleans name desc = P.fieldOptional (applyFieldNameCaseIdentifier tCase name) desc

equalityOperators ::
  (P.IsParse n, 'Input P.<: k) =>
  NamingCase ->
  -- | shall this be collapsed to True when null is given?
  Options.DangerouslyCollapseBooleans ->
  -- | parser for one column value
  Parser k n (UnpreparedValue b) ->
  -- | parser for a list of column values
  Parser k n (UnpreparedValue b) ->
  [InputFieldsParser n (Maybe (OpExpG b (UnpreparedValue b)))]
equalityOperators tCase collapseIfNull valueParser valueListParser =
  [ mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_is", "null"])) Nothing $ bool ANISNOTNULL ANISNULL <$> P.boolean,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__eq) Nothing $ AEQ NonNullableComparison <$> valueParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__neq) Nothing $ ANE NonNullableComparison <$> valueParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__in) Nothing $ AIN <$> valueListParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__nin) Nothing $ ANIN <$> valueListParser
  ]

comparisonOperators ::
  (P.IsParse n, 'Input P.<: k) =>
  NamingCase ->
  -- | shall this be collapsed to True when null is given?
  Options.DangerouslyCollapseBooleans ->
  -- | parser for one column value
  Parser k n (UnpreparedValue b) ->
  [InputFieldsParser n (Maybe (OpExpG b (UnpreparedValue b)))]
comparisonOperators tCase collapseIfNull valueParser =
  [ mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__gt) Nothing $ AGT <$> valueParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__lt) Nothing $ ALT <$> valueParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__gte) Nothing $ AGTE <$> valueParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__lte) Nothing $ ALTE <$> valueParser
  ]
