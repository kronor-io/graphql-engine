{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hasura.GraphQL.Schema.OrderBy
  ( tableOrderByExp,
    logicalModelOrderByExp,
  )
where

import Control.Lens ((^?))
import Data.Has
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.Base.Error
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
import Hasura.LogicalModel.Common (getSelPermInfoForLogicalModel, logicalModelFieldsToFieldInfo)
import Hasura.LogicalModel.Types (LogicalModelName (..))
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G
import Type.Reflection

{-# INLINE orderByOperator #-}
orderByOperator ::
  forall b n.
  (BackendSchema b, P.IsParse n) =>
  NamingCase ->
  SourceInfo b ->
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperator tCase sourceInfo = case tCase of
  HasuraCase -> orderByOperatorsHasuraCase @b sourceInfo
  GraphqlCase -> orderByOperatorsGraphqlCase @b sourceInfo

-- | How a field of a table or logical model is parsed in an order by
-- expression, beside its 'FieldInfo'. See Note [Data-driven table input
-- objects] in Hasura.GraphQL.Schema.TableFields.
--
-- The parsers are lazy: they may be knot-tied.
data OrderByEntry b n
  = -- | a scalar column, ordered by the ordering operator
    OBColumn
  | -- | an object relationship, with its name, target table and the target's
    -- order by expression
    OBObjectRelationship G.Name (TableInfo b) ~(Parser 'Input n [IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)])
  | -- | an array relationship, with the name of its aggregate field, target
    -- table and the target's aggregate order by expression
    OBArrayRelationship G.Name (TableInfo b) ~(Parser 'Input n [IR.OrderByItemG b (IR.AnnotatedAggregateOrderBy b (IR.UnpreparedValue b))])
  | -- | a computed field that returns a scalar, with its name
    OBComputedScalar G.Name
  | -- | a computed field that returns rows of a table, with the name of its
    -- aggregate field, the table and its aggregate order by expression
    OBComputedTable G.Name (TableInfo b) ~(Parser 'Input n [IR.OrderByItemG b (IR.AnnotatedAggregateOrderBy b (IR.UnpreparedValue b))])
  | -- | a nested object column, with its name and the order by expression of
    -- its logical model
    OBNestedObject G.Name ~(Parser 'Input n [IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)])

-- | Corresponds to an object type for an order by.
--
-- > input table_order_by {
-- >   col1: order_by
-- >   col2: order_by
-- >   .     .
-- >   .     .
-- >   coln: order_by
-- >   obj-rel: <remote-table>_order_by
-- > }
--
-- The order by expression of a table or logical model, from its fields. See
-- Note [Data-driven table input objects] in Hasura.GraphQL.Schema.TableFields.
orderByObject ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  G.Name ->
  G.Description ->
  FieldInfoMap (FieldInfo b) ->
  -- | whether a field can be part of the object
  (FieldInfo b -> Bool) ->
  Maybe (SelPermInfo b) ->
  SchemaT r m (Parser 'Input n [IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)])
orderByObject name description fieldInfoMap includeField selectPermissions = do
  roleName <- retrieve scRole
  sourceInfo :: SourceInfo b <- asks getter
  let tCase = _rscNamingConvention $ _siCustomization sourceInfo
  -- all the columns of all the tables share the ordering operator parser
  operator <- sharedOrderByOperator sourceInfo tCase
  -- Strict, so that nothing the entries were built from stays alive.
  !entries <-
    fmap tableFieldEntries $ for (HashMap.elems fieldInfoMap) \fieldInfo ->
      if includeField fieldInfo
        then orderByEntry sourceInfo tCase selectPermissions fieldInfo
        else pure Nothing
  let -- the select permissions of a relationship's target, which filter it
      targetFilter tableInfo =
        fmap partialSQLExpToUnpreparedValue <$> maybe annBoolExpTrue spiFilter (tableSelectPermissions roleName tableInfo)
      orderByOperatorItem column (OrderByOperator order) =
        pure . mkOrderByItemG @b column <$> order
  pure
    $ concat
    <$> tableObject
      TableObject
        { toName = name,
          toDescription = Just description,
          toFieldInfoMap = fieldInfoMap,
          toEntries = entries,
          toFieldName = \fieldInfo -> \case
            OBColumn -> columnFieldName fieldInfo
            OBObjectRelationship fieldName _ _ -> fieldName
            OBArrayRelationship fieldName _ _ -> fieldName
            OBComputedScalar fieldName -> fieldName
            OBComputedTable fieldName _ _ -> fieldName
            OBNestedObject fieldName _ -> fieldName,
          toFieldDefinition = \_ entry fieldName -> case entry of
            OBColumn -> optionalFieldDefinition fieldName operator
            OBObjectRelationship _ _ parser -> optionalFieldDefinition fieldName parser
            OBArrayRelationship _ _ parser -> optionalFieldDefinition fieldName parser
            OBComputedScalar _ -> optionalFieldDefinition fieldName operator
            OBComputedTable _ _ parser -> optionalFieldDefinition fieldName parser
            OBNestedObject _ parser -> optionalFieldDefinition fieldName parser,
          toParseField = \fieldInfo entry fieldName value -> case (entry, fieldInfo) of
            (OBColumn, FIColumn (SCIScalarColumn columnInfo)) -> do
              let redactionExp = fromMaybe NoRedaction $ flip getRedactionExprForColumn (ciColumn columnInfo) =<< selectPermissions
              orderByOperatorItem (IR.AOCColumn columnInfo redactionExp) <$> P.parseOptionalField fieldName operator value
            (OBObjectRelationship _ target parser, FIRelationship relationshipInfo) ->
              fmap (map $ fmap $ IR.AOCObjectRelation relationshipInfo $ targetFilter target)
                <$> P.parseOptionalField fieldName (P.nullable parser) value
            (OBArrayRelationship _ target parser, FIRelationship relationshipInfo) ->
              fmap (map $ fmap $ IR.AOCArrayAggregation relationshipInfo $ targetFilter target)
                <$> P.parseOptionalField fieldName (P.nullable parser) value
            (OBComputedScalar _, FIComputedField computedFieldInfo@ComputedFieldInfo {_cfiName, _cfiReturnType})
              | ReturnsScalar scalarType <- computedFieldReturnType @b _cfiReturnType -> do
                  let redactionExp = fromMaybe NoRedaction $ flip getRedactionExprForComputedField _cfiName =<< selectPermissions
                  orderByOperatorItem (IR.AOCComputedField $ computedFieldOrderBy computedFieldInfo $ IR.CFOBEScalar scalarType redactionExp)
                    <$> P.parseOptionalField fieldName operator value
            (OBComputedTable _ target parser, FIComputedField computedFieldInfo@ComputedFieldInfo {_cfiReturnType})
              | ReturnsTable table <- computedFieldReturnType @b _cfiReturnType ->
                  fmap (map $ fmap $ IR.AOCComputedField . computedFieldOrderBy computedFieldInfo . IR.CFOBETableAggregation table (targetFilter target))
                    <$> P.parseOptionalField fieldName (P.nullable parser) value
            (OBNestedObject _ parser, FIColumn (SCIObjectColumn nestedObjectInfo)) ->
              Just
                . map (fmap $ IR.AOCNestedObject nestedObjectInfo)
                <$> P.parseOptionalField fieldName parser value
            _ -> pure Nothing,
          toTrailing = [] :: [()],
          toTrailingField = const Nothing
        }
  where
    computedFieldOrderBy :: ComputedFieldInfo b -> IR.ComputedFieldOrderByElement b (IR.UnpreparedValue b) -> IR.ComputedFieldOrderBy b (IR.UnpreparedValue b)
    computedFieldOrderBy ComputedFieldInfo {..} =
      let ComputedFieldFunction {..} = _cfiFunction
          functionArgs =
            flip FunctionArgsExp mempty
              $ fromComputedFieldImplicitArguments @b IR.UVSession _cffComputedFieldImplicitArgs
       in IR.ComputedFieldOrderBy _cfiXComputedFieldInfo _cfiName _cffName functionArgs

-- | The entry of a field of a table or logical model in its order by
-- expression, if it has one.
orderByEntry ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  SourceInfo b ->
  NamingCase ->
  Maybe (SelPermInfo b) ->
  FieldInfo b ->
  SchemaT r m (Maybe (Either ((), OrderByEntry b n) (OrderByEntry b n)))
orderByEntry sourceInfo tCase selectPermissions fieldInfo = runMaybeT $ do
  _ <- hoistMaybe selectPermissions
  roleName <- retrieve scRole
  case fieldInfo of
    FIColumn (SCIScalarColumn _) -> pure $ Left ((), OBColumn)
    FIColumn (SCIObjectColumn NestedObjectInfo {..}) -> do
      logicalModelInfo <-
        HashMap.lookup _noiType (_siLogicalModels sourceInfo)
          `onNothing` throw500 ("Logical model " <> _noiType <<> " not found in source " <>> (_siName sourceInfo))
      Right . OBNestedObject _noiName <$> lift (logicalModelOrderByExp @b @r @m @n logicalModelInfo)
    FIColumn (SCIArrayColumn _) -> empty
    FIRelationship relationshipInfo -> do
      case riTarget relationshipInfo of
        RelTargetNativeQuery _ -> hoistMaybe Nothing -- we do not support ordering by a nested Native Query yet
        RelTargetTable remoteTableName -> do
          remoteTableInfo <- askTableInfo remoteTableName
          _ <- hoistMaybe $ tableSelectPermissions roleName remoteTableInfo
          fieldName <- hoistMaybe $ G.mkName $ relNameToTxt $ riName relationshipInfo
          case riType relationshipInfo of
            ObjRel ->
              Right . OBObjectRelationship fieldName remoteTableInfo <$> lift (tableOrderByExp remoteTableInfo)
            ArrRel -> do
              let aggregateFieldName = applyFieldNameCaseIdentifier tCase $ C.fromAutogeneratedTuple (fieldName, [G.convertNameToSuffix Name._aggregate])
              Right . OBArrayRelationship aggregateFieldName remoteTableInfo <$> lift (orderByAggregation sourceInfo remoteTableInfo)
    FIComputedField ComputedFieldInfo {..} -> do
      let ComputedFieldFunction {..} = _cfiFunction
      fieldName <- hoistMaybe $ G.mkName $ toTxt _cfiName
      guard $ _cffInputArgs == mempty -- No input arguments other than table row and session argument
      case computedFieldReturnType @b _cfiReturnType of
        ReturnsScalar _ -> pure $ Right $ OBComputedScalar fieldName
        ReturnsTable table -> do
          let aggregateFieldName = applyFieldNameCaseIdentifier tCase $ C.fromAutogeneratedTuple (fieldName, [G.convertNameToSuffix Name._aggregate])
          tableInfo' <- askTableInfo table
          _ <- hoistMaybe $ tableSelectPermissions roleName tableInfo'
          Right . OBComputedTable aggregateFieldName tableInfo' <$> lift (orderByAggregation sourceInfo tableInfo')
        ReturnsOthers -> empty
    FIRemoteRelationship _ -> empty

-- | Corresponds to an object type for an order by.
--
-- > input table_order_by {
-- >   col1: order_by
-- >   col2: order_by
-- >   .     .
-- >   .     .
-- >   coln: order_by
-- >   obj-rel: <remote-table>_order_by
-- > }
-- TODO: When there are no columns accessible to a role, the
-- `<table>_order_by` will be an empty input object. In such a case,
-- we can avoid exposing the `order_by` argument.
logicalModelOrderByExp ::
  forall b r m n.
  ( MonadBuildSchema b r m n
  ) =>
  LogicalModelInfo b ->
  SchemaT r m (Parser 'Input n [IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)])
logicalModelOrderByExp logicalModel = do
  roleName <- retrieve scRole
  sourceInfo :: SourceInfo b <- asks getter
  let name = getLogicalModelName (_lmiName logicalModel)
      selectPermissions = getSelPermInfoForLogicalModel roleName logicalModel
      description =
        G.Description
          $ "Ordering options when selecting data from "
          <> name
          <<> "."
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
      typeName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableOrderByTypeName (C.fromCustomName name)
  P.memoizeOn 'logicalModelOrderByExp (_siName sourceInfo, name)
    $ orderByObject typeName description (logicalModelFieldsToFieldInfo $ _lmiFields logicalModel) (const True) selectPermissions

-- | Corresponds to an object type for an order by.
--
-- > input table_order_by {
-- >   col1: order_by
-- >   col2: order_by
-- >   .     .
-- >   .     .
-- >   coln: order_by
-- >   obj-rel: <remote-table>_order_by
-- > }
tableOrderByExp ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (Parser 'Input n [IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)])
tableOrderByExp tableInfo = do
  roleName <- retrieve scRole
  tableGQLName <- getTableIdentifierName tableInfo
  tableFields <- tableSelectFields tableInfo
  let selectPermissions = tableSelectPermissions roleName tableInfo
  let description =
        G.Description
          $ "Ordering options when selecting data from "
          <> tableInfoName tableInfo
          <<> "."
  sourceInfo :: SourceInfo b <- asks getter
  P.memoizeOn 'tableOrderByExp (_siName sourceInfo, tableInfoName tableInfo) do
    let customization = _siCustomization sourceInfo
        tCase = _rscNamingConvention customization
        mkTypename = runMkTypename $ _rscTypeNames customization
        name = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableOrderByTypeName tableGQLName
        selectable = HashSet.fromList $ fieldInfoName <$> tableFields
    orderByObject name description (_tciFieldInfoMap $ _tiCoreInfo tableInfo) ((`HashSet.member` selectable) . fieldInfoName) selectPermissions

-- | The result of 'orderByOperator', in a type that can be memoized.
newtype OrderByOperator b = OrderByOperator (Maybe (BasicOrderType b, NullsOrderType b))

-- | 'orderByOperator', built once per role and source rather than for every
-- field that orders by a column.
sharedOrderByOperator ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  SourceInfo b ->
  NamingCase ->
  SchemaT r m (Parser 'Both n (OrderByOperator b))
sharedOrderByOperator sourceInfo tCase =
  P.memoizeOn 'sharedOrderByOperator (_siName sourceInfo) $ pure $ OrderByOperator @b <$> orderByOperator @b tCase sourceInfo

-- | What an aggregate operator of an aggregate order by expression applies
-- to.
data AggregateOperatorKind b
  = -- | numeric columns, e.g. avg
    NumericOperator
  | -- | comparable columns, e.g. max
    ComparisonOperator
  | -- | the columns of the types it maps to the types it returns
    CustomOperator (HashMap (ScalarType b) (ScalarType b))

-- FIXME!
-- those parsers are directly using Postgres' SQL representation of
-- order, rather than using a general intermediary representation

-- | The aggregate order by expression of a table:
--
-- > input table_aggregate_order_by {
-- >   count: order_by
-- >   avg: table_avg_order_by
-- >   ...
-- > }
--
-- Like the order by expressions, it parses its fields from the table's columns
-- and the role's permissions, which are in the schema cache, rather than with
-- a field parser per (operator, column). Only the definitions of its fields
-- are built for introspection, lazily; see Note [Data-driven table input
-- objects] in Hasura.GraphQL.Schema.TableFields.
orderByAggregation ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  SourceInfo b ->
  TableInfo b ->
  SchemaT r m (Parser 'Input n [IR.OrderByItemG b (IR.AnnotatedAggregateOrderBy b (IR.UnpreparedValue b))])
orderByAggregation sourceInfo tableInfo = P.memoizeOn 'orderByAggregation (_siName sourceInfo, tableName) do
  -- WIP NOTE
  -- there is heavy duplication between this and Select.tableAggregationFields
  -- it might be worth putting some of it in common, just to avoid issues when
  -- we change one but not the other?
  tableGQLName <- getTableIdentifierName @b tableInfo
  roleName <- retrieve scRole
  let customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = _rscTypeNames customization
      selectPermissions = tableSelectPermissions roleName tableInfo
  -- all the fields share the ordering operator parser
  operator <- fmap (\(OrderByOperator order) -> order) <$> sharedOrderByOperator sourceInfo tCase
  let -- the columns the role may select, in the order of 'tableSelectColumns'
      allScalarColumns =
        [ (columnInfo, partialSQLExpToUnpreparedValue <$> redactionExp)
        | Just permissions <- [selectPermissions],
          FIColumn (SCIScalarColumn columnInfo) <- HashMap.elems $ _tciFieldInfoMap $ _tiCoreInfo tableInfo,
          Just redactionExp <- [HashMap.lookup (ciColumn columnInfo) (spiCols permissions)]
        ]
      -- the columns an operator of the kind applies to, with the type of the
      -- result of the operator
      kindColumns = \case
        NumericOperator -> [(columnInfo, ciType columnInfo, redactionExp) | (columnInfo, redactionExp) <- allScalarColumns, isNumCol columnInfo]
        ComparisonOperator -> [(columnInfo, ciType columnInfo, redactionExp) | (columnInfo, redactionExp) <- allScalarColumns, isComparableCol columnInfo]
        CustomOperator typeMap ->
          [ (columnInfo, ColumnScalar resultType, redactionExp)
          | (columnInfo, redactionExp) <- allScalarColumns,
            ColumnScalar scalarType <- [ciType columnInfo],
            Just resultType <- [HashMap.lookup scalarType typeMap]
          ]
      -- the kinds of operator that apply to some column, if any
      kindIfAny kind = [kind] <$ listToMaybe (kindColumns kind)
      -- The operators that apply to some column, with the columns they apply
      -- to. Lazy: only needed to parse and introspect.
      operators =
        HashMap.toList
          $ HashMap.catMaybes
          $ HashMap.unionsWith
            (<>)
            [ HashMap.fromList $ (,kindIfAny NumericOperator) <$> numericAggOperators,
              HashMap.fromList $ (,kindIfAny ComparisonOperator) <$> comparisonAggOperators,
              HashMap.mapKeys C.fromCustomName $ kindIfAny . CustomOperator @b <$> getCustomAggregateOperators @b (_siConfiguration sourceInfo)
            ]

      -- > input table_avg_order_by {
      -- >   column: order_by
      -- >   ...
      -- > }
      operatorObject aggregateOperator kinds =
        let columns = concatMap kindColumns kinds
            opText = G.unName $ applyFieldNameCaseIdentifier tCase aggregateOperator
            opTypeName = applyTypeNameCaseIdentifier tCase $ mkTableAggregateOrderByOpTypeName tableGQLName aggregateOperator
            objectName = runMkTypename mkTypename opTypeName
            objectDesc = Just $ G.Description $ "order by " <> opText <> "() on columns of table " <>> tableName
            columnDefinition (columnInfo, _, _) =
              P.Definition (ciName columnInfo) (ciDescription columnInfo) Nothing [] $ P.InputFieldInfo (P.nullableType $ P.pType operator) Nothing
         in P.objectWith objectName objectDesc (columnDefinition <$> columns) \input -> do
              let names = HashSet.fromList [ciName columnInfo | (columnInfo, _, _) <- columns]
              P.checkInputObjectFields objectName (`HashSet.member` names) input
              orders <- for columns \(columnInfo, resultType, redactionExp) ->
                for (HashMap.lookup (ciName columnInfo) input) \value ->
                  fmap (mkOrderByItemG @b (IR.AAOOp $ IR.AggregateOrderByColumn opText resultType columnInfo redactionExp))
                    <$> P.parseOptionalField (ciName columnInfo) operator value
              pure [order | Just (Just order) <- orders]

      objectName = runMkTypename mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableAggregateOrderByTypeName tableGQLName
      description = G.Description $ "order by aggregate values of table " <>> tableName
      definitions =
        optionalFieldDefinition Name._count operator
          : [ optionalFieldDefinition (applyFieldNameCaseIdentifier tCase aggregateOperator) (operatorObject aggregateOperator kinds)
            | (aggregateOperator, kinds) <- operators
            ]
  pure $ P.objectWith objectName (Just description) definitions \input -> do
    let names = HashSet.fromList $ Name._count : [applyFieldNameCaseIdentifier tCase aggregateOperator | (aggregateOperator, _) <- operators]
    P.checkInputObjectFields objectName (`HashSet.member` names) input
    count <- for (HashMap.lookup Name._count input) $ P.parseOptionalField Name._count operator
    orders <- for operators \(aggregateOperator, kinds) -> do
      let fieldName = applyFieldNameCaseIdentifier tCase aggregateOperator
      for (HashMap.lookup fieldName input) $ P.parseOptionalField fieldName (operatorObject aggregateOperator kinds)
    pure $ [mkOrderByItemG @b IR.AAOCount order | Just (Just order) <- [count]] ++ concat (catMaybes orders)
  where
    tableName = tableInfoName tableInfo

orderByOperatorsHasuraCase ::
  forall b n.
  (BackendSchema b, P.IsParse n) =>
  SourceInfo b ->
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperatorsHasuraCase = orderByOperator' @b HasuraCase

orderByOperatorsGraphqlCase ::
  forall b n.
  (BackendSchema b, P.IsParse n) =>
  SourceInfo b ->
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperatorsGraphqlCase = orderByOperator' @b GraphqlCase

orderByOperator' ::
  forall b n.
  (BackendSchema b, P.IsParse n) =>
  NamingCase ->
  SourceInfo b ->
  Parser 'Both n (Maybe (BasicOrderType b, NullsOrderType b))
orderByOperator' tCase sourceInfo =
  let (sourcePrefix, orderOperators) = orderByOperators @b sourceInfo tCase
   in P.nullable $ P.enum (applyTypeNameCaseCust tCase sourcePrefix) (Just "column ordering options") $ orderOperators

mkOrderByItemG :: forall b a. a -> (BasicOrderType b, NullsOrderType b) -> IR.OrderByItemG b a
mkOrderByItemG column (orderType, nullsOrder) =
  IR.OrderByItemG
    { obiType = Just orderType,
      obiColumn = column,
      obiNulls = Just nullsOrder
    }
