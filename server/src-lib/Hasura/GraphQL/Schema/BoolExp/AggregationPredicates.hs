{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | This module defines the schema aspect of the default implementation of
-- aggregation predicates.
module Hasura.GraphQL.Schema.BoolExp.AggregationPredicates
  ( defaultAggregationPredicateFields,

    -- * Data types describing aggregation functions supported by a backend
    FunctionSignature (..),
    ArgumentsSignature (..),
    ArgumentSignature (..),
  )
where

import Data.Functor.Compose
import Data.Has (getter)
import Control.Monad.Memoize qualified as Memoize
import Data.List.NonEmpty qualified as NE
import Data.Text.Casing qualified as C
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Table
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.BoolExp.AggregationPredicates
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Backend qualified as B
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common (relNameToTxt)
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Schema.Options (IncludeAggregationPredicates (..))
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source (SourceInfo (..))
import Hasura.RQL.Types.SourceCustomization
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

-- | This function is meant to serve as the default schema for Aggregation
-- Predicates represented in the IR by the type
-- 'Hasura.RQL.IR.BoolExp.AggregationPredicates.AggregationPredicates'.
--
-- There is a field for each array relationship whose target the role may
-- aggregate. Its value is parsed by the target table's aggregation predicates
-- object, which is memoized: it only depends on the target table, so all the
-- relationships to a table share it.
defaultAggregationPredicateFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b,
    B.AggregationPredicates b ~ AggregationPredicatesImplementation b
  ) =>
  [FunctionSignature b] ->
  TableInfo b ->
  SchemaT r m [AggregationPredicateField b n]
defaultAggregationPredicateFields aggFns ti = fmap (fromMaybe []) $ runMaybeT do
  sourceInfo :: SourceInfo b <- asks getter
  let customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
  -- Check in schema options whether we should include aggregation predicates
  include <- retrieve Options.soIncludeAggregationPredicates
  case include of
    IncludeAggregationPredicates -> return ()
    Don'tIncludeAggregationPredicates -> fails $ return Nothing

  arrayRelationships <- fails $ return $ nonEmpty $ tableArrayRelationships ti
  aggregationFunctions <- fails $ return $ nonEmpty aggFns
  roleName <- retrieve scRole

  lift
    $ fmap catMaybes
    $ for (toList arrayRelationships) \rel -> runMaybeT do
      case riTarget rel of
        RelTargetNativeQuery _ -> hoistMaybe Nothing -- aggregations across native queries not implemented yet
        RelTargetTable relTableName -> do
          relTable <- askTableInfo relTableName
          selectPermissions <- hoistMaybe $ tableSelectPermissions roleName relTable
          guard $ spiAllowAgg selectPermissions
          relGqlName <- textToName $ relNameToTxt $ riName rel
          predicates <- lift $ tableAggregationPredicates aggregationFunctions relTable
          pure
            AggregationPredicateField
              { apfName = applyFieldNameCaseIdentifier tCase (mkTableAggregateTypeName (C.fromCustomName relGqlName)),
                apfParser =
                  fmap
                    ( \predicate ->
                        let rowPermissions = fmap partialSQLExpToUnpreparedValue <$> spiFilter selectPermissions
                         in AggregationPredicatesImplementation rel rowPermissions predicate
                    )
                    <$> predicates
              }
  where
    -- Mark a computation as potentially failing.
    fails :: f (Maybe a) -> MaybeT f a
    fails = MaybeT

-- | The aggregation predicates over the rows of a table, if the role can call
-- at least one of them:
--
-- > input table_aggregate_bool_exp {
-- >   count: table_aggregate_bool_exp_count
-- >   ...
-- > }
--
-- Memoized as a 'Maybe', so it is lazy if it is knot-tied: callers must not
-- force it while building the schema.
tableAggregationPredicates ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b
  ) =>
  NonEmpty (FunctionSignature b) ->
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Input n (AggregationPredicate b (UnpreparedValue b))))
tableAggregationPredicates aggregationFunctions relTable = do
  sourceInfo :: SourceInfo b <- asks getter
  let customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
  Memoize.memoizeOn 'tableAggregationPredicates (_siName sourceInfo, tableInfoName relTable) $ runMaybeT do
    typeGqlName <- mkTableAggregateBoolExpTypeName <$> getTableIdentifierName relTable
    -- We only make a field for aggregations over a relation if at least
    -- some aggregation predicates are callable.
    predicates <-
      (collectOptionalFieldsNE . succeedingBranchesNE)
        ( aggregationFunctions <&> \FunctionSignature {..} -> do
            let relFunGqlName = mkRelationFunctionIdentifier typeGqlName fnGQLName
            aggPredicateField fnGQLName typeGqlName tCase <$> unfuse do
              aggPredArguments <-
                -- We only include an aggregation predicate if we are able to
                -- access columns all its arguments. This might fail due to
                -- permissions or due to no columns of suitable types
                -- existing on the table.
                case fnArguments of
                  ArgumentsStar ->
                    maybe AggregationPredicateArgumentsStar AggregationPredicateArguments
                      . nonEmpty
                      <$> fuse (fieldOptionalDefault Name._arguments Nothing [] . P.list <$> fails (tableSelectColumnsEnum relTable))
                  SingleArgument typ ->
                    AggregationPredicateArguments
                      . (NE.:| [])
                      <$> fuse
                        ( P.field Name._arguments Nothing
                            <$> fails (tableSelectColumnsPredEnum (== (ColumnScalar typ)) relFunGqlName relTable)
                        )
                  Arguments args ->
                    AggregationPredicateArguments
                      <$> fuse
                        ( P.field Name._arguments Nothing
                            . P.object (applyFieldNameCaseIdentifier tCase (mkRelationFunctionArgumentsFieldName typeGqlName fnGQLName)) Nothing
                            <$> collectFieldsNE
                              ( args `for` \ArgumentSignature {..} ->
                                  P.field argName Nothing <$> fails (tableSelectColumnsPredEnum (== (ColumnScalar argType)) relFunGqlName relTable)
                              )
                        )

              aggPredDistinct <- fuse $ return $ fieldOptionalDefault Name._distinct Nothing False P.boolean
              let aggPredFunctionName = fnName
              aggPredPredicate <- fuse $ P.field Name._predicate Nothing <$> lift (comparisonExps @b (ColumnScalar fnReturnType))
              aggPredFilter <- fuse $ P.fieldOptional Name._filter Nothing <$> lift (tableBoolExp relTable)
              pure $ AggregationPredicate {..}
        )
    pure
      $ P.object (applyTypeNameCaseIdentifier tCase typeGqlName) Nothing
      $ predicates
      `P.bindFields` \case
        [predicate] -> pure predicate
        _ -> P.parseError "exactly one predicate should be specified"
  where
    -- Input field for a single aggregation predicate.
    aggPredicateField ::
      G.Name ->
      C.GQLNameIdentifier ->
      NamingCase ->
      InputFieldsParser n (AggregationPredicate b (UnpreparedValue b)) ->
      InputFieldsParser n (Maybe (AggregationPredicate b (UnpreparedValue b)))
    aggPredicateField fnGQLName typeGqlName tCase =
      P.fieldOptional fnGQLName Nothing . P.object (applyFieldNameCaseIdentifier tCase (typeGqlName <> C.fromCustomName fnGQLName)) Nothing

    -- Collect all non-failing branches of optional field parsers.
    -- Fails only when all branches fail.
    -- buildAnyOptionalFields ::
    --   Applicative f =>
    --   NonEmpty (MaybeT f (InputFieldsParser n (Maybe c))) ->
    --   MaybeT f (InputFieldsParser n [c])
    -- buildAnyOptionalFields = fmap collectOptionalFields . succeedingBranchesNE
    --   where

    -- Collect all the non-failed branches, failing if all branches failed.
    succeedingBranchesNE :: forall f a. (Applicative f) => NonEmpty (MaybeT f a) -> MaybeT f (NonEmpty a)
    succeedingBranchesNE xs = MaybeT $ NE.nonEmpty . catMaybes . NE.toList <$> traverse runMaybeT xs

    -- Collect a non-empty list of input field parsers into one input field
    -- parser parsing a non-empty list of the specified values.
    collectFieldsNE ::
      (Functor f) =>
      MaybeT f (NonEmpty (InputFieldsParser n c)) ->
      MaybeT f (InputFieldsParser n (NonEmpty c))
    collectFieldsNE = fmap sequenceA

    -- Collect a non-empty list of optional input field parsers into one input field
    -- parser parsing a list of the specified values.
    collectOptionalFieldsNE ::
      (Functor f) =>
      MaybeT f (NonEmpty (InputFieldsParser n (Maybe a))) ->
      MaybeT f (InputFieldsParser n [a])
    collectOptionalFieldsNE = fmap $ fmap (catMaybes . NE.toList) . sequenceA

    -- Mark a computation as potentially failing.
    fails :: f (Maybe a) -> MaybeT f a
    fails = MaybeT

    -- Compose our monad with InputFieldsParser into one fused Applicative that
    -- acts on the parsed values directly.
    fuse :: MaybeT f (InputFieldsParser n a) -> Compose (MaybeT f) (InputFieldsParser n) a
    fuse = Compose

    -- The inverse of 'fuse'.
    unfuse :: Compose (MaybeT f) (InputFieldsParser n) a -> MaybeT f (InputFieldsParser n a)
    unfuse = getCompose

    -- Optional input field with a default value when the field is elided or null.
    fieldOptionalDefault ::
      forall k a. ('Input P.<: k) => G.Name -> Maybe G.Description -> a -> Parser k n a -> InputFieldsParser n a
    fieldOptionalDefault n d a p = fromMaybe a <$> P.fieldOptional n d p

data FunctionSignature (b :: BackendType) = FunctionSignature
  { fnName :: Text,
    fnGQLName :: G.Name,
    fnArguments :: ArgumentsSignature b,
    fnReturnType :: B.ScalarType b
  }

data ArgumentsSignature (b :: BackendType)
  = ArgumentsStar
  | SingleArgument (B.ScalarType b)
  | Arguments (NonEmpty (ArgumentSignature b))

data ArgumentSignature (b :: BackendType) = ArgumentSignature
  { argType :: B.ScalarType b,
    argName :: G.Name
  }
