-- | Dynamic database connection routing via Kriti connection templates.
--
-- Enables routing GraphQL queries to different database connection pools
-- (primary, read replicas, connection set members) based on session variables,
-- headers, and query context evaluated through Kriti templates.
module Kronor.ConnectionRouting
  ( mkPGExecCtxWithConnRouting,
    resolveSourcePools,
    ResolvedSourcePools (..),
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.NonEmpty qualified as NEMap
import Data.Text.Extended (toTxt)
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection.Settings
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Common (resolveUrlConf)
import Hasura.RQL.Types.ResizePool
import System.Random (randomRIO)
import Data.Aeson qualified as J
import Data.Environment qualified as Env

-- | The result of resolving all connection pools for a Postgres source.
data ResolvedSourcePools = ResolvedSourcePools
  { -- | Pools for read replicas (if configured)
    rspReplicaConnInfos :: Maybe (NonEmpty ConnInfoWithFinalizer),
    -- | Pools for read replicas (passed to exec context)
    rspReplicaPools :: Maybe (NonEmpty PG.PGPool),
    -- | ConnInfo map for connection set members (stored in PGSourceConfig)
    rspConnSetInfoMap :: HashMap PostgresConnectionSetMemberName PG.ConnInfo,
    -- | Pool map for connection set members (passed to exec context)
    rspConnSetPoolMap :: HashMap PostgresConnectionSetMemberName PG.PGPool,
    -- | The resolved connection template config
    rspConnectionTemplateConfig :: ConnectionTemplateConfig
  }

-- | Create pools for read replicas, connection set members, and build the
-- connection template config from a Postgres source configuration.
resolveSourcePools ::
  PG.PGLogger ->
  Env.Environment ->
  J.Value ->
  PostgresConnConfiguration ->
  ExceptT QErr IO ResolvedSourcePools
resolveSourcePools pgLogger env context config = do
  -- Create read replica pools
  (replicaConnInfos, replicaPools) <- case pccReadReplicas config of
    Nothing -> pure (Nothing, Nothing)
    Just replicas -> do
      results <- forM replicas $ \replicaInfo -> do
        let PostgresSourceConnInfo rUrlConf rPoolSettings rAllowPrepare _rIsoLevel _ = replicaInfo
            (rMaxConns, rIdleTimeout, rRetries, rConnLifetime) = getDefaultPGPoolSettingIfNotExists rPoolSettings defaultPostgresPoolSettings
        rConnDetails <- resolveUrlConf env rUrlConf
        let rConnInfo = PG.ConnInfo rRetries rConnDetails
            rConnParams =
              PG.defaultConnParams
                { PG.cpIdleTime = rIdleTimeout,
                  PG.cpConns = rMaxConns,
                  PG.cpAllowPrepare = rAllowPrepare,
                  PG.cpMbLifetime = rConnLifetime,
                  PG.cpTimeout = ppsPoolTimeout =<< rPoolSettings
                }
        pool <- liftIO $ PG.initPGPool rConnInfo context rConnParams pgLogger
        ciwf <- liftIO $ mkConnInfoWithFinalizer rConnInfo (pure ())
        pure (ciwf, pool)
      let (ciwfs, pools) = unzipNE results
      pure (Just ciwfs, Just pools)

  -- Create connection set pools and ConnInfo map
  (connSetInfoMap, connSetPoolMap) <- case pccConnectionSet config of
    Nothing -> pure (mempty, mempty)
    Just (PostgresConnectionSet neMap) -> do
      let members = NEMap.toList neMap
      results <- forM members $ \(name, PostgresConnectionSetMember _ csConnInfo) -> do
        let PostgresSourceConnInfo csUrlConf csPoolSettings csAllowPrepare _csIsoLevel _ = csConnInfo
            (csMaxConns, csIdleTimeout, csRetries, csConnLifetime) = getDefaultPGPoolSettingIfNotExists csPoolSettings defaultPostgresPoolSettings
        csConnDetails <- resolveUrlConf env csUrlConf
        let csConnInfo' = PG.ConnInfo csRetries csConnDetails
            csConnParams =
              PG.defaultConnParams
                { PG.cpIdleTime = csIdleTimeout,
                  PG.cpConns = csMaxConns,
                  PG.cpAllowPrepare = csAllowPrepare,
                  PG.cpMbLifetime = csConnLifetime,
                  PG.cpTimeout = ppsPoolTimeout =<< csPoolSettings
                }
        pool <- liftIO $ PG.initPGPool csConnInfo' context csConnParams pgLogger
        pure (name, csConnInfo', pool)
      let infoMap = HashMap.fromList [(n, ci) | (n, ci, _) <- results]
          poolMap = HashMap.fromList [(n, p) | (n, _, p) <- results]
      pure (infoMap, poolMap)

  -- Build connection template config
  let connectionTemplateConfig = case pccConnectionTemplate config of
        Nothing -> ConnTemplate_NotConfigured
        Just connTemplate ->
          let memberNames = HashMap.keys connSetInfoMap
              resolver = ConnectionTemplateResolver $ \sessionVars headers queryCtx ->
                resolvePostgresConnectionTemplate connTemplate memberNames sessionVars headers queryCtx
           in ConnTemplate_Resolver (ktParsedAST $ ctTemplate connTemplate) resolver

  pure
    ResolvedSourcePools
      { rspReplicaConnInfos = replicaConnInfos,
        rspReplicaPools = replicaPools,
        rspConnSetInfoMap = connSetInfoMap,
        rspConnSetPoolMap = connSetPoolMap,
        rspConnectionTemplateConfig = connectionTemplateConfig
      }
  where
    unzipNE :: NonEmpty (a, b) -> (NonEmpty a, NonEmpty b)
    unzipNE ((a, b) :| rest) =
      let (as, bs) = unzip rest
       in (a :| as, b :| bs)

-- | Creates a Postgres execution context with connection routing support.
-- Routes transactions to different pools based on the resolved connection template.
mkPGExecCtxWithConnRouting ::
  PG.TxIsolation ->
  PG.PGPool ->
  Maybe (NonEmpty PG.PGPool) ->
  HashMap PostgresConnectionSetMemberName PG.PGPool ->
  ResizePoolStrategy ->
  PGExecCtx
mkPGExecCtxWithConnRouting defaultIsoLevel primaryPool replicaPools connSetPools resizeStrategy =
  PGExecCtx
    { _pecDestroyConnections = do
        PG.destroyPGPool primaryPool
        for_ replicaPools (mapM_ PG.destroyPGPool)
        mapM_ PG.destroyPGPool connSetPools,
      _pecResizePools = \serverReplicas ->
        case resizeStrategy of
          NeverResizePool -> pure noPoolsResizedSummary
          ResizePool maxConnections -> do
            resizePostgresPool primaryPool maxConnections serverReplicas
            let replicasResized = isJust replicaPools
            for_ replicaPools $ \replicas ->
              mapM_ (\p -> resizePostgresPool p maxConnections serverReplicas) replicas
            mapM_ (\p -> resizePostgresPool p maxConnections serverReplicas) connSetPools
            pure
              $ SourceResizePoolSummary
                { _srpsPrimaryResized = True,
                  _srpsReadReplicasResized = replicasResized,
                  _srpsConnectionSet = map toTxt (HashMap.keys connSetPools)
                },
      _pecRunTx = \(PGExecCtxInfo txType pgExecFrom) tx -> do
        pool <- selectPool pgExecFrom txType
        case txType of
          NoTxRead -> PG.runTx' pool tx
          NoTxReadWrite -> PG.runTx' pool tx
          Tx txAccess (Just isolationLevel) -> PG.runTx pool (isolationLevel, Just txAccess) tx
          Tx txAccess Nothing -> PG.runTx pool (defaultIsoLevel, Just txAccess) tx
    }
  where
    selectPool :: (MonadIO m, MonadError QErr m) => PGExecFrom -> PGExecTxType -> m PG.PGPool
    selectPool pgExecFrom txType = case pgExecFrom of
      GraphQLQuery (Just (PCTOPrimary _)) -> pure primaryPool
      GraphQLQuery (Just (PCTOReadReplicas _)) -> selectReplica
      GraphQLQuery (Just (PCTOConnectionSet name)) ->
        HashMap.lookup name connSetPools
          `onNothing` throw400 NotFound ("Connection set member '" <> toTxt name <> "' not found")
      GraphQLQuery (Just (PCTODefault _)) -> case txType of
        NoTxRead -> selectReplica
        Tx PG.ReadOnly _ -> selectReplica
        _ -> pure primaryPool
      _ -> pure primaryPool

    selectReplica :: (MonadIO m) => m PG.PGPool
    selectReplica = case replicaPools of
      Nothing -> pure primaryPool
      Just replicas -> liftIO $ do
        let replicaList = toList replicas
        idx <- randomRIO (0, length replicaList - 1)
        pure $ replicaList !! idx
