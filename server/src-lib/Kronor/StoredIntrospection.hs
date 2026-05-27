{-# LANGUAGE QuasiQuotes #-}

-- | Stored source introspection for faster restarts and reload resilience.
--
-- Persists the result of source database introspection to the metadata catalog
-- so that the engine can fall back to stale-but-available schema data when a
-- source is temporarily unreachable during startup or @reload_metadata@.
--
-- Currently uses @appEnvMetadataDbPool@ (the metadata database connection pool).
-- In the future we may want to use a dedicated @appEnvIntrospectionDbPool@ if
-- the introspection payload grows large enough to warrant separating the I/O
-- from the metadata transaction path.
module Kronor.StoredIntrospection
  ( fetchStoredIntrospectionTx,
    storeStoredIntrospectionTx,
  )
where

import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection (defaultTxErrorHandler)
import Hasura.Base.Error (QErr)
import Hasura.Prelude
import Hasura.RQL.Types.SchemaCache (MetadataResourceVersion (..))
import Hasura.RQL.Types.SchemaCache.Build (StoredIntrospection)

-- | Fetch stored introspection from the catalog, returning 'Nothing' when no
-- row exists or the @metadata_resource_version@ does not match.
fetchStoredIntrospectionTx :: MetadataResourceVersion -> PG.TxE QErr (Maybe StoredIntrospection)
fetchStoredIntrospectionTx (MetadataResourceVersion version) = do
  rows <-
    PG.withQE
      defaultTxErrorHandler
      [PG.sql|
        SELECT introspection
        FROM hdb_catalog.hdb_stored_introspection
        WHERE id = 1 AND metadata_resource_version = $1
      |]
      (Identity version)
      True
  case rows of
    [] -> pure Nothing
    [Identity (PG.ViaJSON introspection)] -> pure (Just introspection)
    _ -> pure Nothing -- impossible due to PRIMARY KEY, but safe

-- | Upsert stored introspection into the catalog, associating it with the
-- given @metadata_resource_version@.
storeStoredIntrospectionTx :: StoredIntrospection -> MetadataResourceVersion -> PG.TxE QErr ()
storeStoredIntrospectionTx introspection (MetadataResourceVersion version) =
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
      INSERT INTO hdb_catalog.hdb_stored_introspection (id, introspection, metadata_resource_version)
      VALUES (1, $1::jsonb, $2)
      ON CONFLICT (id) DO UPDATE
      SET introspection = $1::jsonb,
          metadata_resource_version = $2
    |]
    (PG.ViaJSON introspection, version)
    False
