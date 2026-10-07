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

import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import Data.HashMap.Strict qualified as HashMap
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection (defaultTxErrorHandler)
import Hasura.Base.Error (QErr)
import Hasura.EncJSON (encJFromBS)
import Hasura.Prelude
import Hasura.RQL.Types.SchemaCache (MetadataResourceVersion (..))
import Hasura.RQL.Types.SchemaCache.Build (StoredIntrospection (..))

-- | Fetch stored introspection from the catalog, returning 'Nothing' when no
-- row exists or the @metadata_resource_version@ does not match.
--
-- Each source's and remote schema's introspection is read as JSON text, split
-- out by Postgres, rather than decoding the whole column: decoding it builds an
-- aeson 'Value' of every introspection only to encode each one back into the
-- 'EncJSON' that 'StoredIntrospection' holds. The sources are stored as an
-- array of @[name, introspection]@ pairs (the JSON encoding of a 'HashMap'
-- keyed by 'SourceName'), the remote schemas as an object.
fetchStoredIntrospectionTx :: MetadataResourceVersion -> PG.TxE QErr (Maybe StoredIntrospection)
fetchStoredIntrospectionTx (MetadataResourceVersion version) = do
  rows <-
    PG.withQE
      defaultTxErrorHandler
      [PG.sql|
        SELECT 'source', entry ->> 0, (entry -> 1)::text
          FROM hdb_catalog.hdb_stored_introspection,
               jsonb_array_elements(introspection -> 'backend_introspection') AS entry
         WHERE id = 1 AND metadata_resource_version = $1
        UNION ALL
        SELECT 'remote', entry.key, entry.value::text
          FROM hdb_catalog.hdb_stored_introspection,
               jsonb_each(introspection -> 'remotes') AS entry
         WHERE id = 1 AND metadata_resource_version = $1
        UNION ALL
        SELECT 'row', '', ''
          FROM hdb_catalog.hdb_stored_introspection
         WHERE id = 1 AND metadata_resource_version = $1
      |]
      (Identity version)
      True
  pure $ if any (\(kind, _, _) -> kind == ("row" :: Text)) rows then toStoredIntrospection rows else Nothing
  where
    toStoredIntrospection rows = do
      sources <- for [(name, json) | (kind, name, json) <- rows, kind == ("source" :: Text)] \(name, json) ->
        (,encJFromBS (txtToBs json)) <$> J.parseMaybe J.parseJSON (J.String name)
      remotes <- for [(name, json) | (kind, name, json) <- rows, kind == ("remote" :: Text)] \(name, json) ->
        (,encJFromBS (txtToBs json)) <$> J.parseMaybe J.parseJSON (J.String name)
      pure $ StoredIntrospection (HashMap.fromList sources) (HashMap.fromList remotes)

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
