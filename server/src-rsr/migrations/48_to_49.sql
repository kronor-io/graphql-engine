CREATE TABLE hdb_catalog.hdb_stored_introspection (
  id INTEGER PRIMARY KEY CHECK (id = 1),
  introspection JSONB NOT NULL,
  metadata_resource_version INTEGER NOT NULL
);
