"""
Black-box tests for stored source introspection.

Verifies that:
1. Source introspection is persisted to hdb_catalog.hdb_stored_introspection
   after a successful schema cache build.
2. When a source becomes unreachable during reload_metadata, the engine falls
   back to stored introspection — the source's tables stay in the GraphQL
   schema (with an inconsistency warning) instead of disappearing entirely.
"""

import pytest
import requests
import sqlalchemy


def metadata_api(hge_ctx, body, expected_status_code=200):
    """Call the v1/metadata endpoint with admin auth."""
    headers = {
        "X-Hasura-Admin-Secret": hge_ctx.hge_key,
        "Content-Type": "application/json",
    }
    resp = requests.post(
        f"{hge_ctx.hge_url}/v1/metadata",
        json=body,
        headers=headers,
        timeout=30,
    )
    assert resp.status_code == expected_status_code, (
        f"metadata API call failed (expected {expected_status_code}): {resp.text}"
    )
    return resp


def graphql_admin(hge_ctx, query):
    """Send a GraphQL query as admin."""
    headers = {
        "X-Hasura-Admin-Secret": hge_ctx.hge_key,
        "Content-Type": "application/json",
    }
    return requests.post(
        f"{hge_ctx.hge_url}/v1/graphql",
        json={"query": query},
        headers=headers,
        timeout=30,
    )


@pytest.mark.admin_secret
@pytest.mark.usefixtures("per_class_tests_db_state")
class TestStoredIntrospection:
    """Tests that source introspection is stored and used as fallback."""

    @classmethod
    def dir(cls):
        return "queries/fork/stored_introspection"

    def test_introspection_is_stored_after_build(self, hge_ctx, metadata_schema_url):
        """After HGE starts and tracks tables, hdb_stored_introspection should
        have a row with the current metadata_resource_version."""
        engine = sqlalchemy.create_engine(metadata_schema_url)
        try:
            with engine.connect() as conn:
                result = conn.execute(
                    sqlalchemy.text(
                        "SELECT metadata_resource_version, "
                        "       length(introspection::text) as payload_size "
                        "FROM hdb_catalog.hdb_stored_introspection "
                        "WHERE id = 1"
                    )
                )
                row = result.fetchone()

            assert row is not None, (
                "hdb_stored_introspection is empty — introspection was not persisted"
            )
            version, payload_size = row
            assert version > 0, f"Expected positive resource version, got {version}"
            assert payload_size > 2, (
                f"Introspection payload is suspiciously small ({payload_size} bytes)"
            )
        finally:
            engine.dispose()

    def test_introspection_contains_source_data(self, hge_ctx, metadata_schema_url):
        """The stored introspection JSON should contain an entry for the
        default source with our tracked table's catalog information."""
        engine = sqlalchemy.create_engine(metadata_schema_url)
        try:
            with engine.connect() as conn:
                result = conn.execute(
                    sqlalchemy.text(
                        "SELECT introspection -> 'backend_introspection' "
                        "FROM hdb_catalog.hdb_stored_introspection "
                        "WHERE id = 1"
                    )
                )
                row = result.fetchone()

            assert row is not None, "No stored introspection found"
            backend_introspection = row[0]
            assert backend_introspection is not None, (
                "backend_introspection key is missing from stored introspection"
            )
            assert "default" in backend_introspection, (
                "default source not found in stored backend_introspection"
            )
        finally:
            engine.dispose()

    def test_fallback_on_unreachable_source(self, hge_ctx, metadata_schema_url):
        """When a source becomes unreachable, reload_metadata should fall back
        to stored introspection. The source's tables remain in the schema with
        an inconsistency warning rather than disappearing."""

        # 1. Sanity check: tables are queryable before we break the source
        resp = graphql_admin(hge_ctx, "{ fork_si_items { id name } }")
        body = resp.json()
        assert "data" in body, f"Expected data before breaking source, got: {body}"
        assert "errors" not in body, f"Unexpected errors: {body.get('errors')}"

        try:
            # 2. Point the default source at an unreachable URL.
            #    pg_update_source triggers a schema cache rebuild internally.
            #    The rebuild will fail to introspect the source and should
            #    fall back to stored introspection.
            metadata_api(hge_ctx, {
                "type": "pg_update_source",
                "args": {
                    "name": "default",
                    "configuration": {
                        "connection_info": {
                            "database_url": "postgresql://localhost:1/nonexistent",
                            "pool_settings": {
                                "retries": 0,
                            },
                        },
                    },
                },
            })

            # 3. Check for the stale-introspection inconsistency
            resp = metadata_api(hge_ctx, {
                "type": "get_inconsistent_metadata",
                "args": {},
            })
            inconsistencies = resp.json()
            assert inconsistencies.get("is_consistent") is False, (
                f"Expected inconsistent metadata, got: {inconsistencies}"
            )
            inconsistent_objects = inconsistencies.get("inconsistent_objects", [])
            has_stale_warning = any(
                "stale database introspection" in str(obj)
                for obj in inconsistent_objects
            )
            assert has_stale_warning, (
                f"Expected 'stale database introspection' warning in inconsistent objects, "
                f"got: {inconsistent_objects}"
            )

            # 4. Verify the source's tables are still in the GraphQL schema
            #    via an introspection query.
            introspection_query = """
            {
              __schema {
                queryType {
                  fields {
                    name
                  }
                }
              }
            }
            """
            resp = graphql_admin(hge_ctx, introspection_query)
            schema_body = resp.json()
            field_names = [
                f["name"]
                for f in schema_body.get("data", {})
                .get("__schema", {})
                .get("queryType", {})
                .get("fields", [])
            ]
            assert "fork_si_items" in field_names, (
                f"Expected fork_si_items in schema fields after fallback, "
                f"got: {field_names}"
            )

        finally:
            # 5. Restore the source to the working connection URL
            metadata_api(hge_ctx, {
                "type": "pg_update_source",
                "args": {
                    "name": "default",
                    "configuration": {
                        "connection_info": {
                            "database_url": {
                                "from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_1",
                            },
                        },
                    },
                },
            })

            # Reload to rebuild schema cache with the working source
            metadata_api(hge_ctx, {
                "type": "reload_metadata",
                "args": {"reload_sources": True},
            })
