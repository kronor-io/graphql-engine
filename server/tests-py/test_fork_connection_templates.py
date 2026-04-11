"""
Black-box tests for dynamic database connection routing via connection templates.

Tests that Kriti connection templates route GraphQL queries to different
database pools (primary, read replicas, connection set members) based on
session variables.
"""

import os
import pytest
import jwt
import requests
import psycopg2


def make_jwt_token(jwt_conf, role, extra_claims=None):
    """Generate a JWT for the given role using the test JWT configuration."""
    hasura_claims = {
        "x-hasura-default-role": role,
        "x-hasura-allowed-roles": [role],
        "x-hasura-user-id": "1",
    }
    if extra_claims:
        hasura_claims.update(extra_claims)
    payload = {
        "https://hasura.io/jwt/claims": hasura_claims,
    }
    return jwt.encode(payload, jwt_conf.private_key, algorithm=jwt_conf.algorithm)


def graphql(hge_ctx, query, jwt_conf=None, role=None, extra_claims=None):
    """Send a GraphQL query to the engine."""
    headers = {"Content-Type": "application/json"}
    if role and jwt_conf:
        token = make_jwt_token(jwt_conf, role, extra_claims)
        headers["Authorization"] = f"Bearer {token}"
    else:
        headers["X-Hasura-Admin-Secret"] = hge_ctx.hge_key
    body = {"query": query} if isinstance(query, str) else query
    return requests.post(f"{hge_ctx.hge_url}/v1/graphql", json=body, headers=headers)


def metadata_api(hge_ctx, payload):
    """Send a request to the metadata API."""
    headers = {
        "X-Hasura-Admin-Secret": hge_ctx.hge_key,
        "Content-Type": "application/json",
    }
    return requests.post(
        f"{hge_ctx.hge_url}/v1/metadata",
        json=payload,
        headers=headers,
    )


# The Kriti connection template used in tests.
# Routes to "secondary" connection set member when x-hasura-route == "secondary",
# otherwise routes to primary.
CONNECTION_TEMPLATE = """
{{
  if ($.request.session.x-hasura-route == "secondary")
    $.connection_set.secondary
  else
    $.primary
}}
""".strip()


def _skip_unless_two_pg_urls():
    pg_url_1 = os.environ.get("HASURA_GRAPHQL_PG_SOURCE_URL_1")
    pg_url_2 = os.environ.get("HASURA_GRAPHQL_PG_SOURCE_URL_2")
    if not pg_url_1 or not pg_url_2 or pg_url_1 == pg_url_2:
        pytest.skip("Two distinct PG URLs required for connection routing tests")
    return pg_url_1, pg_url_2


def _setup_table_on_db(pg_url, source_value):
    """Create the conn_routing_test table on a database with a specific source value."""
    conn = psycopg2.connect(pg_url)
    conn.autocommit = True
    cur = conn.cursor()
    cur.execute("""
        CREATE TABLE IF NOT EXISTS conn_routing_test (
            id SERIAL PRIMARY KEY,
            source TEXT NOT NULL
        );
        DELETE FROM conn_routing_test;
        INSERT INTO conn_routing_test (id, source) VALUES (1, %s);
    """, (source_value,))
    cur.close()
    conn.close()


def _teardown_table_on_db(pg_url):
    """Drop the conn_routing_test table from a database."""
    try:
        conn = psycopg2.connect(pg_url)
        conn.autocommit = True
        cur = conn.cursor()
        cur.execute("DROP TABLE IF EXISTS conn_routing_test CASCADE;")
        cur.close()
        conn.close()
    except Exception:
        pass


def _add_source_with_template(hge_ctx, connection_template, connection_set=None):
    """Drop the default source and re-add it with a connection template."""
    metadata_api(hge_ctx, {
        "type": "pg_drop_source",
        "args": {"name": "default", "cascade": True},
    })

    source_config = {
        "connection_info": {
            "database_url": {"from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_1"},
        },
        "connection_template": {
            "version": 1,
            "template": connection_template,
        },
    }
    if connection_set:
        source_config["connection_set"] = connection_set

    resp = metadata_api(hge_ctx, {
        "type": "pg_add_source",
        "args": {"name": "default", "configuration": source_config},
    })
    assert resp.status_code == 200, f"pg_add_source failed: {resp.text}"
    return resp


def _restore_default_source(hge_ctx):
    """Restore the default source without connection template."""
    try:
        metadata_api(hge_ctx, {
            "type": "pg_drop_source",
            "args": {"name": "default", "cascade": True},
        })
    except Exception:
        pass
    try:
        metadata_api(hge_ctx, {
            "type": "pg_add_source",
            "args": {
                "name": "default",
                "configuration": {
                    "connection_info": {
                        "database_url": {"from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_1"},
                    },
                },
            },
        })
    except Exception:
        pass


# ---------------------------------------------------------------------------
# Connection Template Routing Tests
# ---------------------------------------------------------------------------

@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration")
@pytest.mark.jwt("rsa")
class TestConnectionTemplateRouting:
    """Test that connection templates route queries to the correct database pool."""

    @pytest.fixture(scope="class", autouse=True)
    def setup_routing(self, hge_ctx, jwt_configuration):
        pg_url_1, pg_url_2 = _skip_unless_two_pg_urls()

        # Create tables with different data in each database
        _setup_table_on_db(pg_url_1, "primary")
        _setup_table_on_db(pg_url_2, "secondary")

        # Add source with connection template and connection set
        _add_source_with_template(
            hge_ctx,
            CONNECTION_TEMPLATE,
            connection_set=[{
                "name": "secondary",
                "connection_info": {
                    "database_url": {"from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_2"},
                },
            }],
        )

        # Track table and set permissions
        resp = metadata_api(hge_ctx, {
            "type": "bulk",
            "args": [
                {
                    "type": "pg_track_table",
                    "args": {
                        "source": "default",
                        "table": {"schema": "public", "name": "conn_routing_test"},
                    },
                },
                {
                    "type": "pg_create_select_permission",
                    "args": {
                        "source": "default",
                        "table": {"schema": "public", "name": "conn_routing_test"},
                        "role": "user",
                        "permission": {"columns": "*", "filter": {}},
                    },
                },
            ],
        })
        assert resp.status_code == 200, f"Track/permission setup failed: {resp.text}"

        yield

        _teardown_table_on_db(pg_url_1)
        _teardown_table_on_db(pg_url_2)
        _restore_default_source(hge_ctx)

    def test_query_without_routing_returns_primary_data(self, hge_ctx, jwt_configuration):
        """Without routing session variable, queries should hit the primary database."""
        resp = graphql(
            hge_ctx,
            "{ conn_routing_test { id source } }",
            jwt_conf=jwt_configuration,
            role="user",
        )
        body = resp.json()
        assert "data" in body, f"Expected data, got: {body}"
        rows = body["data"]["conn_routing_test"]
        assert len(rows) == 1
        assert rows[0]["source"] == "primary", f"Expected 'primary', got: {rows[0]['source']}"

    def test_query_with_routing_returns_secondary_data(self, hge_ctx, jwt_configuration):
        """With x-hasura-route=secondary, queries should hit the secondary database."""
        resp = graphql(
            hge_ctx,
            "{ conn_routing_test { id source } }",
            jwt_conf=jwt_configuration,
            role="user",
            extra_claims={"x-hasura-route": "secondary"},
        )
        body = resp.json()
        assert "data" in body, f"Expected data, got: {body}"
        rows = body["data"]["conn_routing_test"]
        assert len(rows) == 1
        assert rows[0]["source"] == "secondary", f"Expected 'secondary', got: {rows[0]['source']}"

    def test_admin_bypasses_template(self, hge_ctx):
        """Admin requests bypass connection template resolution and hit primary."""
        resp = graphql(
            hge_ctx,
            "{ conn_routing_test { id source } }",
        )
        body = resp.json()
        assert "data" in body, f"Expected data, got: {body}"
        rows = body["data"]["conn_routing_test"]
        assert len(rows) == 1
        assert rows[0]["source"] == "primary", \
            f"Admin should always get primary data, got: {rows[0]['source']}"


# ---------------------------------------------------------------------------
# Connection Template Metadata API Tests
# ---------------------------------------------------------------------------

@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration")
@pytest.mark.jwt("rsa")
class TestConnectionTemplateMetadataAPI:
    """Test the pg_test_connection_template metadata API."""

    @pytest.fixture(scope="class", autouse=True)
    def setup_template_source(self, hge_ctx, jwt_configuration):
        _skip_unless_two_pg_urls()

        _add_source_with_template(
            hge_ctx,
            CONNECTION_TEMPLATE,
            connection_set=[{
                "name": "secondary",
                "connection_info": {
                    "database_url": {"from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_2"},
                },
            }],
        )

        yield

        _restore_default_source(hge_ctx)

    def test_resolves_to_primary(self, hge_ctx):
        """Template resolves to primary when route is not 'secondary'."""
        resp = metadata_api(hge_ctx, {
            "type": "pg_test_connection_template",
            "args": {
                "source_name": "default",
                "request_context": {
                    "headers": {},
                    "session": {"x-hasura-role": "user"},
                    "query": {
                        "operation_name": "MyQuery",
                        "operation_type": "query",
                    },
                },
            },
        })
        assert resp.status_code == 200, f"pg_test_connection_template failed: {resp.text}"
        body = resp.json()
        assert body["result"]["routing_to"] == "primary", f"Expected primary, got: {body}"

    def test_resolves_to_connection_set(self, hge_ctx):
        """Template resolves to connection set member when x-hasura-route=secondary."""
        resp = metadata_api(hge_ctx, {
            "type": "pg_test_connection_template",
            "args": {
                "source_name": "default",
                "request_context": {
                    "headers": {},
                    "session": {
                        "x-hasura-role": "user",
                        "x-hasura-route": "secondary",
                    },
                    "query": {
                        "operation_name": "MyQuery",
                        "operation_type": "query",
                    },
                },
            },
        })
        assert resp.status_code == 200, f"pg_test_connection_template failed: {resp.text}"
        body = resp.json()
        assert body["result"]["routing_to"] == "connection_set", \
            f"Expected connection_set, got: {body}"

    def test_admin_role_rejected(self, hge_ctx):
        """The test API rejects admin role since templates only apply to non-admin."""
        resp = metadata_api(hge_ctx, {
            "type": "pg_test_connection_template",
            "args": {
                "source_name": "default",
                "request_context": {
                    "headers": {},
                    "session": {"x-hasura-role": "admin"},
                    "query": {
                        "operation_name": "MyQuery",
                        "operation_type": "query",
                    },
                },
            },
        })
        assert resp.status_code == 400, \
            f"Expected 400 for admin role, got {resp.status_code}: {resp.text}"


# ---------------------------------------------------------------------------
# Error Handling Tests
# ---------------------------------------------------------------------------

@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration")
@pytest.mark.jwt("rsa")
class TestConnectionTemplateErrors:
    """Test error handling for connection templates."""

    @pytest.fixture(scope="class", autouse=True)
    def setup_bad_template(self, hge_ctx, jwt_configuration):
        pg_url_1, pg_url_2 = _skip_unless_two_pg_urls()

        _setup_table_on_db(pg_url_1, "primary")

        # Template that always tries to access a nonexistent connection set member.
        # Kriti will either error (key not found) or return null which fails parsing.
        bad_template = '{{ $.connection_set.nonexistent }}'

        _add_source_with_template(
            hge_ctx,
            bad_template,
            connection_set=[{
                "name": "secondary",
                "connection_info": {
                    "database_url": {"from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_2"},
                },
            }],
        )

        resp = metadata_api(hge_ctx, {
            "type": "bulk",
            "args": [
                {
                    "type": "pg_track_table",
                    "args": {
                        "source": "default",
                        "table": {"schema": "public", "name": "conn_routing_test"},
                    },
                },
                {
                    "type": "pg_create_select_permission",
                    "args": {
                        "source": "default",
                        "table": {"schema": "public", "name": "conn_routing_test"},
                        "role": "user",
                        "permission": {"columns": "*", "filter": {}},
                    },
                },
            ],
        })
        assert resp.status_code == 200, f"Track/permission setup failed: {resp.text}"

        yield

        _teardown_table_on_db(pg_url_1)
        _restore_default_source(hge_ctx)

    def test_nonexistent_member_returns_error(self, hge_ctx, jwt_configuration):
        """Querying with a template that references a nonexistent member returns an error."""
        resp = graphql(
            hge_ctx,
            "{ conn_routing_test { id source } }",
            jwt_conf=jwt_configuration,
            role="user",
        )
        body = resp.json()
        errors = body.get("errors", [])
        assert len(errors) > 0, \
            f"Expected error for nonexistent connection set member, got: {body}"
