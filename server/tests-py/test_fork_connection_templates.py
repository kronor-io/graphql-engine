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


def run_sql(hge_ctx, sql, source="default"):
    """Run SQL via the v2/query API on a given source."""
    headers = {
        "X-Hasura-Admin-Secret": hge_ctx.hge_key,
        "Content-Type": "application/json",
    }
    return requests.post(
        f"{hge_ctx.hge_url}/v2/query",
        json={"type": "run_sql", "args": {"sql": sql, "source": source}},
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


def setup_secondary_db(pg_url_2):
    """Create the test table on the secondary database with different data."""
    conn = psycopg2.connect(pg_url_2)
    conn.autocommit = True
    cur = conn.cursor()
    cur.execute("""
        CREATE TABLE IF NOT EXISTS conn_routing_test (
            id SERIAL PRIMARY KEY,
            source TEXT NOT NULL
        );
        DELETE FROM conn_routing_test;
        INSERT INTO conn_routing_test (id, source) VALUES (1, 'secondary');
    """)
    cur.close()
    conn.close()


def teardown_secondary_db(pg_url_2):
    """Drop the test table from the secondary database."""
    try:
        conn = psycopg2.connect(pg_url_2)
        conn.autocommit = True
        cur = conn.cursor()
        cur.execute("DROP TABLE IF EXISTS conn_routing_test CASCADE;")
        cur.close()
        conn.close()
    except Exception:
        pass


@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
class TestConnectionTemplateRouting:
    """Test that connection templates route queries to the correct database pool."""

    @classmethod
    def dir(cls):
        return "queries/fork/connection_templates"

    @pytest.fixture(scope="class", autouse=True)
    def configure_connection_routing(self, hge_ctx, jwt_configuration):
        """
        After the base DB state is set up (via per_class_tests_db_state),
        reconfigure the default source to include a connection set and
        connection template, then set up the secondary database.
        """
        pg_url_1 = os.environ.get("HASURA_GRAPHQL_PG_SOURCE_URL_1")
        pg_url_2 = os.environ.get("HASURA_GRAPHQL_PG_SOURCE_URL_2")

        if not pg_url_1 or not pg_url_2 or pg_url_1 == pg_url_2:
            pytest.skip("Two distinct PG URLs required for connection routing tests")

        # Set up the secondary database with its own data
        setup_secondary_db(pg_url_2)

        # Drop the default source to reconfigure it with connection template
        resp = metadata_api(hge_ctx, {
            "type": "pg_drop_source",
            "args": {"name": "default", "cascade": True},
        })
        assert resp.status_code == 200, f"pg_drop_source failed: {resp.text}"

        # Re-add the default source with connection set and template
        resp = metadata_api(hge_ctx, {
            "type": "pg_add_source",
            "args": {
                "name": "default",
                "configuration": {
                    "connection_info": {
                        "database_url": {"from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_1"},
                    },
                    "connection_set": [
                        {
                            "name": "secondary",
                            "connection_info": {
                                "database_url": {"from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_2"},
                            },
                        }
                    ],
                    "connection_template": {
                        "version": 1,
                        "template": CONNECTION_TEMPLATE,
                    },
                },
            },
        })
        assert resp.status_code == 200, f"pg_add_source failed: {resp.text}"

        # Re-track the table and set up permissions
        resp = metadata_api(hge_ctx, {
            "type": "bulk",
            "args": [
                {"type": "pg_track_table", "args": {"source": "default", "table": {"schema": "public", "name": "conn_routing_test"}}},
                {"type": "pg_create_select_permission", "args": {"source": "default", "table": {"schema": "public", "name": "conn_routing_test"}, "role": "user", "permission": {"columns": "*", "filter": {}}}},
            ],
        })
        assert resp.status_code == 200, f"Re-track table failed: {resp.text}"

        yield

        # Cleanup: drop source and clean secondary DB
        teardown_secondary_db(pg_url_2)
        try:
            metadata_api(hge_ctx, {
                "type": "pg_drop_source",
                "args": {"name": "default", "cascade": True},
            })
        except Exception:
            pass

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
        assert rows[0]["source"] == "primary", f"Admin should always get primary data, got: {rows[0]['source']}"


@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
class TestConnectionTemplateMetadataAPI:
    """Test the pg_test_connection_template metadata API."""

    @classmethod
    def dir(cls):
        return "queries/fork/connection_templates"

    @pytest.fixture(scope="class", autouse=True)
    def configure_source_with_template(self, hge_ctx, jwt_configuration):
        """Reconfigure default source with a connection template."""
        pg_url_1 = os.environ.get("HASURA_GRAPHQL_PG_SOURCE_URL_1")
        pg_url_2 = os.environ.get("HASURA_GRAPHQL_PG_SOURCE_URL_2")

        if not pg_url_1 or not pg_url_2 or pg_url_1 == pg_url_2:
            pytest.skip("Two distinct PG URLs required for connection template tests")

        # Drop and re-add with connection template
        resp = metadata_api(hge_ctx, {
            "type": "pg_drop_source",
            "args": {"name": "default", "cascade": True},
        })
        assert resp.status_code == 200, f"pg_drop_source failed: {resp.text}"

        resp = metadata_api(hge_ctx, {
            "type": "pg_add_source",
            "args": {
                "name": "default",
                "configuration": {
                    "connection_info": {
                        "database_url": {"from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_1"},
                    },
                    "connection_set": [
                        {
                            "name": "secondary",
                            "connection_info": {
                                "database_url": {"from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_2"},
                            },
                        }
                    ],
                    "connection_template": {
                        "version": 1,
                        "template": CONNECTION_TEMPLATE,
                    },
                },
            },
        })
        assert resp.status_code == 200, f"pg_add_source with template failed: {resp.text}"

        yield

        try:
            metadata_api(hge_ctx, {
                "type": "pg_drop_source",
                "args": {"name": "default", "cascade": True},
            })
        except Exception:
            pass

    def test_connection_template_resolves_to_primary(self, hge_ctx):
        """Test that the template resolves to primary when route is not 'secondary'."""
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
        assert body["result"]["routing_to"] == "primary", f"Expected routing_to=primary, got: {body}"

    def test_connection_template_resolves_to_connection_set(self, hge_ctx):
        """Test that the template resolves to 'secondary' when x-hasura-route=secondary."""
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
        assert body["result"]["routing_to"] == "connection_set", f"Expected routing_to=connection_set, got: {body}"

    def test_connection_template_with_admin_role_fails(self, hge_ctx):
        """Test that admin role is rejected by the test API."""
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
        # The test API rejects admin role
        assert resp.status_code == 400, f"Expected 400 for admin role, got {resp.status_code}: {resp.text}"


@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
class TestConnectionTemplateErrors:
    """Test error handling for connection templates."""

    @classmethod
    def dir(cls):
        return "queries/fork/connection_templates"

    @pytest.fixture(scope="class", autouse=True)
    def configure_source_with_bad_template(self, hge_ctx, jwt_configuration):
        """Configure source with a template that routes to a nonexistent member."""
        pg_url_1 = os.environ.get("HASURA_GRAPHQL_PG_SOURCE_URL_1")
        pg_url_2 = os.environ.get("HASURA_GRAPHQL_PG_SOURCE_URL_2")

        if not pg_url_1 or not pg_url_2 or pg_url_1 == pg_url_2:
            pytest.skip("Two distinct PG URLs required for connection template tests")

        # Template that always routes to a nonexistent member
        bad_template = '{{ $.connection_set.nonexistent }}'

        resp = metadata_api(hge_ctx, {
            "type": "pg_drop_source",
            "args": {"name": "default", "cascade": True},
        })
        assert resp.status_code == 200, f"pg_drop_source failed: {resp.text}"

        resp = metadata_api(hge_ctx, {
            "type": "pg_add_source",
            "args": {
                "name": "default",
                "configuration": {
                    "connection_info": {
                        "database_url": {"from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_1"},
                    },
                    "connection_set": [
                        {
                            "name": "secondary",
                            "connection_info": {
                                "database_url": {"from_env": "HASURA_GRAPHQL_PG_SOURCE_URL_2"},
                            },
                        }
                    ],
                    "connection_template": {
                        "version": 1,
                        "template": bad_template,
                    },
                },
            },
        })
        assert resp.status_code == 200, f"pg_add_source failed: {resp.text}"

        # Track table and permissions
        resp = metadata_api(hge_ctx, {
            "type": "bulk",
            "args": [
                {"type": "pg_track_table", "args": {"source": "default", "table": {"schema": "public", "name": "conn_routing_test"}}},
                {"type": "pg_create_select_permission", "args": {"source": "default", "table": {"schema": "public", "name": "conn_routing_test"}, "role": "user", "permission": {"columns": "*", "filter": {}}}},
            ],
        })
        assert resp.status_code == 200, f"Re-track table failed: {resp.text}"

        yield

        try:
            metadata_api(hge_ctx, {
                "type": "pg_drop_source",
                "args": {"name": "default", "cascade": True},
            })
        except Exception:
            pass

    def test_nonexistent_connection_set_member_returns_error(self, hge_ctx, jwt_configuration):
        """Querying with a template that references a nonexistent member returns an error."""
        resp = graphql(
            hge_ctx,
            "{ conn_routing_test { id source } }",
            jwt_conf=jwt_configuration,
            role="user",
        )
        body = resp.json()
        errors = body.get("errors", [])
        assert len(errors) > 0, f"Expected error for nonexistent connection set member, got: {body}"
        error_msg = str(errors[0])
        assert "not found" in error_msg.lower() or "template" in error_msg.lower(), \
            f"Expected connection-related error, got: {errors}"
