"""
Black-box tests for the fork's JWT Token Block-list feature.

The engine polls tenant.tokens for blocked tokens and rejects requests
carrying a blocked jti. Uses a short poll interval for faster tests.
"""

import time
import uuid

import jwt
import pytest
import requests
import sqlalchemy


def make_jwt_token(jwt_conf, role, jti=None, extra_claims=None):
    """Generate a JWT with optional jti claim."""
    hasura_claims = {
        "x-hasura-default-role": role,
        "x-hasura-allowed-roles": [role],
        "x-hasura-user-id": "1",
    }
    payload = {
        "https://hasura.io/jwt/claims": hasura_claims,
    }
    if jti is not None:
        payload["jti"] = str(jti)
    if extra_claims:
        payload.update(extra_claims)
    return jwt.encode(payload, jwt_conf.private_key, algorithm=jwt_conf.algorithm)


def graphql(hge_ctx, query, jwt_conf, role, jti=None):
    """Send a GraphQL query with a JWT bearing the given role and jti."""
    token = make_jwt_token(jwt_conf, role, jti=jti)
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {token}",
    }
    return requests.post(
        f"{hge_ctx.hge_url}/v1/graphql",
        json={"query": query},
        headers=headers,
    )


def assert_no_errors(resp, msg=""):
    body = resp.json()
    assert "data" in body, f"{msg} Expected data, got: {body}"
    assert "errors" not in body, f"{msg} Unexpected errors: {body.get('errors')}"


def assert_graphql_error(resp, error_substring, msg=""):
    body = resp.json()
    errors = body.get("errors", [])
    assert len(errors) > 0, f"{msg} Expected errors containing '{error_substring}', got: {body}"
    assert any(error_substring in str(e) for e in errors), \
        f"{msg} Expected error containing '{error_substring}', got: {errors}"


def block_token(engine, token_id, token_type="backend"):
    """Insert a blocked token into tenant.tokens in the metadata DB."""
    with engine.begin() as conn:
        conn.execute(
            sqlalchemy.text(
                "INSERT INTO tenant.tokens (token_id, blocked, token_type) "
                "VALUES (:tid, true, :ttype)"
            ),
            {"tid": str(token_id), "ttype": token_type},
        )


def clear_tokens(engine):
    """Remove all rows from tenant.tokens in the metadata DB."""
    try:
        with engine.begin() as conn:
            conn.execute(sqlalchemy.text("DELETE FROM tenant.tokens"))
    except Exception:
        pass


# The poll interval is set via HASURA_GRAPHQL_TOKEN_POLL_INTERVAL.
# We use 1000ms for tests; wait a bit longer than that for the poller to pick
# up changes.
POLL_WAIT = 3.0


@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
@pytest.mark.hge_env("HASURA_GRAPHQL_TOKEN_POLL_INTERVAL", "1000")
class TestTokenBlocklist:

    @classmethod
    def dir(cls):
        return "queries/fork/token_blocklist"

    @pytest.fixture(scope="class", autouse=True)
    def metadata_db_engine(self, metadata_schema_url):
        """Create tenant.tokens in the metadata DB (where the poller queries)."""
        engine = sqlalchemy.create_engine(metadata_schema_url)
        with engine.begin() as conn:
            conn.execute(sqlalchemy.text("CREATE SCHEMA IF NOT EXISTS tenant"))
            conn.execute(sqlalchemy.text("""
                CREATE TABLE IF NOT EXISTS tenant.tokens (
                    token_id UUID NOT NULL,
                    blocked BOOLEAN NOT NULL DEFAULT false,
                    token_type TEXT NOT NULL
                )
            """))
        yield engine
        with engine.begin() as conn:
            conn.execute(sqlalchemy.text("DROP TABLE IF EXISTS tenant.tokens CASCADE"))
            conn.execute(sqlalchemy.text("DROP SCHEMA IF EXISTS tenant CASCADE"))
        engine.dispose()

    @pytest.fixture(autouse=True)
    def _cleanup(self, metadata_db_engine):
        yield
        clear_tokens(metadata_db_engine)

    def test_valid_token_accepted(self, hge_ctx, jwt_configuration):
        token_id = uuid.uuid4()
        query = "query { fork_items { id name } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=token_id)
        assert_no_errors(resp)

    def test_blocked_token_rejected(self, hge_ctx, jwt_configuration, metadata_db_engine):
        token_id = uuid.uuid4()
        block_token(metadata_db_engine, token_id, "backend")
        time.sleep(POLL_WAIT)
        query = "query { fork_items { id name } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=token_id)
        assert_graphql_error(resp, "Invalid token")

    def test_non_backend_token_type_not_blocked(self, hge_ctx, jwt_configuration, metadata_db_engine):
        token_id = uuid.uuid4()
        block_token(metadata_db_engine, token_id, "frontend")
        time.sleep(POLL_WAIT)
        query = "query { fork_items { id name } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=token_id)
        assert_no_errors(resp)

    def test_token_without_jti_passes(self, hge_ctx, jwt_configuration):
        query = "query { fork_items { id name } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=None)
        assert_no_errors(resp)

    def test_invalid_uuid_jti_rejected(self, hge_ctx, jwt_configuration):
        hasura_claims = {
            "x-hasura-default-role": "user",
            "x-hasura-allowed-roles": ["user"],
            "x-hasura-user-id": "1",
        }
        payload = {
            "https://hasura.io/jwt/claims": hasura_claims,
            "jti": "not-a-valid-uuid",
        }
        token = jwt.encode(payload, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {token}",
        }
        resp = requests.post(
            f"{hge_ctx.hge_url}/v1/graphql",
            json={"query": "query { fork_items { id } }"},
            headers=headers,
        )
        assert_graphql_error(resp, "Invalid token")

    def test_blocking_token_mid_session(self, hge_ctx, jwt_configuration, metadata_db_engine):
        token_id = uuid.uuid4()
        query = "query { fork_items { id name } }"

        # First request succeeds
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=token_id)
        assert_no_errors(resp)

        # Block the token
        block_token(metadata_db_engine, token_id, "backend")
        time.sleep(POLL_WAIT)

        # Second request with same token should fail
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=token_id)
        assert_graphql_error(resp, "Invalid token")
