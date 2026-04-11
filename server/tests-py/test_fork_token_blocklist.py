"""
Black-box tests for the fork's JWT Token Block-list feature.

The engine polls tenant.tokens for blocked tokens and rejects requests
carrying a blocked jti. Uses a short poll interval for faster tests.
"""

import json
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


def block_token(engine, token_id, token_type="backend"):
    """Insert a blocked token into tenant.tokens."""
    with engine.connect() as conn:
        conn.execute(
            sqlalchemy.text(
                "INSERT INTO tenant.tokens (token_id, blocked, token_type) "
                "VALUES (:tid, true, :ttype)"
            ),
            {"tid": str(token_id), "ttype": token_type},
        )


def clear_tokens(engine):
    """Remove all rows from tenant.tokens."""
    with engine.connect() as conn:
        conn.execute(sqlalchemy.text("DELETE FROM tenant.tokens"))


# The poll interval is set via HASURA_GRAPHQL_TOKEN_POLL_INTERVAL.
# We use 1000ms for tests; wait a bit longer than that for the poller to pick
# up changes.
POLL_WAIT = 2.0


@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
@pytest.mark.hge_env("HASURA_GRAPHQL_TOKEN_POLL_INTERVAL", "1000")
class TestTokenBlocklist:

    @classmethod
    def dir(cls):
        return "queries/fork/token_blocklist"

    @pytest.fixture(autouse=True)
    def _cleanup(self, hge_ctx):
        yield
        clear_tokens(hge_ctx.engine)

    def test_valid_token_accepted(self, hge_ctx, jwt_configuration):
        token_id = uuid.uuid4()
        query = "query { fork_items { id name } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=token_id)
        assert resp.status_code == 200, resp.text
        body = resp.json()
        assert "data" in body

    def test_blocked_token_rejected(self, hge_ctx, jwt_configuration):
        token_id = uuid.uuid4()
        block_token(hge_ctx.engine, token_id, "backend")
        # Wait for the poller to pick up the blocked token
        time.sleep(POLL_WAIT)
        query = "query { fork_items { id name } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=token_id)
        assert resp.status_code == 400, f"Expected 400, got {resp.status_code}: {resp.text}"
        body = resp.json()
        errors = body.get("errors", [])
        assert any("Invalid token" in str(e) for e in errors), \
            f"Expected 'Invalid token' error, got: {errors}"

    def test_non_backend_token_type_not_blocked(self, hge_ctx, jwt_configuration):
        token_id = uuid.uuid4()
        # Block with token_type='frontend', should NOT block backend requests
        block_token(hge_ctx.engine, token_id, "frontend")
        time.sleep(POLL_WAIT)
        query = "query { fork_items { id name } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=token_id)
        assert resp.status_code == 200, f"Expected 200, got {resp.status_code}: {resp.text}"

    def test_token_without_jti_passes(self, hge_ctx, jwt_configuration):
        query = "query { fork_items { id name } }"
        # No jti claim in the JWT
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=None)
        assert resp.status_code == 200, resp.text

    def test_invalid_uuid_jti_rejected(self, hge_ctx, jwt_configuration):
        # Forge a JWT with a non-UUID jti value
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
        assert resp.status_code == 400, f"Expected 400, got {resp.status_code}: {resp.text}"

    def test_blocking_token_mid_session(self, hge_ctx, jwt_configuration):
        token_id = uuid.uuid4()
        query = "query { fork_items { id name } }"

        # First request succeeds
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=token_id)
        assert resp.status_code == 200, resp.text

        # Block the token
        block_token(hge_ctx.engine, token_id, "backend")
        time.sleep(POLL_WAIT)

        # Second request with same token should fail
        resp = graphql(hge_ctx, query, jwt_configuration, "user", jti=token_id)
        assert resp.status_code == 400, f"Expected 400, got {resp.status_code}: {resp.text}"
