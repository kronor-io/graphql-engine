"""
Black-box tests for the fork's API Limits feature.

Covers: depth limit, node limit, time limit, batch limit, and lifecycle.
"""

import json
import pytest
import jwt
import requests


def make_jwt_token(jwt_conf, role, extra_claims=None):
    """Generate a JWT for the given role using the test JWT configuration."""
    hasura_claims = {
        "x-hasura-default-role": role,
        "x-hasura-allowed-roles": [role],
        "x-hasura-user-id": "1",
    }
    payload = {
        "https://hasura.io/jwt/claims": hasura_claims,
    }
    if extra_claims:
        payload.update(extra_claims)
    return jwt.encode(payload, jwt_conf.private_key, algorithm=jwt_conf.algorithm)


def graphql(hge_ctx, query, jwt_conf=None, role=None):
    """Send a GraphQL query to the engine."""
    headers = {"Content-Type": "application/json"}
    if role and jwt_conf:
        token = make_jwt_token(jwt_conf, role)
        headers["Authorization"] = f"Bearer {token}"
    else:
        headers["X-Hasura-Admin-Secret"] = hge_ctx.hge_key
    body = {"query": query} if isinstance(query, str) else query
    return requests.post(f"{hge_ctx.hge_url}/v1/graphql", json=body, headers=headers)


def graphql_batch(hge_ctx, queries, jwt_conf=None, role=None):
    """Send a batched GraphQL request."""
    headers = {"Content-Type": "application/json"}
    if role and jwt_conf:
        token = make_jwt_token(jwt_conf, role)
        headers["Authorization"] = f"Bearer {token}"
    else:
        headers["X-Hasura-Admin-Secret"] = hge_ctx.hge_key
    body = [{"query": q} if isinstance(q, str) else q for q in queries]
    return requests.post(f"{hge_ctx.hge_url}/v1/graphql", json=body, headers=headers)


def set_api_limits(hge_ctx, **kwargs):
    """Set API limits via the metadata API."""
    headers = {
        "X-Hasura-Admin-Secret": hge_ctx.hge_key,
        "Content-Type": "application/json",
    }
    resp = requests.post(
        f"{hge_ctx.hge_url}/v1/metadata",
        json={"type": "set_api_limits", "args": kwargs},
        headers=headers,
    )
    assert resp.status_code == 200, f"set_api_limits failed: {resp.text}"
    return resp


def remove_api_limits(hge_ctx):
    """Remove all API limits via the metadata API."""
    headers = {
        "X-Hasura-Admin-Secret": hge_ctx.hge_key,
        "Content-Type": "application/json",
    }
    resp = requests.post(
        f"{hge_ctx.hge_url}/v1/metadata",
        json={"type": "remove_api_limits", "args": {}},
        headers=headers,
    )
    assert resp.status_code == 200, f"remove_api_limits failed: {resp.text}"
    return resp


# ---------------------------------------------------------------------------
# Depth Limit
# ---------------------------------------------------------------------------

@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
class TestDepthLimit:

    @classmethod
    def dir(cls):
        return "queries/fork/api_limits"

    @pytest.fixture(autouse=True)
    def _cleanup(self, hge_ctx):
        yield
        try:
            remove_api_limits(hge_ctx)
        except Exception:
            pass

    def test_query_at_depth_limit_succeeds(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 5})
        # Depth: fork_articles(1) > author(2) > articles(3) > author(4) > name(5)
        query = """
        query {
          fork_articles {
            author {
              articles {
                author {
                  name
                }
              }
            }
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 200, resp.text
        body = resp.json()
        assert "data" in body, f"Expected data in response, got: {body}"

    def test_query_exceeding_depth_limit_rejected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 3})
        # Depth 5, exceeds global limit of 3
        query = """
        query {
          fork_articles {
            author {
              articles {
                author {
                  name
                }
              }
            }
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 429, f"Expected 429, got {resp.status_code}: {resp.text}"

    def test_per_role_override_respected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 10, "per_role": {"user": 2}})
        # Depth 3: fork_articles > author > name
        query = """
        query {
          fork_articles {
            author {
              name
            }
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 429, f"Expected 429 for user, got {resp.status_code}: {resp.text}"

    def test_role_without_override_uses_global(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 10, "per_role": {"user": 2}})
        # Depth 3, within global limit of 10
        query = """
        query {
          fork_articles {
            author {
              name
            }
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "anonymous")
        assert resp.status_code == 200, f"Expected 200 for anonymous, got {resp.status_code}: {resp.text}"

    def test_introspection_fields_exempt(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 1})
        # Deep introspection query — depth comes from __schema/__type
        query = """
        query {
          __schema {
            types {
              name
              fields {
                name
                type {
                  name
                }
              }
            }
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 200, f"Expected 200 for introspection, got {resp.status_code}: {resp.text}"

    def test_limits_disabled_globally(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 1}, disabled=True)
        query = """
        query {
          fork_articles {
            author {
              articles {
                author { name }
              }
            }
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 200, f"Expected 200 when disabled, got {resp.status_code}: {resp.text}"


# ---------------------------------------------------------------------------
# Node Limit
# ---------------------------------------------------------------------------

@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
class TestNodeLimit:

    @classmethod
    def dir(cls):
        return "queries/fork/api_limits"

    @pytest.fixture(autouse=True)
    def _cleanup(self, hge_ctx):
        yield
        try:
            remove_api_limits(hge_ctx)
        except Exception:
            pass

    def test_query_at_node_limit_succeeds(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, node_limit={"global": 10})
        # A few nodes; limit is generous enough to pass
        query = "query { fork_articles { id title content } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 200, resp.text

    def test_query_exceeding_node_limit_rejected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, node_limit={"global": 2})
        # 4 nodes, exceeds limit of 2
        query = "query { fork_articles { id title content } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 429, f"Expected 429, got {resp.status_code}: {resp.text}"

    def test_per_role_override_respected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, node_limit={"global": 100, "per_role": {"user": 2}})
        query = "query { fork_articles { id title content } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 429, f"Expected 429 for user, got {resp.status_code}: {resp.text}"

    def test_introspection_fields_exempt(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, node_limit={"global": 2})
        # __typename is an introspection field and should be exempt
        query = """
        query {
          fork_articles {
            __typename
            id
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        # fork_articles + id = 2 nodes; __typename is exempt
        assert resp.status_code == 200, f"Expected 200, got {resp.status_code}: {resp.text}"


# ---------------------------------------------------------------------------
# Time Limit
# ---------------------------------------------------------------------------

@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
class TestTimeLimit:

    @classmethod
    def dir(cls):
        return "queries/fork/api_limits"

    @pytest.fixture(autouse=True)
    def _cleanup(self, hge_ctx):
        yield
        try:
            remove_api_limits(hge_ctx)
        except Exception:
            pass

    def test_fast_query_within_time_limit_succeeds(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, time_limit={"global": 30})
        query = "query { fork_articles { id title } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 200, resp.text

    def test_slow_query_exceeding_time_limit_killed(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, time_limit={"global": 1})
        # fork_slow_function does pg_sleep(3), exceeding the 1-second limit
        query = "query { fork_slow_function { id title } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 500, f"Expected 500, got {resp.status_code}: {resp.text}"
        body = resp.json()
        errors = body.get("errors", [])
        assert any(
            "time-limit-exceeded" in str(e) or "timed out" in str(e).lower()
            for e in errors
        ), f"Expected time-limit-exceeded error, got: {errors}"

    def test_per_role_override_respected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, time_limit={"global": 30, "per_role": {"user": 1}})
        query = "query { fork_slow_function { id title } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 500, f"Expected 500 for user, got {resp.status_code}: {resp.text}"


# ---------------------------------------------------------------------------
# Batch Limit
# ---------------------------------------------------------------------------

@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
class TestBatchLimit:

    @classmethod
    def dir(cls):
        return "queries/fork/api_limits"

    @pytest.fixture(autouse=True)
    def _cleanup(self, hge_ctx):
        yield
        try:
            remove_api_limits(hge_ctx)
        except Exception:
            pass

    def test_batch_within_limit_succeeds(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, batch_limit={"global": 3})
        queries = [
            "query { fork_articles { id } }",
            "query { fork_articles { title } }",
            "query { fork_articles { content } }",
        ]
        resp = graphql_batch(hge_ctx, queries, jwt_configuration, "user")
        assert resp.status_code == 200, resp.text

    def test_batch_exceeding_limit_rejected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, batch_limit={"global": 2})
        queries = [
            "query { fork_articles { id } }",
            "query { fork_articles { title } }",
            "query { fork_articles { content } }",
        ]
        resp = graphql_batch(hge_ctx, queries, jwt_configuration, "user")
        assert resp.status_code == 429, f"Expected 429, got {resp.status_code}: {resp.text}"

    def test_per_role_override_respected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, batch_limit={"global": 10, "per_role": {"user": 1}})
        queries = [
            "query { fork_articles { id } }",
            "query { fork_articles { title } }",
        ]
        resp = graphql_batch(hge_ctx, queries, jwt_configuration, "user")
        assert resp.status_code == 429, f"Expected 429 for user, got {resp.status_code}: {resp.text}"


# ---------------------------------------------------------------------------
# Limit Lifecycle
# ---------------------------------------------------------------------------

@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
class TestLimitLifecycle:

    @classmethod
    def dir(cls):
        return "queries/fork/api_limits"

    @pytest.fixture(autouse=True)
    def _cleanup(self, hge_ctx):
        yield
        try:
            remove_api_limits(hge_ctx)
        except Exception:
            pass

    def test_limits_applied_after_set(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 1})
        query = "query { fork_articles { author { name } } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 429

    def test_limits_removed_after_remove(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 1})
        remove_api_limits(hge_ctx)
        query = "query { fork_articles { author { name } } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 200, resp.text

    def test_updating_limits_replaces_previous(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 1})
        set_api_limits(hge_ctx, depth_limit={"global": 10})
        query = "query { fork_articles { author { name } } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert resp.status_code == 200, resp.text
