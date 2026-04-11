"""
Black-box tests for the fork's API Limits feature.

Covers: depth limit, node limit, time limit, batch limit, and lifecycle.

Note: GraphQL always returns HTTP 200. Limit violations are returned as
errors in the response body, not via HTTP status codes.
"""

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


def assert_no_errors(resp, msg=""):
    """Assert the GraphQL response has data and no errors."""
    body = resp.json()
    assert "data" in body, f"{msg} Expected data, got: {body}"
    assert "errors" not in body, f"{msg} Unexpected errors: {body.get('errors')}"


def assert_graphql_error(resp, error_substring, msg=""):
    """Assert the GraphQL response contains an error with the given substring."""
    body = resp.json()
    errors = body.get("errors", [])
    assert len(errors) > 0, f"{msg} Expected errors containing '{error_substring}', got: {body}"
    assert any(error_substring in str(e) for e in errors), \
        f"{msg} Expected error containing '{error_substring}', got: {errors}"


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
        assert_no_errors(resp)

    def test_query_exceeding_depth_limit_rejected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 3})
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
        assert_graphql_error(resp, "depth limit exceeded")

    def test_per_role_override_respected(self, hge_ctx, jwt_configuration):
        # Depth counting: leaf=0, nested=1+max(children).
        # fork_articles > author > articles > title: depth = 3
        set_api_limits(hge_ctx, depth_limit={"global": 10, "per_role": {"user": 2}})
        query = """
        query {
          fork_articles {
            author {
              articles {
                title
              }
            }
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert_graphql_error(resp, "depth limit exceeded")

    def test_role_without_override_uses_global(self, hge_ctx, jwt_configuration):
        # Same query but as anonymous (no per-role override), within global limit
        set_api_limits(hge_ctx, depth_limit={"global": 10, "per_role": {"user": 2}})
        query = """
        query {
          fork_articles {
            author {
              articles {
                title
              }
            }
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "anonymous")
        assert_no_errors(resp)

    def test_introspection_fields_exempt(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 1})
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
        assert_no_errors(resp)

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
        assert_no_errors(resp)


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
        query = "query { fork_articles { id title content } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert_no_errors(resp)

    def test_query_exceeding_node_limit_rejected(self, hge_ctx, jwt_configuration):
        # Node counting: only fields with nested sub-selections count as 1.
        # fork_articles(1) > author(1) > articles(1) = 3 nodes
        set_api_limits(hge_ctx, node_limit={"global": 2})
        query = """
        query {
          fork_articles {
            author {
              articles {
                title
              }
            }
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert_graphql_error(resp, "too many nodes")

    def test_per_role_override_respected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, node_limit={"global": 100, "per_role": {"user": 2}})
        query = """
        query {
          fork_articles {
            author {
              articles {
                title
              }
            }
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert_graphql_error(resp, "too many nodes")

    def test_introspection_fields_exempt(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, node_limit={"global": 2})
        query = """
        query {
          fork_articles {
            __typename
            id
          }
        }
        """
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert_no_errors(resp)


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
        assert_no_errors(resp)

    def test_slow_query_exceeding_time_limit_killed(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, time_limit={"global": 1})
        query = "query { fork_slow_function { id title } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        body = resp.json()
        errors = body.get("errors", [])
        assert any(
            "time-limit-exceeded" in str(e) or "timed out" in str(e).lower()
            for e in errors
        ), f"Expected time-limit-exceeded error, got: {body}"

    def test_per_role_override_respected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, time_limit={"global": 30, "per_role": {"user": 1}})
        query = "query { fork_slow_function { id title } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        body = resp.json()
        errors = body.get("errors", [])
        assert any(
            "time-limit-exceeded" in str(e) or "timed out" in str(e).lower()
            for e in errors
        ), f"Expected time-limit-exceeded error, got: {body}"


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
        body = resp.json()
        # Batch response is a list of results
        assert isinstance(body, list), f"Expected list response, got: {body}"

    def test_batch_exceeding_limit_rejected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, batch_limit={"global": 2})
        queries = [
            "query { fork_articles { id } }",
            "query { fork_articles { title } }",
            "query { fork_articles { content } }",
        ]
        resp = graphql_batch(hge_ctx, queries, jwt_configuration, "user")
        assert_graphql_error(resp, "too many batched requests")

    def test_per_role_override_respected(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, batch_limit={"global": 10, "per_role": {"user": 1}})
        queries = [
            "query { fork_articles { id } }",
            "query { fork_articles { title } }",
        ]
        resp = graphql_batch(hge_ctx, queries, jwt_configuration, "user")
        assert_graphql_error(resp, "too many batched requests")


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
        assert_graphql_error(resp, "depth limit exceeded")

    def test_limits_removed_after_remove(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 1})
        remove_api_limits(hge_ctx)
        query = "query { fork_articles { author { name } } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert_no_errors(resp)

    def test_updating_limits_replaces_previous(self, hge_ctx, jwt_configuration):
        set_api_limits(hge_ctx, depth_limit={"global": 1})
        set_api_limits(hge_ctx, depth_limit={"global": 10})
        query = "query { fork_articles { author { name } } }"
        resp = graphql(hge_ctx, query, jwt_configuration, "user")
        assert_no_errors(resp)
