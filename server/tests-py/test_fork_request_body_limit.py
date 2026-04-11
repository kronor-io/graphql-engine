"""
Black-box tests for the fork's Request Body Size Limit feature.

The engine enforces a maximum request body size on all endpoints except
/v1/metadata. Configured via HASURA_GRAPHQL_MAX_REQUEST_BODY_LENGTH.
"""

import json
import pytest
import requests


def graphql_raw(hge_ctx, body_str):
    """Send raw body bytes to the GraphQL endpoint."""
    headers = {
        "X-Hasura-Admin-Secret": hge_ctx.hge_key,
        "Content-Type": "application/json",
    }
    return requests.post(
        f"{hge_ctx.hge_url}/v1/graphql",
        data=body_str.encode("utf-8"),
        headers=headers,
    )


def metadata_raw(hge_ctx, body_str):
    """Send raw body bytes to the metadata endpoint."""
    headers = {
        "X-Hasura-Admin-Secret": hge_ctx.hge_key,
        "Content-Type": "application/json",
    }
    return requests.post(
        f"{hge_ctx.hge_url}/v1/metadata",
        data=body_str.encode("utf-8"),
        headers=headers,
    )


# ---------------------------------------------------------------------------
# Tests with a custom low limit (1024 bytes)
# ---------------------------------------------------------------------------

@pytest.mark.admin_secret
@pytest.mark.usefixtures("per_class_tests_db_state")
@pytest.mark.hge_env("HASURA_GRAPHQL_MAX_REQUEST_BODY_LENGTH", "1024")
class TestRequestBodyLimitCustom:

    @classmethod
    def dir(cls):
        return "queries/fork/request_body_limit"

    def test_request_within_limit_succeeds(self, hge_ctx):
        small_query = json.dumps({"query": "{ fork_items { id } }"})
        assert len(small_query) < 1024
        resp = graphql_raw(hge_ctx, small_query)
        assert resp.status_code == 200, resp.text

    def test_request_exceeding_limit_rejected(self, hge_ctx):
        # Create a body larger than 1024 bytes using a padded variable
        padding = "x" * 2000
        big_body = json.dumps({
            "query": "query ($v: String) { fork_items { id } }",
            "variables": {"v": padding},
        })
        assert len(big_body) > 1024
        resp = graphql_raw(hge_ctx, big_body)
        assert resp.status_code == 400, f"Expected 400, got {resp.status_code}: {resp.text}"

    def test_metadata_endpoint_exempt(self, hge_ctx):
        # The /v1/metadata endpoint should NOT enforce the body size limit.
        # The body-size middleware returns HTTP 400 with an empty body when
        # the limit is exceeded. We verify that /v1/metadata processes the
        # request (non-empty response) even when the body exceeds the limit.
        body = json.dumps({
            "type": "export_metadata",
            "version": 2,
            "args": {},
        })
        # requests will set Content-Length from the data parameter
        padded = body + " " * 2000
        assert len(padded.encode("utf-8")) > 1024
        resp = metadata_raw(hge_ctx, padded)
        # The size-limit middleware returns an empty-body 400. If we get
        # any response with content, the request was not size-limited.
        assert len(resp.content) > 0, \
            "Got empty response — body size limit may be applied to /v1/metadata"

    def test_v1_graphql_enforced(self, hge_ctx):
        # Explicitly verify the /v1/graphql endpoint enforces the limit
        padding = "x" * 2000
        big_body = json.dumps({
            "query": "{ fork_items { id } }",
            "variables": {"pad": padding},
        })
        resp = graphql_raw(hge_ctx, big_body)
        assert resp.status_code == 400, f"Expected 400, got {resp.status_code}: {resp.text}"


# ---------------------------------------------------------------------------
# Tests with the default limit (50 KiB)
# ---------------------------------------------------------------------------

@pytest.mark.admin_secret
@pytest.mark.usefixtures("per_class_tests_db_state")
class TestRequestBodyLimitDefault:

    @classmethod
    def dir(cls):
        return "queries/fork/request_body_limit"

    def test_default_limit_rejects_large_body(self, hge_ctx):
        # Default is 50 KiB = 51200 bytes
        padding = "x" * 60000
        big_body = json.dumps({
            "query": "{ fork_items { id } }",
            "variables": {"pad": padding},
        })
        assert len(big_body) > 51200
        resp = graphql_raw(hge_ctx, big_body)
        assert resp.status_code == 400, f"Expected 400, got {resp.status_code}: {resp.text}"
