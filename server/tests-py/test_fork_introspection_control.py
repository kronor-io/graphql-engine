"""
Black-box tests for the fork's GraphQL Introspection Control feature.

Configurable in two mutually exclusive modes:
  * `disabled_for_roles` — deny-list. Listed roles cannot introspect.
  * `enabled_for_roles`  — allow-list. Only listed roles can introspect.

Sending both keys is rejected by the metadata API. Admin-secret requests
always bypass the restriction.
"""

import pytest
import jwt
import requests


def make_jwt_token(jwt_conf, role):
    """Generate a JWT for the given role."""
    hasura_claims = {
        "x-hasura-default-role": role,
        "x-hasura-allowed-roles": [role],
        "x-hasura-user-id": "1",
    }
    payload = {
        "https://hasura.io/jwt/claims": hasura_claims,
    }
    return jwt.encode(payload, jwt_conf.private_key, algorithm=jwt_conf.algorithm)


def graphql(hge_ctx, query, jwt_conf=None, role=None, use_admin_secret=False):
    """Send a GraphQL query."""
    headers = {"Content-Type": "application/json"}
    if use_admin_secret:
        headers["X-Hasura-Admin-Secret"] = hge_ctx.hge_key
    if role and jwt_conf:
        token = make_jwt_token(jwt_conf, role)
        headers["Authorization"] = f"Bearer {token}"
    elif not use_admin_secret:
        headers["X-Hasura-Admin-Secret"] = hge_ctx.hge_key
    return requests.post(
        f"{hge_ctx.hge_url}/v1/graphql",
        json={"query": query},
        headers=headers,
    )


def _post_introspection_options(hge_ctx, args, *, expect_ok=True):
    headers = {
        "X-Hasura-Admin-Secret": hge_ctx.hge_key,
        "Content-Type": "application/json",
    }
    resp = requests.post(
        f"{hge_ctx.hge_url}/v1/metadata",
        json={
            "type": "set_graphql_schema_introspection_options",
            "args": args,
        },
        headers=headers,
    )
    if expect_ok:
        assert resp.status_code == 200, f"set introspection options failed: {resp.text}"
    return resp


def set_introspection_options(hge_ctx, disabled_for_roles):
    """Configure introspection in deny-list mode."""
    return _post_introspection_options(
        hge_ctx, {"disabled_for_roles": disabled_for_roles}
    )


def set_enabled_for_roles(hge_ctx, enabled_for_roles):
    """Configure introspection in allow-list mode."""
    return _post_introspection_options(
        hge_ctx, {"enabled_for_roles": enabled_for_roles}
    )


INTROSPECTION_QUERY = "query { __schema { types { name } } }"
DATA_QUERY = "query { fork_items { id name } }"


@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
class TestIntrospectionControl:

    @classmethod
    def dir(cls):
        return "queries/fork/introspection_control"

    @pytest.fixture(autouse=True)
    def _cleanup(self, hge_ctx):
        yield
        # Reset: enable introspection for all roles
        try:
            set_introspection_options(hge_ctx, [])
        except Exception:
            pass

    def test_introspection_works_by_default(self, hge_ctx, jwt_configuration):
        resp = graphql(hge_ctx, INTROSPECTION_QUERY, jwt_configuration, "user")
        assert resp.status_code == 200, resp.text
        body = resp.json()
        assert "data" in body
        assert "__schema" in body["data"]

    def test_introspection_disabled_for_role(self, hge_ctx, jwt_configuration):
        set_introspection_options(hge_ctx, ["user"])
        resp = graphql(hge_ctx, INTROSPECTION_QUERY, jwt_configuration, "user")
        body = resp.json()
        errors = body.get("errors", [])
        assert any("ntrospection disabled" in str(e) for e in errors), \
            f"Expected introspection disabled error, got: {body}"

    def test_introspection_still_works_for_non_disabled_role(self, hge_ctx, jwt_configuration):
        set_introspection_options(hge_ctx, ["user"])
        resp = graphql(hge_ctx, INTROSPECTION_QUERY, jwt_configuration, "anonymous")
        assert resp.status_code == 200, f"Expected 200 for anonymous, got {resp.status_code}: {resp.text}"
        body = resp.json()
        assert "data" in body

    def test_admin_secret_bypasses_restriction(self, hge_ctx, jwt_configuration):
        set_introspection_options(hge_ctx, ["user"])
        # Send introspection as user role BUT include admin secret
        resp = graphql(
            hge_ctx, INTROSPECTION_QUERY, jwt_configuration, "user",
            use_admin_secret=True,
        )
        assert resp.status_code == 200, f"Expected 200 with admin secret, got {resp.status_code}: {resp.text}"

    def test_non_introspection_queries_unaffected(self, hge_ctx, jwt_configuration):
        set_introspection_options(hge_ctx, ["user"])
        resp = graphql(hge_ctx, DATA_QUERY, jwt_configuration, "user")
        assert resp.status_code == 200, f"Expected 200 for data query, got {resp.status_code}: {resp.text}"
        body = resp.json()
        assert "data" in body

    def test_disabling_for_multiple_roles(self, hge_ctx, jwt_configuration):
        set_introspection_options(hge_ctx, ["user", "anonymous"])

        resp_user = graphql(hge_ctx, INTROSPECTION_QUERY, jwt_configuration, "user")
        user_errors = resp_user.json().get("errors", [])
        assert any("ntrospection disabled" in str(e) for e in user_errors), \
            f"Expected introspection disabled for user, got: {resp_user.json()}"

        resp_anon = graphql(hge_ctx, INTROSPECTION_QUERY, jwt_configuration, "anonymous")
        anon_errors = resp_anon.json().get("errors", [])
        assert any("ntrospection disabled" in str(e) for e in anon_errors), \
            f"Expected introspection disabled for anonymous, got: {resp_anon.json()}"


@pytest.mark.admin_secret
@pytest.mark.usefixtures("jwt_configuration", "per_class_tests_db_state")
@pytest.mark.jwt("rsa")
class TestIntrospectionAllowList:
    """`enabled_for_roles` mode: only listed roles may introspect."""

    @classmethod
    def dir(cls):
        return "queries/fork/introspection_control"

    @pytest.fixture(autouse=True)
    def _cleanup(self, hge_ctx):
        yield
        # Reset to deny-list mode with no roles disabled — introspection on for all.
        try:
            set_introspection_options(hge_ctx, [])
        except Exception:
            pass

    def test_allow_list_permits_listed_role(self, hge_ctx, jwt_configuration):
        set_enabled_for_roles(hge_ctx, ["user"])
        resp = graphql(hge_ctx, INTROSPECTION_QUERY, jwt_configuration, "user")
        assert resp.status_code == 200, resp.text
        body = resp.json()
        assert "data" in body and "__schema" in body["data"], body

    def test_allow_list_blocks_unlisted_role(self, hge_ctx, jwt_configuration):
        set_enabled_for_roles(hge_ctx, ["user"])
        resp = graphql(hge_ctx, INTROSPECTION_QUERY, jwt_configuration, "anonymous")
        body = resp.json()
        errors = body.get("errors", [])
        assert any("ntrospection disabled" in str(e) for e in errors), \
            f"Expected introspection disabled for anonymous, got: {body}"

    def test_empty_allow_list_blocks_every_role(self, hge_ctx, jwt_configuration):
        # The motivating safety property: an empty allow-list denies every
        # non-admin role, so adding a new role can never accidentally grant
        # introspection access.
        set_enabled_for_roles(hge_ctx, [])

        for role in ("user", "anonymous"):
            resp = graphql(hge_ctx, INTROSPECTION_QUERY, jwt_configuration, role)
            errors = resp.json().get("errors", [])
            assert any("ntrospection disabled" in str(e) for e in errors), \
                f"Expected introspection disabled for {role}, got: {resp.json()}"

    def test_admin_secret_bypasses_allow_list(self, hge_ctx, jwt_configuration):
        set_enabled_for_roles(hge_ctx, [])
        resp = graphql(
            hge_ctx, INTROSPECTION_QUERY, jwt_configuration, "user",
            use_admin_secret=True,
        )
        assert resp.status_code == 200, resp.text
        body = resp.json()
        assert "data" in body and "__schema" in body["data"], body

    def test_non_introspection_queries_unaffected(self, hge_ctx, jwt_configuration):
        set_enabled_for_roles(hge_ctx, [])
        resp = graphql(hge_ctx, DATA_QUERY, jwt_configuration, "user")
        assert resp.status_code == 200, resp.text
        body = resp.json()
        assert "data" in body

    def test_allow_list_with_multiple_roles(self, hge_ctx, jwt_configuration):
        set_enabled_for_roles(hge_ctx, ["user", "anonymous"])

        for role in ("user", "anonymous"):
            resp = graphql(hge_ctx, INTROSPECTION_QUERY, jwt_configuration, role)
            assert resp.status_code == 200, f"{role}: {resp.text}"
            body = resp.json()
            assert "data" in body and "__schema" in body["data"], \
                f"Expected introspection allowed for {role}, got: {body}"

    def test_switching_modes_disabled_to_enabled(self, hge_ctx, jwt_configuration):
        # Start in deny-list mode where `user` is allowed by default.
        set_introspection_options(hge_ctx, [])
        resp = graphql(hge_ctx, INTROSPECTION_QUERY, jwt_configuration, "user")
        assert resp.status_code == 200, resp.text

        # Switch to allow-list mode listing only `anonymous` — `user` now denied.
        set_enabled_for_roles(hge_ctx, ["anonymous"])
        resp = graphql(hge_ctx, INTROSPECTION_QUERY, jwt_configuration, "user")
        errors = resp.json().get("errors", [])
        assert any("ntrospection disabled" in str(e) for e in errors), \
            f"Expected user denied after switching to allow-list, got: {resp.json()}"

    def test_both_keys_rejected(self, hge_ctx):
        # The metadata API enforces that exactly one of the two keys is set.
        resp = _post_introspection_options(
            hge_ctx,
            {"disabled_for_roles": ["user"], "enabled_for_roles": ["user"]},
            expect_ok=False,
        )
        assert resp.status_code != 200, \
            f"Expected metadata API to reject both keys, got: {resp.status_code} {resp.text}"
