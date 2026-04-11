"""
Black-box tests for the fork's OpenTelemetry Tracing feature.

Verifies that GraphQL operations produce traces that are exported to Jaeger.
The tests query the Jaeger HTTP API to inspect collected spans.

Required environment variables (set by docker-compose port discovery):
  OTEL_EXPORTER_OTLP_ENDPOINT - OTLP HTTP endpoint (e.g. http://localhost:4318)
  JAEGER_QUERY_URL             - Jaeger query API (e.g. http://localhost:16686)
"""

import os
import time

import pytest
import requests


OTEL_SERVICE_NAME = "graphql-engine-test"


def graphql(hge_ctx, query):
    """Send a GraphQL query as admin."""
    headers = {
        "X-Hasura-Admin-Secret": hge_ctx.hge_key,
        "Content-Type": "application/json",
    }
    return requests.post(
        f"{hge_ctx.hge_url}/v1/graphql",
        json={"query": query},
        headers=headers,
    )


def get_jaeger_url():
    return os.environ.get("JAEGER_QUERY_URL", "http://localhost:16686")


def wait_for_traces(service, retries=10, delay=1.0):
    """Poll Jaeger until at least one trace appears for the given service."""
    jaeger_url = get_jaeger_url()
    params = {"service": service, "limit": 10}
    for _ in range(retries):
        try:
            resp = requests.get(f"{jaeger_url}/api/traces", params=params, timeout=5)
            if resp.status_code == 200:
                data = resp.json().get("data", [])
                if data:
                    return data
        except requests.ConnectionError:
            pass
        time.sleep(delay)
    return []


def get_all_spans(traces):
    """Flatten a list of Jaeger traces into a list of spans."""
    spans = []
    for trace in traces:
        spans.extend(trace.get("spans", []))
    return spans


def get_resource_tags(traces):
    """Extract resource-level tags from Jaeger trace data."""
    tags = {}
    for trace in traces:
        for process in trace.get("processes", {}).values():
            for tag in process.get("tags", []):
                tags[tag["key"]] = tag["value"]
    return tags


def span_has_tag(span, key):
    return any(t["key"] == key for t in span.get("tags", []))


def get_span_tag(span, key):
    for t in span.get("tags", []):
        if t["key"] == key:
            return t["value"]
    return None


@pytest.mark.admin_secret
@pytest.mark.usefixtures("per_class_tests_db_state")
@pytest.mark.hge_env("OTEL_EXPORTER_OTLP_ENDPOINT", os.environ.get("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318"))
@pytest.mark.hge_env("OTEL_EXPORTER_OTLP_PROTOCOL", "http/protobuf")
@pytest.mark.hge_env("OTEL_TRACES_EXPORTER", "otlp")
@pytest.mark.hge_env("DD_SERVICE", OTEL_SERVICE_NAME)
@pytest.mark.hge_env("DD_ENV", "test")
@pytest.mark.hge_env("DD_VERSION", "0.0.0-test")
class TestOpenTelemetry:

    @classmethod
    def dir(cls):
        return "queries/fork/opentelemetry"

    def test_graphql_query_produces_trace(self, hge_ctx):
        query = "query { fork_items { id name } }"
        resp = graphql(hge_ctx, query)
        assert resp.status_code == 200, resp.text

        time.sleep(2)

        traces = wait_for_traces(OTEL_SERVICE_NAME)
        assert len(traces) > 0, (
            f"No traces found for service '{OTEL_SERVICE_NAME}'. "
            "Is Jaeger running and OTEL_EXPORTER_OTLP_ENDPOINT set?"
        )

        spans = get_all_spans(traces)
        assert len(spans) > 0, "Expected at least one span"
        assert any(
            span.get("traceID") and span["traceID"] != "0" * 32
            for span in spans
        ), "Expected a span with a non-zero trace ID"

    def test_span_metadata_includes_expected_attributes(self, hge_ctx):
        query = "query { fork_items { id name } }"
        resp = graphql(hge_ctx, query)
        assert resp.status_code == 200, resp.text

        time.sleep(2)

        traces = wait_for_traces(OTEL_SERVICE_NAME)
        assert len(traces) > 0, "No traces found"

        spans = get_all_spans(traces)
        http_spans = [s for s in spans if span_has_tag(s, "http.request.method")]
        if http_spans:
            span = http_spans[0]
            assert get_span_tag(span, "http.request.method") is not None

    def test_datadog_tags_appear_as_resource_attributes(self, hge_ctx):
        query = "query { fork_items { id name } }"
        resp = graphql(hge_ctx, query)
        assert resp.status_code == 200, resp.text

        time.sleep(2)

        traces = wait_for_traces(OTEL_SERVICE_NAME)
        assert len(traces) > 0, "No traces found"

        resource_tags = get_resource_tags(traces)
        # DD_ENV=test should appear as the "env" resource attribute
        assert resource_tags.get("env") == "test", \
            f"Expected env=test in resource tags: {resource_tags}"
        # DD_VERSION should appear as "version" or "service.version"
        assert "0.0.0-test" in resource_tags.get("version", "") or \
            "0.0.0-test" in resource_tags.get("service.version", ""), \
            f"Expected version=0.0.0-test in resource tags: {resource_tags}"
