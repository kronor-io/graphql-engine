#!/usr/bin/env bash
# Run fork-specific Python integration tests.
#
# Usage:
#   ./run-fork-tests.sh                          # all fork tests
#   ./run-fork-tests.sh connection_templates      # just connection template tests
#   ./run-fork-tests.sh api_limits introspection  # multiple test groups
#
# Requires: docker compose services (postgres, jaeger) running,
#           HGE built, and Python venv set up.

set -euo pipefail
cd "$(dirname "$0")"

source .hasura-dev-python-venv/bin/activate

HGE_BIN=$(cabal list-bin graphql-engine:exe:graphql-engine)
PG_PORT_1=$(docker compose port --index 1 postgres 5432 | sed -E 's/.*://')
PG_PORT_2=$(docker compose port --index 2 postgres 5432 | sed -E 's/.*://')
PG_URL_1="postgresql://postgres:hasura@localhost:${PG_PORT_1}/postgres"
PG_URL_2="postgresql://postgres:hasura@localhost:${PG_PORT_2}/postgres"
JAEGER_QUERY_PORT=$(docker compose port jaeger 16686 | sed -E 's/.*://')
OTLP_PORT=$(docker compose port jaeger 4318 | sed -E 's/.*://')

export HASURA_GRAPHQL_PG_SOURCE_URL_1="$PG_URL_1"
export HASURA_GRAPHQL_PG_SOURCE_URL_2="$PG_URL_2"
export OTEL_EXPORTER_OTLP_ENDPOINT="http://localhost:${OTLP_PORT}"
export JAEGER_QUERY_URL="http://localhost:${JAEGER_QUERY_PORT}"

# Remove stale HPC tix file if present
rm -f graphql-engine.tix

ALL_FORK_TESTS=(
  test_fork_api_limits.py
  test_fork_connection_templates.py
  test_fork_introspection_control.py
  test_fork_opentelemetry.py
  test_fork_request_body_limit.py
  test_fork_stored_introspection.py
  test_fork_token_blocklist.py
)

if [ $# -eq 0 ]; then
  TEST_FILES=("${ALL_FORK_TESTS[@]}")
else
  TEST_FILES=()
  for arg in "$@"; do
    TEST_FILES+=("test_fork_${arg}.py")
  done
fi

echo "Running: ${TEST_FILES[*]}"
echo "HGE: $HGE_BIN"
echo "PG1: $PG_URL_1"
echo "PG2: $PG_URL_2"
echo ""

exec pytest \
  --hge-bin="$HGE_BIN" \
  --pg-urls "$PG_URL_1" "$PG_URL_2" \
  --dist=loadscope -n1 -v \
  "${TEST_FILES[@]}"
