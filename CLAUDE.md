# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is the **Kronor fork** of [hasura/graphql-engine](https://github.com/hasura/graphql-engine) (v2 series). The fork adds API limits, JWT token block-listing, introspection control, OpenTelemetry tracing, and request body size limits. See `FORK_FEATURES.md` for full documentation of fork-specific features.

Remotes: `origin` = hasura/graphql-engine (upstream), `kronor` = kronor-io/graphql-engine (fork).

## Build Commands

```sh
# Build the graphql-engine binary (requires GHC 9.10.2 + cabal 3.10+)
cabal build exe:graphql-engine

# Find the built binary
cabal list-bin graphql-engine:exe:graphql-engine

# Build everything including test suites
make build-all

# Format Haskell code (ormolu)
make format-hs

# Lint (hlint + shellcheck)
make lint
```

## Running the Engine Locally

```sh
# Start Postgres (and other DBs) via docker-compose
docker compose up -d postgres

# Run HGE with dev.sh (builds + starts)
scripts/dev.sh graphql-engine

# Or run directly with a pre-built binary
HGE_BIN=$(cabal list-bin graphql-engine:exe:graphql-engine)
$HGE_BIN --database-url "postgresql://hasura:hasura@localhost:65002/hasura" serve --dev-mode
```

## Running Tests

### Full Python integration suite
```sh
# Easiest: builds HGE, starts DBs, runs all tests
cd server/tests-py && ./run.sh

# Filter to specific test files
./run.sh -- test_fork_api_limits.py
```

### Fork-specific tests only (requires running Postgres + Jaeger)
```sh
cd server/tests-py
source .hasura-dev-python-venv/bin/activate
docker compose up -d --wait postgres jaeger

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

# Run all fork tests
pytest --hge-bin="$HGE_BIN" --pg-urls "$PG_URL_1" "$PG_URL_2" \
  --dist=loadscope -n1 -v \
  test_fork_api_limits.py test_fork_introspection_control.py \
  test_fork_token_blocklist.py test_fork_request_body_limit.py \
  test_fork_opentelemetry.py

# Run a single test class
pytest --hge-bin="$HGE_BIN" --pg-urls "$PG_URL_1" "$PG_URL_2" \
  --dist=loadscope -n1 -v \
  test_fork_api_limits.py::TestDepthLimit
```

### Haskell unit tests
```sh
make test-postgres   # Haskell API tests against Postgres
make test-backends   # All backend tests
```

## Architecture

### Server (Haskell)

- **Entry point**: `server/src-exec/Main.hs`
- **Core application**: `server/src-lib/Hasura/App.hs` — initializes AppEnv, starts background threads (token poller, schema sync), configures Warp middleware
- **GraphQL execution pipeline**: Query parsing → permission checking → API limits enforcement → execution → response
- **Metadata API**: `server/src-lib/Hasura/Server/API/Metadata.hs` dispatches `v1/metadata` requests (set_api_limits, set_graphql_schema_introspection_options, etc.)
- **Configuration flow**: CLI/env vars (`Server/Init/Arg/Command/Serve.hs`) → `ServeOptionsRaw` → `ServeOptions` (`Server/Init/Config.hs`) → `AppEnv` (`App.hs`)
- **Internal libraries**: `server/lib/` contains ~20 packages (hasura-prelude, pg-client-hs, schema-parsers, etc.)

### Fork-specific code (`server/src-lib/Kronor/`)

All fork features are in the `Kronor` namespace, integrated via `Hasura/App.hs`:

| Module | Purpose | Integration point in App.hs |
|---|---|---|
| `ApiLimitsEnforcer.hs` | Depth/node/time/batch limits per query | `MonadGQLExecutionCheck` instance (~line 794) |
| `TokenValidator.hs` | Background poller for blocked JWT tokens | `initialiseAppEnv` (~line 474), uses **metadata DB pool** |
| `IntrospectionOptionsEnforcer.hs` | Disable introspection per role | `executeIntrospection` (~line 797) |
| `OpenTelemetryReporter.hs` | OTEL tracer + Datadog auto-detection | `initialiseAppEnv` (~line 475), `runAppM` (~line 704) |

Request body size limit is implemented as Warp middleware in `App.hs` (~line 1015).

### Key implementation details

- **API limits**: GraphQL always returns HTTP 200; limit violations appear as `errors` in the response body (not HTTP 429)
- **Depth counting**: Leaf fields = 0 depth, nested fields = 1 + max(children). Only fields with sub-selections count
- **Node counting**: Only fields with nested sub-selections count as 1 node. Leaf fields are 0
- **Introspection fields** (`__schema`, `__type`, `__typename`) are exempt from depth/node limits
- **Token poller** queries the **metadata database** (not the source DB) for `tenant.tokens`

### Python test infrastructure (`server/tests-py/`)

- **conftest.py**: Complex fixture chain — spawns HGE per test class, manages DB setup/teardown
- **Markers**: `@pytest.mark.admin_secret`, `@pytest.mark.jwt('rsa')`, `@pytest.mark.hge_env("KEY", "VALUE")`
- **DB state**: YAML setup/teardown files in `queries/` directories, executed via `per_class_tests_db_state` fixture
- **Fork test fixtures**: `queries/fork/{api_limits,token_blocklist,introspection_control,request_body_limit,opentelemetry}/`
- **Important**: `hge_ctx.engine` points to the metadata DB, not the source DB. Use `run_sql` via the metadata API or `metadata_schema_url` fixture for direct metadata DB access
