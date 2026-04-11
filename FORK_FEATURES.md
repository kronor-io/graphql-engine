# Kronor Fork Features

This document describes all features added to our fork of
[hasura/graphql-engine](https://github.com/hasura/graphql-engine) (v2 series).

---

## 1. API Limits

Enforces per-query execution limits on GraphQL operations. All limits support
global defaults and per-role overrides. A global `disabled` flag turns off all
enforcement.

### 1.1 Query Depth Limit

Limits the nesting depth of a GraphQL query. Introspection fields (`__schema`,
`__type`, `__typename`) are exempt.

**Error:** HTTP 429 `"node depth limit exceeded"`

### 1.2 Query Node Limit

Limits the total number of field selections in a GraphQL query. Introspection
fields are exempt.

**Error:** HTTP 429 `"too many nodes in a single query"`

### 1.3 Time Limit

Enforces a maximum execution time for a GraphQL operation. Uses
`System.Timeout.Lifted` under the hood.

**Error:** HTTP 500 with code `time-limit-exceeded`, `"operation timed out"`

### 1.4 Batch Request Limit

Limits the number of operations in a single batched GraphQL request.

**Error:** HTTP 429 `"too many batched requests in a single request"`

### Configuration

Configured via the metadata API:

```json
// Set limits
POST /v1/metadata
{
  "type": "set_api_limits",
  "args": {
    "depth_limit": {
      "global": 10,
      "per_role": { "user": 5 }
    },
    "node_limit": {
      "global": 100,
      "per_role": { "user": 50 }
    },
    "time_limit": {
      "global": 30,
      "per_role": { "user": 10 }
    },
    "batch_limit": {
      "global": 10,
      "per_role": { "user": 5 }
    },
    "disabled": false
  }
}

// Remove all limits
POST /v1/metadata
{ "type": "remove_api_limits" }
```

### Code

- Enforcement: `server/src-lib/Kronor/ApiLimitsEnforcer.hs`
- Types: `server/src-lib/Hasura/RQL/Types/ApiLimit.hs`
- Integration: `server/src-lib/Hasura/App.hs` (lines ~731, ~794)

---

## 2. JWT Token Block-list

Maintains an in-memory set of blocked JWT token IDs, fetched from a database
table on a background polling loop. On every GraphQL request, the
`x-hasura-jwt-id` session variable (set from the JWT `jti` or `tid` claim) is
checked against this set.

### Behavior

- A background thread polls the database every 5 seconds.
- Blocked tokens are **unioned** into the in-memory set (tokens are never
  removed without a restart).
- If a request carries a blocked token, it is rejected immediately.

**Error:** HTTP 400 `"Invalid token"`

### Database Requirement

The following table must exist (external to Hasura):

```sql
-- schema: tenant
CREATE TABLE tenant.tokens (
  token_id  UUID    NOT NULL,
  blocked   BOOLEAN NOT NULL DEFAULT false,
  token_type TEXT   NOT NULL
);
```

The poller runs:

```sql
SELECT token_id, 1
FROM tenant.tokens
WHERE blocked = true AND token_type = 'backend';
```

### JWT Claim Integration

The `jti` (or `tid`) claim from the JWT payload is inserted into session
variables as `x-hasura-jwt-id` during JWT processing
(`server/src-lib/Hasura/Server/Auth/JWT.hs`).

### Code

- Poller & typeclass: `server/src-lib/Kronor/TokenValidator.hs`
- Validation check: `server/src-lib/Kronor/ApiLimitsEnforcer.hs` (lines ~31-42)
- JWT claim insertion: `server/src-lib/Hasura/Server/Auth/JWT.hs` (line ~697)

---

## 3. GraphQL Introspection Control

Allows disabling GraphQL introspection for specific roles. Users authenticated
with `X-Hasura-Admin-Secret` always bypass the restriction, regardless of their
effective role.

### Configuration

```json
POST /v1/metadata
{
  "type": "set_graphql_schema_introspection_options",
  "args": {
    "disabled_for_roles": ["user", "anonymous"]
  }
}
```

### Admin Bypass

When a request includes `X-Hasura-Admin-Secret`, the `_uiFallbackRole` field on
`UserInfo` is set to the fallback role from authentication. The introspection
enforcer checks this: if the fallback role is `admin`, introspection is allowed
even if the effective role is in the disabled list.

**Error:** HTTP 401 `"Introspection disabled"`

### Code

- Enforcer: `server/src-lib/Kronor/IntrospectionOptionsEnforcer.hs`
- Fallback role: `server/src-lib/Hasura/Authentication/User.hs` (`_uiFallbackRole` field)
- Types: `server/src-lib/Hasura/RQL/Types/GraphqlSchemaIntrospection.hs`

---

## 4. OpenTelemetry Tracing

Replaces Hasura's built-in tracing with OpenTelemetry-native span creation and
context propagation. All GraphQL operations and outgoing HTTP requests are
traced.

### Features

- Span creation via the OpenTelemetry SDK (`hs-opentelemetry-api`)
- Context propagation on outgoing HTTP requests using the TracerProvider's
  configured propagator (W3C Trace Context, B3, etc.)
- Datadog resource attribute auto-detection from environment variables
- Tracer is named `"graphql-engine"`

### Outgoing HTTP Metadata

When a trace context is present, outgoing HTTP request spans include:

| Attribute                | Value                     |
|--------------------------|---------------------------|
| `http.request.body.size` | Request body size in bytes |
| `http.request.method`    | HTTP method               |
| `http.request.uri`       | Request URI               |
| `span.type`              | `"http"`                  |
| `span.kind`              | `"client"`                |

### Environment Variables

| Variable            | Description                                    |
|---------------------|------------------------------------------------|
| `DD_ENV`            | Datadog environment (also reads `OTEL_SERVICE_NAME`) |
| `DD_VERSION`        | Datadog version (maps to `version` and `service.version`) |
| `DD_SERVICE`        | Datadog service name (maps to `service.name`)  |

Standard OpenTelemetry SDK environment variables (`OTEL_EXPORTER_*`, etc.) are
also respected by the underlying `hs-opentelemetry-sdk` library.

### Code

- Reporter: `server/src-lib/Kronor/OpenTelemetryReporter.hs`
- HTTP propagation: `server/src-lib/Hasura/Tracing/Utils.hs`
- Trace context: `server/src-lib/Hasura/Tracing/Context.hs`
- Initialization: `server/src-lib/Hasura/App.hs` (lines ~475-477)

---

## 5. Request Body Size Limit

Limits the size of incoming HTTP request bodies. The `/v1/metadata` endpoint is
exempt (to allow large metadata uploads).

### Configuration

| Method               | Value                                          |
|----------------------|------------------------------------------------|
| Environment variable | `HASURA_GRAPHQL_MAX_REQUEST_BODY_LENGTH`        |
| CLI flag             | `--max-request-body-length`                     |
| Default              | `51200` (50 KiB)                                |
| Unit                 | Bytes                                           |

**Response on exceed:** HTTP 400 Bad Request

### Code

- CLI option: `server/src-lib/Hasura/Server/Init/Arg/Command/Serve.hs` (lines ~1299-1314)
- Middleware: `server/src-lib/Hasura/App.hs` (lines ~1015-1021)

---

## 6. WebSocket DoS Hardening

Protects WebSocket connections against denial-of-service attacks through frame/message
size limits and per-connection message rate limiting.

### 6.1 Frame Payload Size Limit

Limits the maximum size of a single WebSocket frame. Enforced at the frame parsing
level by the `websockets` library. Oversized frames cause the connection to close.

| Method               | Value                                                  |
|----------------------|--------------------------------------------------------|
| Environment variable | `HASURA_GRAPHQL_WEBSOCKET_FRAME_PAYLOAD_SIZE_LIMIT`    |
| CLI flag             | `--websocket-frame-payload-size-limit`                 |
| Default              | `51200` (50 KiB)                                       |
| Unit                 | Bytes                                                  |

### 6.2 Message Data Size Limit

Limits the total size of a WebSocket message spanning multiple frames. Protects
against many small frames assembling a huge message.

| Method               | Value                                                  |
|----------------------|--------------------------------------------------------|
| Environment variable | `HASURA_GRAPHQL_WEBSOCKET_MESSAGE_DATA_SIZE_LIMIT`     |
| CLI flag             | `--websocket-message-data-size-limit`                  |
| Default              | `153600` (150 KiB)                                     |
| Unit                 | Bytes                                                  |

**Response on exceed (both size limits):** Connection closed (websockets library
throws `ParseException`)

### 6.3 Message Rate Limit

Limits the number of data messages per connection within a sliding time window.
Disabled by default (opt-in).

| Method               | Value                                                  |
|----------------------|--------------------------------------------------------|
| Environment variable | `HASURA_GRAPHQL_WEBSOCKET_MESSAGE_RATE_LIMIT`          |
| CLI flag             | `--websocket-message-rate-limit`                       |
| Default              | No limit (disabled)                                    |
| Unit                 | Messages per window                                    |

| Method               | Value                                                  |
|----------------------|--------------------------------------------------------|
| Environment variable | `HASURA_GRAPHQL_WEBSOCKET_MESSAGE_RATE_LIMIT_WINDOW`   |
| CLI flag             | `--websocket-message-rate-limit-window`                |
| Default              | `1`                                                    |
| Unit                 | Seconds                                                |

**Response on exceed:** Connection closed with close code `4429`

### Known Limitation

The rate limiter operates at the `receiveData` level, which only sees data
messages. WebSocket control frames (ping/pong) are handled transparently by the
websockets library before rate limiting and are not counted.

### Code

- Rate limiter: `server/src-lib/Kronor/WebSocketRateLimiter.hs`
- CLI options: `server/src-lib/Hasura/Server/Init/Arg/Command/Serve.hs`
- Size limit wiring: `server/src-lib/Hasura/Server/Init.hs` (ConnectionOptions)
- Rate limit integration: `server/src-lib/Hasura/GraphQL/Transport/WebSocket/Server.hs` (rcv loop)

---

## 7. Dynamic Database Connection Routing (Connection Templates)

Enables Hasura's connection template feature, which was previously gated behind
the Cloud/Enterprise edition. Allows routing GraphQL queries to different
database connection pools based on Kriti templates that evaluate request context
(session variables, headers, query type).

### How It Works

1. A **connection template** (Kriti expression) is configured on a Postgres source
2. **Connection set members** provide named alternative database connections
3. On each non-admin GraphQL request, the template evaluates with the request context
4. The result determines which pool handles the query: primary, read replicas,
   or a named connection set member
5. Admin requests bypass template resolution and always use the primary pool

### Routing Targets

| Template Result       | Behavior                                              |
|-----------------------|-------------------------------------------------------|
| `$.primary`           | Route to primary database                             |
| `$.read_replicas`     | Route to a random read replica (fallback to primary)  |
| `$.default`           | Reads → replicas, writes → primary                    |
| `$.connection_set.X`  | Route to named connection set member `X`              |

### Configuration

Configured as part of the source configuration via `pg_add_source`:

```json
POST /v1/metadata
{
  "type": "pg_add_source",
  "args": {
    "name": "default",
    "configuration": {
      "connection_info": {
        "database_url": "postgresql://..."
      },
      "read_replicas": [
        { "database_url": "postgresql://replica1/..." }
      ],
      "connection_set": [
        {
          "name": "analytics",
          "connection_info": { "database_url": "postgresql://analytics/..." }
        }
      ],
      "connection_template": {
        "version": 1,
        "template": "{{ if ($.request.session.x-hasura-role == \"analyst\") $.connection_set.analytics else $.primary }}"
      }
    }
  }
}
```

### Testing Templates

Use the `pg_test_connection_template` metadata API to test template resolution
without executing a query:

```json
POST /v1/metadata
{
  "type": "pg_test_connection_template",
  "args": {
    "source_name": "default",
    "request_context": {
      "headers": {},
      "session": { "x-hasura-role": "user", "x-hasura-route": "analytics" },
      "query": { "operation_name": "MyQuery", "operation_type": "query" }
    }
  }
}
```

### Code

- Pool creation & template config: `server/src-lib/Hasura/App.hs` (`mkPgSourceResolver`)
- Connection routing exec context: `server/src-lib/Hasura/Backends/Postgres/Execute/Types.hs` (`mkPGExecCtxWithConnRouting`)
- Template resolution: `server/src-lib/Hasura/Backends/Postgres/Execute/ConnectionTemplate.hs`
- Metadata types: `server/src-lib/Hasura/Backends/Postgres/Connection/Settings.hs`
- Test API: `server/src-lib/Hasura/RQL/DDL/ConnectionTemplate.hs`
