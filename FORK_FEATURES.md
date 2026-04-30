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

Controls which roles can run GraphQL introspection. Configurable as either a
deny-list (`disabled_for_roles`) or an allow-list (`enabled_for_roles`). Users
authenticated with `X-Hasura-Admin-Secret` always bypass the restriction,
regardless of their effective role.

### Configuration

The metadata accepts **exactly one** of `disabled_for_roles` or
`enabled_for_roles` — never both. Sending both is rejected by the server and by
the CLI metadata loader.

Deny-list (introspection on by default, disabled for the listed roles):

```json
POST /v1/metadata
{
  "type": "set_graphql_schema_introspection_options",
  "args": {
    "disabled_for_roles": ["user", "anonymous"]
  }
}
```

Allow-list (introspection off by default, enabled only for the listed roles):

```json
POST /v1/metadata
{
  "type": "set_graphql_schema_introspection_options",
  "args": {
    "enabled_for_roles": ["developer"]
  }
}
```

`enabled_for_roles` is the recommended mode for new deployments: a role not
in the list is denied, so adding a new role cannot accidentally grant
introspection access. `disabled_for_roles` is preserved for backwards
compatibility.

### CLI Metadata File

`metadata/graphql_schema_introspection.yaml` mirrors the same shape:

```yaml
disabled_for_roles:
  - user
  - anonymous
```

or

```yaml
enabled_for_roles:
  - developer
```

Empty arrays are valid and are sent through to the server. An empty file
defaults to `disabled_for_roles: []` (introspection enabled for all roles), to
match the server-side empty default.

### Admin Bypass

When a request includes `X-Hasura-Admin-Secret`, the `_uiFallbackRole` field on
`UserInfo` is set to the fallback role from authentication. The introspection
enforcer checks this: if the fallback role is `admin`, introspection is allowed
even if the effective role is in the disabled list (or absent from the enabled
list).

**Error:** HTTP 401 `"Introspection disabled"`

### Code

- Enforcer: `server/src-lib/Kronor/IntrospectionOptionsEnforcer.hs`
- Fallback role: `server/src-lib/Hasura/Authentication/User.hs` (`_uiFallbackRole` field)
- Types: `server/src-lib/Hasura/RQL/Types/GraphqlSchemaIntrospection.hs`
- CLI metadata object: `cli/internal/metadataobject/graphql_schema_introspection/graphql_schema_introspection.go`

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
