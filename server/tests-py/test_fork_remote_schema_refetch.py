"""
Black-box tests: a metadata apply that changes a remote schema's permissions
fetches the remote schema again.

A deploy rolls out a remote schema server that serves a new field, then applies
metadata that grants a role that field. The remote schema's definition (its URL
and headers) is unchanged, only its permissions are. The apply must check those
permissions against what the server serves now, not against the introspection
fetched before the server was rolled out.
"""

import copy
import http.server
import json
import threading

import pytest
from graphql import build_ast_schema, graphql, parse
from graphql.error import format_error

import ports

pytestmark = [
    pytest.mark.admin_secret,
    pytest.mark.hge_env('HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS', 'true'),
]

REMOTE_SCHEMA_NAME = 'fork_refetch_remote'
ROLE = 'fork_refetch_user'

OLD_SDL = 'type Query { hello: String }'
NEW_SDL = 'type Query { hello: String greet(name: String!): String }'


class SwappableGraphQLServer:
    """A GraphQL server whose schema can be replaced, or taken down, while it runs."""

    def __init__(self, worker_id, sdl):
        self.serve(sdl)
        self.down = False
        self.port = ports.find_free_port(worker_id)
        server = self

        class Handler(http.server.BaseHTTPRequestHandler):
            def do_POST(self):
                if server.down:
                    self.send_response(503)
                    self.send_header('Content-Length', '0')
                    self.end_headers()
                    return
                length = int(self.headers['Content-Length'])
                request = json.loads(self.rfile.read(length))
                result = graphql(
                    server.schema,
                    request['query'],
                    variable_values=request.get('variables'),
                    operation_name=request.get('operationName'),
                )
                response = {'data': result.data}
                if result.errors:
                    response['errors'] = [format_error(e) for e in result.errors]
                body = json.dumps(response).encode()
                self.send_response(200)
                self.send_header('Content-Type', 'application/json')
                self.send_header('Content-Length', str(len(body)))
                self.end_headers()
                self.wfile.write(body)

            def log_message(self, *args):
                pass

        self.httpd = http.server.ThreadingHTTPServer(('127.0.0.1', self.port), Handler)
        self.thread = threading.Thread(target=self.httpd.serve_forever, daemon=True)

    def serve(self, sdl):
        # graphql-core 2 needs the schema definition spelled out.
        schema = build_ast_schema(parse(sdl + ' schema { query: Query }'))
        greet = schema.get_query_type().fields.get('greet')
        if greet:
            greet.resolver = lambda root, info, name: f'hello {name}'
        self.schema = schema

    @property
    def url(self):
        return f'http://127.0.0.1:{self.port}'


@pytest.fixture
def remote_server(worker_id):
    server = SwappableGraphQLServer(worker_id, OLD_SDL)
    server.thread.start()
    yield server
    server.httpd.shutdown()
    server.httpd.server_close()


@pytest.fixture
def remote_schema(hge_ctx, remote_server):
    """Adds the remote server as a remote schema that grants ROLE the old schema.
    Yields the metadata and its remote schema entry, to edit and apply."""
    original_metadata = hge_ctx.v1metadataq({'type': 'export_metadata', 'args': {}})
    metadata = copy.deepcopy(original_metadata)
    remote_schema = {
        'name': REMOTE_SCHEMA_NAME,
        'definition': {'url': remote_server.url, 'timeout_seconds': 10},
        'permissions': [{'role': ROLE, 'definition': {'schema': OLD_SDL}}],
    }
    metadata.setdefault('remote_schemas', []).append(remote_schema)
    hge_ctx.v1metadataq({'type': 'replace_metadata', 'args': metadata})
    yield metadata, remote_schema
    hge_ctx.v1metadataq({'type': 'replace_metadata', 'args': original_metadata})


def grant_new_field(remote_schema):
    remote_schema['permissions'] = [{'role': ROLE, 'definition': {'schema': NEW_SDL}}]


def assert_role_can_query_new_field(hge_ctx):
    resp = hge_ctx.v1graphqlq({'query': '{ greet(name: "x") }'}, headers={'X-Hasura-Role': ROLE})
    assert resp == {'data': {'greet': 'hello x'}}, resp


class TestRemoteSchemaRefetch:

    def test_apply_with_changed_permissions_fetches_remote_schema_again(self, hge_ctx, remote_server, remote_schema):
        metadata, remote_schema_entry = remote_schema

        # The server starts serving a new field; nothing reloads the remote schema.
        remote_server.serve(NEW_SDL)
        grant_new_field(remote_schema_entry)
        hge_ctx.v1metadataq({'type': 'replace_metadata', 'args': metadata})

        assert_role_can_query_new_field(hge_ctx)

    def test_retried_apply_fetches_remote_schema_again(self, hge_ctx, remote_server, remote_schema):
        metadata, remote_schema_entry = remote_schema

        # The apply runs while the server is down, so it falls back to the
        # stored introspection, which lacks the new field, and fails.
        remote_server.serve(NEW_SDL)
        remote_server.down = True
        grant_new_field(remote_schema_entry)
        hge_ctx.v1metadataq({'type': 'replace_metadata', 'args': metadata}, expected_status_code=400)

        # Retrying the same apply once the server is back must fetch it again,
        # not reuse what the failed apply fetched.
        remote_server.down = False
        hge_ctx.v1metadataq({'type': 'replace_metadata', 'args': metadata})

        assert_role_can_query_new_field(hge_ctx)
