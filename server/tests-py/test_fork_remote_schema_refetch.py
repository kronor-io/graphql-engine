"""
Black-box test: a metadata apply that changes a remote schema's permissions
fetches the remote schema again.

A deploy rolls out a remote schema server that serves a new field, then applies
metadata that grants a role that field. The remote schema's definition (its URL
and headers) is unchanged, only its permissions are. The apply must check those
permissions against what the server serves now, not against the introspection
fetched before the server was rolled out.
"""

import http.server
import json
import threading

import pytest
import requests
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
    """A GraphQL server whose schema can be replaced while it runs."""

    def __init__(self, worker_id, sdl):
        self.serve(sdl)
        self.port = ports.find_free_port(worker_id)
        server = self

        class Handler(http.server.BaseHTTPRequestHandler):
            def do_POST(self):
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

        self.httpd = http.server.ThreadingHTTPServer(('localhost', self.port), Handler)
        self.thread = threading.Thread(target=self.httpd.serve_forever, daemon=True)

    def serve(self, sdl):
        # graphql-core 2 needs the schema definition spelled out.
        self.schema = build_ast_schema(parse(sdl + ' schema { query: Query }'))

    @property
    def url(self):
        return f'http://localhost:{self.port}'


@pytest.fixture
def remote_server(worker_id):
    server = SwappableGraphQLServer(worker_id, OLD_SDL)
    server.thread.start()
    yield server
    server.httpd.shutdown()
    server.httpd.server_close()


def metadata_api(hge_ctx, body):
    return requests.post(
        f'{hge_ctx.hge_url}/v1/metadata',
        json=body,
        headers={'X-Hasura-Admin-Secret': hge_ctx.hge_key},
        timeout=30,
    )


def permissions_metadata(sdl):
    return [{'role': ROLE, 'definition': {'schema': sdl}}]


class TestRemoteSchemaRefetch:

    def test_apply_with_changed_permissions_fetches_remote_schema_again(self, hge_ctx, remote_server):
        original_metadata = metadata_api(hge_ctx, {'type': 'export_metadata', 'args': {}}).json()

        try:
            metadata = json.loads(json.dumps(original_metadata))
            metadata.setdefault('remote_schemas', []).append({
                'name': REMOTE_SCHEMA_NAME,
                'definition': {'url': remote_server.url, 'timeout_seconds': 10},
                'permissions': permissions_metadata(OLD_SDL),
            })
            resp = metadata_api(hge_ctx, {'type': 'replace_metadata', 'args': metadata})
            assert resp.status_code == 200, resp.text

            # The server starts serving a new field; nothing reloads the remote schema.
            remote_server.serve(NEW_SDL)

            for remote_schema in metadata['remote_schemas']:
                if remote_schema['name'] == REMOTE_SCHEMA_NAME:
                    remote_schema['permissions'] = permissions_metadata(NEW_SDL)
            resp = metadata_api(hge_ctx, {'type': 'replace_metadata', 'args': metadata})
            assert resp.status_code == 200, (
                f'apply granting the new field was checked against the old introspection: {resp.text}'
            )

            resp = requests.post(
                f'{hge_ctx.hge_url}/v1/graphql',
                json={'query': '{ greet(name: "x") }'},
                headers={
                    'X-Hasura-Admin-Secret': hge_ctx.hge_key,
                    'X-Hasura-Role': ROLE,
                },
                timeout=30,
            )
            body = resp.json()
            assert 'errors' not in body, body
        finally:
            resp = metadata_api(hge_ctx, {'type': 'replace_metadata', 'args': original_metadata})
            assert resp.status_code == 200, resp.text
