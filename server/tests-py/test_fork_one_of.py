"""
Black-box tests for OneOf Input Objects (@oneOf) from remote schemas.

The remote is a stub serving a canned introspection result, captured from a
server whose PaymentMethodInput and LookupInput are OneOf Input Objects:

    input PaymentMethodInput @oneOf { card: CardInput, swish: String, ids: [Int!] }
    input LookupInput @oneOf { byId: Int, byName: String }
    type Query {
      pay(method: PaymentMethodInput!): String!
      find(lookup: LookupInput!): String!
    }

Verifies that:
1. `__Type.isOneOf` is fetched from remotes that support it, and exposed by
   Hasura's own introspection, together with the `@oneOf` directive.
2. Remotes without `__Type.isOneOf` keep working: the extra query isn't sent.
3. Invalid OneOf values are rejected by Hasura without calling the remote.
4. Roles of remote schema permissions keep the OneOf semantics.
"""

import copy
import json
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

import pytest
import requests

pytestmark = [
    pytest.mark.admin_secret,
    pytest.mark.hge_env('HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS', 'true'),
]

FIXTURES = 'queries/fork/one_of'


def load(name):
    with open(f'{FIXTURES}/{name}') as f:
        return json.load(f)


def without_is_one_of(introspection):
    """The same schema, served by a remote that doesn't know `__Type.isOneOf`."""
    result = copy.deepcopy(introspection)
    for t in result['data']['__schema']['types']:
        if t['name'] == '__Type':
            t['fields'] = [f for f in t['fields'] if f['name'] != 'isOneOf']
    return result


class StubRemote:
    def __init__(self, supports_is_one_of):
        introspection = load('introspection.json')
        self.introspection = introspection if supports_is_one_of else without_is_one_of(introspection)
        self.one_of_flags = load('introspection_one_of.json')
        self.requests = []
        stub = self

        class Handler(BaseHTTPRequestHandler):
            def do_POST(self):
                body = json.loads(self.rfile.read(int(self.headers['Content-Length'])))
                stub.requests.append(body)
                operation = body.get('operationName')
                if operation == 'IntrospectionQuery':
                    response = stub.introspection
                elif operation == 'IntrospectionOneOf':
                    response = stub.one_of_flags
                else:
                    field = 'pay' if 'pay' in body['query'] else 'find'
                    response = {'data': {field: 'ok'}}
                payload = json.dumps(response).encode()
                self.send_response(200)
                self.send_header('Content-Type', 'application/json')
                self.send_header('Content-Length', str(len(payload)))
                self.end_headers()
                self.wfile.write(payload)

            def log_message(self, *args):
                pass

        self.server = ThreadingHTTPServer(('127.0.0.1', 0), Handler)
        self.url = f'http://127.0.0.1:{self.server.server_address[1]}'
        threading.Thread(target=self.server.serve_forever, daemon=True).start()

    def operations(self):
        return [r.get('operationName') for r in self.requests]

    def executed(self):
        return [r for r in self.requests if r.get('operationName') not in ('IntrospectionQuery', 'IntrospectionOneOf')]

    def stop(self):
        self.server.shutdown()


def metadata(hge_ctx, body):
    resp = requests.post(
        f'{hge_ctx.hge_url}/v1/metadata',
        json=body,
        headers={'X-Hasura-Admin-Secret': hge_ctx.hge_key},
        timeout=30,
    )
    assert resp.status_code == 200, resp.text
    return resp.json()


def graphql(hge_ctx, query, variables=None, role=None):
    headers = {'X-Hasura-Admin-Secret': hge_ctx.hge_key}
    if role:
        headers['X-Hasura-Role'] = role
    resp = requests.post(
        f'{hge_ctx.hge_url}/v1/graphql',
        json={'query': query, 'variables': variables or {}},
        headers=headers,
        timeout=30,
    )
    assert resp.status_code == 200, resp.text
    return resp.json()


def remote(hge_ctx, supports_is_one_of):
    stub = StubRemote(supports_is_one_of)
    metadata(hge_ctx, {
        'type': 'add_remote_schema',
        'args': {'name': 'one_of', 'definition': {'url': stub.url, 'timeout_seconds': 10}},
    })
    return stub


@pytest.fixture
def one_of_remote(hge_ctx):
    stub = remote(hge_ctx, supports_is_one_of=True)
    yield stub
    metadata(hge_ctx, {'type': 'remove_remote_schema', 'args': {'name': 'one_of'}})
    stub.stop()


@pytest.fixture
def legacy_remote(hge_ctx):
    stub = remote(hge_ctx, supports_is_one_of=False)
    yield stub
    metadata(hge_ctx, {'type': 'remove_remote_schema', 'args': {'name': 'one_of'}})
    stub.stop()


IS_ONE_OF_QUERY = '''
{
  payment: __type(name: "PaymentMethodInput") { isOneOf }
  lookup: __type(name: "LookupInput") { isOneOf }
  card: __type(name: "CardInput") { isOneOf }
  query: __type(name: "query_root") { isOneOf }
  __schema { directives { name locations args { name } } }
}
'''


class TestOneOfRemoteSchema:

    def test_introspection(self, hge_ctx, one_of_remote):
        assert 'IntrospectionOneOf' in one_of_remote.operations()
        data = graphql(hge_ctx, IS_ONE_OF_QUERY)['data']
        assert data['payment'] == {'isOneOf': True}
        assert data['lookup'] == {'isOneOf': True}
        assert data['card'] == {'isOneOf': False}
        assert data['query'] == {'isOneOf': None}
        assert {'name': 'oneOf', 'locations': ['INPUT_OBJECT'], 'args': []} in data['__schema']['directives']

    def test_remote_without_is_one_of(self, hge_ctx, legacy_remote):
        assert 'IntrospectionOneOf' not in legacy_remote.operations()
        data = graphql(hge_ctx, IS_ONE_OF_QUERY)['data']
        assert data['payment'] == {'isOneOf': False}
        # no OneOf validation: the remote is the one to decide
        assert graphql(hge_ctx, '{ pay(method: { swish: "a", ids: [1] }) }') == {'data': {'pay': 'ok'}}

    @pytest.mark.parametrize('query, variables', [
        ('{ pay(method: { card: { number: "4111" } }) }', None),
        ('{ pay(method: { ids: 1 }) }', None),
        ('query ($m: PaymentMethodInput!) { pay(method: $m) }', {'m': {'swish': '0701234567'}}),
        ('query ($s: String!) { pay(method: { swish: $s }) }', {'s': '0701234567'}),
        ('{ find(lookup: { byName: "Alice" }) }', None),
    ])
    def test_valid_values_are_forwarded(self, hge_ctx, one_of_remote, query, variables):
        response = graphql(hge_ctx, query, variables)
        assert 'errors' not in response, response
        assert len(one_of_remote.executed()) == 1

    @pytest.mark.parametrize('query, variables, message', [
        ('{ pay(method: {}) }', None, 'exactly one field must be specified'),
        ('{ pay(method: { swish: "a", ids: [1] }) }', None, 'exactly one field must be specified'),
        ('{ pay(method: { swish: null }) }', None, 'must not be null'),
        ('query ($s: String) { pay(method: { swish: $s }) }', {'s': 'a'}, 'must be non-nullable'),
        ('query ($m: PaymentMethodInput!) { pay(method: $m) }', {'m': {'swish': 'a', 'ids': [1]}}, 'exactly one field must be specified'),
        ('query ($m: PaymentMethodInput!) { pay(method: $m) }', {'m': {'swish': None}}, 'must not be null'),
        ('query ($m: PaymentMethodInput!) { pay(method: $m) }', {'m': {}}, 'exactly one field must be specified'),
    ])
    def test_invalid_values_are_rejected(self, hge_ctx, one_of_remote, query, variables, message):
        response = graphql(hge_ctx, query, variables)
        assert 'data' not in response or response['data'] is None, response
        assert message in response['errors'][0]['message'], response
        assert one_of_remote.executed() == []

    def test_role_keeps_one_of(self, hge_ctx, one_of_remote):
        # the role's schema doesn't repeat @oneOf, and only exposes some of the fields
        metadata(hge_ctx, {
            'type': 'add_remote_schema_permissions',
            'args': {
                'remote_schema': 'one_of',
                'role': 'user',
                'definition': {'schema': '''
                    input CardInput { number: String! }
                    input PaymentMethodInput { card: CardInput swish: String }
                    type Query { pay(method: PaymentMethodInput!): String! }
                    schema { query: Query }
                '''},
            },
        })
        data = graphql(hge_ctx, '{ __type(name: "PaymentMethodInput") { isOneOf inputFields { name } } }', role='user')['data']
        assert data['__type']['isOneOf'] is True
        assert [f['name'] for f in data['__type']['inputFields']] == ['card', 'swish']
        response = graphql(hge_ctx, '{ pay(method: { card: { number: "1" }, swish: "a" }) }', role='user')
        assert 'exactly one field must be specified' in response['errors'][0]['message'], response
        assert graphql(hge_ctx, '{ pay(method: { swish: "a" }) }', role='user') == {'data': {'pay': 'ok'}}
