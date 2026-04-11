"""
Black-box tests for WebSocket DoS hardening features:

- Frame payload size limit (connectionFramePayloadSizeLimit)
- Message data size limit (connectionMessageDataSizeLimit)
- Per-connection message rate limiting

These tests use the synchronous `websocket.create_connection` client
for precise control over frame sending and close-code observation.
"""

import json
import time
from urllib.parse import urlparse

import pytest
import websocket


def ws_url(hge_ctx, path="/v1/graphql"):
    return urlparse(hge_ctx.hge_url)._replace(scheme="ws", path=path).geturl()


def ws_connect_and_init(hge_ctx, path="/v1/graphql", timeout=5):
    """Create a WebSocket connection and send connection_init."""
    ws = websocket.create_connection(ws_url(hge_ctx, path), timeout=timeout)
    init_msg = {
        "type": "connection_init",
        "payload": {"headers": {"X-Hasura-Admin-Secret": hge_ctx.hge_key}},
    }
    ws.send(json.dumps(init_msg))
    # Wait for connection_ack, skipping keep-alive messages
    while True:
        msg = json.loads(ws.recv())
        if msg["type"] == "ka":
            continue
        assert msg["type"] == "connection_ack", f"Expected connection_ack, got: {msg}"
        break
    return ws


def ws_recv_data(ws):
    """Receive the next non-keep-alive message from the WebSocket."""
    while True:
        msg = json.loads(ws.recv())
        if msg["type"] == "ka":
            continue
        return msg


# ---------------------------------------------------------------------------
# Frame payload size limit tests
# ---------------------------------------------------------------------------


@pytest.mark.admin_secret
@pytest.mark.usefixtures("per_class_tests_db_state")
@pytest.mark.hge_env("HASURA_GRAPHQL_WEBSOCKET_FRAME_PAYLOAD_SIZE_LIMIT", "1024")
@pytest.mark.hge_env("HASURA_GRAPHQL_WEBSOCKET_MESSAGE_DATA_SIZE_LIMIT", "153600")
class TestWebSocketFramePayloadSizeLimit:
    """Test that oversized single frames are rejected."""

    @classmethod
    def dir(cls):
        return "queries/fork/websocket_limits"

    def test_small_frame_succeeds(self, hge_ctx):
        """A frame well within the 1024-byte limit should work."""
        ws = ws_connect_and_init(hge_ctx)
        try:
            query_msg = json.dumps({
                "id": "1",
                "type": "start",
                "payload": {"query": "{ fork_ws_items { id } }"},
            })
            assert len(query_msg.encode("utf-8")) < 1024
            ws.send(query_msg)
            resp = ws_recv_data(ws)
            assert resp["type"] == "data", f"Expected data, got: {resp}"
        finally:
            ws.close()

    def test_oversized_frame_closes_connection(self, hge_ctx):
        """A frame exceeding 1024 bytes should cause the server to close."""
        ws = ws_connect_and_init(hge_ctx)
        try:
            # Build a message larger than 1024 bytes
            padding = "x" * 2000
            big_msg = json.dumps({
                "id": "1",
                "type": "start",
                "payload": {
                    "query": "query ($v: String) { fork_ws_items { id } }",
                    "variables": {"v": padding},
                },
            })
            assert len(big_msg.encode("utf-8")) > 1024
            ws.send(big_msg)
            # The server should close the connection. It may:
            # 1. Raise an exception immediately
            # 2. Return empty data
            # 3. Return a close frame, then subsequent recv raises
            connection_closed = False
            try:
                for _ in range(5):
                    data = ws.recv()
                    if not data:
                        connection_closed = True
                        break
            except (
                websocket.WebSocketConnectionClosedException,
                ConnectionError,
            ):
                connection_closed = True
            assert connection_closed, "Expected connection to be closed due to oversized frame"
        finally:
            try:
                ws.close()
            except Exception:
                pass


# ---------------------------------------------------------------------------
# Message data size limit tests
# ---------------------------------------------------------------------------


@pytest.mark.admin_secret
@pytest.mark.usefixtures("per_class_tests_db_state")
@pytest.mark.hge_env("HASURA_GRAPHQL_WEBSOCKET_MESSAGE_DATA_SIZE_LIMIT", "1024")
@pytest.mark.hge_env("HASURA_GRAPHQL_WEBSOCKET_FRAME_PAYLOAD_SIZE_LIMIT", "51200")
class TestWebSocketMessageDataSizeLimit:
    """Test that oversized messages (possibly spanning frames) are rejected."""

    @classmethod
    def dir(cls):
        return "queries/fork/websocket_limits"

    def test_small_message_succeeds(self, hge_ctx):
        """A message well within the 1024-byte limit should work."""
        ws = ws_connect_and_init(hge_ctx)
        try:
            query_msg = json.dumps({
                "id": "1",
                "type": "start",
                "payload": {"query": "{ fork_ws_items { id } }"},
            })
            assert len(query_msg.encode("utf-8")) < 1024
            ws.send(query_msg)
            resp = ws_recv_data(ws)
            assert resp["type"] == "data", f"Expected data, got: {resp}"
        finally:
            ws.close()

    def test_oversized_message_closes_connection(self, hge_ctx):
        """A message exceeding 1024 bytes should cause the server to close."""
        ws = ws_connect_and_init(hge_ctx)
        try:
            padding = "x" * 2000
            big_msg = json.dumps({
                "id": "1",
                "type": "start",
                "payload": {
                    "query": "query ($v: String) { fork_ws_items { id } }",
                    "variables": {"v": padding},
                },
            })
            assert len(big_msg.encode("utf-8")) > 1024
            ws.send(big_msg)
            # The server should close the connection
            connection_closed = False
            try:
                for _ in range(5):
                    data = ws.recv()
                    if not data:
                        connection_closed = True
                        break
            except (
                websocket.WebSocketConnectionClosedException,
                ConnectionError,
            ):
                connection_closed = True
            assert connection_closed, "Expected connection to be closed due to oversized message"
        finally:
            try:
                ws.close()
            except Exception:
                pass


# ---------------------------------------------------------------------------
# Rate limiting tests
# ---------------------------------------------------------------------------


@pytest.mark.admin_secret
@pytest.mark.usefixtures("per_class_tests_db_state")
@pytest.mark.hge_env("HASURA_GRAPHQL_WEBSOCKET_MESSAGE_RATE_LIMIT", "5")
@pytest.mark.hge_env("HASURA_GRAPHQL_WEBSOCKET_MESSAGE_RATE_LIMIT_WINDOW", "2")
class TestWebSocketRateLimit:
    """Test per-connection message rate limiting (5 messages per 2 seconds)."""

    @classmethod
    def dir(cls):
        return "queries/fork/websocket_limits"

    def test_within_rate_limit_succeeds(self, hge_ctx):
        """Sending fewer messages than the limit should work."""
        ws = ws_connect_and_init(hge_ctx)
        try:
            for i in range(3):
                msg = json.dumps({
                    "id": str(i),
                    "type": "start",
                    "payload": {"query": "{ fork_ws_items { id } }"},
                })
                ws.send(msg)
                resp = ws_recv_data(ws)
                assert resp["type"] == "data", f"Message {i}: expected data, got: {resp}"
                # Also receive the 'complete' message
                complete = ws_recv_data(ws)
                assert complete["type"] == "complete", f"Message {i}: expected complete, got: {complete}"
        finally:
            ws.close()

    def test_exceeding_rate_limit_closes_connection(self, hge_ctx):
        """Sending more messages than the limit should close the connection.

        The rate limit is 5 messages per 2-second window. The connection_init
        counts as a message, so we have 4 remaining. We send rapidly to exceed
        the limit.
        """
        ws = ws_connect_and_init(hge_ctx)
        connection_closed = False
        try:
            # Send messages rapidly to exceed the rate limit.
            # connection_init already used 1 of 5 messages in this window.
            for i in range(10):
                msg = json.dumps({
                    "id": str(i),
                    "type": "start",
                    "payload": {"query": "{ fork_ws_items { id } }"},
                })
                try:
                    ws.send(msg)
                except (
                    websocket.WebSocketConnectionClosedException,
                    BrokenPipeError,
                    ConnectionError,
                ):
                    connection_closed = True
                    break
                try:
                    resp_raw = ws.recv()
                    if not resp_raw:
                        connection_closed = True
                        break
                except (
                    websocket.WebSocketConnectionClosedException,
                    ConnectionError,
                ):
                    connection_closed = True
                    break

            assert connection_closed, (
                "Expected connection to be closed due to rate limiting, "
                "but all messages were processed"
            )
        finally:
            try:
                ws.close()
            except Exception:
                pass

    def test_rate_limit_resets_after_window(self, hge_ctx):
        """After waiting for the window to pass, messages should be accepted again."""
        ws = ws_connect_and_init(hge_ctx)
        try:
            # Send a few messages (within limit)
            for i in range(3):
                msg = json.dumps({
                    "id": str(i),
                    "type": "start",
                    "payload": {"query": "{ fork_ws_items { id } }"},
                })
                ws.send(msg)
                resp = ws_recv_data(ws)
                assert resp["type"] == "data", f"Expected data, got: {resp}"
                complete = ws_recv_data(ws)
                assert complete["type"] == "complete"

            # Wait for the 2-second window to reset
            time.sleep(2.5)

            # Should be able to send more messages
            msg = json.dumps({
                "id": "after-reset",
                "type": "start",
                "payload": {"query": "{ fork_ws_items { id } }"},
            })
            ws.send(msg)
            resp = ws_recv_data(ws)
            assert resp["type"] == "data", f"Expected data after reset, got: {resp}"
        finally:
            ws.close()


# ---------------------------------------------------------------------------
# Normal operation tests (default limits)
# ---------------------------------------------------------------------------


@pytest.mark.admin_secret
@pytest.mark.usefixtures("per_class_tests_db_state")
class TestWebSocketNormalOperation:
    """Verify normal WebSocket operations work with default limits."""

    @classmethod
    def dir(cls):
        return "queries/fork/websocket_limits"

    def test_basic_query_over_websocket(self, hge_ctx):
        """A normal query should succeed with default limits."""
        ws = ws_connect_and_init(hge_ctx)
        try:
            msg = json.dumps({
                "id": "1",
                "type": "start",
                "payload": {"query": "{ fork_ws_items { id name } }"},
            })
            ws.send(msg)
            resp = ws_recv_data(ws)
            assert resp["type"] == "data", f"Expected data, got: {resp}"
            assert resp["id"] == "1"
            data = resp["payload"]["data"]
            assert "fork_ws_items" in data
        finally:
            ws.close()

    def test_connection_init_and_terminate(self, hge_ctx):
        """Test the full connection lifecycle: init, query, terminate."""
        ws = ws_connect_and_init(hge_ctx)
        try:
            # Send a query
            msg = json.dumps({
                "id": "1",
                "type": "start",
                "payload": {"query": "{ fork_ws_items { id } }"},
            })
            ws.send(msg)
            resp = ws_recv_data(ws)
            assert resp["type"] == "data"

            # Send connection terminate
            ws.send(json.dumps({"type": "connection_terminate"}))
        finally:
            try:
                ws.close()
            except Exception:
                pass
