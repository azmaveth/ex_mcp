# HTTP Transport Redesign Roadmap

**Based on:** Audit of MCP TypeScript SDK `StreamableHTTPClientTransport` vs ExMCP `HTTP` transport.
**Conformance status:** 223/223 client, 39/39 server (100%). All conformance achieved without full redesign — targeted fixes resolved the issues. This document remains as a guide for architectural improvements.

## Current Architecture

ExMCP uses a dual-channel model:
- **POST** for client→server requests (synchronous via `:httpc`)
- **GET SSE** for server→client notifications (separate `SSEClient` GenServer)
- **OAuth** integrated at transport level (`perform_and_maybe_auth` wrapper)

The TypeScript SDK uses a unified model:
- **POST** for client→server requests (via fetch)
- **SSE** for all server responses (unified callback)
- **OAuth** as fetch middleware (wraps fetch function)

## Priority 1: Safety & Correctness

### ~~1a. Auth loop protection~~ ✅ DONE
`auth_completed` flag in transport state prevents infinite 401 retry loops.

### ~~1b. Unified SSE POST response handling~~ ✅ DONE
`parse_sse_events` + `trigger_sse_reconnect` + `handle_sse_post_response` process POST SSE responses through the same parser, extract `retry:` fields, and notify SSEClient.

### ~~1c. 405 handling for GET SSE~~ ✅ DONE
SSEClient returns `:sse_not_supported` on 405, transport falls back to POST-only mode.

## Priority 2: OAuth Architecture

### ~~2a. OAuth as middleware, not transport integration~~ ✅ DONE
`ExMCP.Authorization.Provider` behaviour with `init/1`, `get_token/1`, `handle_unauthorized/3`, `handle_forbidden/3`. Two built-in providers: `Provider.OAuth` (full PKCE flow) and `Provider.Static` (pre-existing tokens). Transport accepts `:auth_provider` option. Backwards compatible — `:auth` config auto-creates OAuth provider.

### ~~2b. Scope negotiation from WWW-Authenticate~~ ✅ DONE
`extract_scope_from_www_auth` + 403 handling with scope step-up. Full OAuth flow re-runs with broader scopes on `insufficient_scope`.

## Priority 3: Stream Lifecycle

### ~~3a. SSE reconnection timing~~ ✅ DONE
Server-specified `retry:` values preserved across reconnections. `stream_start` no longer resets delay.

### ~~3b. POST SSE stream lifecycle → GET reconnection~~ ✅ DONE
`trigger_sse_reconnect` notifies SSEClient when POST SSE stream closes.

### ~~3c. Last-Event-ID resumption~~ ✅ DONE
Last-Event-ID properly tracked and sent on GET SSE reconnection.

## Priority 4: API Design

### 4a. Unified message callback — DEFERRED
**Reason:** Current model works and passes 100% conformance. Low ROI for the churn.

### 4b. Receiver loop vs. callback model — DEFERRED
**Reason:** Polling receiver is functional. Full rewrite risks regressions with insufficient benefit.

### ~~4c. Transport state threading~~ ✅ RESOLVED
**Resolution:** The Provider pattern (2a) encapsulates auth state. Analysis of the receiver loop shows it only receives SSE events — it never makes HTTP requests or uses auth state. The only state it modifies is `last_event_id`. Since auth state changes in `send_message` don't affect the receiver, no ETS/Agent bridge is needed.

## Priority 5: Completeness

### ~~5a. Session termination~~ ✅ DONE
`terminate_session/1` sends DELETE with `Mcp-Session-Id` header. Called automatically from `close/1`. Server-side `HttpPlug` handles DELETE on any path with session header.

### ~~5b. Configurable reconnection options~~ ✅ DONE
`max_retry_delay`, `initial_retry_delay`, `connect_timeout`, `idle_timeout` all configurable via transport options, propagated to SSEClient.

## What ExMCP Does Better Than TS SDK

- **Stateless server fallback**: Works with serverless/Lambda backends
- **Full RFC compliance**: PRM discovery, dynamic registration, PKCE, Retry-After
- **Scope step-up detection**: Auto-detects insufficient_scope
- **Heartbeat monitoring**: Detects stale streams proactively
- **Security validation**: Explicit SecurityGuard layer
- **Comprehensive state tracking**: Last-Event-ID, retry counts, response buffering
- **OTP resilience**: Supervised SSE GenServer with crash recovery

## Remaining Work

All items complete. The transport redesign is finished.
