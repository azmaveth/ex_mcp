# HTTP Transport Redesign Roadmap

**Based on:** Audit of MCP TypeScript SDK `StreamableHTTPClientTransport` vs ExMCP `HTTP` transport.
**Conformance status:** 200/202 client checks passing (99%). Remaining 2 failures are transport architecture issues.

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

### 1a. Auth loop protection
**Problem:** No guard against infinite 401 retry on misconfigured servers.
**TS SDK pattern:** `_hasCompletedAuthFlow` boolean flag — after successful auth, if next request still returns 401, throw error instead of retrying.
**Fix:** Add `auth_completed: boolean()` to transport state. Set true after successful OAuth flow. On 401 with `auth_completed: true`, return error.

### 1b. Unified SSE POST response handling
**Problem:** POST responses with `text/event-stream` content type are parsed differently than GET SSE events. The `retry:` field from POST SSE responses isn't processed. Stream close on POST doesn't trigger GET reconnection.
**TS SDK pattern:** All server messages flow through a single `onmessage` callback regardless of source.
**Fix:** When POST returns SSE-formatted response, process it through the same SSE event parser used by `SSEClient`. Extract `retry:` fields. Notify `SSEClient` of stream lifecycle events.

### 1c. 405 handling for GET SSE
**Problem:** If server returns 405 Method Not Allowed for GET SSE request, we don't handle it gracefully.
**TS SDK pattern:** Silently falls back to POST-only mode.
**Fix:** In `SSEClient.connect_sse`, handle 405 by notifying parent to disable SSE and continue with sync POST.

## Priority 2: OAuth Architecture

### 2a. OAuth as middleware, not transport integration
**Problem:** OAuth logic is deeply integrated into `perform_and_maybe_auth` in the HTTP transport. Makes it hard to test, hard to swap providers, and tightly couples auth to transport.
**TS SDK pattern:** `OAuthClientProvider` interface injected at transport construction. Auth wraps `fetch` as middleware.
**Fix:** Create `ExMCP.Authorization.Provider` behaviour with `tokens/0`, `save_tokens/1`, `authenticate/1`. Transport accepts `:auth_provider` option. Provider wraps request function.

### 2b. Scope negotiation from WWW-Authenticate
**Problem:** We extract scopes from `WWW-Authenticate` header but only use them for the initial auth flow. Scope step-up (re-auth with broader scopes on 403) is detected but the transport state doesn't propagate properly for subsequent requests.
**TS SDK pattern:** `withOAuthRetry` middleware handles both 401 and 403, re-runs full auth flow with new scope.
**Fix:** Clear `access_token` AND re-run auth with extracted scopes on `insufficient_scope`. Ensure transport state propagates through the receiver loop.

## Priority 3: Stream Lifecycle

### 3a. SSE reconnection timing
**Problem:** `retry:` field from SSE events updates `retry_delay` in state, but when stream ends, the reconnection timer may use stale state if the retry event and stream_end arrive in rapid succession.
**TS SDK pattern:** Reconnection delay is calculated at reconnection time, not stored.
**Fix:** In `schedule_reconnect`, re-read current `retry_delay` from state. Ensure GenServer processes `:stream` before `:stream_end` (they should be in order from `:httpc`, but verify).

### 3b. POST SSE stream lifecycle → GET reconnection
**Problem:** Conformance test `sse-retry` closes the POST tools/call SSE response stream and expects the client to reconnect the GET SSE stream. Our architecture treats POST and GET as independent.
**TS SDK pattern:** Unified stream management — any stream close triggers reconnection logic.
**Fix:** When `handle_http_response` receives a closed SSE response, notify `SSEClient` to verify its GET stream is still alive and reconnect if needed.

### 3c. Last-Event-ID resumption
**Problem:** We track `last_event_id` but it's only used in SSEClient. The HTTP transport's `build_request_headers` includes it for POST requests too, which may confuse stateless servers.
**Fix:** Only send `Last-Event-ID` on GET SSE reconnection requests, not on POST.

## Priority 4: API Design

### 4a. Unified message callback
**Problem:** `send_message` and `receive_message` are separate with different semantics. Server-initiated requests (elicitation, permission) arrive via `receive_message` but need to be responded to via `send_message`. This creates a state management gap.
**TS SDK pattern:** Single `onmessage` callback handles all incoming messages. Client registers handlers for specific methods.
**Fix:** Consider adding `on_server_request` callback to transport that allows the transport to respond inline, without requiring a separate `send_message` call.

### 4b. Receiver loop vs. callback model
**Problem:** `ConnectionManager.receive_loop` polls `receive_message` in a tight loop with different behavior for SSE, non-SSE, and waiting-for-session states. This is error-prone.
**TS SDK pattern:** Event-driven — SSE stream fires callbacks on data, no polling needed.
**Fix:** Replace polling receiver loop with event-driven model. `SSEClient` already sends messages to parent — extend HTTP transport to also push POST responses via messages instead of storing in `last_response`.

### 4c. Transport state threading
**Problem:** Transport state changes during `send_message` (new access_token, new session_id) must propagate to the receiver loop, but the receiver has its own copy of state. This causes the scope-step-up failure — the receiver doesn't see the updated token.
**TS SDK pattern:** Stateless transport — auth state is in the provider, not the transport.
**Fix:** Move auth state (access_token) out of transport struct into a separate Agent/ETS store that both send and receive paths can access.

## Priority 5: Completeness

### 5a. Session termination
**Problem:** No way to gracefully close a session (DELETE to endpoint).
**TS SDK pattern:** `terminateSession()` sends DELETE with session ID header.
**Fix:** Add `terminate_session/1` to transport API.

### 5b. Configurable reconnection options
**Problem:** Reconnection delays are hardcoded module attributes.
**TS SDK pattern:** `_reconnectionOptions` config with `initialDelayMs`, `maxDelayMs`.
**Fix:** Accept reconnection config at transport construction.

## What ExMCP Does Better Than TS SDK

- **Stateless server fallback**: Works with serverless/Lambda backends
- **Full RFC compliance**: PRM discovery, dynamic registration, PKCE, Retry-After
- **Scope step-up detection**: Auto-detects insufficient_scope
- **Heartbeat monitoring**: Detects stale streams proactively
- **Security validation**: Explicit SecurityGuard layer
- **Comprehensive state tracking**: Last-Event-ID, retry counts, response buffering
- **OTP resilience**: Supervised SSE GenServer with crash recovery

## Implementation Order

1. Auth loop protection (1a) — quick fix, critical safety
2. 405 handling (1c) — quick fix, spec compliance
3. OAuth provider behaviour (2a) — clean architecture
4. Transport state threading (4c) — fixes scope-step-up conformance
5. Unified POST SSE handling (1b) — fixes sse-retry conformance
6. Event-driven receiver (4b) — replaces polling, reduces complexity
7. Everything else — incremental improvements
