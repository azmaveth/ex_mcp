# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Deprecated
- **`ExMCP.Server.Tools` API** — `ExMCP.Server.Tools`, `ExMCP.Server.Tools.Simplified`, and companion modules (`Builder`, `Helpers`, `Registry`, `ResponseNormalizer`, `ASTValidator`) are deprecated and will be **removed in 1.1.0**. `use ExMCP.Server.Tools` and `use ExMCP.Server.Tools.Simplified` emit compile-time warnings. Migrate to `ExMCP.Server.Handler` + `ExMCP.Server.DSL` (see the DSL guide and migration guide).
- **Image processing stubs** — Compress/resize/thumbnail/encoding conversion helpers under `ExMCP.Content.Transformer`, `ExMCP.Content.Builders`, and related Validation pipelines are deprecated for **removal in 1.1.0**. They were never required by MCP/ACP (which only define **image content blocks**: base64 + MIME). Use `ExMCP.Content.image/2` for protocol content; do image processing in the application if needed.
- **`remove_metadata` / face-color analysis stubs** — Same deprecation class; not protocol features.

### Improved
- **Protocol sync verification (2026-07-11)** — Confirmed alignment with MCP **2025-11-25** (latest stable) and ACP major **v1**. Official conformance re-run: **39/39** server and **226/226** client (`@modelcontextprotocol/conformance@0.1.16` core suite). All-versions server/client suites green for 2025-11-25 and 2025-06-18. Refreshed local `docs/mcp-specs/2025-11-25` via `mix mcp.sync_spec --force`. Documented that MCP **2026-07-28** draft/RC is intentionally not implemented yet.
- **SSE bidirectional stability** — `ExMCP.Server.SSESession` waits for a live GET stream (and sole-session fallback), drops dead ETS registrations, cleans up on stream exit, and tolerates int/string request-id echoes. Conformance harness frees port 3099 between runs so elicitation/sampling tools no longer flake with “Server did not request elicitation/sampling”.
- **DSL compile errors** — `ExMCP.Server.DSL` now raises `CompileError`s with file/line and actionable messages for missing handlers, duplicate tool/resource/prompt ids, wrong instructions per declaration kind, unknown instructions (with suggestions such as `inputSchema` → `input_schema`), non-literal values, and invalid param types.
- **DSL docs and examples** — Documented param types, compile-time checks, and `ToolResult` scoping in the DSL guide and quick start; examples note the `ToolResult` alias.
- **1.0 cleanup** — Documented API stability (README / `ExMCP` moduledoc); fixed stale agent docs (`CLAUDE.md`) that still referenced removed client adapters; HexDocs module groups now feature core APIs and group deprecated Tools modules; content modules clarify protocol vs experimental helpers; client list-changed notifications emit telemetry instead of bare TODOs; removed empty `lib` directories and finished docs audit plan file; pruned dead `.dialyzer_ignore.exs` entries.
- **Content schema validation** — `ExMCP.Content.SchemaValidator.validate_schema/2` and `ExMCP.Content.Validation.validate_schema/2` now validate with **ExJsonSchema**.
- **Unicode NFC** — `Sanitizer.normalize_unicode/1` and related Validation sanitization use `:unicode.characters_to_nfc_binary/1`.

### Fixed
- **Bare `:array` param types** — `param :name, :array` no longer silently maps to a string schema. Use `{:array, :string}` (or another item type). Examples (`advanced_dsl_server`, `weather_service`) were updated accordingly.
- **Empty-list param defaults** — `default: []` is preserved in generated JSON Schema properties.
- **Honest image transform behaviour** — Deprecated resize no longer pretends to change width/height without processing pixels.

## [1.0.0-rc.4] - 2026-07-08

### Added
- **HTTP handler initialization context** — `ExMCP.HttpPlug` now accepts `:handler_opts` as a static term, `Plug.Conn` function, `Plug.Conn` plus decoded request function, or MFA tuple, and passes the resolved value into temporary handler GenServers. This lets Phoenix/Plug pipelines pass verified request context such as authenticated users, tenants, or signed request data into handler `init/1`.

### Fixed
- **MessageProcessor handler startup** — `ExMCP.MessageProcessor` now starts temporary handler GenServers with the configured handler options and consistently assigns `server_info` before dispatching initialize requests.
- **MessageProcessor handler reply normalization** — `ExMCP.MessageProcessor.MethodHandlers` now accepts map-shaped list results and GenServer-style three-tuple replies from handler processes, preserving compatibility with existing handlers while returning MCP tool errors as successful `isError` tool results.
- **Release docs** — Updated active installation snippets and Phoenix request-context examples for the rc.4 release line.

## [1.0.0-rc.3] - 2026-07-03

### Fixed
- **State machine test determinism** — Removed an idle-timeout race from the state machine test transport so successful handshakes are not sampled after the fake transport has already disconnected.

## [1.0.0-rc.2] - 2026-07-03

### Changed
- **ACP protocol updates** — Added current ACP request cancellation support, propagated prompt `messageId` values through client/agent/protocol helpers, and refreshed adapter chunk handling for upstream-compatible message streaming.
- **Codex ACP upstream sync** — Synced `ExMCP.ACP.Adapters.Codex` with `agentclientprotocol/codex-acp` v1.1.0 behavior, including upstream auth ids (`api-key`, `chat-gpt`, and opt-in `gateway`), `read-only`/`agent`/`agent-full-access` modes, app-server `session/delete` through thread archive, additional workspace directories, upstream prompt content shapes, HTTP MCP `http_headers`, per-turn model/mode/reasoning/fast-mode application, `/status`, richer terminal/MCP metadata, usage updates, guardian review, image view, and fuzzy file search events.
- **Claude ACP upstream sync** — Synced `ExMCP.ACP.Adapters.ClaudeSDK` with compatible `agentclientprotocol/claude-agent-acp` v0.55.0 behavior, including Claude Agent SDK 0.3.198 launch metadata, initialize-aware terminal/gateway auth method advertisement, `auth.logout` capability, upstream `mode` config option id, fast-mode and agent config controls when advertised by the SDK, MCP slash-command prompt rewriting, resource-link plus embedded-context prompt shapes, HTTP image prompt support, tool-call-before-permission ordering, Bash terminal metadata, result usage updates, and richer model/agent config option metadata.
- **Pi ACP upstream sync** — Synced `ExMCP.ACP.Adapters.Pi` with compatible `svkozak/pi-acp` v0.0.31 behavior, including upstream-compatible Pi RPC spawn arguments, per-session `model` and `thought_level` config options, config-option sync updates after model/thinking changes, and Pi session listing that prefers latest message activity over later metadata-only timestamps.
- **MCP conformance tooling** — Updated the draft conformance runner pin to the latest published alpha while keeping stable conformance runs on the published `latest` runner.

### Fixed
- **Claude final-result fallback** — `ExMCP.ACP.Adapters.ClaudeSDK` now emits an `agent_message_chunk` from Claude SDK `session_result` text when the SDK completes without streaming message chunks, so ACP clients that accumulate streamed text do not receive an empty answer.

### Breaking Changes
- **Codex ACP mode IDs** — `ExMCP.ACP.Adapters.Codex` now only accepts upstream codex-acp-compatible mode IDs: `read-only`, `agent`, and `agent-full-access`. The older `auto` and `full-access` ids are no longer accepted, along with the prior `suggest`, `auto-edit`, and `full-auto` aliases.

## [1.0.0-rc.1] - 2026-06-15

### Changed
- **BEAM-local MCP cleanup** — Removed the public `ExMCP.Native` direct dispatcher stack, `ExMCP.Service`, and `ExMCP.ServiceRegistry.*`. BEAM-local MCP now uses `transport: :beam` with a server pid, preserving MCP initialization, capabilities, request ids, and tool/resource/prompt semantics.
- **Examples and docs refresh** — Updated README, guides, getting-started docs, and examples to use `ExMCP.Server.Handler` plus `ExMCP.Server.DSL`; removed stale debug/test examples and the placeholder OAuth full-flow script.

### Removed
- **Optional Horde dependency** — Removed the old optional Horde dependency and registry wiring because the Native dispatcher API no longer exists.
- **Legacy Claude ACP adapter** — Removed `ExMCP.ACP.Adapters.Claude`; Claude Code integrations now use `ExMCP.ACP.Adapters.ClaudeSDK`.
- **Unused public cleanup modules** — Removed `ExMCP.ClientConfigEnhanced`, `ExMCP.ClientConfig.Macros`, and the old distributed `ExMCP.Transport.Beam.*` cluster namespace. The active BEAM-local MCP path remains `transport: :beam`.

## [1.0.0-rc.0] - 2026-06-08

### Added
- **Claude SDK ACP adapter** — Added `ExMCP.ACP.Adapters.ClaudeSDK`, a new Claude Code adapter that uses the SDK-compatible stream-json control protocol with permission bridging, partial tool-call lifecycle events, SDK interrupt cancellation, runtime model/mode/effort config, richer status updates, plan updates, and SDK launch environment support.
- **Claude SDK session store** — Added pure Elixir helpers for Claude Code's SDK session store, including SDK-compatible project key derivation, JSONL metadata extraction, sidechain filtering, transcript reads, ACP `session/list`, disk-backed `session/fork`, and validated `session/delete`.
- **Codex ACP parity** — Expanded `ExMCP.ACP.Adapters.Codex` with app-server-backed `session/list`, `session/load`/resume replay, `session/close`, embedded-context prompts, ACP HTTP/stdio MCP descriptor forwarding, Codex auth methods, runtime model/reasoning config updates, `session/set_model`, Codex model catalog state from `model/list`, slash commands (`/compact`, `/init`, `/review`, `/review-branch`, `/review-commit`, `/logout`), and permission request bridging for Codex approval prompts.
- **Pi ACP parity** — Reworked `ExMCP.ACP.Adapters.Pi` around ACP-native session setup/load/list, terminal auth advertisement, model state and `session/set_model`, thinking levels through `session/set_mode`, prompt queuing, image/resource/audio prompt normalization, built-in and markdown slash commands, structured tool updates, edit diffs, session replay, and a pure Elixir Pi session map helper.
- **ACP session fork support** — Added unstable `session/fork` protocol/client/agent/adapter-bridge support, including `ExMCP.ACP.Client.fork_session/4`, native agent `handle_fork_session/3`, adapter `fork_session/2`, and capability auto-advertisement.
- **Adapter bridge direct replies** — Adapter implementations can now return direct JSON-RPC results, emit ACP messages before direct replies, emit messages from notifications, or reply while also writing to the subprocess, so adapted agents can own session IDs, replay history, and runtime config responses without bridge-synthesized placeholders.
- **Adapter-managed bridge mode** — Adapters can now return `:adapter_managed` from `command/1`, own persistent subprocess Ports directly, receive forwarded process messages through `handle_adapter_message/2`, and clean up resources through `shutdown/1`.
- **Adapter auth method advertisement** — ACP adapters can implement `auth_methods/1` so the bridge initialize response reflects implemented authentication methods instead of always returning an empty list.

### Changed
- **ACP docs** — Updated README and the ACP guide to recommend `ClaudeSDK` for new Claude Code integrations while keeping the legacy `Claude` stream-json adapter documented.
- **Claude SDK session capabilities** — The new adapter now advertises live session setup/load/resume/fork/close plus disk-backed `session/list` and `session/delete` support for Claude Code's local SDK store. `session/load` now replays persisted JSONL transcript entries as ACP updates before returning.
- **Claude SDK MCP capabilities** — Replaced legacy `"mcpServers" => true` advertisement with official ACP `mcpCapabilities` and an ExMCP-only `_meta.ex_mcp.mcpCapabilities.beam` flag for BEAM-local MCP transport negotiation.
- **Claude SDK prompt lifecycle** — Claude SDK prompts now queue while another prompt is active and drain in FIFO order after the active prompt result; queued prompts for a cancelled/closed/deleted session receive cancelled prompt responses.
- **Codex adapter session safety** — Codex prompt, cancel, mode, and config requests now require an explicit known `sessionId` instead of falling back to the last active thread. Codex-specific session metadata now lives under `_meta.ex_mcp.codex` instead of a non-spec root `metadata` field.
- **Codex app-server event coverage** — The Codex adapter now handles current app-server camelCase item variants (`commandExecution`, `fileChange`, `mcpToolCall`, `dynamicToolCall`, `webSearch`, `imageGeneration`), reasoning summary deltas, plan/status/goal/compaction notifications, and richer replay of loaded turn history. Unsupported dynamic tool calls, request-user-input prompts, ChatGPT token refresh, and attestation requests are rejected explicitly instead of being treated as approval prompts.
- **Pi adapter ACP surface** — Pi-specific control now uses ACP methods and slash commands instead of custom extension methods. `session/new` and `session/load` wait for Pi state/model/command responses instead of returning bridge-generated placeholder sessions, and `session/list` scans Pi JSONL sessions with optional cwd filtering.
- **Pi adapter remaining parity** — Pi now runs through adapter-managed subprocesses, supports `session/resume`, `session/close`, and `session/delete`, returns paginated `session/list` result maps with `nextCursor`, merges global/project Pi settings, filters skill commands from settings, emits startup/prelude context, adds `/changelog`, improves slash-command result messages, and maps prompt-time auth failures to ACP auth-required errors.
- **Pi destructive defaults** — Pi `session/delete` removes ExMCP session-map state by default and only deletes backing Pi JSONL files when `delete_session_files: true` is configured. Pi registry update notices are opt-in through `update_notice: true` or `PI_ACP_UPDATE_NOTICE=true`.
- **Adapter bridge set_model behavior** — `AdapterBridge` now returns method-not-found when an adapter skips `session/set_model`, instead of synthesizing a successful empty result for an unimplemented model change.

### Fixed
- **HexDocs package contents** — Included the public guide docs in the Hex package and updated ExDoc module grouping so the 1.0 RC documentation builds against current public modules.

### Breaking Changes
- **MCP server DSL compatibility removal** — Removed the legacy `use ExMCP.Server` macro, `deftool`/`defresource`/`defprompt` declarations, generated getter APIs (`get_tools/0`, `get_resources/0`, `get_prompts/0`), `ExMCP.DSL.*` modules, and the old `ExMCP.Server.start_link` helper. Server implementations should use `ExMCP.Server.Handler` directly, optionally combined with `ExMCP.Server.DSL`, and start handler processes with `MyServer.start_link/1`, `ExMCP.Server.HandlerServer.start_link/1`, or `ExMCP.start_server/1`.
- **MCP handler server rename** — Renamed `ExMCP.Server.Legacy` to `ExMCP.Server.HandlerServer` and removed migration-only `MessageProcessor` dispatcher/handler facades for direct/genserver legacy server modes.
- **Server notification spelling** — Removed the legacy `notify_resource_updated` alias. Use `ExMCP.Server.notify_resource_update/2`.
- **Refactor compatibility helpers removed** — Removed unused `ExMCP.Server.RefactorHelpers`, old DSL-oriented `ExMCP.ContentHelpers`, `ExMCP.ErrorHelpers`, and transport `send`/`recv` compatibility aliases. Use `ExMCP.Protocol.ResponseBuilder`, `ExMCP.Content`, `ExMCP.Error`, and transport `send_message/2`/`receive_message/1` respectively.
- **Root protocol wrapper removed** — Removed deprecated `ExMCP.Protocol`. Low-level protocol construction/parsing remains internal; public protocol utility modules under `ExMCP.Protocol.*` remain available.
- **Client adapter switching layer removed** — Removed `ExMCP.Client.Adapter`, `ExMCP.Client.LegacyAdapter`, `ExMCP.Client.StateMachineAdapter`, `ExMCP.Client.Wrapper`, and `ExMCP.Client.Configuration`. Use `ExMCP.Client` as the supported public client API.
- **Handler init arguments are explicit** — `ExMCP.Server.HandlerServer` no longer passes leftover transport/system options to `handler.init/1`. Use `handler_args: term` when a handler needs initialization data.
- **Transport aliases removed** — Removed public `transport: :sse` and `transport: :native`. Use `transport: :http, use_sse: true` for HTTP SSE streaming and `transport: :beam` for BEAM-local MCP transport. BEAM transport now carries MCP-shaped maps/lists as Elixir terms without JSON serialization.
- **ACP BEAM MCP metadata renamed** — ExMCP ACP metadata now advertises BEAM-local MCP support only as `_meta.ex_mcp.mcpCapabilities.beam`; the previous `native` metadata flag and `:native_mcp` builder option were removed.
- **ACP adapter session listing callback** — Custom ACP adapters should implement `list_sessions(params, state)` instead of `list_sessions(state)`. The bridge now forwards decoded `session/list` params directly to adapters and no longer carries the legacy arity.
- **Codex ACP mode IDs** — `ExMCP.ACP.Adapters.Codex` now only accepts upstream codex-acp-compatible mode IDs: `read-only`, `agent`, and `agent-full-access`. The old `suggest`, `auto-edit`, `auto`, `full-auto`, and `full-access` aliases were removed.
- **Adapter authenticate behavior** — `AdapterBridge` no longer returns `{}` for `authenticate` when an adapter skips the request. Unimplemented auth now returns method-not-found or invalid-params, while adapters that write to their subprocess can produce the eventual auth response themselves.
- **Pi extension methods removed** — `ExMCP.ACP.Adapters.Pi` no longer implements `_ex_mcp.pi/*` or legacy `pi/*` control methods. Use `session/set_model`, `session/set_mode`, `session/load`, `session/list`, or prompt slash commands such as `/compact`, `/export`, `/session`, `/name`, `/steering`, and `/follow-up`.

## [0.12.0] - 2026-06-07

### Added
- **ACP functional cores** — Added focused ACP modules for capabilities, lifecycle params, envelopes, metadata, map helpers, name/value normalization, and adapter event normalization. These keep protocol transformation logic pure and easier to test while preserving the existing facade APIs.
- **ACP interoperability coverage** — Expanded integration fixtures and tests for official ACP TypeScript libraries, including everything-style agent/client flows, prompt lifecycle events, auth/logout, permissions, session list/resume/close/delete, and rich content blocks.

### Changed
- **ACP stable spec alignment** — Updated ACP protocol encoding/decoding, session lifecycle params, prompt/session updates, capability builders, MCP server config shapes, extension method names, and adapter output to match the current stable ACP schema.
- **ACP adapter behavior** — Claude, Codex, and Pi adapters now produce normalized stable update events for tool calls, tool call progress, usage, plan/config/mode/session info, and streaming message chunks. Deprecated Pi extension aliases are still accepted, but only `_ex_mcp.pi/*` methods are advertised.
- **Claude adapter permission defaults** — The Claude adapter no longer defaults to `--permission-mode bypassPermissions`; it now inherits the Claude CLI default unless a permission mode is configured explicitly. Permission mode atoms now map to Claude CLI's real mode names.
- **HTTP Plug internals** — Extracted request parsing, session resolution, CORS/origin checks, response shaping, and SSE handling into a smaller functional core so side effects stay at the Plug boundary.

### Fixed
- **Official ACP library compatibility** — Fixed response result shapes, session id propagation, update discriminators, error responses, prompt capability handling, and adapter bridge request handling that prevented clean interop with official ACP SDK clients and agents.
- **MCP HTTP/SSE edge cases** — Hardened streamable HTTP and SSE parsing, session manager selection, response conversion, and structured output wrapping across conformance and integration paths.
- **Client state-machine tests** — Reworked timing-sensitive client tests to use telemetry-driven synchronization instead of sleeps.

### Security
- **MCP HTTP hardening** — OAuth and HTTP guard paths now fail closed on invalid auth state, validate scopes consistently, preserve configured session managers, enforce origin/CORS decisions at the boundary, and keep request body handling bounded.
- **ACP adapter hardening** — Adapter process startup now avoids leaking configured API keys into child environments unless explicitly required, and adapter error responses are normalized before crossing the ACP boundary.

## [0.11.0] - 2026-06-05

### Added
- **ACP Claude adapter — `sessionId` on prompt response result** — `ExMCP.ACP.Adapters.Claude` now includes `"sessionId"` in the prompt response result map when `state.session_id` is non-nil. The field sits alongside the existing optional `"thinking"` blocks and is omitted entirely when no session id is available, so downstream consumers can rely on `Map.has_key?(result, "sessionId")` as an absence signal. Backwards-compatible — callers that ignore it keep working unchanged. Useful for callers that need to correlate a prompt response with the underlying Claude SDK session, drive `--resume` themselves, or attach the session id to audit / telemetry.

## [0.10.1] - 2026-05-29

### Fixed
- **CI test determinism** — Explicitly load target modules before API export assertions so tests do not depend on execution order.

## [0.10.0] - 2026-05-29

### Added
- **Native ACP agents** — `ExMCP.ACP.Agent` and `ExMCP.ACP.Agent.Handler` let Elixir applications implement the agent side of the Agent Client Protocol over the same transports as the ACP client.
- **ACP agent facade helpers** — `ExMCP.ACP.start_agent/1`, `run_agent/1`, and streaming helpers provide a symmetrical API for building controllers and agents.
- **ACP examples** — Added an end-to-end native Elixir ACP echo agent and controller under `examples/acp`.
- **ACP cross-SDK interop fixtures** — Added TypeScript SDK agent/client fixtures plus ExMCP integration tests that cover both directions of ACP controller/agent interoperability.
- **ACP everything-style interop coverage** — Added broad ACP fixtures that exercise auth/logout, session lifecycle, prompt/cancel, mode/config updates, permission requests, filesystem requests, terminal requests, session updates, and rich content blocks.
- **ACP `usage_update` helpers** — Added protocol, type, and agent helper APIs for emitting stable context-window usage updates.

### Changed
- **ACP documentation** — Updated README, examples, and the ACP guide to cover both controller-side and agent-side protocol support.
- **ACP capabilities** — Updated ACP interop agents to use the official schema shape for session capability declarations.

### Fixed
- **ACP streamed prompt isolation** — `ExMCP.ACP.Client` now accumulates streamed `agent_message_chunk` text only while a matching prompt is pending, preventing out-of-band updates such as `session/load` chunks from leaking into the next prompt result.

## [0.9.2] - 2026-05-28

### Added
- **ACP Registry helpers** — `ExMCP.ACP.Registry` can fetch, parse, search, and build `npx` commands from the public ACP Registry.
- **ACP handler runner** — Agent-originated requests and session update handlers now run outside the ACP client process, so slow permission, file, terminal, or update handlers cannot block streamed updates or prompt completion.
- **MCP 2025-11-25 conformance coverage** — Conformance scripts and tests now cover the latest supported MCP spec version and updated server behaviors.

### Changed
- **ACP stable spec alignment** — Updated ACP method names, content/resource builders, permission responses, terminal delegation, config options, prompt capabilities, session capabilities, and adapter update shapes to match the current stable ACP v1 schema.
- **Adapter update normalization** — Claude, Codex, and Pi adapters now emit stable `agent_thought_chunk`, `tool_call`, and `tool_call_update` shapes for core streaming and tool lifecycle events.
- **Prompt text handling** — `Client.prompt/4` now folds streamed `agent_message_chunk` text into the returned result when agents stream the answer instead of returning inline text.

### Fixed
- **Permission cancellation** — `Client.cancel/2` now replies to pending `session/request_permission` requests with the required `cancelled` outcome without waiting for a blocked handler.
- **MCP protocol edge cases** — Updated method handling and version registry tests for newly covered MCP conformance cases.

## [0.9.1] - 2026-04-11

### Added
- **`HttpPlug`: cached body support via `conn.assigns[:raw_body]`** — Upstream
  plugs (e.g., signature-verification auth pipelines) can now pre-read the
  request body and stash it in `conn.assigns[:raw_body]`. The HTTP plug
  checks for a cached body before calling `read_body/1`, avoiding the
  empty-body issue that occurs when the underlying adapter has already
  been consumed by an upstream plug.

  Backwards compatible: callers that don't pre-read the body see no change
  in behavior. The new helper falls through to `read_body/1` when
  `raw_body` is absent.

  Use case: enables HTTP-authentication plugs that need to verify the body
  bytes (e.g., per-request request signing) before ExMCP processes the
  request, without forcing the auth plug to patch ExMCP, swap the conn
  adapter, or replace `ExMCP.HttpPlug` entirely.

## [0.9.0] - 2026-03-18

### Added
- **Pi ACP Adapter** — Full adapter for the Pi coding agent (badlogic/pi-mono) with 25 RPC commands and 14 event types
  - Text/thinking streaming, tool execution lifecycle, auto-compaction/retry events
  - Extension UI request/response bridge for dialog flows
  - Session persistence via `--session` flag, session directory scanning for `session/list`
  - Image support with data-url prefix stripping
  - 6 config options routed to native Pi RPC (model, thinking_level, auto_compaction, auto_retry, steering_mode, follow_up_mode)
- **ACP Spec Compliance** — All stabilized ACP features now implemented
  - `session/list` method (stabilized March 9, 2026)
  - All 8 official session update types: `user_message_chunk`, `agent_message_chunk`, `tool_call_update`, `plan_update`, `available_commands_update`, `config_option_update`, `current_mode_update`, `session_info_update`
  - Content blocks: `audio`, `resource_link`, `resource` (in addition to existing `text`, `image`)
  - `sessionCapabilities` in agent capabilities
  - ACP error codes: `-32000` (auth_required), `-32002` (resource_not_found)
  - Terminal request routing in Client (`terminal/*` methods delegated to handler)
- **Adapter Behaviour Extensions** — 3 new optional callbacks
  - `modes/0` — declare supported operational modes (advertised in initialize response)
  - `config_options/0` — declare supported config options (advertised in initialize response)
  - `list_sessions/1` — return available sessions for `session/list`
- **AdapterBridge Enhancements**
  - `session/list` handler with adapter delegation or empty fallback
  - `session/set_mode` handler with synthesized OK response
  - `session/set_config_option` handler routed through adapters
  - `authenticate` handler with synthesized OK response (RFD draft scaffolding)
  - Initialize response includes modes, configOptions, sessionCapabilities from adapter callbacks
- **Authentication Scaffolding** — protocol authentication encoding, `Client.authenticate/3`, and `Types.auth_required_code/0`
- **Plan Mode Builders** — `Types.plan_entry/3`, `Types.plan_update/2` for structured plan updates

### Changed
- **Claude Adapter** — Zed-parity tool introspection
  - Context-aware tool titles: "Read lib/app.ex (10-29)", "Search: defmodule"
  - Structured metadata: `kind` (read/write/execute/search/think), `locations` (file:line for jump-to-source), `content` (diff/terminal/text)
  - Tool calls now use spec-compliant `tool_call_update` (was non-standard `tool_call`)
  - Tool results include `completed`/`failed` status
  - Project-relative display paths when cwd is known
  - Usage streaming notification emitted before final result
  - System event and rate_limit_event forwarding as status notifications
  - Richer stop reason classification (end_turn, max_tokens, tool_use, error)
  - Declares `config_options` (model, thinking_budget)
- **Codex Adapter** — Tool call lifecycle and enrichments
  - Tool call notifications: `item/created` with `function_call` type
  - Tool completion: `item/completed` for function_call, function_call_output, patch types
  - Command execution lifecycle (started/outputDelta/completed)
  - Web search events (started/completed)
  - Image content in prompts
  - Session resume via `session/load` → `thread/start` with threadId
  - Status notification on turn/completed
  - Declares `modes` (suggest, auto-edit, full-auto) and `config_options` (model)
- **Pi Adapter** — Enhanced tool result parsing matching pi-acp reference
  - Content blocks, diff details, stdout/stderr/exitCode formatting
  - Replaces simpler `extract_tool_content` with full `extract_tool_result_text`

### MCP Protocol Conformance — 100% Client and Server
- **Official MCP Conformance** — 223/223 client checks, 39/39 server checks (0 failures, 0 warnings)
- **Full OAuth 2.1 Authorization Code Flow with PKCE** (`ExMCP.Authorization.FullOAuthFlow`)
  - Protected Resource Metadata discovery (RFC 9728) with path-based and root fallback
  - OIDC/OAuth AS metadata discovery with 4 URL patterns (RFC 8414)
  - Dynamic Client Registration (RFC 7591)
  - Local redirect server for authorization code callback
  - Token endpoint auth method selection (client_secret_basic, client_secret_post, none)
  - Client ID Metadata Document support (CIMD)
  - Scope negotiation from WWW-Authenticate header and PRM scopes_supported
  - Scope step-up on 403 insufficient_scope
  - Resource mismatch validation (RFC 8707)
- **HTTP Transport OAuth Integration**
  - Automatic 401/403 → OAuth discovery → token → retry
  - Auth loop protection (prevents infinite retry)
  - Unified FullOAuthFlow for both pre-existing credentials and browser auth
  - SSE POST response parsing with retry field extraction
  - SSE forced reconnection for pending tool results
  - Last-Event-ID propagation on reconnection
- **Elicitation Support**
  - `ExMCP.Client.ElicitationHandler` — configurable auto-accept/decline
  - Capability-aware routing (method-not-found when not declared)
  - `ExMCP.Testing.SchemaGenerator` — generate test values from JSON Schema
- **Server-Side SSE Sessions** (`ExMCP.Server.SSESession`)
  - Bidirectional SSE for server→client requests (elicitation, sampling)
  - ETS-based pending request tracking
  - GET SSE stream registration and event loop
- **DNS Rebinding Protection** (`ExMCP.Plugs.DnsRebinding`)
- **MessageProcessor Fixes**
  - `deep_stringify_keys` for all handler response paths (atom→string keys)
  - Initialize response normalization for Handler path
  - tools/call result wrapping in `{content: [...]}` format
  - `logging/setLevel` and `completion/complete` handlers
  - Default protocol version updated to `2025-11-25`
- **Test Infrastructure**
  - `scripts/test.sh` — saves output on every run
  - `scripts/conformance.sh` — runs official MCP conformance framework
  - `capture_log: true` — logs shown on test failures
  - Expected-failures baseline for CI
- **HTTP Transport Improvements**
  - URL auto-splitting (extract endpoint from URL path)
  - `:sse` transport alias → HTTP with use_sse: true (was broken)
  - SSE receive loop stability (waiting_for_session, not_supported_in_sync_mode)
  - 405 handling for GET SSE (graceful fallback)
  - SSE retry field parsing and timing buffer

### Removed
- Misleading `supportedModes` from Claude and Codex capabilities (removed features that weren't actually implemented)

## [0.8.4] - 2026-03-10

### Fixed
- `AdapterTransport` receiver loop now uses `:infinity` timeout (was 30s) for `AdapterBridge.receive_message/2`. This prevents spurious `receiver_exited` errors when CLI agents (Claude, Codex) take longer than 30 seconds to produce their first output line — common during complex reasoning or multi-turn tool use. The timeout is now configurable via `receive_timeout:` in transport opts.

## [0.8.3] - 2026-03-09

### Added
- Claude adapter handles multi-turn tool use sequences (`assistant(thinking)→assistant(tool_use)→user(tool_result)→assistant(text)→result`). Emits `tool_call` and `tool_result` session updates for observability.

## [0.8.2] - 2026-03-09

### Fixed
- **BREAKING:** Comprehensive ACP spec conformance audit — align all method names, field names, and message structures with the [ACP specification](https://agentclientprotocol.com/)
  - `session/prompt` params key is `"prompt"` (not `"content"`) per spec
  - `initialize` request uses `"clientCapabilities"` (not `"capabilities"`)
  - `initialize` response reads `"agentCapabilities"` (not `"capabilities"`)
  - Method names use snake_case: `session/set_mode`, `session/set_config_option`, `session/request_permission`
  - File system methods: `fs/read_text_file` / `fs/write_text_file` (not `session/fileRead` / `session/fileWrite`)
  - `session/update` notifications use nested `"update"` object with `"sessionUpdate"` discriminator (not flat `"kind"`)
  - Text updates use `"sessionUpdate": "agent_message_chunk"` with content block (not `"kind": "text"`)
  - Permission options use `"optionId"` field (not `"id"`)
  - Permission response is flat `{"outcome": "selected", "optionId": "..."}` (not wrapped)
  - `fs/write_text_file` response returns `null` result (not empty map)
  - Image content blocks use `"mimeType"` (not `"mediaType"`)
  - Plan entries use `"content"` / `"priority"` (not `"id"` / `"title"`)
  - Capabilities restructured to match spec (`loadSession`, `promptCapabilities`, `mcp`, `fs`, `terminal`)

## [0.8.1] - 2026-03-09

### Fixed
- ACP `session/prompt` message uses correct `"content"` param key instead of `"prompt"` to match the ACP specification

## [0.8.0] - 2026-03-08

### Added
- **Agent Client Protocol (ACP) Support** -- Full implementation of the [Agent Client Protocol](https://agentclientprotocol.com/) for controlling coding agents programmatically
  - `ExMCP.ACP` facade module for quick client startup
  - `ExMCP.ACP.Client` GenServer for managing ACP agent connections over stdio
  - `ExMCP.ACP.Protocol` for ACP-specific JSON-RPC 2.0 message encoding (integer protocol versions, ACP method names)
  - `ExMCP.ACP.Types` with type specifications and builder functions for ACP messages
  - `ExMCP.ACP.Client.Handler` behaviour for handling session events (updates, permission requests, file access)
  - `ExMCP.ACP.Client.DefaultHandler` implementation that auto-allows permissions
- **ACP Adapter System** for non-native agents
  - `ExMCP.ACP.Adapter` behaviour for protocol translation between ACP and agent-native formats
  - `ExMCP.ACP.AdapterBridge` GenServer bridge managing adapted agent subprocesses
  - `ExMCP.ACP.AdapterTransport` transport implementation delegating to the adapter bridge
  - `ExMCP.ACP.Adapters.Claude` -- Adapter for Claude Code CLI (NDJSON stream-json protocol)
  - `ExMCP.ACP.Adapters.Codex` -- Adapter for Codex CLI (app-server JSON-RPC protocol)
- **ACP Session Management** -- Create, resume, prompt, cancel, and configure sessions
  - `session/new`, `session/load`, `session/prompt`, `session/cancel` methods
  - `session/set_mode`, `session/set_config_option` for runtime agent configuration
  - Streaming session updates via notifications
  - Bidirectional communication for permission and file access requests
- **ACP Documentation** -- New [ACP Guide](docs/ACP_GUIDE.md) with usage examples and adapter development instructions

## [0.7.4] - 2026-02-14

### Fixed
- Fixed compile warnings for users without Horde installed -- `ExMCP.ServiceRegistry.Horde` now uses `apply/3` for all `Horde.Registry` calls to avoid compile-time "module is not available" warnings
- Removed 15 dead test files left over from DSL migration (eliminates ExUnit `test_load_filters` warning)

## [0.7.3] - 2026-02-13

### Added
- **OAuth Client Credentials with JWT Authentication** (`private_key_jwt`) -- RFC 7523 Section 2.2 client assertions as an alternative to client secrets for machine-to-machine auth
- **Enterprise-Managed Authorization (ID-JAG)** -- RFC 8693 token exchange + RFC 7523 JWT bearer grants for enterprise SSO flows
- **JWT Infrastructure** (`ExMCP.Authorization.JWT`) -- General-purpose JWT module wrapping JOSE for key management, signing, verification, and claims validation
- **Client Assertion Module** (`ExMCP.Authorization.ClientAssertion`) -- Build and verify JWT client assertions for token endpoint authentication
- **Discovery Flow** (`ExMCP.Authorization.DiscoveryFlow`) -- Full 401-to-discovery-to-auth orchestrator supporting both `client_secret` and `private_key_jwt` methods
- **Token Exchange** (`ExMCP.Authorization.TokenExchange`) -- RFC 8693 token exchange for swapping ID tokens for ID-JAG tokens
- **JWT Bearer Grant** (`ExMCP.Authorization.JWTBearerAssertion`) -- RFC 7523 Section 2.1 JWT bearer authorization grants
- **ID-JAG Creation and Validation** (`ExMCP.Authorization.IdJag`) -- Create and validate ID-JAG JWTs with `typ="oauth-id-jag+jwt"`
- **ID-JAG Server Handler** (`ExMCP.Authorization.IdJagHandler`) -- Server-side processing of JWT bearer grants containing ID-JAG tokens
- **Enterprise Flow** (`ExMCP.Authorization.EnterpriseFlow`) -- Client-side enterprise SSO orchestrator (OIDC -> token exchange -> JWT bearer grant)
- Extended `OAuthFlow` with `client_credentials_jwt_flow/1` for private_key_jwt auth
- Extended `HTTPClient` metadata parsing with `token_endpoint_auth_methods_supported`, `token_endpoint_auth_signing_alg_values_supported`, `issuer`, `jwks_uri`, and `issued_token_type`
- Extended `Validator` with JWT bearer and token exchange grant type validation
- Extended `AuthorizationServerMetadata` with auth method metadata fields
- Extended `TokenManager` with `auth_method` awareness (`:client_secret`, `:private_key_jwt`, `:enterprise_idjag`)
- Added `{:jose, "~> 1.11"}` dependency for JWT operations
- **Pluggable Service Registry** (`ExMCP.ServiceRegistry`) -- Registry abstraction with `Local` (built-in `Registry`, zero deps) and `Horde` adapters for `ExMCP.Native`
- `ExMCP.ServiceRegistry.Local` -- Default adapter using Elixir's built-in `Registry` for single-node service discovery
- `ExMCP.ServiceRegistry.Horde` -- Distributed adapter wrapping `Horde.Registry` for cross-node clusters (opt-in)

### Changed
- `Horde` is now fully optional -- default service registry uses Elixir's built-in `Registry` with zero extra dependencies
- `ExMCP.Native` uses pluggable registry via `ExMCP.ServiceRegistry.adapter()` instead of hardcoded `Horde.Registry`
- Application supervision tree starts the configured registry adapter's child specs instead of hardcoded Horde processes

### Fixed
- All examples updated to use correct DSL syntax (`meta do` + `input_schema`) -- previously used invalid syntax that would fail to compile
- Removed unnecessary Horde references from examples and getting-started guides
- Updated all documentation to present DSL (`use ExMCP.Server`) as the primary server API
- User Guide rewritten to lead with DSL examples; low-level Handler API preserved as one reference section

## [0.7.2] - 2026-02-12

### Fixed
- Aligned Tools DSL `handle_call_tool` with Handler behaviour arity
- Resolved CI failures in compliance test and dialyzer
- Made `ConsentCache.clear/0` synchronous to fix test isolation race condition
- Eliminated Elixir 1.19 type warnings in handler bridge
- Fixed `@before_compile` ordering for GenServer bridge in Elixir 1.19
- Injected GenServer bridge via `__using__` macro for HttpPlug compatibility
- Suppressed dialyzer pattern_match warnings in generated GenServer bridge at the source
- Fixed `@behaviour` vs `use` in handler20250618 compliance test (Elixir 1.17 compat)

## [0.7.0] - 2026-02-11

### Added
- **MCP Protocol Version 2025-11-25 Support** - Latest protocol version with full spec compliance
- **Streamable HTTP Spec Compliance** - Client and server now fully comply with MCP Streamable HTTP spec:
  - Server provides session ID (not client); first POST omits `Mcp-Session-Id` header
  - `Accept: application/json, text/event-stream` header sent on requests
  - SSE GET handled on same endpoint as POST (not `/sse`)
  - `mcp-protocol-version` header included in all responses
  - POST responses return 200 with JSON body even when SSE is enabled
- **TypeScript MCP SDK Interop Tests** - Verified interoperability with the official TypeScript MCP SDK
- **Agent Simulation Integration Tests** - Integration tests with MockLLM for testing agent workflows
- **`mix mcp.sync_spec` Task** - Automated task for syncing MCP protocol specifications
- **Conformance Test Suites** - Automated conformance tests for all 4 protocol versions (2024-11-05, 2025-03-26, 2025-06-18, 2025-11-25)
- **Client State Machine Adapter** - Refactored client using GenStateMachine with:
  - Formal state transitions with guards
  - State-specific data structures
  - Comprehensive telemetry events for observability
  - Enhanced reconnection logic with exponential backoff
  - Integration with `ExMCP.ProgressTracker`
- Structured error types with `ExMCP.Error` module
- Comprehensive telemetry instrumentation:
  - `[:ex_mcp, :request, :start/stop]` events
  - `[:ex_mcp, :tool, :start/stop]` events
  - `[:ex_mcp, :resource, :read, :start/stop]` events
  - `[:ex_mcp, :prompt, :get, :start/stop]` events
- Bidirectional communication for MCP server-to-client requests
- Comprehensive protocol version validation

### Changed
- **BREAKING:** Refactored internal architecture of `ExMCP.Server` module
  - Split monolithic 1,488-line module into focused components:
    - `ExMCP.Protocol.ResponseBuilder` - Response formatting
    - `ExMCP.Protocol.RequestTracker` - Request lifecycle management
    - `ExMCP.Protocol.RequestProcessor` - Request routing and handling
    - `ExMCP.Server.Transport.Coordinator` - Transport management
    - `ExMCP.DSL.CodeGenerator` - DSL macro code generation
  - Public API remains unchanged - 100% backward compatible
- Replaced deprecated `preferred_cli_env` with `cli/0` callback
- Reduced cyclomatic complexity in `TestTransport` and `Reliability.Supervisor`

### Fixed
- DSL type narrowing warnings for unreachable clauses (closes #3)
- Test isolation issues in session and property tests
- Conformance test alignment with MCP spec
- ETF deserialization security in BEAM transport
- Security guard robustness against malformed consent handlers
- Flaky security test race conditions
- Test infrastructure race conditions
- HTTP transport communication reliability
- Replaced unsafe `String.to_atom` usage with safe alternatives (atom exhaustion prevention)
- All compiler warnings in test files resolved

### Removed
- 22 stale planning/internal docs from root directory
- 16 stale docs and 5 stale subdirectories from `docs/`
- Non-existent `USER_GUIDE.md` and `EXTENSIONS.md` from hex package file list

### Security
- Prevented atom exhaustion attacks by using string keys instead of dynamic atom creation
- Enhanced ETF deserialization security in BEAM transport

## [0.6.0] - 2025-06-26

### 🎉 Major Release: Production-Ready ExMCP

This release represents the completion of an 18-week comprehensive test remediation and enhancement project that transformed ExMCP from alpha software into a production-ready MCP implementation. **100% MCP protocol compliance achieved** across all supported protocol versions.

### 🏆 18-Week Project Achievements

**📊 Quantitative Results:**
- **100% MCP Compliance**: All 270/270 compliance tests passing across 3 protocol versions
- **Complete Protocol Support**: 2024-11-05, 2025-03-26, and 2025-06-18 MCP specifications
- **High Performance**: <10ms average latency, >100 ops/sec throughput, ~15μs native BEAM calls
- **Comprehensive Testing**: 8 test suites with 95%+ coverage and organized tagging strategy
- **Security Implementation**: OAuth 2.1, TLS/SSL, comprehensive audit logging
- **Documentation**: 80+ documentation files with complete guides and examples

**🛡️ Enterprise-Grade Reliability:**
- Circuit breaker pattern for automatic failure detection and recovery
- Configurable retry policies with exponential backoff
- Health monitoring and connection recovery
- Performance baselines with regression detection
- Comprehensive security audit with OAuth 2.1 compliance

**⚡ Performance & Scalability:**
- Native BEAM service dispatcher with zero serialization overhead
- Cross-node distributed service discovery via Horde.Registry
- Performance profiling infrastructure with baseline establishment
- Concurrent load testing and throughput optimization
- Memory efficiency optimization for production workloads

### Added
- **Comprehensive Python MCP SDK Interoperability Examples**
  - Complete bidirectional integration between ExMCP (Elixir) and Python MCP SDK
  - **Elixir → Python Integration:**
    - `elixir_to_python_stdio.ex` - Elixir clients connecting to Python subprocess servers
    - `elixir_to_python_http.ex` - Elixir clients connecting to Python HTTP servers with load balancing and failover
  - **Python → Elixir Integration:**
    - `python_clients/elixir_client.py` - Python clients connecting to Elixir servers via stdio
    - `elixir_servers_for_python.ex` - Elixir servers with rich schemas designed for Python clients
  - **Python MCP Server Examples:**
    - `python_mcp_servers/calculator_server.py` - Full stdio MCP server with history and statistics
    - `python_mcp_servers/http_server.py` - FastAPI-based HTTP MCP server with REST endpoints
  - **Hybrid Architecture Example:**
    - `hybrid_architecture.ex` - Production-ready architecture combining Native Elixir (~15μs), Python stdio (~1-5ms), and Python HTTP (~5-20ms) services
    - ServiceRegistry for managing multi-language service types
    - HybridOrchestrator with intelligent routing and automatic failover
    - Performance-based service selection and load balancing
  - **Complete Documentation:**
    - Comprehensive setup instructions and prerequisites
    - Performance comparisons across transport types
    - Cross-language JSON-RPC compatibility examples
    - Production deployment patterns and best practices
- **Native Service Dispatcher Migration**
  - Migrated 30+ example files from non-existent `:beam` transport to Native Service Dispatcher pattern
  - Updated examples to use `ExMCP.Service` macro for automatic service registration
  - Enhanced `ExMCP.Native` calls with zero serialization overhead for ultra-high performance
  - Fixed references to internal modules now in `ExMCP.Internal.*` namespace
  - Updated all BEAM transport examples to use Horde.Registry for service discovery
- **Comprehensive Test Tagging Strategy**
  - Implemented test tagging system based on ex_llm approach for efficient test execution
  - Created `mix test.suite` task with predefined test suites: unit, compliance, integration, transport, security, performance, all, ci
  - Created `mix test.tags` task to list all available tags and descriptions
  - Added 100+ test files with appropriate module tags for categorization
  - Default exclusions for fast development: integration, external, slow, performance tests excluded by default
  - Test categories: `:unit`, `:integration`, `:compliance`, `:security`, `:performance`, `:transport`, feature-specific tags
  - Transport-specific tags: `:beam`, `:http`, `:stdio` with requirement tags `:requires_beam`, `:requires_http`, `:requires_stdio`
  - Feature tags: `:progress`, `:roots`, `:resources`, `:prompts`, `:protocol`, `:cancellation`, `:batch`, `:logging`
  - Development tags: `:slow`, `:wip`, `:skip`, `:manual_only` for test lifecycle management
  - Reduced default test run time from ~30s to ~5s while maintaining full test coverage
  - Updated test tags from `:sse` to `:http` to align with MCP "Streamable HTTP transport" naming convention
  - Removed unused `ExMCP.Test.MockSSEServer` module and cleaned up references
- **Enhanced Compliance Test Organization**
  - Extracted MCP protocol compliance tests from implementation-specific test files
  - Created 7 new compliance test files by extracting tests from non-compliance files:
    - `cancellation_compliance_test.exs` - Cancellation protocol validation
    - `version_negotiation_compliance_test.exs` - Version negotiation compliance  
    - `roots_compliance_test.exs` - Roots functionality protocol compliance
    - `security_compliance_test.exs` - MCP security requirements
  - All 241 compliance tests now centralized in `test/ex_mcp/compliance/` directory
  - Updated compliance test statistics: 218 passing, 0 failing, 23 skipped
  - Created comprehensive documentation: `TAGGING_STRATEGY.md`, `TAGGING_IMPLEMENTATION.md`, `EXTRACTION_LOG.md`
- **Configurable SSE Endpoint**
  - HTTP transport now supports custom endpoint configuration via `:endpoint` option
  - Defaults to "/mcp/v1" for backward compatibility
  - Handles trailing slashes and empty endpoints properly
  - Example: `ExMCP.Client.start_link(transport: :http, url: "http://localhost", endpoint: "/custom/api")`
- **Progress Token and _meta Field Support**
  - Added `_meta` field support to all MCP request methods in Protocol module
  - Extended Client API to accept `:meta` option for all methods
  - Progress tokens can now be passed via `meta: %{"progressToken" => token}`
  - Backward compatibility maintained for `:progress_token` option in `call_tool/4`
  - All protocol methods now support arbitrary metadata passthrough
  - Server handlers receive _meta in tool arguments (for tools/call) or params (for other methods)
- **OAuth 2.1 Authorization Framework** (MCP 2025-03-26 specification)
  - Full OAuth 2.1 implementation with:
    - Authorization Code Flow with mandatory PKCE (RFC 7636)
    - Client Credentials Flow for service-to-service authentication
    - Authorization Server Metadata Discovery (RFC 8414)
    - Dynamic Client Registration (RFC 7591)
    - Protected Resource Metadata Discovery (RFC 9728 draft)
  - Token Management:
    - Automatic token refresh with configurable window
    - Token rotation for public clients
    - Token validation and introspection support
    - Secure token storage in GenServer state
  - Security Features:
    - PKCE S256 code challenge method required for all authorization code flows
    - HTTPS enforcement for all OAuth endpoints (except localhost)
    - No tokens in URLs - all tokens in headers
    - Bearer token authentication for HTTP transports
  - Integration:
    - `ExMCP.Authorization` module for OAuth flows
    - `ExMCP.Authorization.TokenManager` for automatic token lifecycle
    - `ExMCP.Authorization.PKCE` for code challenge generation/verification
    - Transport-level OAuth support for HTTP streaming and WebSocket
  - Comprehensive test coverage: 217+ passing OAuth tests

- **Production-Grade Reliability Framework**
  - **Circuit Breaker Integration**: Automatic failure detection and recovery across all transports
  - **Enhanced Retry Policies**: Configurable retry strategies with exponential backoff for all MCP operations
  - **Health Monitoring**: Real-time transport and connection health tracking
  - **Connection Recovery**: Automatic reconnection with intelligent backoff strategies
  - **Error Recovery**: Comprehensive error handling and graceful degradation
  - **Reliability Testing**: 100+ integration tests for circuit breakers, retry policies, and health monitoring

- **Performance Infrastructure & Benchmarking**
  - **Performance Profiling Utility**: Comprehensive metrics collection including execution time, memory usage, GC statistics
  - **Baseline Establishment**: Performance baselines stored for regression detection across all operations
  - **Benchmark Test Suites**: 7 comprehensive test suites covering basic operations, payload scaling, concurrent load, throughput
  - **Performance Regression Detection**: Automated comparison against established baselines
  - **Memory Efficiency Tracking**: Detailed memory delta monitoring and optimization
  - **Throughput Optimization**: Benchmarked >100 ops/sec for basic operations with concurrent client support

- **Comprehensive Testing Framework**
  - **8 Organized Test Suites**: Unit, compliance, integration, transport, security, performance, CI, and comprehensive suites
  - **Advanced Test Tagging**: Efficient test execution with 15+ tags for categorization and selection
  - **Cross-Transport Testing**: Comprehensive compatibility tests across stdio, HTTP, SSE, and Native BEAM transports
  - **Integration Test Framework**: End-to-end scenario validation with real component testing
  - **Process Cleanup Automation**: Automated test environment cleanup for reliable test execution
  - **CI/CD Integration**: Complete automated testing pipeline with quality gates

- **Cancellation Protocol Implementation** (MCP specification compliance)
  - Full support for `notifications/cancelled` messages
  - Client-side cancellation API: `ExMCP.Client.send_cancelled/3`
  - Request tracking: `ExMCP.Client.get_pending_requests/1` 
  - Automatic cleanup of cancelled in-progress requests
  - Validation that initialize request cannot be cancelled per spec
  - Proper handling of race conditions and late cancellations
  - Comprehensive test coverage with 12 passing tests

- **Logging Control Implementation** (MCP specification compliance)
  - `logging/setLevel` request handler with RFC 5424 syslog levels
  - Full integration with Elixir's Logger system
  - `ExMCP.Logging` module for centralized logging management
  - Automatic log level conversion between MCP and Elixir formats
  - Structured logging via `notifications/message`
  - Security features:
    - Automatic sanitization of sensitive data (passwords, tokens, keys)
    - Rate limiting support
    - Configurable logger names
  - Comprehensive test coverage with 33 passing tests

- **MCP Specification Compliance Updates**
  - Initialize request batch validation - prevents `initialize` from being part of JSON-RPC batch per spec
  - Audio content type support with `ExMCP.Content` module and examples
  - Completions capability declaration with `hasArguments` and `values` fields
  - Enhanced HTTP transport flexibility:
    - Session management with `Mcp-Session-Id` header
    - Non-streaming mode for single JSON responses
    - Configurable endpoint (defaults to `/mcp/v1`)
    - Resumability support with Last-Event-ID
  - Security requirements enforcement:
    - Origin validation for DNS rebinding protection
    - HTTPS enforcement for non-localhost deployments
    - Localhost binding security checks
    - Enhanced `SecureServer` module with all security features

### Fixed

#### 🔧 18-Week Remediation Project Fixes

**Phase 1: Critical Infrastructure (Weeks 1-4)**
- **Transport Configuration**: Fixed transport selection and configuration inconsistencies
- **Response Migration**: Resolved ExMCP.Response struct vs map access patterns throughout codebase  
- **Error Protocol**: Standardized isError/is_error field handling across all protocol versions
- **Connection State**: Fixed connection status tracking and state machine transitions

**Phase 2: Protocol Compliance (Weeks 5-8)**
- **Protocol Methods**: Implemented missing MCP protocol methods for 100% coverage
- **Message Field Normalization**: Fixed camelCase/snake_case field access inconsistencies
- **Pagination Standardization**: Resolved cursor handling and nextCursor field presence issues
- **Resource Operations**: Fixed resource read protocol compliance and resource/prompt operations

**Phase 3: Reliability & Performance (Weeks 9-12)**
- **Transport Behaviors**: Standardized transport implementations and error handling patterns
- **Connection Validation**: Implemented consistent connection validation across all transports
- **Message Format**: Standardized message format handling and protocol encoding/decoding

**Phase 4: Advanced Features & Testing (Weeks 13-16)**
- **Cross-Transport Compatibility**: Fixed DSL server integration and response format issues
- **Performance Profiling**: Resolved JSON serialization issues in performance metrics storage
- **HTTP Transport Communication**: Identified and documented HTTP/SSE client-server communication issues
- **Integration Framework**: Fixed test infrastructure and end-to-end scenario validation

**Phase 5: Documentation & Validation (Weeks 17-18)**
- **Documentation Completeness**: Updated 80+ documentation files for consistency and accuracy
- **Security Validation**: Confirmed OAuth 2.1 compliance and security audit requirements
- **Final Validation**: Verified 100% MCP compliance maintained across all protocol versions

#### Other Fixes
- All Credo code quality issues resolved (0 issues)
- Logger metadata warnings fixed with proper configuration
- Dialyzer type checking issues resolved across all modules
- Memory leaks and process cleanup issues in test environment
- Performance regression detection false positives
- Security audit logging configuration for production environments

## [0.5.0] - 2025-05-28

### Breaking Changes
- **Removed `:sse` transport identifier** - Use `:http` instead for Streamable HTTP transport
- **Renamed SSE references** - All documentation and APIs now use "HTTP streaming" or "Streamable HTTP" terminology

### Fixed
- **Logging Notification Method Name** - Changed from `notifications/log` to `notifications/message` to match MCP specification exactly

### Added

#### Current MCP Specification (2025-03-26) Features
- **OAuth 2.1 Authorization Support**
  - Full OAuth 2.1 implementation with PKCE support
  - Automatic token refresh before expiration
  - TokenManager GenServer for token lifecycle management
  - Authorization error handling for 401/403 responses
  - Request interceptor for automatic header injection
  - Integration with HTTP transport for seamless auth
  - Example demonstrating OAuth-protected MCP servers
- **Enhanced Streamable HTTP Transport**
  - Automatic reconnection with exponential backoff
  - Built-in keep-alive mechanism (30-second heartbeat)
  - Support for Last-Event-ID header for event resumption with HTTP streaming
  - Improved connection stability and error recovery

#### Draft MCP Specification Features (Experimental)
- **Structured Tool Output** (Draft feature - not in MCP 2025-03-26)
  - Tools can define `outputSchema` in their schema
  - Tool results can include `structuredContent` alongside regular content
  - Marked with "Draft feature" comments in code
- **Logging Level Control** (Draft feature - not in MCP 2025-03-26)
  - Added `logging/setLevel` handler implementation
  - `Client.set_log_level/3` for adjusting server log verbosity
  - `handle_set_log_level/2` callback in server handlers
- **Security Best Practices Implementation** (Draft specification)
  - Token validation with audience checking (prevents confused deputy)
  - Client registration and accountability system
  - Consent management for dynamic client registration
  - Request audit trail maintenance
  - Trust boundary enforcement
  - SecureServer module with built-in security features
  - Security supervisor for managing security components

#### Other Enhancements
- **Lifecycle Management Improvements**
  - Improved BEAM transport server lifecycle (supports reconnections)
  - Dynamic client capability building based on handler
  - Protocol version validation and negotiation
- **Client Roots Tests and Examples** (MCP specification compliance)
  - Comprehensive tests for client roots functionality
  - Root demo showing client-server root exchange
  - Server tools for requesting and analyzing client roots
  - Default handler providing current directory as root
  - Protocol compliance verification for roots/list requests
  - Note: Client roots functionality already fully implemented
- **Progress Notifications Tests and Examples** (MCP specification compliance)
  - Integration tests for progress tracking in long-running operations
  - Progress demo server showing various progress patterns
  - Support for string and integer progress tokens
  - Progress updates with current/total values
  - Note: Progress notifications already fully implemented
- **Server Utilities Tests** (MCP specification compliance)
  - Comprehensive test coverage for pagination across all list operations
  - Completion utility tests for prompt and resource references
  - Logging utility verification with all severity levels
  - Cursor-based pagination with proper error handling
  - Note: All utilities (completion, logging, pagination) already fully implemented
- **Tools Feature Tests** (MCP specification compliance)
  - Comprehensive test coverage for existing tools functionality
  - Tests for tool discovery, invocation, and error handling
  - Verification of isError flag support for tool execution errors
  - Batch tool request testing
  - Multiple content type support (text, image)
  - Progress token support verification
  - Note: Tools functionality including isError support already fully implemented
- **Resources Feature Tests and Examples** (MCP specification compliance)
  - Comprehensive test coverage for existing resources functionality
  - Example server demonstrating various resource types (text, JSON, binary)
  - Support for resource subscriptions and update notifications
  - Resource templates for dynamic URI patterns
  - Pagination support for resource listing
  - Multiple URI schemes (file://, config://, data://, db://)
  - Note: Resources functionality was already fully implemented
- **Prompts Feature Tests and Examples** (MCP specification compliance)
  - Comprehensive test coverage for existing prompts functionality
  - Example server demonstrating various prompt patterns
  - Support for parameterized prompts with required/optional arguments
  - Pagination support for prompt listing
  - Dynamic prompt list changes with notifications
  - Note: Prompts functionality was already fully implemented
- **Ping Utility Tests and Examples** (MCP specification compliance)
  - Comprehensive test coverage for existing ping functionality
  - Health check pattern examples and best practices
  - Bidirectional ping demonstration
  - Connection monitoring and verification patterns
  - Performance measurement examples
  - Note: Ping functionality was already fully implemented
- **Request Cancellation Support** (MCP specification compliance)
  - Complete implementation of `notifications/cancelled` method
  - Client and server can cancel in-progress requests
  - Automatic request tracking and resource cleanup
  - Initialize request cannot be cancelled (as per spec)
  - Graceful handling of unknown/completed requests
  - Malformed cancellation notification validation
  - Comprehensive test coverage and example implementation
- **OAuth 2.1 Authorization Support** (MCP specification compliance)
  - Complete OAuth 2.1 implementation with PKCE support
  - Authorization code flow with mandatory PKCE for security
  - Client credentials flow for application-to-application communication
  - Server metadata discovery (RFC 8414)
  - Dynamic client registration (RFC 7591)
  - Token validation and introspection
  - HTTPS enforcement with localhost development support
  - Comprehensive test coverage for all authorization flows
- Enhanced protocol version negotiation (MCP specification compliance)
  - Handlers now receive client's protocol version in params
  - Servers can check client version and propose alternatives
  - Comprehensive documentation and examples for version negotiation
  - Full test coverage for various negotiation scenarios

### Changed
- **BREAKING:** Renamed SSE transport to HTTP transport (MCP specification update)
  - `ExMCP.Transport.SSE` is now `ExMCP.Transport.HTTP`
  - Use `transport: :http` instead of `transport: :sse`
  - Transport identifier `:sse` is now `:http` (`:sse` still works for compatibility)
  - Updated documentation to reflect "Streamable HTTP" terminology from MCP spec 2025-03-26
  - All tests and examples updated to use new naming

## [0.4.0] - 2025-05-27

### Added
- Tool execution error reporting with `isError` flag (MCP specification compliance)
  - Handlers can return `{:ok, %{content: [...], isError: true}, state}` for tool errors
  - Distinguishes between protocol errors and tool execution errors
  - Full test coverage demonstrating proper error handling
- Pagination support for list methods (MCP specification compliance)
  - Added cursor parameter to `list_tools`, `list_resources`, and `list_prompts`
  - Server handlers now return optional `nextCursor` for paginated results
  - Client API changed to accept options keyword list for cursor and timeout
  - Full test coverage for pagination functionality
- JSON-RPC batch request support (MCP specification compliance)
  - `batch_request/3` client method for sending multiple requests as a batch
  - Server automatically handles batch requests and returns batch responses
  - Full integration tests demonstrating batch functionality
- Bi-directional communication support (server-to-client requests)
  - New `ExMCP.Client.Handler` behaviour for handling server requests
  - Server can ping clients with `ping/2`
  - Server can request client roots with `list_roots/2`
  - Server can request client to sample LLM with `create_message/3`
  - Client automatically advertises capabilities when handler is provided
- Human-in-the-loop (HITL) interaction support
  - `ExMCP.Approval` behaviour for implementing approval flows
  - `ExMCP.Client.DefaultHandler` with built-in approval support
  - `ExMCP.Approval.Console` for terminal-based approval prompts
  - Approval required for LLM sampling requests and responses
  - Support for approving, denying, or modifying requests/responses
  - Full test coverage with approval and HITL integration tests
- WebSocket transport implementation (client-side only)
  - Support for ws:// and wss:// protocols
  - Automatic ping/pong frame handling
  - Full integration with ExMCP transport system
  - TLS/SSL support for secure connections
- Comprehensive security features across all transports
  - New `ExMCP.Security` module for unified security configuration
  - Authentication support: Bearer tokens, API keys, Basic auth, custom headers, node cookies
  - HTTP streaming transport: Origin validation, CORS headers, security headers
  - WebSocket transport: Authentication headers, TLS configuration
  - BEAM transport: Process-level authentication, node cookie support
  - TLS/SSL configuration with certificate validation
  - Mutual TLS support for HTTP streaming and WebSocket transports
  - Comprehensive security documentation in docs/SECURITY.md
- Native format support for BEAM transport
  - `:json` format (default) maintains MCP compatibility
  - `:native` format for direct Elixir term passing between processes
  - Configurable via `:format` option in connect/accept
- HTTP test server for streaming testing
  - Implemented with Plug and Cowboy
  - Supports Server-Sent Events connections and message endpoints
  - Request tracking for test assertions
  - Proper Server-Sent Events streaming with keep-alive

### Changed
- **BREAKING:** Client list methods now take options keyword list instead of timeout
  - `list_tools(client, timeout)` → `list_tools(client, opts \\ [])`
  - `list_resources(client, timeout)` → `list_resources(client, opts \\ [])`
  - `list_prompts(client, timeout)` → `list_prompts(client, opts \\ [])`
  - Options include `:timeout` and `:cursor` for pagination
- **BREAKING:** Server handler callbacks for list methods now include cursor parameter
  - `handle_list_tools(state)` → `handle_list_tools(cursor, state)`
  - `handle_list_resources(state)` → `handle_list_resources(cursor, state)`
  - `handle_list_prompts(state)` → `handle_list_prompts(cursor, state)`
  - All must return 4-tuple with optional `next_cursor`
- SSE transport endpoint is now configurable (was hardcoded to /mcp/v1)
- BEAM transport now supports both JSON and native Elixir term formats

### Fixed
- Dialyzer type errors in WebSocket and BEAM transports
- BEAM transport connection format to support security authentication
- Test compatibility issues with new security features

### Documentation
- Added comprehensive security guide (docs/SECURITY.md)
- Added local copy of MCP specification (docs/mcp-llms-full.txt)
- Updated TASKS.md with detailed compliance status

## [0.3.0] - 2025-05-26

### Added
- Protocol version updated to "2025-03-26" (latest MCP specification)
- Roots capability for URI-based resource boundaries
  - `list_roots/2` client method
  - `handle_list_roots/1` server callback
  - `notify_roots_changed/1` for dynamic root updates
- Resource subscription support
  - `subscribe_resource/3` and `unsubscribe_resource/3` client methods
  - `handle_subscribe_resource/2` and `handle_unsubscribe_resource/2` server callbacks
- Resource templates support
  - `list_resource_templates/2` client method  
  - `handle_list_resource_templates/1` server callback
- Enhanced protocol method support
  - `ping/2` for connection health checks
  - `complete/4` for completion/autocomplete features
  - `send_cancelled/3` for request cancellation notifications
  - `log_message/4` for structured logging
- Tool annotations (readOnlyHint, destructiveHint, idempotentHint, openWorldHint)
- Audio content support (`audio_content` type)
- Embedded resource support in content
- Pagination support with cursor/nextCursor
- RFC-5424 compliant logging levels
- Progress tokens now use `_progressToken` in `_meta` as per spec
- Comprehensive USER_GUIDE.md documentation
- API_REFERENCE.md with complete module documentation
- Enhanced examples for all new features

### Changed  
- **BREAKING**: JSON field names now use camelCase to match official MCP schema
  - `mime_type` → `mimeType`
  - `progress_token` → `_progressToken` (in `_meta`)
  - All response fields follow camelCase convention
- **BREAKING**: ModelHint is now an object with optional `name` field (was array)
- Type specifications completely rewritten to match official schema
- Updated all documentation to reflect protocol version 2025-03-26
- Enhanced capabilities type to include roots and other new features

### Fixed
- Protocol compliance with official MCP schema
- Missing cancellation and logging notification handlers
- Type definitions for multimodal content
- Progress token parameter location (now in `_meta._progressToken`)

## [0.2.0] - 2025-05-26

### Added
- Sampling/createMessage support for LLM integrations
- Change notifications (resources, tools, prompts)
- Progress notifications with token support
- Comprehensive BEAM transport examples
- Code quality tooling (Credo, Dialyzer, Sobelow, ExCoveralls)
- Git hooks for pre-commit and pre-push checks
- GitHub Actions CI/CD pipeline

### Changed
- **BREAKING**: Simplified BEAM transport architecture to Native BEAM transport
  - Removed complex TCP-based BEAM transport modules (`ExMCP.Transport.Beam.Server`, `Client`, etc.)
  - Implemented `ExMCP.Transport.Native` for direct process communication
  - Added Registry-based service discovery and registration
  - Improved performance: ~15μs local calls vs previous TCP overhead
  - Note: Requires migration from old TCP-based API to new service registration pattern

### Fixed
- BEAM transport now properly supports server-initiated notifications
- Documentation discrepancies between claimed and actual features
- Server handler callback specs for sampling support

## [0.1.0] - 2025-05-25

### Added
- Initial release of ExMCP
- Complete Model Context Protocol implementation
- Protocol encoder/decoder for JSON-RPC messages
- Client implementation with automatic reconnection
- Server implementation with handler behaviour
- stdio transport for process communication
- SSE (Server-Sent Events) transport for HTTP streaming
- BEAM transport for native Erlang/Elixir communication
- Tool discovery and execution
- Resource listing and reading
- Prompt management
- Server manager for multiple connections
- Server discovery (npm packages, local directories)
- Request/response correlation
- Concurrent request handling
- Error handling and validation
- Type specifications throughout

### Features
- Full MCP specification compliance
- Multiple transport layer support (stdio, Streamable HTTP with optional SSE, BEAM)
- Both client and server implementations
- Extensible architecture
- Supervision tree integration
