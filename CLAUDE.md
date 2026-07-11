# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ExMCP is an Elixir implementation of the Model Context Protocol (MCP), enabling AI models to communicate with external tools and resources through a standardized protocol.

## Version Management

### When to Bump Versions
- **Patch version (0.x.Y)**: Bug fixes, documentation updates, minor improvements
- **Minor version (0.X.0)**: New features, non-breaking API changes
- **Major version (X.0.0)**: Breaking API changes (after 1.0.0 release)

### Version Update Checklist
1. Update version in `mix.exs`
2. Update CHANGELOG.md with:
   - Version number and date
   - Added/Changed/Fixed/Removed sections
   - **BREAKING:** prefix for any breaking changes
3. Commit with message: `chore: bump version to X.Y.Z`

### CHANGELOG Format
```markdown
## [X.Y.Z] - YYYY-MM-DD

### Added
- New features

### Changed
- Changes in existing functionality
- **BREAKING:** API changes that break compatibility

### Fixed
- Bug fixes

### Removed
- Removed features
- **BREAKING:** Removed APIs
```

## Development Commands

```bash
# Essential commands
mix deps.get          # Install dependencies
mix test              # Run all tests
mix test test/ex_mcp/protocol_test.exs  # Run specific test file
mix format            # Format code (required before committing)
mix credo             # Static code analysis
mix dialyzer          # Type checking (run after significant changes)
mix docs              # Generate documentation
iex -S mix            # Start interactive shell with project loaded

# Development workflow
mix compile --warnings-as-errors  # Compile with strict warnings
MIX_ENV=test mix compile         # Compile for test environment
mix sobelow --skip               # Security analysis
mix coveralls.html               # Generate coverage report

```

## Architecture

The library follows a layered architecture:

1. **Transport Layer** (`lib/ex_mcp/transport/`)
   - Defines behaviour for different communication protocols
   - Implementations: stdio, SSE, BEAM (Erlang processes)
   - Each transport handles message framing and delivery

2. **Protocol Layer** (`lib/ex_mcp/protocol.ex`)
   - JSON-RPC 2.0 message encoding/decoding
   - Request/response correlation
   - Error handling

3. **Client/Server Layer**
   - `ExMCP.Client`: Manages connections, auto-reconnection, request routing
   - `ExMCP.Server`: Request handling, capability negotiation
   - `ExMCP.Server.Handler`: Behaviour for implementing server handlers

4. **ACP Layer** (`lib/ex_mcp/acp/`)
   - Agent Client Protocol for controlling coding agents
   - `ExMCP.ACP.Client`: GenServer managing agent connections over stdio
   - `ExMCP.ACP.Adapter`: Behaviour for adapting non-native agents (Claude Code, Codex)
   - `ExMCP.ACP.AdapterBridge`: Bridge between ACP and agent-native protocols

5. **Application Layer** (`lib/ex_mcp/application.ex`)
   - OTP application supervision tree
   - Server discovery and management

## Key Patterns

- All public APIs use `{:ok, result}` or `{:error, reason}` tuples
- Transport implementations must handle the `ExMCP.Transport` behaviour
- Server handlers implement the `ExMCP.Server.Handler` behaviour
- Use `ExMCP.Types` for type definitions and specs
- Protocol messages follow MCP specification exactly

## Testing Approach

- Unit tests with Mox for transport mocking
- Property-based testing for protocol encoding/decoding
- Integration tests for client-server communication
- Test files mirror source structure in `test/`

## Common Tasks

When implementing new features:
1. Follow existing patterns in similar modules
2. Add comprehensive tests before implementation
3. Run `mix format` and `mix credo` before committing
4. Update type specs in `lib/ex_mcp/types.ex` if adding new message types
5. Prefer `ExMCP.Server.DSL` over the deprecated `ExMCP.Server.Tools` API

## Client implementation

The public MCP client API is **`ExMCP.Client`** (GenServer). There is no
`client_adapter` / `LegacyAdapter` / `StateMachineAdapter` switch anymore —
those modules were removed before 1.0.

Internal connection lifecycle helpers live under `ExMCP.Client.*` (for example
`ExMCP.Client.StateMachine`, transitions, request handler). Prefer `ExMCP.Client`
and the top-level `ExMCP.start_client/1` helpers in application code.

### Telemetry (client)

The client stack emits telemetry such as:

```elixir
# State transitions (where applicable)
[:ex_mcp, :client, :state_transition]

# Request lifecycle
[:ex_mcp, :client, :request, :start]
[:ex_mcp, :client, :request, :success]
[:ex_mcp, :client, :request, :error]

# Connection / transport
[:ex_mcp, :client, :connection, :success]
[:ex_mcp, :client, :transport, :error]
[:ex_mcp, :client, :transport, :closed]

# Handshake
[:ex_mcp, :client, :handshake, :start]
[:ex_mcp, :client, :handshake, :success]
[:ex_mcp, :client, :handshake, :error]

# Reconnection
[:ex_mcp, :client, :reconnect, :attempt]
[:ex_mcp, :client, :reconnect, :success]
[:ex_mcp, :client, :reconnect, :error]
[:ex_mcp, :client, :reconnect, :timeout]

# Progress tracking
[:ex_mcp, :client, :progress, :update]
[:ex_mcp, :client, :progress, :unknown_token]
[:ex_mcp, :client, :progress, :rate_limited]
```

### Server DSL

- Prefer `ExMCP.Server.Handler` + `ExMCP.Server.DSL` for tools/resources/prompts.
- `ExMCP.Server.Tools` is **deprecated** and will be removed in **1.1.0**.

## Deprecated / planned removals

| API | Status |
|-----|--------|
| `ExMCP.Server.Tools` (+ `Simplified`, helpers) | Deprecated → **removed in 1.1.0** |
| Client adapter layer (`LegacyAdapter`, etc.) | Already removed; use `ExMCP.Client` |

## Development notes

- Primary public APIs: `ExMCP`, `ExMCP.Client`, `ExMCP.Server` / `Handler` / `DSL`, transports, `ExMCP.HttpPlug`, `ExMCP.ACP.*`, `ExMCP.Authorization`, `ExMCP.Content`, `ExMCP.Types`.
- Other modules under `ExMCP.*` are internal unless documented otherwise.