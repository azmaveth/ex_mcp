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

# Using Makefile shortcuts
make setup                       # Initial setup with git hooks
make quality                     # Run format check, credo, and compile checks
make all                         # Run all quality checks, tests, and dialyzer
make coverage                    # Generate HTML coverage report
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

4. **Application Layer** (`lib/ex_mcp/application.ex`)
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
1. Check TASKS.md for current development status
2. Follow existing patterns in similar modules
3. Add comprehensive tests before implementation
4. Run `mix format` and `mix credo` before committing
5. Update type specs in `lib/ex_mcp/types.ex` if adding new message types