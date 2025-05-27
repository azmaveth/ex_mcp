# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
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
  - SSE transport: Origin validation, CORS headers, security headers
  - WebSocket transport: Authentication headers, TLS configuration
  - BEAM transport: Process-level authentication, node cookie support
  - TLS/SSL configuration with certificate validation
  - Mutual TLS support for SSE and WebSocket transports
  - Comprehensive security documentation in docs/SECURITY.md
- Native format support for BEAM transport
  - `:json` format (default) maintains MCP compatibility
  - `:native` format for direct Elixir term passing between processes
  - Configurable via `:format` option in connect/accept

### Changed
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
- **BREAKING**: Redesigned BEAM transport to use mailbox process pairs
  - Removed `ExMCP.Transport.Beam.Server` module
  - Transport now supports bidirectional communication like stdio/SSE
  - Improved fault tolerance and connection handling
  - Note: Public API remains unchanged - users of `ExMCP.Client` and `ExMCP.Server` are not affected

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
- Multiple transport layer support (stdio, SSE, BEAM)
- Both client and server implementations
- Extensible architecture
- Supervision tree integration