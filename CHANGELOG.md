# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.5.0] - 2025-05-28

### Breaking Changes
- **Removed `:sse` transport identifier** - Use `:http` instead for Streamable HTTP transport
- **Renamed SSE references** - All documentation and APIs now use "Streamable HTTP" terminology

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
  - Support for Last-Event-ID header for event resumption with SSE
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
  - Transport identifier `:sse` is now `:http` (`:sse` still works for compatibility)
  - Updated documentation to reflect "Streamable HTTP" terminology from MCP spec 2025-03-26
  - All tests and examples updated to use new naming

## [0.4.0] - 2025-01-27

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
- HTTP test server for SSE testing
  - Implemented with Plug and Cowboy
  - Supports SSE connections and message endpoints
  - Request tracking for test assertions
  - Proper SSE event streaming with keep-alive

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