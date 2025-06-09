# ExMCP Tasks

## Recent Accomplishments (Latest Session)

### Test Suite Stabilization
- **Fixed all SSE client tests** (11 tests, 0 failures)
  - Resolved exponential backoff retry delay bug
  - Fixed connection reconnection mechanism
  - Eliminated port collision issues
- **Fixed hot reload tests** (10 tests, 0 failures, 1 skipped)
  - Added missing callbacks to test handlers
  - Fixed GenServer timeout handling
  - Resolved state migration and validation issues
- **Eliminated intermittent test failures**
  - Fixed race condition in async SSE tests
  - Overall: 729 tests, 0 failures (was 37+ failures)

### Security & Transport Enhancements
- **Implemented full SSE security**
  - Origin header validation
  - CORS support with configurable origins
  - Security headers (X-Content-Type-Options, etc.)
- **Added authentication support**
  - Bearer token authentication
  - Custom header support
  - TLS/SSL configuration
- **Enhanced SSE reliability**
  - Keep-alive/heartbeat mechanism
  - Automatic retry with exponential backoff
  - Proper connection cleanup

## Completed

### Core Implementation
- [x] Initial project structure and configuration
- [x] Core protocol implementation with encoding/decoding
- [x] Type definitions for MCP entities
- [x] Transport behaviour definition
- [x] stdio transport implementation
- [x] Client implementation with automatic reconnection
- [x] Server implementation with handler behaviour
- [x] Application supervisor setup
- [x] Protocol tests - all 17 tests passing
- [x] Fix protocol encoding to use string keys for JSON-RPC compatibility
- [x] Fix unused variable warning in stdio transport
- [x] BEAM transport implementation with native process communication
- [x] BEAM transport supports both local and distributed connections
- [x] BEAM transport server discovery and registration
- [x] Comprehensive BEAM transport tests - all 35 tests passing
- [x] SSE (Server-Sent Events) transport implementation
- [x] SSE transport tests

### MCP Specification Compliance (Major Achievement)
- [x] Comprehensive test coverage for all MCP specification versions
  - [x] 2024-11-05 specification tests (spec_2024_11_05_test.exs)
  - [x] 2025-03-26 specification tests (spec_2025_03_26_test.exs) 
  - [x] Draft specification tests (spec_draft_test.exs)
  - [x] Version negotiation comprehensive tests (version_negotiation_comprehensive_test.exs)
- [x] Batch request support (JSONRPCBatchRequest/JSONRPCBatchResponse)
  - [x] Implemented send_batch/3 method in Client with version checking
  - [x] Full integration tests with all protocol versions
  - [x] Proper rejection of batch requests in draft version
- [x] Bi-directional requests (server-to-client)
  - [x] Implement server ping requests to client
  - [x] Implement server createMessage requests to client  
  - [x] Implement server listRoots requests to client
  - [x] Client.Handler behaviour for handling server requests
  - [x] Server can make requests with ping/1, list_roots/1, create_message/2
  - [x] Full test coverage with TestClientHandler
- [x] Human-in-the-loop interaction support
  - [x] Add approval flow for sampling/createMessage
  - [x] Add approval flow before returning sampled messages
  - [x] Created ExMCP.Approval behaviour for approval handlers
  - [x] Created ExMCP.Client.DefaultHandler with approval support
  - [x] Created ExMCP.Approval.Console for terminal-based approvals
  - [x] Full test coverage with approval_test.exs and hitl_integration_test.exs
- [x] WebSocket transport (implemented - client mode only)

### Advanced Transport Features
- [x] Basic BEAM transport implementation completed
- [x] Advanced BEAM transport features:
  - [x] Support for supervised GenServer-based MCP servers (via mailbox processes)
  - [x] Distributed BEAM node connections (supports {:name, :"node@host"} syntax)
  - [x] Process monitoring and automatic reconnection (via Process.monitor and DOWN handling)

## High Priority Tasks (Next Sprint)

### Security & Authentication (Critical for Production)
**Priority: URGENT - Required for safe production use**
- [x] SSE Security Headers
  - [x] Implement Origin header validation to prevent DNS rebinding attacks
  - [x] Add CORS header support with configurable origins
  - [x] Add security headers (X-Content-Type-Options, etc.)
- [x] Transport Security
  - [x] Add TLS/SSL configuration options for all transports
  - [x] Implement certificate validation options
  - [ ] Add mutual TLS support
- [x] OAuth 2.1 Authorization Framework (2025-03-26 feature) ✅
  - [x] Implement client credentials flow
  - [x] Add authorization code flow with PKCE
  - [x] Token refresh mechanism
  - [x] Bearer token authentication for HTTP transport
  - [x] Authorization server metadata discovery (RFC 8414)
  - [x] Dynamic client registration (RFC 7591)
  - [x] Protected resource metadata (RFC 9728 draft)
  - [x] PKCE S256 code challenge method (RFC 7636)
  - [x] Token rotation for public clients
  - [x] Automatic token refresh with configurable window

### Protocol Compliance Gaps (High)
**Priority: HIGH - Required for full MCP spec compliance**
- [x] Cancellation Protocol ✅
  - [x] Implement notifications/cancelled message handling
  - [x] Add request cancellation API in client (`Client.send_cancelled/3`)
  - [x] Handle cancelled requests in server
  - [x] Track pending requests (`Client.get_pending_requests/1`)
  - [x] Validate that initialize request cannot be cancelled
  - [x] Handle race conditions and late cancellations gracefully
- [x] Logging Control ✅
  - [x] Implement logging/setLevel request handler
  - [x] Add configurable logging levels (RFC 5424 syslog levels)
  - [x] Integrate with Elixir Logger properly
  - [x] Automatic log level conversion between MCP and Elixir Logger
  - [x] Structured logging with `notifications/message`
  - [x] Security: automatic sanitization of sensitive data
  - [x] `ExMCP.Logging` module for centralized logging management
- [x] Missing Protocol Methods ✅
  - [x] Add full progress token support across all methods
  - [x] Support _meta field in all request types
  - [x] Updated Protocol module to support _meta in all request methods
  - [x] Updated Client API to accept :meta option for all methods
  - [x] Backward compatibility maintained for :progress_token option in call_tool

### BEAM Transport Performance (Medium)
**Priority: MEDIUM - Performance optimizations**
- [x] Native BEAM clustering support
  - [x] Service discovery and registration across cluster nodes
  - [x] Load balancing strategies (round-robin, least-connections, weighted)
  - [x] Health monitoring and automatic service removal
  - [x] Fault tolerance with circuit breakers
  - [x] Network partition detection and healing
  - [x] Dynamic cluster membership management
  - [x] Comprehensive clustering test suite (9/14 tests passing)
  - [x] Interactive clustering example with real-time demonstration
- [x] Hot code reloading for MCP servers
  - [x] Handler module validation and state migration
  - [x] Multiple watch strategies (manual, module timestamp, file watching)
  - [x] Automatic rollback on reload failures
  - [x] Connection preservation during reloads
  - [x] Comprehensive test suite and interactive example
- [x] Streaming support for large payloads/responses
  - [x] Stream-based message delivery for handling large tool outputs
  - [x] Backpressure handling for flow control
  - [x] Chunked transfer for resources
  - [x] Window-based flow control (similar to TCP sliding window)
  - [x] Resource streaming with automatic chunking
  - [x] Connection-level stream management
- [ ] Performance optimizations for local connections
- [ ] Zero-copy message passing for large payloads

## PRIORITY ANALYSIS & RECOMMENDATIONS

### ✅ MAJOR ACHIEVEMENT: Comprehensive MCP Specification Compliance
**COMPLETED**: ExMCP now has complete test coverage for all MCP specification versions (2024-11-05, 2025-03-26, draft) with comprehensive version negotiation testing. This is a significant milestone that ensures spec compliance and future compatibility.

### ✅ COMPLETED: Security & Authentication (Production Ready)
**COMPLETED**: All critical security features have been implemented:
1. **Origin validation & CORS** - ✅ Prevents DNS rebinding attacks
2. **TLS/SSL configuration** - ✅ Essential for production security  
3. **Bearer token authentication** - ✅ Basic auth support implemented
4. **Security headers** - ✅ All recommended headers implemented

### 📈 HIGH VALUE: Protocol Compliance Gaps
**RECOMMENDATION**: Complete these core protocol features for full MCP specification compliance:
1. **Cancellation protocol** - Important for user experience
2. **Logging control** - Required for production debugging
3. **OAuth 2.1 full implementation** - Complete authorization flows
4. **Enhanced progress/meta support** - Improves protocol robustness

### ⚠️ SPEC COMPLIANCE VALIDATION
**All remaining high/medium priority tasks maintain spec compliance.** No tasks would break MCP specification requirements.

### 🎯 RECOMMENDED NEXT SPRINT FOCUS
1. **Week 1-2**: OAuth 2.1 authorization framework (complete flows)
2. **Week 3-4**: Cancellation protocol implementation  
3. **Week 5-6**: Logging control and debug mode
4. **Week 7-8**: Example application demonstrating all features

---

## Medium Priority Tasks

### Transport Enhancements (Medium)
**Priority: MEDIUM - Improve transport reliability and features**
- [x] SSE Transport Improvements
  - [ ] Make SSE endpoint configurable (not hardcoded /mcp/v1)
  - [ ] Add comprehensive HTTP status code handling
  - [x] Implement keep-alive/heartbeat mechanism
  - [x] Add automatic retry with exponential backoff
  - [x] Support custom HTTP headers
- [ ] Connection Management
  - [ ] Implement automatic reconnection for all transports
  - [ ] Add connection pooling support
  - [ ] Implement request queuing during reconnection
  - [ ] Add connection state change notifications

### Core Features (Medium)
**Priority: MEDIUM - Non-spec features that improve usability**
- [ ] Connection pooling for clients (optimization, not required by spec)
- [ ] Context inclusion options for sampling (partially implemented - types exist but logic incomplete)
- [ ] Tool registration and management
- [ ] Resource provider implementation
- [ ] Prompt template system
- [ ] Completion integration
- [ ] Logging integration
- [ ] Metrics and monitoring

## Lower Priority Tasks

### Testing & Development Tools (Low)
**Priority: LOW - Development experience improvements**
- [ ] Performance benchmarks
- [ ] Property-based tests for protocol encoding/decoding
- [ ] CLI tool for testing MCP servers
- [ ] Debug mode with protocol tracing
- [ ] Connection diagnostics
- [ ] Schema validation for messages

### Advanced Features (Low)
**Priority: LOW - Nice-to-have features not part of the spec**
- [ ] Middleware support for clients and servers
- [ ] Rate limiting  
- [ ] Message compression
- [ ] Circuit breaker for failed connections
- [ ] Graceful shutdown procedures
- [ ] WebSocket server mode implementation (requires HTTP server with upgrade support)
- [ ] WebSocket reconnection support
- [ ] WebSocket connection pooling

## Moved to Completed Section (Already Done)

### Core Components (Already Implemented)
- [x] Server Manager for multi-server support (implemented in server_manager.ex)
- [x] Discovery mechanism for finding available servers (implemented in discovery.ex)
- [x] Request/response timeout handling (implemented in client.ex)
- [x] Progress notification handling (implemented with progressToken support)
- [x] Automatic reconnection with exponential backoff (implemented in client.ex)

### MCP Protocol Features (Already Implemented)
- [x] Sampling/createMessage support for LLM interactions
- [x] Change notifications (resources/tools/prompts list_changed)
- [x] Progress notifications for long-running operations
- [x] Roots capability (roots/list, roots/list_changed notification)
- [x] Resource subscriptions (resources/subscribe, resources/unsubscribe)
- [x] Update protocol version to latest (now using 2025-03-26)
- [x] Tool annotations (readOnlyHint, destructiveHint, idempotentHint, openWorldHint)  
- [x] Multimodal content support (text, image, audio, embedded resources)
- [x] Request cancellation support (send_cancelled/3, handle cancelled notifications)
- [x] Logging protocol (log_message/4, handle log notifications)
- [x] Resource templates with URI patterns (list_resource_templates/2, handle_list_resource_templates/1)
- [x] Model preference hints (implemented in types and protocol)
- [x] Resource subscription notifications for dynamic resources (subscribe/unsubscribe/resource_updated implemented)
- [x] Completion support (complete/3 method implemented)

### Features
- [ ] Tool registration and management
- [ ] Resource provider implementation
- [ ] Prompt template system
- [ ] Completion integration
- [ ] Logging integration
- [ ] Metrics and monitoring

### Example Application
- [ ] Create unified example app demonstrating all ExMCP features
  - [ ] Setup project structure (examples/example_app/)
  - [ ] Implement MCP server with all protocol features
    - [ ] Tools (calculator, file operations, data processing)
    - [ ] Resources (config files, logs, data streams)
    - [ ] Prompts (templates with parameters)
    - [ ] Sampling/LLM integration with Ollama
    - [ ] Progress tracking for long operations
    - [ ] Subscriptions and notifications
  - [ ] Implement MCP client showcasing all capabilities
    - [ ] Tool invocation examples
    - [ ] Resource reading and subscription
    - [ ] Prompt usage
    - [ ] Batch operations
    - [ ] Cancellation examples
    - [ ] Human-in-the-loop approvals
  - [ ] Ollama integration
    - [ ] Configure both client and server to use Ollama
    - [ ] Use Qwen2.5-Coder:3b or similar small, fast model
    - [ ] Demonstrate sampling/createMessage with local LLM
    - [ ] Show model preference hints
  - [ ] Transport demonstrations
    - [ ] BEAM transport for local development
    - [ ] stdio transport for CLI usage
    - [ ] SSE transport for web integration
    - [ ] WebSocket client example
  - [ ] Security features
    - [ ] OAuth authorization flow
    - [ ] Token validation
    - [ ] Client consent management
    - [ ] Audit logging
  - [ ] Advanced patterns
    - [ ] Bidirectional communication
    - [ ] Server requesting from client
    - [ ] Health monitoring with pings
    - [ ] Distributed BEAM setup
    - [ ] OTP supervision integration
  - [ ] Developer experience
    - [ ] Configuration management
    - [ ] Error handling examples
    - [ ] Debug mode with tracing
    - [ ] Performance monitoring
    - [ ] Graceful shutdown
  - [ ] Documentation
    - [ ] README with setup instructions
    - [ ] Architecture overview
    - [ ] Feature walkthrough
    - [ ] Configuration guide
    - [ ] Troubleshooting section
  - [ ] Testing
    - [ ] Integration tests
    - [ ] Example test patterns
    - [ ] Mock server/client examples
  - [ ] Migration of existing examples
    - [ ] Consolidate batch_requests.exs
    - [ ] Consolidate bidirectional_communication.exs
    - [ ] Consolidate cancellation_example.exs
    - [ ] Consolidate comprehensive_example.exs (merge features)
    - [ ] Consolidate ping_health_check.exs
    - [ ] Consolidate progress_demo.exs
    - [ ] Consolidate prompts_server.exs
    - [ ] Consolidate resources_server.exs
    - [ ] Consolidate roots_demo.exs
    - [ ] Consolidate human_in_the_loop.exs
    - [ ] Consolidate oauth_authorization.exs
    - [ ] Consolidate secure_server_example.exs
    - [ ] Consolidate draft_features_example.exs
    - [ ] Consolidate resource_subscription_example.exs
    - [ ] Consolidate beam_transport examples
    - [ ] Consolidate advanced_features examples
    - [ ] Remove old example files after migration

### Testing
- [x] Integration tests with mock servers (implemented via transport tests)
- [x] Transport-specific tests (beam_test.exs, sse_test.exs, stdio mock tests)
- [x] Error handling and edge case tests (connection failures, invalid JSON, etc.)
- [x] HTTP test server for SSE testing (test_http_server.ex with Plug/Cowboy)
- [ ] Performance benchmarks
- [ ] Property-based tests for protocol encoding/decoding

### Documentation
- [x] Comprehensive README with examples
- [x] API documentation (API_REFERENCE.md)
- [x] User guide (USER_GUIDE.md)
- [ ] Protocol implementation guide
- [ ] Transport implementation guide
- [ ] Server handler implementation examples
- [ ] Client usage examples
- [ ] Server configuration persistence (mentioned in CHANGELOG but not implemented)

### Documentation Improvements (NEW - For clarity)
- [ ] Clear separation of client vs server modules
  - [ ] Add module-level docs clarifying ExMCP.Client connects TO servers
  - [ ] Add module-level docs clarifying ExMCP.Server is for IMPLEMENTING servers
  - [ ] Add conceptual overview of MCP architecture (clients connect to servers via transports)
- [ ] Integration guide
  - [ ] How to integrate MCP clients into existing applications
  - [ ] How to handle server lifecycle management
  - [ ] Best practices for error handling and reconnection
- [ ] Troubleshooting guide
  - [ ] Common connection issues and solutions
  - [ ] How to debug protocol messages
  - [ ] How to diagnose transport problems

### Utilities
- [ ] CLI tool for testing MCP servers
- [ ] Debug mode with protocol tracing
- [ ] Connection diagnostics
- [ ] Schema validation for messages

### Testing  
- [x] Client tests (implemented, all passing)
- [x] Server tests (implemented, all passing)
- [x] Server Manager tests (implemented via server_manager.ex tests)
- [x] Discovery tests (implemented, all passing)
- [x] stdio transport tests (implemented, all passing)
- [x] BEAM transport tests (implemented, all passing)
- [x] SSE transport tests (implemented, all passing)
- [x] Protocol tests (implemented, all passing)
- [x] Progress notification tests (implemented, all passing)

### Integration Testing Infrastructure (NEW - For mcp_chat compatibility)
- [ ] Full stdio integration test examples
  - [ ] Example of starting a server as OS process and connecting client
  - [ ] Example of proper setup/teardown for integration tests
  - [ ] Example of handling server lifecycle in tests
- [ ] Test helper utilities
  - [ ] Helper for starting MCP server processes with proper error handling
  - [ ] Helper for waiting for server initialization
  - [ ] Helper for graceful server shutdown
- [ ] BEAM transport testing utilities
  - [ ] Example of using BEAM transport for in-process testing
  - [ ] Helper for creating mock MCP servers using BEAM transport
- [ ] Debug mode enhancements
  - [ ] Add debug: true option to client for protocol message logging
  - [ ] Add protocol message tracing for troubleshooting
  - [ ] Add transport-level debug logging

### Advanced Features (Not part of MCP spec - nice to have)
- [ ] Middleware support for clients and servers
- [ ] Authentication and authorization
- [ ] Rate limiting  
- [ ] Message compression
- [ ] Circuit breaker for failed connections
- [ ] Graceful shutdown procedures
- [ ] WebSocket server mode implementation (requires HTTP server with upgrade support)
- [ ] WebSocket reconnection support
- [ ] WebSocket connection pooling

### Phase 16 Features (from mcp_chat)
- [ ] Health Monitoring Infrastructure
  - [ ] Health check protocol extension
  - [ ] Periodic health polling with configurable intervals
  - [ ] Server status types (healthy, degraded, unhealthy, unreachable)
  - [ ] Health metrics collection (latency, success rate, uptime)
  - [ ] Telemetry integration with :telemetry library
  - [ ] Health status change notifications
  - [ ] Automatic degraded mode handling
- [ ] Advanced Resource Management
  - [ ] Resource caching layer with TTL
  - [ ] Cache invalidation strategies
  - [ ] Resource versioning with ETags
  - [ ] Resource access pattern tracking
  - [ ] Batch resource fetching
  - [ ] Resource compression support
  - [ ] Distributed cache support (via Registry/PubSub)
- [ ] Connection Management
  - [ ] Connection pooling for multiple clients
  - [ ] Connection health tracking
  - [ ] Graceful degradation on connection issues
  - [ ] Request queuing with priorities
  - [ ] Dead letter queue for failed requests
  - [ ] Connection metrics and analytics
- [ ] Tool Execution Enhancements
  - [ ] Tool execution history tracking
  - [ ] Tool result caching with invalidation
  - [ ] Batch tool execution support
  - [ ] Tool execution analytics
  - [ ] Tool dependency resolution
  - [ ] Parallel tool execution where safe
  - [ ] Tool execution cost tracking

## Notes

- The library implements the Model Context Protocol specification version 2025-03-26
- Four transports are implemented: stdio (primary), BEAM (native Elixir), SSE, and WebSocket (client-only)
- The client includes automatic reconnection with exponential backoff
- Server handlers can be implemented using the ExMCP.Server.Handler behaviour
- All protocol messages use string keys for JSON compatibility
- All tests are passing (729 tests, 0 failures, 15 skipped) as of the latest updates
- Fixed SSE client exponential backoff and reconnection issues
- Fixed hot reload test edge cases and added proper cleanup
- Fixed intermittent test failures caused by async race conditions
- Progress notifications use progressToken (without underscore) per MCP spec
- Discovery module is an ExMCP extension providing comprehensive server discovery

### Implementation Priority
Features are prioritized as follows:
1. **High Priority**: MCP specification required features (batch requests, bi-directional communication, human-in-the-loop)
2. **Medium Priority**: Transport enhancements and optimizations
3. **Low Priority**: Nice-to-have features not part of the spec (middleware, auth, rate limiting)

### Protocol Compliance Status
Based on the latest MCP specification (2025-03-26), the following features have been implemented:
1. **Roots capability** - ✅ Implemented (roots/list method and roots/list_changed notification)
2. **Resource subscriptions** - ✅ Implemented (resources/subscribe, resources/unsubscribe methods)
3. **Tool annotations** - ✅ Implemented (readOnlyHint, destructiveHint, costHint properties in types)
4. **Multimodal content** - ✅ Already supported (text_content and image_content types)
5. **Protocol version** - ✅ Updated to latest version (2025-03-26)

The library implements most major features from the latest MCP specification.

## MCP Specification Compliance Gaps

Based on thorough review of the MCP specification (docs/mcp-llms-full.txt), the following features need implementation for full compliance:

### High Priority - Security & Authentication (COMPLETED)
- [x] SSE Authentication Support
  - [x] Add authentication header support in SSE transport
  - [x] Implement token-based authentication (Bearer tokens)
  - [x] Add API key authentication option
  - [x] Document authentication configuration
- [x] SSE Security Headers
  - [x] Implement Origin header validation to prevent DNS rebinding attacks
  - [x] Add CORS header support with configurable origins
  - [x] Add security headers (X-Content-Type-Options, etc.)
- [x] Transport Security
  - [x] Add TLS/SSL configuration options for all transports
  - [x] Implement certificate validation options
  - [ ] Add mutual TLS support

### High Priority - Protocol Compliance  
- [x] Cancellation Protocol ✅ **COMPLETED**
  - [x] Implement notifications/cancelled message handling
  - [x] Add request cancellation API in client
  - [x] Handle cancelled requests in server
  - [x] Fixed request ID type handling (integer/string compatibility)
  - [x] Verified cancellation works across all transports (BEAM, SSE, WebSocket, stdio)
  - [x] Comprehensive example in examples/cancellation_example.exs
- [x] Logging Control ✅ **COMPLETED**
  - [x] Implement logging/setLevel request handler
  - [x] Add configurable logging levels with ExMCP.Logging module
  - [x] Integrate with Elixir Logger properly (dual logging to MCP clients and Elixir Logger)
  - [x] Add security features (automatic sanitization of sensitive data)
  - [x] Support all RFC 5424 log levels (debug, info, notice, warning, error, critical, alert, emergency)
  - [x] Global log level configuration with persistent_term storage
  - [x] Comprehensive logging tests (33 tests passing)
  - [x] Enhanced server handler with logging capability declaration
- [ ] Missing Protocol Methods
  - [x] completion/complete endpoint (implemented in server.ex)
  - [x] resources/templates/list method (implemented as list_resource_templates)
  - [ ] Add full progress token support across all methods
  - [ ] Support _meta field in all request types

### Medium Priority - Transport Enhancements  
- [x] SSE Transport Improvements (MOSTLY COMPLETE)
  - [ ] Make SSE endpoint configurable (not hardcoded /mcp/v1)
  - [ ] Add comprehensive HTTP status code handling
  - [x] Implement keep-alive/heartbeat mechanism
  - [x] Add automatic retry with exponential backoff
  - [x] Support custom HTTP headers
- [ ] Connection Management
  - [ ] Implement automatic reconnection for all transports
  - [ ] Add connection pooling support
  - [ ] Implement request queuing during reconnection
  - [ ] Add connection state change notifications
- [ ] Error Handling Improvements
  - [ ] Add structured error data in responses
  - [ ] Implement automatic error recovery
  - [ ] Add configurable request timeouts
  - [ ] Implement rate limiting support

### Medium Priority - Discovery & Multi-Server  
- [x] Discovery Implementation (ExMCP.Discovery - extension module)
  - [x] Environment variable discovery (MCP_SERVERS JSON)
  - [x] Pattern-based env var discovery (*_MCP_SERVER, *_SERVER_URL)
  - [x] Configuration file discovery (mcp.json, .mcp/config.json)
  - [x] Well-known locations scanning
  - [x] NPM package discovery
  - [x] Python package discovery  
  - [x] Executable server detection
  - [x] Service registration API
  - [ ] DNS-SD discovery support (future enhancement)
- [ ] Server Metadata Enhancement
  - [x] Basic server metadata support
  - [ ] Query server capabilities during discovery
  - [ ] Cache discovered server metadata
  - [ ] Add server description and documentation URLs

### Low Priority - Advanced Features
- [ ] Batch Request API
  - [ ] Add high-level batch request API
  - [ ] Implement batch response correlation
  - [ ] Add batch error handling
- [ ] Session Persistence
  - [ ] Add session state persistence
  - [ ] Implement session recovery after restart
  - [ ] Add session migration support
- [ ] Multi-Transport Server
  - [ ] Allow servers to listen on multiple transports
  - [ ] Add transport selection negotiation
  - [ ] Implement transport fallback
- [ ] Experimental Features Framework
  - [ ] Add support for experimental capabilities
  - [ ] Implement feature flags system
  - [ ] Add protocol extension mechanism

### Testing & Development Tools
- [ ] Mock Transport
  - [ ] Create mock transport for testing
  - [ ] Add request/response recording
  - [ ] Implement playback mode
- [ ] Protocol Validation
  - [ ] Add message schema validation
  - [ ] Create strict compliance mode
  - [ ] Add protocol conformance tests
- [ ] Debug & Tracing
  - [ ] Add debug mode with message tracing
  - [ ] Implement performance profiling
  - [ ] Add protocol analyzer tool

### Documentation Updates
- [ ] Security Guide
  - [ ] Document authentication setup
  - [ ] Add security best practices
  - [ ] Include threat model
- [ ] Transport Implementation Guide
  - [ ] Document transport behaviour in detail
  - [ ] Add example transport implementation
  - [ ] Include testing guidelines
- [ ] Error Reference
  - [ ] Create comprehensive error code reference
  - [ ] Document error recovery strategies
  - [ ] Add troubleshooting guide
- [ ] Migration Guides
  - [ ] Add protocol version migration guide
  - [ ] Document breaking changes
  - [ ] Include upgrade scripts

### Missing MCP Specification Features Summary
The following key features from the MCP spec need implementation:
1. ~~**Authentication** - SSE transport lacks authentication support (Bearer tokens, API keys)~~ ✅ COMPLETED
2. ~~**Security Headers** - Missing Origin validation and CORS support in SSE~~ ✅ COMPLETED 
3. **Cancellation** - No notifications/cancelled message handling
4. **Logging Control** - Missing logging/setLevel request handler
5. ~~**Keep-Alive** - No heartbeat mechanism for SSE connections~~ ✅ COMPLETED
6. **Configurable SSE Endpoint** - SSE endpoint is hardcoded to /mcp/v1
7. **Batch Request API** - Low-level support exists but no high-level API
8. **Request Timeouts** - No configurable timeout support (fixed at 30s)
9. **Debug Mode** - No protocol message tracing or debugging support
10. **Extended Meta Fields** - Limited _meta field support (only for progress tokens)

### Implementation Status Summary
The ExMCP library has:
- ✅ All core MCP protocol methods (initialize, resources, tools, prompts, sampling, completion)
- ✅ Bi-directional communication and server-initiated requests
- ✅ Human-in-the-loop support with approval flows
- ✅ Comprehensive server discovery (extension feature)
- ✅ Multi-transport support (stdio, SSE, BEAM, WebSocket client)
- ✅ Automatic reconnection with exponential backoff
- ✅ Progress notifications and subscriptions
- ✅ Server manager for multi-server support
- ✅ Transport-level security features (auth, CORS, Origin validation, TLS/SSL)
- ✅ SSE transport with keep-alive, exponential backoff, and authentication
- ✅ Hot code reloading for BEAM transport servers
- ✅ BEAM clustering with load balancing and fault tolerance
- ✅ Streaming support for large payloads with backpressure handling
- ⚠️  Context inclusion for sampling (types exist but implementation incomplete)
- ❌ Advanced protocol features (cancellation, logging control, debug mode)
- ❌ OAuth 2.1 authorization framework (partial - Bearer tokens supported)

## Improvements from Hermes MCP Analysis

After analyzing the Hermes MCP implementation, the following improvements should be considered:

### High Priority - Enhanced Developer Experience

1. **Telemetry Integration**
   - [ ] Add comprehensive telemetry events using `:telemetry` library
   - [ ] Emit events for: client init/terminate, request/response, transport connect/disconnect
   - [ ] Include timing metrics, error tracking, and progress updates
   - [ ] Follow Hermes pattern: `[:ex_mcp, component, action]` namespace

2. **Interactive CLI Tool**
   - [ ] Create an interactive shell for testing MCP servers (like Hermes' `mix sse.interactive`)
   - [ ] Support all transports with unified interface
   - [ ] Add command completion and history
   - [ ] Include pretty-printed output with colors
   - [ ] Build standalone binary with Burrito for non-Elixir users

3. **Structured Error Handling**
   - [ ] Create dedicated error module with standardized error types
   - [ ] Implement error categorization (transport, client, server, domain errors)
   - [ ] Add custom inspect implementation for better error messages
   - [ ] Use structured error codes following JSON-RPC conventions

4. **Schema Validation with Peri**
   - [ ] Use Peri library for runtime schema validation (like Hermes)
   - [ ] Validate client options, transport configs, and protocol messages
   - [ ] Provide clear error messages for invalid configurations
   - [ ] Add compile-time validation where possible

### Medium Priority - Architectural Improvements

5. **State Management Separation**
   - [ ] Extract client state into dedicated module (like Hermes.Client.State)
   - [ ] Separate operation tracking into Operation module
   - [ ] Implement request queue management module
   - [ ] Add clear state transition documentation

6. **Protocol Version Management**
   - [ ] Support multiple protocol versions (2024-11-05 and 2025-03-26)
   - [ ] Add version negotiation layer
   - [ ] Implement backward compatibility handling
   - [ ] Plan migration path for new protocol features

7. **Enhanced Mix Tasks**
   - [ ] Add `mix ex_mcp.test` for testing MCP servers
   - [ ] Create `mix ex_mcp.validate` for protocol compliance checking
   - [ ] Implement `mix ex_mcp.gen.server` for scaffolding new servers
   - [ ] Add `mix ex_mcp.gen.handler` for creating handlers

### Low Priority - Future Enhancements

8. **Documentation Generation**
   - [ ] Auto-generate capability documentation from server metadata
   - [ ] Create protocol flow diagrams
   - [ ] Add interactive examples in documentation
   - [ ] Generate OpenAPI/AsyncAPI specs for HTTP transports

9. **Testing Utilities**
   - [ ] Create mock transport for easier testing (like Hermes.MockTransport)
   - [ ] Add protocol compliance test suite
   - [ ] Implement property-based testing for protocol messages
   - [ ] Create test fixtures and factories

10. **Performance Monitoring**
    - [ ] Add request/response timing metrics via telemetry
    - [ ] Implement connection pool statistics
    - [ ] Track resource usage per client/server
    - [ ] Add performance benchmarking suite

### Implementation Notes

- Hermes uses Peri for schema validation which provides better runtime safety
- Their telemetry integration enables excellent observability
- The interactive CLI significantly improves developer experience
- Structured error handling makes debugging much easier
- Clear separation of concerns in their module structure

## Improvements from mcp_ex Analysis

After analyzing the mcp_ex implementation, the following additional improvements should be considered:

### High Priority - Testing Infrastructure

1. **In-Memory Test Transport**
   - [ ] Create a test transport that operates fully in memory (like mcp_ex's Transport.Test)
   - [ ] Implement a TestServer that simulates server responses
   - [ ] Allow controllable message flow and timing for deterministic tests
   - [ ] Enable observable message exchange for test assertions
   - [ ] Support configurable error scenarios

2. **Schema-Based Validation**
   - [ ] Load and cache the official MCP JSON schema
   - [ ] Validate all incoming and outgoing messages against schema
   - [ ] Provide detailed validation error messages
   - [ ] Support multiple schema versions for protocol evolution
   - [ ] Create mix task to update/verify schema files

3. **Shell Environment Integration**
   - [ ] Detect shell type (interactive, login, plain) for stdio transport
   - [ ] Handle shell initialization delays properly
   - [ ] Support CommandUtils for enhanced shell environment setup
   - [ ] Add wrapper script support for complex command execution

### Medium Priority - Protocol Enhancements

4. **Standardized Error Codes**
   - [ ] Define MCP-specific error codes (-32800 to -32899)
   - [ ] Add request_cancelled (-32800) and content_too_large (-32801)
   - [ ] Create specialized error constructors for common cases
   - [ ] Implement error_response_json helper for quick formatting

5. **HTTP Transport Improvements**
   - [ ] Follow MCP HTTP transport spec with endpoint discovery
   - [ ] Support dynamic message endpoint configuration via SSE
   - [ ] Add comprehensive Req.new options passthrough
   - [ ] Implement proper origin construction for message endpoints
   - [ ] Add custom HTTP client injection for testing

6. **Transport Abstraction Layer**
   - [ ] Create unified transport behavior with standard callbacks
   - [ ] Implement transport type detection from process info
   - [ ] Add transport-agnostic send/receive interface
   - [ ] Support graceful transport switching/fallback

### Low Priority - Developer Tools

7. **Protocol Type System**
   - [ ] Generate type specs from JSON schema
   - [ ] Create comprehensive type definitions for all protocol messages
   - [ ] Add @type annotations for better dialyzer support
   - [ ] Document type relationships and constraints

8. **Enhanced Logging**
   - [ ] Add structured logging for protocol messages
   - [ ] Implement configurable log levels per component
   - [ ] Create protocol trace mode for debugging
   - [ ] Add performance metrics to log entries

9. **Configuration Management**
   - [ ] Support environment-specific configs (dev, test, prod)
   - [ ] Add configuration validation on startup
   - [ ] Implement config reloading without restart
   - [ ] Create config generators for common scenarios

### Implementation Insights from mcp_ex

- **Test-First Design**: mcp_ex's test transport shows the value of building testing infrastructure alongside features
- **Schema Validation**: Using the official JSON schema provides strong guarantees about protocol compliance
- **Shell Integration**: Proper handling of shell environments is crucial for stdio transport reliability
- **HTTP Spec Compliance**: Following the exact MCP HTTP transport specification (with endpoint discovery) ensures compatibility
- **Error Standardization**: Using consistent error codes across the library improves debuggability

## Improvements from ash_ai MCP Analysis

After analyzing the ash_ai MCP implementation, the following additional improvements should be considered:

### High Priority - Server Integration Improvements

1. **Plug.Router Wrapper for HTTP Transport**
   - [ ] Create a Plug.Router wrapper around ExMCP.Server for easier web integration
   - [ ] Enable simple `forward "/mcp", ExMCP.Plug` pattern in Phoenix
   - [ ] Support inline tool configuration in router options
   - [ ] Handle HTTP-specific concerns (headers, SSE formatting) at Plug level
   - [ ] Provide simplified API for common use cases

2. **Framework Integration Patterns**
   - [ ] Create generic integration patterns for Phoenix applications
   - [ ] Support forward routing with configurable paths
   - [ ] Enable middleware integration (auth, logging, etc.)
   - [ ] Provide mix tasks for generating MCP server setup
   - [ ] Add Igniter support for automated installation

3. **Tool Registration System**
   - [ ] Create a declarative tool definition system
   - [ ] Support tool discovery and listing
   - [ ] Enable dynamic tool registration
   - [ ] Add tool metadata (descriptions, schemas)
   - [ ] Support filtering tools by capabilities or tags

### Medium Priority - Protocol Features

4. **SSE Event Streaming**
   - [ ] Implement proper SSE event formatting
   - [ ] Support endpoint events for dynamic configuration
   - [ ] Add event ID support for resumability
   - [ ] Enable chunked transfer encoding
   - [ ] Support keep-alive ping events

5. **Session Management**
   - [ ] Implement lightweight session tracking
   - [ ] Support session IDs without full GenServer processes
   - [ ] Add session context propagation
   - [ ] Enable session-based state management
   - [ ] Support session termination via DELETE

6. **Flexible Protocol Versioning**
   - [ ] Support configurable protocol version statements
   - [ ] Enable backward compatibility modes
   - [ ] Add version negotiation helpers
   - [ ] Support version-specific feature flags

### Low Priority - Advanced Features

7. **Resource Integration**
   - [ ] Create patterns for exposing database resources
   - [ ] Support resource listing with pagination
   - [ ] Enable resource subscriptions
   - [ ] Add resource change notifications
   - [ ] Implement resource templates

8. **Authentication Integration**
   - [ ] Add API key authentication support
   - [ ] Create OAuth flow helpers
   - [ ] Support token-based authentication
   - [ ] Enable role-based access control
   - [ ] Add authentication middleware

9. **Development Tools**
   - [ ] Create MCP server generator (mix task)
   - [ ] Add live reload for tool definitions
   - [ ] Implement request/response logging
   - [ ] Create debugging middleware
   - [ ] Add performance monitoring

### Implementation Insights from ash_ai

- **Simplicity First**: ash_ai shows that a minimal MCP server can be very effective
- **Plug Integration**: Using Plug.Router makes it easy to integrate into existing web apps
- **Tool-Centric**: Focusing on tools as the primary integration point simplifies adoption
- **Framework Agnostic**: The implementation works with any Plug-compatible framework
- **Session Optional**: Session management can be added later without breaking compatibility

### Key Differences from ExMCP's Current Server Implementation

While ExMCP already has comprehensive server support, ash_ai demonstrates some additional patterns that could enhance ExMCP:

1. **Plug.Router Integration** - ash_ai shows how to expose MCP as a simple Plug that can be forwarded to in Phoenix/other web apps
2. **Lightweight HTTP Handling** - Direct HTTP/SSE handling without full transport abstraction
3. **Tool-First Configuration** - Simple tool list configuration in the router
4. **Mix Task Generators** - Automated setup for common integration patterns
5. **Framework-Specific Helpers** - Phoenix-specific forward routing patterns