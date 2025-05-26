# ExMCP Tasks

## Completed

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

## In Progress

### MCP Specification Required Features (High Priority)
- [x] Batch request support (JSONRPCBatchRequest/JSONRPCBatchResponse)
  - Implemented in Protocol, Client, and Server modules
  - Full integration tests with BEAM transport
- [x] Bi-directional requests (server-to-client)
  - [x] Implement server ping requests to client
  - [x] Implement server createMessage requests to client  
  - [x] Implement server listRoots requests to client
  - Client.Handler behaviour for handling server requests
  - Server can make requests with ping/1, list_roots/1, create_message/2
  - Full test coverage with TestClientHandler
- [ ] Human-in-the-loop interaction support
  - [ ] Add approval flow for sampling/createMessage
  - [ ] Add approval flow before returning sampled messages

- [ ] Additional transport implementations
  - [ ] WebSocket transport
  
### BEAM Transport Enhancements (from mcp_chat Phase 9)
- [x] Basic BEAM transport implementation completed
- [x] Advanced BEAM transport features:
  - [x] Support for supervised GenServer-based MCP servers (via mailbox processes)
  - [x] Distributed BEAM node connections (supports {:name, :"node@host"} syntax)
  - [ ] Performance optimizations for local connections
  - [ ] Zero-copy message passing for large payloads
  - [ ] Native BEAM clustering support
  - [ ] Hot code reloading for MCP servers
  - [x] Process monitoring and automatic reconnection (via Process.monitor and DOWN handling)
  - [ ] Streaming support for large payloads/responses
    - [ ] Stream-based message delivery for handling large tool outputs
    - [ ] Backpressure handling for flow control
    - [ ] Chunked transfer for resources

## Todo

### Core Components (Non-spec features)
- [x] Server Manager for multi-server support (implemented)
- [x] Discovery mechanism for finding available servers (implemented)
- [ ] Connection pooling for clients (optimization, not required by spec)
- [x] Request/response timeout handling (implemented in client.ex)
- [x] Progress notification handling (implemented with progressToken support)

### MCP Protocol Features (Missing from Current Implementation)
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
- [ ] Context inclusion options for sampling (partially implemented - types exist but logic incomplete)
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

### Testing
- [x] Integration tests with mock servers (implemented via transport tests)
- [x] Transport-specific tests (beam_test.exs, sse_test.exs, stdio mock tests)
- [x] Error handling and edge case tests (connection failures, invalid JSON, etc.)
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
- Three transports are implemented: stdio (primary), BEAM (native Elixir), and SSE
- The client includes automatic reconnection with exponential backoff
- Server handlers can be implemented using the ExMCP.Server.Handler behaviour
- All protocol messages use string keys for JSON compatibility
- All tests are passing (0 failures) as of the latest updates
- Progress notifications use progressToken (without underscore) per MCP spec

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

### Missing MCP Specification Features
The following features are defined in the MCP spec but not yet implemented:
1. **Batch Requests** - Support for JSONRPCBatchRequest/JSONRPCBatchResponse
2. **Bi-directional Requests** - Server-initiated requests (ping, createMessage, listRoots)
3. **Human-in-the-loop** - Approval flows for sampling and message generation
4. **Context Inclusion** - Full implementation of includeContext parameter in sampling