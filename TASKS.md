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

- [ ] Additional transport implementations
  - [ ] WebSocket transport
  
### BEAM Transport Enhancements (from mcp_chat Phase 9)
- [x] Basic BEAM transport implementation completed
- [ ] Advanced BEAM transport features:
  - [ ] Support for supervised GenServer-based MCP servers
  - [ ] Distributed BEAM node connections
  - [ ] Performance optimizations for local connections
  - [ ] Zero-copy message passing for large payloads
  - [ ] Native BEAM clustering support
  - [ ] Hot code reloading for MCP servers
  - [ ] Process monitoring and automatic reconnection

## Todo

### Core Components
- [x] Server Manager for multi-server support (implemented)
- [x] Discovery mechanism for finding available servers (implemented)
- [ ] Connection pooling for clients
- [ ] Request/response timeout handling
- [ ] Batch request support
- [ ] Progress notification handling

### MCP Protocol Features (Missing from Spec)
- [x] Sampling/createMessage support for LLM interactions
- [x] Change notifications (resources/tools/prompts list_changed)
- [x] Progress notifications for long-running operations
- [ ] Request cancellation support
- [ ] Logging protocol (logging/setLevel, log notifications)
- [ ] Roots capability (roots/list, roots/list_changed)
- [ ] Resource templates with URI patterns
- [ ] Resource subscriptions for dynamic resources
- [ ] Bi-directional requests (server-to-client)
- [ ] Human-in-the-loop interaction support
- [ ] Context inclusion options for sampling
- [ ] Model preference hints

### Features
- [ ] Tool registration and management
- [ ] Resource provider implementation
- [ ] Prompt template system
- [ ] Completion integration
- [ ] Logging integration
- [ ] Metrics and monitoring

### Testing
- [ ] Integration tests with mock servers
- [ ] Transport-specific tests
- [ ] Error handling and edge case tests
- [ ] Performance benchmarks
- [ ] Property-based tests for protocol encoding/decoding

### Documentation
- [ ] Comprehensive README with examples
- [ ] API documentation
- [ ] Protocol implementation guide
- [ ] Transport implementation guide
- [ ] Server handler implementation examples
- [ ] Client usage examples
- [ ] Server configuration persistence (mentioned in CHANGELOG but not implemented)

### Utilities
- [ ] CLI tool for testing MCP servers
- [ ] Debug mode with protocol tracing
- [ ] Connection diagnostics
- [ ] Schema validation for messages

### Testing
- [ ] Client tests
- [ ] Server tests
- [ ] Server Manager tests
- [ ] Discovery tests
- [ ] stdio transport tests

### Advanced Features
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

- The library implements the Model Context Protocol specification version 2024-11-05
- stdio transport is the primary transport mechanism, matching the reference implementation
- The client includes automatic reconnection with exponential backoff
- Server handlers can be implemented using the ExMCP.Server.Handler behaviour
- All protocol messages use string keys for JSON compatibility