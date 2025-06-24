# ExMCP Critical Issues Resolution Plan

## Executive Summary

Based on the comprehensive Batch 1 test migration analysis, the ExMCP library has fundamental integration issues preventing proper MCP protocol implementation. This document outlines a systematic plan to address these issues through a hybrid incremental approach.

## Issues Identified

### API Incompleteness
- `Client.disconnect/1` - Referenced in tests but doesn't exist
- `Client.complete/3` - Expected for MCP completion protocol  
- `Client.log_message/3,4` - Expected for MCP logging protocol
- `Server.list_roots/2` - Removed during consolidation but still referenced

### Message Processor Integration Issues
- Handler servers don't integrate properly with client-server transport
- Message processor expects DSL functions (get_prompts()) but handlers use callbacks (handle_list_prompts/2)
- Inconsistent routing between handler and DSL patterns

### Transport Compatibility Issues
- Handler servers support :test transport but have message processor issues
- DSL servers only work reliably with :native transport
- Client-server communication broken for handler-based servers

### Code Quality & Consistency Issues
- Tests referencing non-existent APIs
- Inconsistent response formats (string vs atom keys)
- Missing protocol implementations

## Resolution Plan

### Dependency Flow
```
PHASE 0 (Foundation)
     |
     v
PHASE 1 (Critical Path) --> PHASE 2 (Parallel Development)
     |                           |
     v                           v
PHASE 3 (Integration & Consistency)
     |
     v
Complete MCP Protocol Compliance
```

### Phase 0: Diagnostic & Foundation
**Critical Enabler - Must Complete First**

#### Task 0.1: Message Processor Analysis
- Read and analyze `lib/ex_mcp/message_processor.ex` in detail
- Document current routing logic for each MCP method
- Identify exactly where handler vs DSL detection should occur
- Map out complete call flow from client request to server handler

#### Task 0.2: Create Validation Framework
- Build minimal test demonstrating current handler routing failure
- Create test harness validating both handler and DSL routing
- Establish baseline metrics for what works/fails in each pattern

### Phase 1: Message Processor Core Fix
**Critical Path - Unlocks Handler-Client Communication**

#### Task 1.1: Server Type Detection
- Implement server introspection logic
- Detect whether server is handler-based or DSL-based
- Create routing decision framework

#### Task 1.2: Dual Routing Implementation
- Add handler routing paths (handle_list_prompts/2, etc.)
- Maintain existing DSL routing paths (get_prompts(), etc.)
- Ensure both patterns can coexist safely

#### Task 1.3: Handler Integration Testing
- Validate handler servers can communicate with clients
- Test all MCP operations (tools, prompts, resources)
- Verify DSL servers still work (backward compatibility)

### Phase 2: Client API Implementation
**Parallel Development - Can Start After Phase 1**

#### Task 2.1: Client Connection Management
- Implement `Client.disconnect/1` with proper connection state handling
- Add graceful shutdown and cleanup logic
- Test connection lifecycle with both server types

#### Task 2.2: Client Completion API
- Implement `Client.complete/3` following MCP completion/complete protocol
- Add proper request/response handling and error management
- Test with handler completion logic (handle_complete callback)

#### Task 2.3: Client Logging API
- Implement `Client.log_message/3` and `Client.log_message/4`
- Follow MCP notifications/message protocol specification
- Add support for different severity levels and structured data

#### Task 2.4: Server Roots API Restoration
- Re-implement `Server.list_roots/2` functionality
- Add proper client communication and response handling
- Test roots protocol end-to-end

### Phase 3: Transport & Consistency
**Integration Layer - Requires Phases 1 & 2**

#### Task 3.1: Response Format Standardization
- Fix string vs atom key inconsistencies across all responses
- Ensure consistent format between handler and DSL patterns
- Update response processing to handle both formats during transition

#### Task 3.2: Transport Layer Unification
- Fix :test transport issues with DSL servers
- Ensure :native transport works reliably with handler servers
- Standardize initialization and capability negotiation across transports

#### Task 3.3: End-to-End Validation
- Run all migrated tests against fixed implementation
- Validate complete MCP protocol compliance
- Performance and stability testing

## Implementation Examples

### Server Type Detection
```elixir
defp detect_server_type(server_module) do
  cond do
    function_exported?(server_module, :get_tools, 0) -> :dsl_server
    function_exported?(server_module, :handle_list_tools, 2) -> :handler_server
    true -> :unknown
  end
end
```

### Client Disconnect Implementation
```elixir
def disconnect(client) do
  GenServer.call(client, :disconnect)
end
```

### Client Complete Implementation
```elixir
def complete(client, ref, argument, opts \\ []) do
  timeout = Keyword.get(opts, :timeout, 5_000)
  params = %{"ref" => ref, "argument" => argument}
  case GenServer.call(client, {:request, "completion/complete", params}, timeout) do
    {:ok, response} -> format_response(response, :map)
    error -> error
  end
end
```

### Client Log Message Implementation
```elixir
def log_message(client, level, message, data \\ %{}) do
  params = %{"level" => level, "message" => message, "data" => data}
  GenServer.cast(client, {:notification, "notifications/message", params})
end
```

### Server List Roots Implementation
```elixir
def list_roots(server, timeout \\ 5000) do
  GenServer.call(server, {:request, "roots/list", %{}}, timeout)
end
```

## Critical Success Factors

1. **Backward Compatibility**: Existing DSL servers must continue working
2. **Incremental Delivery**: Each phase must deliver testable functionality
3. **Protocol Compliance**: New APIs must follow MCP specifications exactly
4. **Integration Testing**: Validate both patterns work with all transports

## Risk Mitigation

- **Phase 1 Risk**: Breaking existing DSL functionality
  - **Mitigation**: Comprehensive testing of DSL backward compatibility
  
- **Phase 2 Risk**: API design not matching MCP specifications
  - **Mitigation**: Reference MCP protocol docs for each new API
  
- **Phase 3 Risk**: Transport changes causing new integration issues
  - **Mitigation**: Incremental transport fixes with extensive testing

## Progress Tracking

### Phase 0 Status: COMPLETED
- [x] Task 0.1: Message Processor Analysis
- [x] Task 0.2: Create Validation Framework

### Phase 1 Status: COMPLETED
- [x] Task 1.1: Server Type Detection
- [x] Task 1.2: Dual Routing Implementation
- [x] Task 1.3: Handler Integration Testing

### Phase 2 Status: NOT STARTED
- [ ] Task 2.1: Client Connection Management
- [ ] Task 2.2: Client Completion API
- [ ] Task 2.3: Client Logging API
- [ ] Task 2.4: Server Roots API Restoration

### Phase 3 Status: NOT STARTED
- [ ] Task 3.1: Response Format Standardization
- [ ] Task 3.2: Transport Layer Unification
- [ ] Task 3.3: End-to-End Validation

## Next Steps

**IMMEDIATE ACTION**: Begin Phase 0, Task 0.1 - Message Processor Analysis

The foundation work is critical for understanding the current system before making any changes. This analysis will validate our assumptions and guide the implementation approach for Phase 1.