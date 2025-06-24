# Message Processor Analysis - Phase 0, Task 0.1

## Executive Summary

The message processor has a fundamental flaw: it attempts to detect DSL vs handler servers incorrectly, and the routing logic for handler-based servers expects DSL-style functions that don't exist.

## Critical Issue Identified

The core problem is in `process_with_handler/3` (lines 201-221):
- It calls `handler_module.get_tools()`, `handler_module.get_prompts()`, etc.
- These functions ONLY exist on DSL servers
- Handler-based servers don't have these functions, they have callbacks like `handle_list_tools/2`

## Current Routing Logic

### 1. Entry Point: `process/2` (lines 117-144)
```elixir
def process(%Conn{} = conn, opts) do
  handler = Map.get(opts, :handler)
  ...
  if function_exported?(handler_module, :start_link, 1) and
     function_exported?(handler_module, :handle_resource_read, 3) do
    process_with_dsl_server(conn, handler_module, server_info)
  else
    process_with_handler(conn, handler_module, server_info)
  end
end
```

**Problem**: The detection logic is flawed. It checks for `:start_link` and `:handle_resource_read`, but:
- BOTH DSL servers and handler servers can have these functions
- This causes handler servers to sometimes be routed as DSL servers

### 2. DSL Server Path: `process_with_dsl_server/3` (lines 147-173)
- Starts a temporary GenServer instance
- Routes to `process_with_server_pid/3`
- Uses GenServer.call for all operations

### 3. Handler Path: `process_with_handler/3` (lines 201-221)
**THIS IS THE BROKEN PATH**
- Attempts to call DSL-style functions directly on handler module:
  - `handler_module.get_tools()` (line 250)
  - `handler_module.get_prompts()` (line 308)
  - `handler_module.get_resources()` (line 277)
- These functions DON'T EXIST on handler modules!

### 4. Server PID Path: `process_with_server_pid/3` (lines 176-193)
- Used by DSL servers after starting GenServer
- Correctly uses GenServer.call for all operations

## Method Routing Table

| MCP Method | Handler Path (BROKEN) | Server Path (WORKS) |
|------------|----------------------|---------------------|
| `tools/list` | `handler_module.get_tools()` | `GenServer.call(pid, :get_tools)` |
| `prompts/list` | `handler_module.get_prompts()` | `GenServer.call(pid, :get_prompts)` |
| `resources/list` | `handler_module.get_resources()` | `GenServer.call(pid, :get_resources)` |
| `tools/call` | `handler_module.handle_tool_call/3` | `GenServer.call(pid, {:handle_tool_call, ...})` |
| `prompts/get` | `handler_module.handle_prompt_get/3` | `GenServer.call(pid, {:handle_prompt_get, ...})` |
| `resources/read` | `handler_module.handle_resource_read/3` | `GenServer.call(pid, {:handle_resource_read, ...})` |

## Call Flow Analysis

### Current Flow (Broken for Handlers)
```
Client Request
    |
    v
MessageProcessor.process/2
    |
    +--> DSL Detection (flawed)
    |       |
    |       +--> Yes: process_with_dsl_server/3
    |       |         (Starts GenServer, uses GenServer.call)
    |       |
    |       +--> No: process_with_handler/3
    |                 (Calls DSL functions that don't exist!)
    |
    v
Response (or crash for handlers)
```

### Expected Flow for Handlers
```
Client Request
    |
    v
MessageProcessor.process/2
    |
    +--> Proper Detection
    |       |
    |       +--> DSL Server: Use get_tools(), etc.
    |       |
    |       +--> Handler Server: Use GenServer.call with handler callbacks
    |                             - {:handle_list_tools, cursor}
    |                             - {:handle_list_prompts, cursor}
    |                             - etc.
    |
    v
Response
```

## Root Cause

The message processor was designed primarily for DSL servers and the handler support was added incorrectly. The `process_with_handler/3` function tries to call DSL-style functions directly on the handler module instead of:

1. Starting the handler as a GenServer (if needed)
2. Using the proper handler callbacks via GenServer.call

## Fix Strategy

### Option 1: Unified GenServer Approach (Recommended)
- Always start servers as GenServers (both DSL and handler)
- Use GenServer.call for all operations
- The server's GenServer implementation handles routing to appropriate callbacks

### Option 2: Dual Path with Proper Detection
- Fix detection to properly identify DSL vs handler servers
- Fix handler path to use proper callbacks
- Maintain separate code paths

### Option 3: Handler Adapter
- Create an adapter that wraps handler servers
- Adapter provides DSL-style functions that delegate to handler callbacks
- Minimal changes to existing message processor

## Next Steps

1. Create a test case that demonstrates the current failure
2. Implement proper server type detection
3. Fix the routing to use appropriate functions for each server type
4. Validate both patterns work correctly