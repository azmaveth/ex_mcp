# Advanced ExMCP v2 Examples

This directory contains comprehensive examples showcasing advanced ExMCP features.

## Examples

### complete_server.exs
A full-featured MCP server demonstrating:
- Multiple tools with different input schemas
- Various resource types
- Prompt definitions for AI assistants
- Complete DSL usage

### stateful_server.exs
Advanced server with state management:
- Stateful tools (todo list management)
- Dynamic resource generation
- Complex validation and error handling
- Progress notifications
- URI pattern matching for resources

### error_handling_server.exs
Comprehensive error handling patterns:
- Different error types and responses
- Exception handling in tools
- Domain-specific errors
- Error tracking and reporting

### weather_service.exs
Production-like service example:
- HTTP/SSE transport
- Real-time updates
- Progress notifications
- Natural language prompt handling
- CORS support

## Running the Examples

Each example is self-contained:

```bash
# Run any server
elixir <example_name>.exs

# Some examples include built-in clients
# Others can be tested with the basic client:
elixir ../basic_client_v2.exs
```

## Key Concepts Demonstrated

1. **State Management** - Maintaining server state across requests
2. **Validation** - Input validation beyond JSON Schema
3. **Progress Updates** - Long-running operations with feedback
4. **Dynamic Resources** - Resources with URI parameters
5. **Error Recovery** - Graceful error handling and recovery
6. **Production Features** - CORS, health checks, monitoring