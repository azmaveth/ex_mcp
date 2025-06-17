# MCP Interoperability Examples

This directory contains examples demonstrating interoperability between ExMCP (Elixir) and other MCP implementations, particularly the Python MCP SDK.

## Overview

The Model Context Protocol (MCP) is designed for cross-language interoperability. These examples show how to:

1. **Elixir Client → Python Server**: ExMCP clients connecting to Python MCP servers
2. **Python Client → Elixir Server**: Python MCP clients connecting to ExMCP servers
3. **Mixed Architectures**: Complex scenarios with multiple languages

## Transport Support

ExMCP supports the standard MCP transports for interoperability:

- **stdio** - Standard I/O transport (most common for subprocess communication)
- **http** - HTTP transport with optional SSE streaming (for networked services)

## Examples Structure

### Elixir Client Examples
- `elixir_to_python_stdio.ex` - Elixir client connecting to Python server via stdio
- `elixir_to_python_http.ex` - Elixir client connecting to Python server via HTTP
- `multi_python_servers.ex` - Elixir orchestrating multiple Python servers

### Python Integration Examples  
- `python_mcp_servers/` - Sample Python MCP servers for testing
- `python_clients/` - Sample Python clients that connect to Elixir servers
- `elixir_servers_for_python.ex` - Elixir servers designed for Python clients

### Advanced Scenarios
- `hybrid_architecture.ex` - Mixed Elixir/Python service architecture
- `performance_comparison.ex` - Performance tests between Native vs interop calls

## Prerequisites

To run these examples, you'll need:

1. **Python 3.8+** with the MCP SDK:
   ```bash
   pip install mcp
   ```

2. **ExMCP** (this library):
   ```bash
   mix deps.get
   ```

## Quick Start

1. **Start a Python MCP server**:
   ```bash
   cd examples/interoperability/python_mcp_servers
   python calculator_server.py
   ```

2. **Run Elixir client**:
   ```bash
   iex -S mix
   iex> ExMCP.Examples.Interop.ElixirToPythonStdio.run()
   ```

3. **Or run Python client with Elixir server**:
   ```bash
   # Terminal 1: Start Elixir server
   iex -S mix
   iex> ExMCP.Examples.Interop.ElixirServerForPython.run()
   
   # Terminal 2: Run Python client
   cd python_clients
   python elixir_client.py
   ```

## Performance Considerations

- **Native Service Dispatcher**: ~15μs per call (Elixir-to-Elixir only)
- **stdio transport**: ~1-5ms per call (cross-language)
- **HTTP transport**: ~5-20ms per call (networked, cross-language)

For high-performance scenarios, use Native Service Dispatcher for Elixir services and standard transports only when language interoperability is required.

## Common Patterns

### Pattern 1: Elixir Orchestrator with Python Workers
```elixir
# Elixir manages multiple Python specialized services
python_servers = [
  ml_service: ["python", "ml_server.py"],
  data_analysis: ["python", "analysis_server.py"],
  image_processing: ["python", "vision_server.py"]
]
```

### Pattern 2: Python Client with Elixir Backend
```python
# Python AI/ML application using Elixir for data/concurrency
import mcp
client = mcp.Client(transport="stdio", command=["elixir", "backend_server.exs"])
```

### Pattern 3: Hybrid Service Mesh
- Native Service Dispatcher for Elixir-to-Elixir
- stdio/HTTP transports for cross-language communication
- Centralized service registry and discovery

## Troubleshooting

### Common Issues

1. **Path/Command Issues**: Ensure Python and Elixir executables are in PATH
2. **JSON Encoding**: ExMCP handles JSON automatically, but watch for encoding differences
3. **Process Management**: stdio transport manages subprocesses - ensure proper cleanup
4. **Port Conflicts**: HTTP transport examples use configurable ports

### Debugging Tips

- Enable verbose logging: `Logger.configure(level: :debug)`
- Use `transport: :test` for unit testing interop logic
- Monitor process communication with `:observer.start()`

## Contributing

When adding new interoperability examples:

1. Include both language perspectives (Elixir → Python and Python → Elixir)
2. Document transport choice rationale
3. Add performance measurements
4. Include error handling examples
5. Provide setup/installation instructions