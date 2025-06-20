# ExMCP Troubleshooting Guide

This guide covers common issues and their solutions when working with ExMCP.

## STDIO Transport Issues

### "Unexpected end of JSON input" Error

**Problem**: When using STDIO transport with MCP Inspector or other clients, you see:
```
Error from MCP server: SyntaxError: Unexpected end of JSON input
```

**Cause**: The MCP STDIO protocol requires that ONLY valid JSON-RPC messages appear on stdout. Any other output (logs, Mix.install messages, etc.) contaminates the stream.

**Status**: ✅ **FIXED** - ExMCP now handles non-JSON lines gracefully during startup.

**Common Sources of Contamination**:
1. `Mix.install` output (e.g., "==> ex_mcp", "Compiling...")
2. Logger messages going to stdout instead of stderr
3. Horde registry/supervisor startup logs
4. Any `IO.puts` calls without specifying `:stderr`

**What We Fixed**:
- STDIO server now ignores non-JSON lines instead of sending error responses
- Implemented protocol version negotiation between client and server
- Added configurable startup delay for Mix.install output
- Improved logging configuration for STDIO transport

**Solutions**:

#### For Scripts Using Mix.install

Configure logging BEFORE calling Mix.install:

```elixir
#!/usr/bin/env elixir

# CRITICAL: Configure before Mix.install
Application.put_env(:ex_mcp, :stdio_mode, true)
Application.put_env(:ex_mcp, :stdio_startup_delay, 500)  # ms

# Suppress all logging
System.put_env("ELIXIR_LOG_LEVEL", "emergency")
Application.put_env(:logger, :level, :emergency)

Mix.install([
  {:ex_mcp, "~> 0.1"}
], verbose: false)

# Your server code here...
```

#### For Production Servers

1. **Use Releases**: Build a release that doesn't need Mix.install (recommended)
2. **Use StdioLauncher**: Helper module that handles startup properly
   ```elixir
   ExMCP.StdioLauncher.start(MyServer, [
     {:ex_mcp, "~> 0.1"}
   ])
   ```

**Note**: While ExMCP now gracefully handles non-JSON output during startup, Mix.install may still produce some stdout output that cannot be completely suppressed. For absolute zero contamination in production, use compiled releases.

#### Debugging Output

Always send debug output to stderr:
```elixir
# Good
IO.puts(:stderr, "Debug message")

# Bad - contaminates stdout
IO.puts("Debug message")
```

### Server Hangs After Starting

**Problem**: The STDIO server starts but doesn't respond to requests.

**Cause**: The server isn't properly entering STDIO transport mode.

**Solution**: Ensure you're using `transport: :stdio` when starting:
```elixir
MyServer.start_link(transport: :stdio)
```

## DSL Issues

### Tools Not Appearing

**Problem**: Defined tools don't show up when client lists them.

**Cause**: Missing or incorrect callback implementation.

**Solution**: Ensure you implement the `handle_tool_call/3` callback:
```elixir
@impl true
def handle_tool_call(tool_name, args, state) do
  # Handle the tool call
  {:ok, result, state}
end
```

### "Unknown Key" Warnings

**Problem**: Client shows warnings about `__unknown_key__` in responses.

**Cause**: This was a bug in older versions where protocol data was being atomized incorrectly.

**Solution**: Update to the latest version of ExMCP. Protocol data is now kept as strings.

## HTTP Transport Issues

### Port Already in Use

**Problem**: Starting HTTP server fails with "address already in use".

**Solution**: 
1. Check if another server is running on the port
2. Use a different port: `MyServer.start_link(transport: :http, port: 8080)`
3. Kill the existing process using the port

### CORS Errors

**Problem**: Browser clients get CORS errors when connecting.

**Solution**: CORS is enabled by default. If still having issues:
```elixir
MyServer.start_link(
  transport: :http,
  cors_enabled: true
)
```

## General Debugging Tips

### Enable Debug Logging

For non-STDIO transports, enable debug logging:
```elixir
Logger.configure(level: :debug)
```

### Check Server State

Use the native transport for debugging:
```elixir
{:ok, server} = MyServer.start_link(transport: :native)
:sys.get_state(server)
```

### Test with Simple Client

Use the ExMCP client to test your server:
```elixir
{:ok, client} = ExMCP.Client.connect(url: "stdio://path/to/server.exs")
{:ok, response} = ExMCP.Client.call_tool(client, "tool_name", %{arg: "value"})
```

## Common Mistakes

1. **Using atoms for protocol keys**: Protocol data should use string keys
2. **Not implementing callbacks**: Ensure all required callbacks are implemented
3. **Logging to stdout in STDIO mode**: Always use `:stderr` for STDIO servers
4. **Not handling errors**: Always return proper error tuples from handlers

## Getting Help

If you're still having issues:

1. Check the examples in `examples/getting_started/`
2. Review the test files for usage patterns
3. Open an issue on GitHub with:
   - ExMCP version
   - Elixir/OTP versions
   - Minimal reproduction code
   - Full error messages