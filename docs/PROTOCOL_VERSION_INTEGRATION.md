# Protocol Version Integration Guide

This guide explains how to integrate the MCP 2025-06-18 protocol version validation into your ExMCP server.

## Overview

The protocol version feature adds validation for the `MCP-Protocol-Version` header as required by the MCP 2025-06-18 specification. This ensures clients and servers are using compatible protocol versions.

### Supported Versions

- `2025-06-18` - Latest version with structured output and OAuth 2.1 support (no batch requests)
- `2025-03-26` - Previous version with batch request support
- `2024-11-05` - Legacy version for backwards compatibility

## Components

### 1. Feature Flags (`lib/ex_mcp/feature_flags.ex`)
Controls the rollout of new features:
- `:protocol_version_header` - Enable/disable version validation
- `:structured_output` - Enable structured tool responses (future)
- `:oauth2_auth` - Enable OAuth 2.1 (future)

### 2. Protocol Version Plug (`lib/ex_mcp/plugs/protocol_version.ex`)
Validates the `MCP-Protocol-Version` header:
- Defaults to "2025-06-18" if header is missing
- Returns 400 Bad Request for unsupported versions
- Stores version in `conn.assigns[:mcp_version]`

### 3. Version Negotiator (`lib/ex_mcp/protocol/version_negotiator.ex`)
Handles version negotiation during initialization:
- Negotiates best mutually supported version
- Builds capability responses

## Integration Methods

### Method 1: Using Plug.Builder (Recommended)

```elixir
defmodule MyApp.MCPRouter do
  use Plug.Builder
  
  # Add protocol version validation
  plug ExMCP.Plugs.ProtocolVersion
  
  # Your existing plugs
  plug Plug.Logger
  plug :match
  plug :dispatch
  
  # Forward to MCP handler
  forward "/mcp", to: ExMCP.Transport.HTTPServer,
    init_opts: [
      handler: MyApp.MCPHandler,
      security: %{validate_origin: true}
    ]
end
```

### Method 2: Phoenix Router Integration

```elixir
defmodule MyAppWeb.Router do
  use MyAppWeb, :router
  
  pipeline :mcp do
    plug ExMCP.Plugs.ProtocolVersion
    plug :accepts, ["json"]
  end
  
  scope "/api/mcp" do
    pipe_through :mcp
    
    forward "/", ExMCP.Transport.HTTPServer,
      handler: MyApp.MCPHandler
  end
end
```

### Method 3: Direct HTTPServer Modification

For existing HTTPServer users, you can create a wrapper:

```elixir
defmodule MyApp.MCPServer do
  use Plug.Builder
  
  plug ExMCP.Plugs.ProtocolVersion
  plug ExMCP.Transport.HTTPServer
  
  def init(opts), do: opts
end
```

## Configuration

### Enable Protocol Version Validation

```elixir
# In config/config.exs or runtime.exs
config :ex_mcp,
  protocol_version_required: true  # Enable validation
```

### Gradual Rollout

```elixir
# Start with logging only
config :ex_mcp,
  protocol_version_required: false  # Disabled by default

# Monitor logs for version distribution
# Then enable when ready
```

## Using Version Information

The validated protocol version is available in your handler:

```elixir
defmodule MyApp.MCPHandler do
  use ExMCP.Server
  
  def handle_request(method, params, context) do
    # Access protocol version from context
    version = context[:protocol_version] || "2025-06-18"
    
    case version do
      "2025-06-18" ->
        # Handle new protocol features
        handle_new_protocol(method, params)
        
      "2025-03-26" ->
        # Handle previous protocol with batch support
        handle_v2025_03_protocol(method, params)
        
      "2024-11-05" ->
        # Handle legacy protocol
        handle_legacy_protocol(method, params)
    end
  end
end
```

## Version Negotiation

During initialization, use the version negotiator:

```elixir
def handle_initialize(params, _context) do
  client_versions = params["protocolVersions"] || ["2025-06-18"]
  
  case ExMCP.Protocol.VersionNegotiator.negotiate(client_versions) do
    {:ok, version} ->
      capabilities = ExMCP.Protocol.VersionNegotiator.build_capabilities(version)
      {:ok, capabilities}
      
    {:error, :no_compatible_version} ->
      {:error, %{
        code: -32002,
        message: "No compatible protocol version",
        data: %{
          supported_versions: ExMCP.Protocol.VersionNegotiator.supported_versions()
        }
      }}
  end
end
```

## Testing

### Unit Testing

```elixir
defmodule MyApp.MCPRouterTest do
  use ExUnit.Case
  use Plug.Test
  
  test "accepts valid protocol version" do
    conn =
      conn(:post, "/mcp")
      |> put_req_header("mcp-protocol-version", "2025-06-18")
      |> MyApp.MCPRouter.call([])
    
    refute conn.halted
    assert conn.assigns[:mcp_version] == "2025-06-18"
  end
  
  test "rejects invalid protocol version" do
    conn =
      conn(:post, "/mcp")
      |> put_req_header("mcp-protocol-version", "2024-01-01")
      |> MyApp.MCPRouter.call([])
    
    assert conn.status == 400
    assert conn.halted
  end
end
```

### Integration Testing

```elixir
# Enable feature for tests
setup do
  Application.put_env(:ex_mcp, :protocol_version_required, true)
  
  on_exit(fn ->
    Application.delete_env(:ex_mcp, :protocol_version_required)
  end)
end
```

## Migration Checklist

- [ ] Add `ExMCP.Plugs.ProtocolVersion` to your plug pipeline
- [ ] Update initialization handler to negotiate versions
- [ ] Test with both protocol versions (2025-06-18 and 2025-03-26)
- [ ] Enable feature flag in staging environment
- [ ] Monitor logs for unsupported version attempts
- [ ] Enable in production after validation

## Troubleshooting

### Common Issues

1. **400 Bad Request for all requests**
   - Check if client is sending correct header name: `MCP-Protocol-Version`
   - Verify version format (YYYY-MM-DD)

2. **Feature not working**
   - Ensure feature flag is enabled in config
   - Verify plug is in the pipeline before HTTPServer

3. **Version not available in handler**
   - Check conn.assigns[:mcp_version] is being passed to context
   - Ensure plug ordering is correct

### Debug Logging

Enable debug logs to see version validation:

```elixir
# In config/dev.exs
config :logger, level: :debug
```

This will show:
- When default version is used
- Which version was validated
- When invalid versions are rejected