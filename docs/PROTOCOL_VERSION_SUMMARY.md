# Protocol Version Support Summary

## Supported Versions

ExMCP now supports three MCP protocol versions:

### 2025-06-18 (Latest)
- **Features**: 
  - Structured tool output with schema validation
  - OAuth 2.1 authorization
  - HTTP MCP-Protocol-Version header requirement
- **Breaking Changes**: 
  - Batch requests removed
- **Default**: This is the default version when no header is provided

### 2025-03-26
- **Features**: 
  - Batch request support
  - All features from 2024-11-05
- **Usage**: Clients should explicitly request this version

### 2024-11-05 (Legacy)
- **Features**: 
  - Basic MCP functionality
  - Batch request support
- **Usage**: For backwards compatibility with older clients

## Version Negotiation

During initialization, the server negotiates the best mutually supported version:

```elixir
# Client sends supported versions
%{
  "method" => "initialize",
  "params" => %{
    "protocolVersions" => ["2025-06-18", "2025-03-26", "2024-11-05"]
  }
}

# Server responds with negotiated version
%{
  "result" => %{
    "protocolVersion" => "2025-06-18",
    "capabilities" => %{
      "experimental" => %{
        "protocolVersionHeader" => true,
        "structuredOutput" => false,  # Depends on feature flag
        "oauth2" => false             # Depends on feature flag
      }
    }
  }
}
```

## HTTP Header Validation

For HTTP transports, the `MCP-Protocol-Version` header is validated on each request:

```
POST /mcp HTTP/1.1
MCP-Protocol-Version: 2025-06-18
Content-Type: application/json

{...}
```

Missing headers default to `2025-06-18`. Invalid versions return `400 Bad Request`.

## Feature Detection

Use the negotiated version to determine available features:

```elixir
case context[:protocol_version] do
  "2025-06-18" ->
    # No batch support, but has structured output
    
  version when version in ["2025-03-26", "2024-11-05"] ->
    # Batch requests supported
end
```

## Migration Notes

- Clients using batch requests should specify version `2025-03-26` or `2024-11-05`
- New clients should use `2025-06-18` for latest features
- The protocol version header is only enforced when the feature flag is enabled