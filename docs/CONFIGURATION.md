# ExMCP Configuration Guide

This guide covers all configuration options available in ExMCP, from basic setup to advanced production configurations.

## Table of Contents

1. [Basic Configuration](#basic-configuration)
2. [Protocol Version Configuration](#protocol-version-configuration)
3. [Transport Configuration](#transport-configuration)
4. [Security Configuration](#security-configuration)
5. [Logging Configuration](#logging-configuration)
6. [Performance Configuration](#performance-configuration)
7. [Production Configuration](#production-configuration)
8. [Environment-Specific Configuration](#environment-specific-configuration)

## Basic Configuration

### Application Configuration

Configure ExMCP in your `config/config.exs`:

```elixir
# config/config.exs
config :ex_mcp,
  # Default protocol version
  protocol_version: "2025-03-26",
  
  # Global timeout settings
  default_timeout: 30_000,
  request_timeout: 15_000,
  
  # Connection settings
  auto_reconnect: true,
  reconnect_backoff: [initial: 1000, max: 30_000],
  
  # Logging level
  log_level: :info
```

### Mix Project Configuration

In your `mix.exs`:

```elixir
def deps do
  [
    {:ex_mcp, "~> 0.6.0"}
  ]
end
```

## Protocol Version Configuration

ExMCP supports multiple MCP protocol versions. Choose based on your needs:

### Available Versions

```elixir
config :ex_mcp,
  protocol_version: "2024-11-05"  # Maximum compatibility
  # OR
  protocol_version: "2025-03-26"  # Latest stable (recommended)
  # OR
  protocol_version: "2025-06-18"  # Latest features
```

### Version Feature Matrix

| Feature | 2024-11-05 | 2025-03-26 | 2025-06-18 |
|---------|:----------:|:----------:|:----------:|
| **Core Features** | | | |
| Tools, Resources, Prompts | ✅ | ✅ | ✅ |
| Bi-directional requests | ✅ | ✅ | ✅ |
| Request cancellation | ✅ | ✅ | ✅ |
| Progress notifications | ✅ | ✅ | ✅ |
| **2025-03-26 Features** | | | |
| Resource subscriptions | ❌ | ✅ | ✅ |
| Roots | ❌ | ✅ | ✅ |
| Structured logging | ❌ | ✅ | ✅ |
| Tool annotations | ❌ | ✅ | ✅ |
| **2025-06-18 Features** | | | |
| Structured tool output | ❌ | ❌ | ✅ |
| Elicitation support | ❌ | ❌ | ✅ |
| OAuth 2.1 Resource Server | ❌ | ❌ | ✅ |

### Recommendations

- **Production**: Use `"2025-03-26"` for latest stable features
- **Compatibility**: Use `"2024-11-05"` for maximum compatibility  
- **Latest**: Use `"2025-06-18"` for newest features (may have more breaking changes)

## Transport Configuration

### stdio Transport

For subprocess communication:

```elixir
# Server configuration
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :stdio
)

# Client configuration
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["python", "mcp-server.py"],
  args: ["--config", "prod.json"],
  timeout: 30_000,
  
  # Optional environment variables
  env: [{"PYTHONPATH", "/opt/myapp"}],
  
  # Working directory
  cd: "/path/to/server"
)
```

### HTTP Transport (Streamable HTTP)

For network communication with optional Server-Sent Events:

```elixir
# Server configuration
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,
  port: 8080,
  path: "/mcp",
  
  # SSE configuration
  sse_enabled: true,
  sse_path: "/events",
  
  # Security
  cors_enabled: true,
  allowed_origins: ["https://app.example.com"],
  
  # Connection limits
  max_connections: 1000,
  timeout: 60_000
)

# Client configuration
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:8080",
  endpoint: "/mcp/v1",
  
  # Authentication
  headers: [
    {"Authorization", "Bearer #{token}"},
    {"X-API-Key", api_key}
  ],
  
  # Connection options
  timeout: 30_000,
  request_timeout: 15_000,
  
  # SSE options
  use_sse: true,
  sse_timeout: 120_000
)
```

### Native BEAM Transport

For high-performance intra-cluster communication:

```elixir
# Service configuration using macro
defmodule MyToolService do
  use ExMCP.Service, name: :my_tools
  
  # Service automatically registers with ExMCP.Native
  @impl true
  def handle_mcp_request(method, params, state) do
    # Handle MCP requests
    {:ok, response, state}
  end
end

# Start service
{:ok, _} = MyToolService.start_link()

# Direct calls
{:ok, result} = ExMCP.Native.call(:my_tools, "list_tools", %{})

# Cross-node calls
{:ok, result} = ExMCP.Native.call(
  {:my_service, :"node@cluster.local"}, 
  "tools/call", 
  %{"name" => "calculator", "arguments" => %{"a" => 1, "b" => 2}}
)

# Configuration for Horde.Registry
config :horde, Horde.Registry,
  name: ExMCP.Native.Registry,
  keys: :unique,
  members: :auto
```

## Security Configuration

### Authentication Methods

#### Bearer Token Authentication

```elixir
# Client with bearer token
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  security: %{
    auth: {:bearer, "your-jwt-token"}
  }
)
```

#### API Key Authentication

```elixir
# Client with API key
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  headers: [{"X-API-Key", "your-api-key"}]
)
```

#### Basic Authentication

```elixir
# Client with basic auth
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  security: %{
    auth: {:basic, "username", "password"}
  }
)
```

#### OAuth 2.1 Configuration

```elixir
# OAuth 2.1 Resource Server (MCP 2025-06-18)
config :ex_mcp, :oauth,
  issuer: "https://auth.example.com",
  audience: "mcp-api",
  jwks_uri: "https://auth.example.com/.well-known/jwks.json",
  cache_jwks: true,
  cache_ttl: 3600
```

### TLS/SSL Configuration

```elixir
# Server with TLS
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,
  port: 8443,
  
  # TLS options
  https: [
    port: 8443,
    keyfile: "/path/to/private.key",
    certfile: "/path/to/certificate.crt",
    cacertfile: "/path/to/ca-bundle.crt"
  ]
)

# Client with TLS verification
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://secure.example.com:8443",
  
  # TLS options
  ssl: [
    verify: :verify_peer,
    cacertfile: "/path/to/ca-bundle.crt",
    depth: 2
  ]
)
```

### CORS Configuration

```elixir
# Server with CORS
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,
  
  # CORS settings
  cors_enabled: true,
  allowed_origins: [
    "https://app.example.com",
    "https://staging.example.com"
  ],
  allowed_methods: ["GET", "POST", "OPTIONS"],
  allowed_headers: ["Authorization", "Content-Type"],
  max_age: 3600
)
```

### Origin Validation

```elixir
# Client with origin validation
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  security: %{
    validate_origin: true,
    allowed_origins: ["https://app.example.com"]
  }
)
```

## Logging Configuration

### Global Logging

```elixir
# Set global ExMCP log level
ExMCP.Logging.set_global_level("debug")

# Available levels: "debug", "info", "warning", "error"
```

### Production Logger Configuration

ExMCP includes security audit logging with structured metadata. Configure appropriately for your environment:

#### Development Configuration

```elixir
# config/dev.exs
config :logger, :console,
  metadata: [:request_id, :tag, :audit, :client_id, :reason, :registration_type]
```

#### Production Configuration Options

**Option 1: JSON Structured Logging (Recommended)**

```elixir
# In mix.exs
{:logger_json, "~> 5.1"}

# In config/prod.exs
config :logger,
  backends: [LoggerJSON]

config :logger_json, :backend,
  metadata: :all,  # Captures ALL metadata automatically
  json_encoder: Jason,
  formatter: LoggerJSON.Formatters.GoogleCloudLogger
```

**Option 2: Separate Security Audit Logs**

```elixir
# config/prod.exs
config :logger,
  backends: [:console, {LoggerFileBackend, :security_audit}]

config :logger, :security_audit,
  path: "/var/log/ex_mcp/security_audit.log",
  level: :info,
  format: "$date $time [$level] $metadata $message\n",
  metadata: [:tag, :audit, :client_id, :reason, :registration_type],
  metadata_filter: [tag: :security_audit]  # Only security logs
```

**Option 3: External Log Aggregation**

```elixir
# For ELK Stack, Datadog, etc.
{:logstash_logger_backend, "~> 3.0"}

config :logger,
  backends: [{LogstashLoggerBackend, :logstash}]

config :logger, :logstash,
  host: "logstash.example.com",
  port: 5514,
  metadata: :all,
  type: "ex_mcp_security"
```

#### Logger Metadata Fields

ExMCP uses these metadata fields for security and debugging:

- **`:tag`** - Log categorization (e.g., `:security_audit`, `:client_registration`)
- **`:audit`** - Detailed audit log entries with timestamps and actions
- **`:client_id`** - Client identification for request tracking
- **`:reason`** - Failure reasons and error details
- **`:registration_type`** - Client registration type (`:static` or `:dynamic`)
- **`:request_id`** - Correlation ID for distributed tracing

### Structured Logging in Code

```elixir
# Server logging with automatic security sanitization
defmodule MyServer do
  use ExMCP.Server.Handler

  @impl true
  def handle_call_tool("login", params, state) do
    server = self()
    
    # This logs to both MCP clients AND Elixir Logger
    # Sensitive data is automatically sanitized
    ExMCP.Logging.info(server, "User login attempt", %{
      username: params["username"],
      password: params["password"],  # Will be sanitized to "***"
      timestamp: DateTime.utc_now()
    })
    
    # ... handle login logic
    {:ok, result, state}
  end
end

# All RFC 5424 log levels supported
ExMCP.Logging.debug(server, "Debug info", %{details: "..."})
ExMCP.Logging.info(server, "Operation completed") 
ExMCP.Logging.warning(server, "Deprecated feature used")
ExMCP.Logging.error(server, "Operation failed", %{error: "connection_timeout"})
ExMCP.Logging.critical(server, "System component failure")
```

## Performance Configuration

### Connection Pooling

```elixir
# HTTP client with connection pooling
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  
  # Connection pool settings
  pool_size: 10,
  pool_max_overflow: 5,
  pool_timeout: 5000,
  
  # Keep-alive settings
  recv_timeout: 30_000,
  keepalive_timeout: 60_000
)
```

### Native BEAM Optimization

```elixir
# Configure Horde for performance
config :horde, Horde.Registry,
  name: ExMCP.Native.Registry,
  keys: :unique,
  members: :auto,
  
  # Performance tuning
  sync_interval: 2000,
  max_delta_size: 200,
  delta_crdt: Horde.DeltaCrdt
```

### Progress Tracking Configuration

```elixir
# Configure progress tracking thresholds
config :ex_mcp, :progress,
  # Minimum interval between progress notifications (ms)
  min_interval: 100,
  
  # Maximum number of pending progress tokens
  max_pending: 1000,
  
  # Cleanup interval for expired tokens (ms)
  cleanup_interval: 30_000
```

## Production Configuration

### Complete Production Example

```elixir
# config/prod.exs
import Config

# ExMCP main configuration
config :ex_mcp,
  protocol_version: "2025-03-26",
  default_timeout: 30_000,
  request_timeout: 15_000,
  auto_reconnect: true,
  reconnect_backoff: [initial: 1000, max: 30_000],
  log_level: :info

# Security configuration
config :ex_mcp, :security,
  enforce_https: true,
  validate_origins: true,
  max_request_size: 10_485_760  # 10MB

# OAuth 2.1 configuration (if using)
config :ex_mcp, :oauth,
  issuer: {:system, "OAUTH_ISSUER"},
  audience: {:system, "OAUTH_AUDIENCE"},
  jwks_uri: {:system, "OAUTH_JWKS_URI"},
  cache_jwks: true,
  cache_ttl: 3600

# Logging for production
config :logger,
  backends: [LoggerJSON]

config :logger_json, :backend,
  metadata: :all,
  json_encoder: Jason,
  formatter: LoggerJSON.Formatters.GoogleCloudLogger

# Horde configuration for Native BEAM
config :horde, Horde.Registry,
  name: ExMCP.Native.Registry,
  keys: :unique,
  members: :auto,
  sync_interval: 2000,
  max_delta_size: 200

# SSL/TLS configuration
config :ex_mcp, :ssl,
  verify: :verify_peer,
  depth: 2,
  versions: [:"tlsv1.2", :"tlsv1.3"],
  ciphers: [
    "ECDHE-RSA-AES256-GCM-SHA384",
    "ECDHE-RSA-AES128-GCM-SHA256"
  ]
```

### Environment Variables

Support for runtime configuration via environment variables:

```elixir
# config/runtime.exs
import Config

if config_env() == :prod do
  config :ex_mcp,
    protocol_version: System.get_env("MCP_PROTOCOL_VERSION", "2025-03-26"),
    default_timeout: String.to_integer(System.get_env("MCP_TIMEOUT", "30000")),
    log_level: String.to_atom(System.get_env("MCP_LOG_LEVEL", "info"))

  # OAuth configuration from environment
  if oauth_issuer = System.get_env("OAUTH_ISSUER") do
    config :ex_mcp, :oauth,
      issuer: oauth_issuer,
      audience: System.get_env("OAUTH_AUDIENCE"),
      jwks_uri: System.get_env("OAUTH_JWKS_URI")
  end
end
```

## Environment-Specific Configuration

### Development Configuration

```elixir
# config/dev.exs
import Config

config :ex_mcp,
  protocol_version: "2025-03-26",
  log_level: :debug,
  auto_reconnect: false  # Easier debugging

# Verbose logging for development
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :tag, :audit, :client_id]
```

### Test Configuration

```elixir
# config/test.exs
import Config

config :ex_mcp,
  protocol_version: "2025-03-26",
  default_timeout: 5_000,  # Faster tests
  log_level: :warning,     # Reduce test noise
  auto_reconnect: false    # Predictable test behavior

# Test logging
config :logger,
  level: :warning,
  backends: []  # Disable logging in tests
```

### Staging Configuration

```elixir
# config/staging.exs
import Config

# Similar to production but with more lenient settings
config :ex_mcp,
  protocol_version: "2025-06-18",  # Test latest features
  log_level: :debug,               # More verbose for debugging
  
  # Relaxed security for testing
  security: %{
    enforce_https: false,
    validate_origins: false
  }
```

## Configuration Validation

### Runtime Validation

```elixir
# Add to application startup
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    # Validate ExMCP configuration on startup
    :ok = ExMCP.Config.validate!()
    
    children = [
      # ... your children
    ]
    
    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

### Configuration Helpers

```elixir
# Check current configuration
ExMCP.Config.get(:protocol_version)
ExMCP.Config.get(:default_timeout)

# Validate specific settings
ExMCP.Config.validate_protocol_version("2025-03-26")
ExMCP.Config.validate_transport_config(:http, options)
```

## Troubleshooting Configuration

### Common Issues

1. **Protocol Version Mismatch**
   ```
   Error: Unsupported protocol version "invalid"
   Solution: Use "2024-11-05", "2025-03-26", or "2025-06-18"
   ```

2. **Transport Configuration**
   ```
   Error: Invalid transport options
   Solution: Check transport-specific configuration requirements
   ```

3. **Authentication Failures**
   ```
   Error: Authentication failed
   Solution: Verify tokens, certificates, and authentication method
   ```

### Configuration Testing

```elixir
# Test configuration in IEx
iex> ExMCP.Config.test_config()
%{
  protocol_version: "2025-03-26",
  transports: [:stdio, :http, :native],
  security: %{...},
  valid: true
}
```

### Debug Configuration

Enable debug logging to troubleshoot configuration issues:

```elixir
# Temporarily enable debug logging
ExMCP.Logging.set_global_level("debug")

# Check what configuration is actually loaded
ExMCP.Config.dump()
```

For more configuration help, see:
- [Security Guide](SECURITY.md) for security-specific configuration
- [Transport Guide](TRANSPORT_GUIDE.md) for transport optimization
- [Development Guide](DEVELOPMENT.md) for development setup