# ExMCP Transport Guide

This guide provides comprehensive information about ExMCP's transport layers, helping you choose and configure the right transport for your use case.

## Table of Contents

1. [Transport Overview](#transport-overview)
2. [stdio Transport](#stdio-transport)
3. [HTTP Transport (Streamable HTTP)](#http-transport-streamable-http)
4. [Native BEAM Transport](#native-beam-transport)
5. [Transport Comparison](#transport-comparison)
6. [Performance Optimization](#performance-optimization)
7. [Security Considerations](#security-considerations)
8. [Troubleshooting](#troubleshooting)

## Transport Overview

ExMCP supports three transport layers, each optimized for different use cases:

| Transport | Latency | Best For | Security Model |
|-----------|---------|----------|----------------|
| **Native BEAM** | ~15μs | Internal services | Process isolation + Erlang distribution |
| **stdio** | ~1-5ms | External tools | Process isolation |
| **HTTP/SSE** | ~5-20ms | Network clients | TLS + Authentication headers |

### Choosing the Right Transport

**Use Native BEAM when:**
- Communicating within trusted Elixir clusters
- Need ultra-low latency (~15μs)
- Want zero serialization overhead
- Building microservices in Elixir

**Use stdio when:**
- Integrating external command-line tools
- Subprocess communication
- Language interoperability (Python, Node.js, etc.)
- Legacy MCP server compatibility

**Use HTTP when:**
- Network communication required
- Web application integration
- Need standard HTTP security
- Client-server across different machines

## stdio Transport

Best for subprocess communication and external tool integration.

### Server Configuration

```elixir
# Basic stdio server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :stdio
)
```

### Client Configuration

```elixir
# Connect to external MCP server
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["python", "mcp-server.py"],
  args: ["--config", "prod.json"],
  
  # Optional configuration
  timeout: 30_000,
  env: [{"PYTHONPATH", "/opt/myapp"}],
  cd: "/path/to/server"
)
```

### Advanced stdio Configuration

```elixir
# Comprehensive stdio client setup
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  
  # Command and arguments
  command: ["node", "dist/index.js"],
  args: ["--port", "8080", "--verbose"],
  
  # Environment
  env: [
    {"NODE_ENV", "production"},
    {"LOG_LEVEL", "info"}
  ],
  
  # Working directory
  cd: "/opt/mcp-server",
  
  # Process options
  timeout: 60_000,
  kill_timeout: 5_000,
  
  # Reconnection
  auto_reconnect: true,
  max_reconnect_attempts: 5,
  reconnect_backoff: [initial: 1000, max: 30_000]
)
```

### stdio Process Management

```elixir
# Monitor subprocess health
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["python", "server.py"],
  
  # Health check configuration
  health_check: [
    enabled: true,
    interval: 10_000,
    timeout: 5_000,
    failure_threshold: 3
  ]
)

# Manual health check
case ExMCP.Client.health_check(client) do
  :ok -> IO.puts("Server healthy")
  {:error, reason} -> IO.puts("Server unhealthy: #{reason}")
end
```

### stdio Error Handling

```elixir
# Handle subprocess failures
defmodule StdioHandler do
  def handle_process_exit(client, exit_code, reason) do
    Logger.warning("MCP server exited: code=#{exit_code}, reason=#{reason}")
    
    case exit_code do
      0 -> :normal_exit
      1 -> :restart_required
      _ -> :error
    end
  end
  
  def handle_connection_lost(client, reason) do
    Logger.error("Connection lost: #{reason}")
    # Implement custom reconnection logic
  end
end
```

## HTTP Transport (Streamable HTTP)

For network communication with optional Server-Sent Events (SSE) streaming.

### Server Configuration

```elixir
# Basic HTTP server
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,
  port: 8080,
  path: "/mcp"
)
```

### Advanced HTTP Server Configuration

```elixir
# Production HTTP server with all features
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,
  
  # Basic settings
  port: 8080,
  path: "/mcp",
  
  # SSE configuration
  sse_enabled: true,
  sse_path: "/events",
  sse_heartbeat_interval: 30_000,
  
  # Security
  cors_enabled: true,
  allowed_origins: [
    "https://app.example.com",
    "https://staging.example.com"
  ],
  allowed_methods: ["GET", "POST", "OPTIONS"],
  allowed_headers: ["Authorization", "Content-Type"],
  max_age: 3600,
  
  # Connection limits
  max_connections: 1000,
  timeout: 60_000,
  request_timeout: 30_000,
  
  # TLS (for HTTPS)
  https: [
    port: 8443,
    keyfile: "/path/to/private.key",
    certfile: "/path/to/certificate.crt",
    cacertfile: "/path/to/ca-bundle.crt"
  ]
)
```

### Client Configuration

```elixir
# Basic HTTP client
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "http://localhost:8080",
  endpoint: "/mcp/v1"
)
```

### Advanced HTTP Client Configuration

```elixir
# Production HTTP client with authentication and optimization
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  endpoint: "/mcp/v1",
  
  # Authentication
  headers: [
    {"Authorization", "Bearer #{jwt_token}"},
    {"X-API-Key", api_key},
    {"User-Agent", "MyApp/1.0.0"}
  ],
  
  # Connection options
  timeout: 30_000,
  request_timeout: 15_000,
  
  # Connection pooling
  pool_size: 10,
  pool_max_overflow: 5,
  pool_timeout: 5000,
  
  # Keep-alive
  recv_timeout: 30_000,
  keepalive_timeout: 60_000,
  
  # SSE options
  use_sse: true,
  sse_timeout: 120_000,
  sse_reconnect: true,
  
  # TLS options
  ssl: [
    verify: :verify_peer,
    cacertfile: "/path/to/ca-bundle.crt",
    depth: 2,
    versions: [:"tlsv1.2", :"tlsv1.3"]
  ],
  
  # Retry configuration
  retry: [
    max_attempts: 3,
    backoff_factor: 1.5,
    initial_delay: 1000
  ]
)
```

### Server-Sent Events (SSE)

SSE enables real-time streaming for notifications and progress updates:

```elixir
# Server with SSE support
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,
  
  # Enable SSE
  sse_enabled: true,
  sse_path: "/events",
  sse_heartbeat_interval: 30_000,
  
  # SSE security
  sse_cors_enabled: true,
  sse_allowed_origins: ["https://app.example.com"]
)

# Client using SSE
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  
  # Enable SSE client-side
  use_sse: true,
  sse_timeout: 120_000,
  sse_reconnect: true
)

# SSE event handling
defmodule SSEHandler do
  def handle_sse_event(client, event) do
    case event do
      %{"type" => "progress", "data" => data} ->
        handle_progress_update(data)
      
      %{"type" => "notification", "data" => data} ->
        handle_notification(data)
      
      %{"type" => "resource_updated", "data" => data} ->
        handle_resource_update(data)
    end
  end
end
```

### HTTP Security Configuration

```elixir
# Server with comprehensive security
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,
  
  # Basic security
  cors_enabled: true,
  allowed_origins: ["https://trusted-app.com"],
  
  # Authentication middleware
  middleware: [
    {MyApp.AuthMiddleware, [
      required_scopes: ["mcp:read", "mcp:write"],
      token_validation: :jwt
    ]}
  ],
  
  # Rate limiting
  rate_limit: [
    requests_per_minute: 1000,
    burst_size: 100
  ],
  
  # Security headers
  security_headers: [
    {"X-Content-Type-Options", "nosniff"},
    {"X-Frame-Options", "DENY"},
    {"X-XSS-Protection", "1; mode=block"}
  ]
)
```

## Native BEAM Transport

Ultra-fast service dispatcher for trusted Elixir clusters.

### Basic Service Creation

```elixir
# Create a service using the ExMCP.Service macro
defmodule MyToolService do
  use ExMCP.Service, name: :my_tools

  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "ping",
        "description" => "Test tool",
        "inputSchema" => %{"type" => "object", "properties" => %{}}
      }
    ]
    {:ok, %{"tools" => tools}, state}
  end

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "ping"}, state) do
    {:ok, %{"content" => [%{"type" => "text", "text" => "Pong!"}]}, state}
  end

  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
end

# Start your service (automatically registers with ExMCP.Native)
{:ok, _} = MyToolService.start_link()
```

### Advanced Service Configuration

```elixir
defmodule AdvancedService do
  use ExMCP.Service, 
    name: :advanced_service,
    # Service metadata
    metadata: %{
      description: "Advanced MCP service",
      version: "1.0.0",
      capabilities: ["tools", "resources", "prompts"]
    }

  @impl true
  def init(_args) do
    # Custom initialization
    state = %{
      start_time: DateTime.utc_now(),
      request_count: 0
    }
    {:ok, state}
  end

  @impl true
  def handle_mcp_request(method, params, state) do
    # Update request counter
    new_state = %{state | request_count: state.request_count + 1}
    
    # Log request
    Logger.info("Handling #{method}", %{
      service: :advanced_service,
      request_count: new_state.request_count
    })
    
    # Handle request
    case method do
      "ping" -> {:ok, %{"message" => "pong"}, new_state}
      _ -> {:error, %{"code" => -32601, "message" => "Unknown method"}, new_state}
    end
  end
end
```

### Service Discovery and Communication

```elixir
# Direct service calls (same node)
{:ok, tools} = ExMCP.Native.call(:my_tools, "list_tools", %{})

# Cross-node communication (distributed)
{:ok, result} = ExMCP.Native.call(
  {:data_service, :"worker@cluster.local"}, 
  "process_dataset", 
  %{"dataset_id" => "abc123"}
)

# Fire-and-forget notifications
:ok = ExMCP.Native.notify(:event_service, "resource_updated", %{
  "uri" => "file:///config.json",
  "type" => "modified"
})

# Service discovery
services = ExMCP.Native.list_services()
#=> [{:my_tools, #PID<0.123.0>, %{registered_at: ~U[...]}}, ...]

# Check service availability
available? = ExMCP.Native.service_available?(:my_service)
```

### Performance Features

#### Zero Serialization Overhead

```elixir
# Data passed directly as Elixir terms between processes
large_data = File.read!("large_dataset.json")  # 10MB file

# No serialization overhead - data passed by reference
{:ok, result} = ExMCP.Native.call(:data_service, "process_data", %{
  "data" => large_data
})

# Performance characteristics:
# - Local calls: ~15μs latency
# - Cross-node calls: ~50μs latency  
# - Memory overhead: Single Horde.Registry entry per service
```

#### Distributed Service Discovery

```elixir
# Services automatically register when using ExMCP.Service macro
defmodule CalculatorService do
  use ExMCP.Service, name: :calculator
  # Service automatically registered on startup
end

# Discover all available services (across all nodes)
services = ExMCP.Native.list_services()
#=> [{:calculator, #PID<0.123.0>, %{registered_at: ~U[...]}}, ...]

# Check service availability
if ExMCP.Native.service_available?(:calculator) do
  {:ok, result} = ExMCP.Native.call(:calculator, "add", %{"a" => 1, "b" => 2})
end
```

### Horde Configuration

```elixir
# config/config.exs
config :horde, Horde.Registry,
  name: ExMCP.Native.Registry,
  keys: :unique,
  members: :auto,
  
  # Performance tuning
  sync_interval: 2000,
  max_delta_size: 200,
  delta_crdt: Horde.DeltaCrdt

# For distributed clusters
config :libcluster,
  topologies: [
    mcp_cluster: [
      strategy: Cluster.Strategy.Epmd,
      config: [hosts: [:"node1@host", :"node2@host", :"node3@host"]]
    ]
  ]
```

### Resilience Patterns

Optional resilience when needed without compromising core performance:

```elixir
# Retry with exponential backoff
{:ok, result} = ExMCP.Resilience.call_with_retry(
  :flaky_service,
  "process_data",
  %{"input" => "data"},
  max_attempts: 3,
  backoff: :exponential
)

# Fallback for unavailable services
result = ExMCP.Resilience.call_with_fallback(
  :unreliable_service,
  "get_data",
  %{},
  fallback: fn -> {:ok, %{"data" => "cached_value"}} end
)

# Circuit breaker protection
protected_call = ExMCP.Resilience.protect(
  fn -> ExMCP.Native.call(:external_service, "api_call", params) end,
  failure_threshold: 5,
  reset_timeout: 30_000
)

{:ok, result} = protected_call.()
```

## Transport Comparison

### Performance Benchmarks

| Operation | Native BEAM | stdio | HTTP |
|-----------|-------------|-------|------|
| **Simple tool call** | ~15μs | ~1-2ms | ~5-10ms |
| **Large payload (1MB)** | ~100μs | ~10-20ms | ~50-100ms |
| **1000 requests** | ~15ms | ~1-2s | ~5-10s |
| **Cross-network** | ~50μs* | N/A | ~20-50ms |

*Via Erlang distribution

### Memory Overhead

| Transport | Per Connection | Serialization | Notes |
|-----------|----------------|---------------|-------|
| **Native BEAM** | ~1KB | None | Registry entry only |
| **stdio** | ~10KB | JSON | Process + pipes |
| **HTTP** | ~5KB | JSON | HTTP connection state |

### Scalability Limits

| Transport | Max Connections | Bottlenecks |
|-----------|-----------------|-------------|
| **Native BEAM** | ~1M processes | VM process limit |
| **stdio** | ~1K processes | OS process limit |
| **HTTP** | ~10K connections | TCP/socket limits |

## Performance Optimization

### Native BEAM Optimization

```elixir
# Optimize for high-throughput scenarios
defmodule HighThroughputService do
  use ExMCP.Service, name: :high_throughput
  
  # Pre-compile responses for common requests
  @ping_response %{"content" => [%{"type" => "text", "text" => "pong"}]}
  
  @impl true
  def handle_mcp_request("ping", _params, state) do
    # Return pre-compiled response (no allocation)
    {:ok, @ping_response, state}
  end
  
  # Batch processing for efficiency
  @impl true
  def handle_mcp_request("batch_process", %{"items" => items}, state) do
    # Process in parallel using Task.async_stream
    results = items
    |> Task.async_stream(&process_item/1, max_concurrency: System.schedulers_online())
    |> Enum.map(fn {:ok, result} -> result end)
    
    {:ok, %{"results" => results}, state}
  end
end
```

### HTTP Optimization

```elixir
# Optimize HTTP client for high performance
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://api.example.com",
  
  # Connection pooling for reuse
  pool_size: 20,
  pool_max_overflow: 10,
  
  # Keep connections alive
  keepalive_timeout: 300_000,
  
  # Optimize for throughput
  request_timeout: 5_000,
  recv_timeout: 10_000,
  
  # HTTP/2 support (if server supports)
  http_version: :"2.0",
  
  # Compression
  compression: true
)
```

### stdio Optimization

```elixir
# Optimize stdio for reduced latency
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["python", "-u", "server.py"],  # Unbuffered output
  
  # Reduce JSON parsing overhead
  json_library: :jiffy,  # Faster JSON parser
  
  # Optimize buffer sizes
  buffer_size: 64_000,
  
  # Reduce process overhead
  spawn_timeout: 1_000,
  
  # Pre-allocate message buffers
  preallocate_buffers: true
)
```

## Security Considerations

### Transport Security Matrix

| Feature | Native BEAM | stdio | HTTP |
|---------|-------------|-------|------|
| **Process Isolation** | ✅ | ✅ | ✅ |
| **Network Encryption** | ✅* | ❌ | ✅ |
| **Authentication** | Process-based | Process-based | Header-based |
| **Authorization** | OTP permissions | OS permissions | Token/OAuth |

*Via Erlang distribution TLS

### Native BEAM Security

```elixir
# Secure service registration
defmodule SecureService do
  use ExMCP.Service, 
    name: :secure_service,
    # Access control
    access_control: [
      allowed_nodes: [:"trusted@cluster.local"],
      required_permissions: [:mcp_access]
    ]

  @impl true
  def handle_mcp_request(method, params, state) do
    # Validate caller permissions
    case validate_caller_permissions() do
      :ok -> handle_request(method, params, state)
      {:error, reason} -> {:error, %{"code" => -32000, "message" => reason}, state}
    end
  end
end

# Configure Erlang distribution security
# config/config.exs
config :kernel,
  inet_dist_use_interface: {127, 0, 0, 1},  # Local only
  inet_dist_listen_min: 9100,
  inet_dist_listen_max: 9200

# Use TLS for distributed connections
config :ssl,
  protocol_version: [:"tlsv1.2", :"tlsv1.3"]
```

### stdio Security

```elixir
# Secure subprocess execution
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["python", "server.py"],
  
  # Limit environment
  env: [
    {"PATH", "/usr/local/bin:/usr/bin:/bin"},
    {"PYTHONPATH", "/opt/safe-packages"}
  ],
  
  # Change working directory
  cd: "/opt/sandbox",
  
  # Resource limits (if supported by OS)
  limits: [
    memory_mb: 100,
    cpu_percent: 50,
    execution_time_seconds: 300
  ],
  
  # Input validation
  input_validator: &validate_mcp_message/1
)
```

### HTTP Security

```elixir
# Comprehensive HTTP security
{:ok, server} = ExMCP.Server.start_link(
  handler: MyHandler,
  transport: :http,
  
  # TLS configuration
  https: [
    port: 8443,
    keyfile: "/path/to/private.key",
    certfile: "/path/to/certificate.crt",
    
    # Strong TLS configuration
    versions: [:"tlsv1.2", :"tlsv1.3"],
    ciphers: [
      "ECDHE-RSA-AES256-GCM-SHA384",
      "ECDHE-RSA-AES128-GCM-SHA256"
    ],
    honor_cipher_order: true
  ],
  
  # Security middleware
  middleware: [
    # Rate limiting
    {Plug.RateLimit, [
      max_requests: 1000,
      window_seconds: 60
    ]},
    
    # Authentication
    {MyApp.AuthPlug, [
      verify_token: true,
      required_scopes: ["mcp:access"]
    ]},
    
    # Input validation
    {MyApp.ValidationPlug, [
      max_payload_size: 10_485_760,  # 10MB
      validate_json: true
    ]}
  ]
)
```

## Troubleshooting

### Common Issues

#### Native BEAM Issues

**Service not found:**
```elixir
# Check if service is registered
ExMCP.Native.list_services()

# Verify Horde registry status
Horde.Registry.lookup(ExMCP.Native.Registry, :my_service)

# Check node connectivity
Node.list()
```

**Performance issues:**
```elixir
# Monitor service performance
:observer.start()

# Check registry size
Horde.Registry.count(ExMCP.Native.Registry)

# Profile service calls
:fprof.start()
{:ok, result} = ExMCP.Native.call(:my_service, "method", %{})
:fprof.stop()
:fprof.analyse()
```

#### stdio Issues

**Process won't start:**
```bash
# Check command availability
which python

# Test command manually
python mcp-server.py --help

# Check permissions
ls -la mcp-server.py
```

**Communication timeout:**
```elixir
# Increase timeouts
{:ok, client} = ExMCP.Client.start_link(
  transport: :stdio,
  command: ["slow-server"],
  timeout: 60_000,
  request_timeout: 30_000
)

# Enable debug logging
ExMCP.Logging.set_global_level("debug")
```

#### HTTP Issues

**Connection refused:**
```bash
# Check if server is running
netstat -tlnp | grep 8080

# Test with curl
curl -X POST http://localhost:8080/mcp/v1 \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"ping","id":1}'
```

**SSL/TLS errors:**
```elixir
# Disable SSL verification for testing (NOT for production)
{:ok, client} = ExMCP.Client.start_link(
  transport: :http,
  url: "https://localhost:8443",
  ssl: [verify: :verify_none]
)

# Check certificate validity
openssl s_client -connect localhost:8443 -servername localhost
```

### Debug Configuration

```elixir
# Enable comprehensive debug logging
config :logger,
  level: :debug

config :ex_mcp,
  log_level: :debug,
  
  # Transport-specific debugging
  debug_transports: [:stdio, :http, :native],
  
  # Log all requests/responses
  log_requests: true,
  log_responses: true
```

### Performance Monitoring

```elixir
# Monitor transport performance
defmodule TransportMonitor do
  def start_monitoring() do
    # Native BEAM monitoring
    :telemetry.attach("native-calls", 
      [:ex_mcp, :native, :call], 
      &handle_native_call/4, nil)
    
    # HTTP monitoring
    :telemetry.attach("http-requests", 
      [:ex_mcp, :http, :request], 
      &handle_http_request/4, nil)
  end
  
  def handle_native_call(_event, measurements, metadata, _config) do
    IO.puts("Native call: #{metadata.method} took #{measurements.duration}μs")
  end
  
  def handle_http_request(_event, measurements, metadata, _config) do
    IO.puts("HTTP request: #{metadata.method} took #{measurements.duration}ms")
  end
end
```

For more specific transport help:
- [Configuration Guide](CONFIGURATION.md) for detailed configuration options
- [Security Guide](SECURITY.md) for security best practices
- [Development Guide](DEVELOPMENT.md) for development and testing setup