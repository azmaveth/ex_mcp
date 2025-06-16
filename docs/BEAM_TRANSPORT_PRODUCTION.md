# Native BEAM Transport - Production Deployment Guide

ExMCP's Native BEAM transport provides enterprise-grade capabilities for high-performance MCP deployments within Elixir clusters. This document covers production deployment patterns and best practices.

## Overview

The Native BEAM transport offers a complete production solution with:

- ⚡ **Zero Serialization** - Direct process communication with no JSON overhead
- 🔍 **Service Discovery** - Built-in Registry-based service discovery  
- 🛡️ **OTP Security** - Process isolation and supervision
- 📊 **Observability** - Standard OTP telemetry and monitoring
- 🔄 **Fault Tolerance** - Automatic restart and recovery through supervision

## Architecture

### Production Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                 Native BEAM Transport                       │
├─────────────────────────────────────────────────────────────┤
│ Application Layer                                           │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ MCP Services (GenServers)                               │ │
│ │ ┌──────────────┐ ┌──────────────┐ ┌──────────────────┐ │ │
│ │ │ Calculator   │ │ File Service │ │ Data Processor   │ │ │
│ │ │ Service      │ │              │ │                  │ │ │
│ │ └──────────────┘ └──────────────┘ └──────────────────┘ │ │
│ └─────────────────────────────────────────────────────────┘ │
│                              │                             │
│                              ▼                             │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ Registry Layer (Service Discovery)                      │ │
│ │ • Service Registration                                  │ │
│ │ • Cross-node Discovery                                 │ │
│ │ • Health Monitoring                                    │ │
│ └─────────────────────────────────────────────────────────┘ │
│                              │                             │
│                              ▼                             │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ OTP Supervision Tree                                    │ │
│ │ • Process Supervision                                   │ │
│ │ • Automatic Restart                                     │ │
│ │ • Fault Isolation                                       │ │
│ └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

## Production Patterns

### Service Architecture

```elixir
defmodule MyApp.MCPSupervisor do
  @moduledoc """
  Production supervisor for MCP services.
  """
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def init(_init_arg) do
    children = [
      # Core services
      {MyApp.CalculatorService, []},
      {MyApp.FileService, []},
      {MyApp.DataProcessor, []},
      
      # Supporting services
      {MyApp.HealthMonitor, []},
      {MyApp.MetricsCollector, []}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

### Service Implementation

```elixir
defmodule MyApp.CalculatorService do
  @moduledoc """
  Production calculator service with comprehensive error handling.
  """
  use GenServer
  require Logger

  @service_name :calculator_service

  # Client API

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  # Server Implementation

  def init(_args) do
    Logger.info("Starting calculator service...")
    
    # Register with Native BEAM transport
    case ExMCP.Transport.Native.register_service(@service_name) do
      :ok ->
        Logger.info("Calculator service registered successfully")
        {:ok, %{started_at: DateTime.utc_now(), request_count: 0}}
        
      {:error, reason} ->
        Logger.error("Failed to register calculator service: #{inspect(reason)}")
        {:stop, reason}
    end
  end

  def handle_call({:mcp_request, %{"method" => "list_tools"}}, _from, state) do
    tools = [
      %{
        "name" => "add",
        "description" => "Add two numbers",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "a" => %{"type" => "number"},
            "b" => %{"type" => "number"}
          },
          "required" => ["a", "b"]
        }
      },
      %{
        "name" => "multiply", 
        "description" => "Multiply two numbers",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "a" => %{"type" => "number"},
            "b" => %{"type" => "number"}
          },
          "required" => ["a", "b"]
        }
      }
    ]
    
    new_state = %{state | request_count: state.request_count + 1}
    {:reply, {:ok, %{"tools" => tools}}, new_state}
  end

  def handle_call({:mcp_request, %{"method" => "tools/call", "params" => %{"name" => "add", "arguments" => %{"a" => a, "b" => b}}}}, _from, state) 
      when is_number(a) and is_number(b) do
    
    result = a + b
    content = [%{
      "type" => "text",
      "text" => "#{a} + #{b} = #{result}"
    }]
    
    new_state = %{state | request_count: state.request_count + 1}
    {:reply, {:ok, %{"content" => content}}, new_state}
  end

  def handle_call({:mcp_request, %{"method" => "tools/call", "params" => %{"name" => "multiply", "arguments" => %{"a" => a, "b" => b}}}}, _from, state) 
      when is_number(a) and is_number(b) do
    
    result = a * b
    content = [%{
      "type" => "text", 
      "text" => "#{a} × #{b} = #{result}"
    }]
    
    new_state = %{state | request_count: state.request_count + 1}
    {:reply, {:ok, %{"content" => content}}, new_state}
  end

  def handle_call({:mcp_request, %{"method" => "tools/call", "params" => %{"name" => tool_name}}}, _from, state) do
    error = %{
      "code" => -32601,
      "message" => "Unknown tool: #{tool_name}"
    }
    {:reply, {:error, error}, state}
  end

  def handle_call({:mcp_request, message}, _from, state) do
    Logger.warning("Unhandled MCP request: #{inspect(message)}")
    error = %{
      "code" => -32600,
      "message" => "Invalid request"
    }
    {:reply, {:error, error}, state}
  end

  def handle_call(:get_stats, _from, state) do
    stats = %{
      started_at: state.started_at,
      request_count: state.request_count,
      uptime_seconds: DateTime.diff(DateTime.utc_now(), state.started_at)
    }
    {:reply, stats, state}
  end

  def terminate(reason, _state) do
    Logger.info("Calculator service terminating: #{inspect(reason)}")
    ExMCP.Transport.Native.unregister_service(@service_name)
    :ok
  end
end
```

## Service Discovery and Health Monitoring

### Health Monitor Service

```elixir
defmodule MyApp.HealthMonitor do
  @moduledoc """
  Monitors health of all MCP services.
  """
  use GenServer
  require Logger

  @check_interval 30_000  # 30 seconds

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_args) do
    Logger.info("Starting health monitor...")
    schedule_health_check()
    {:ok, %{last_check: DateTime.utc_now(), failed_services: MapSet.new()}}
  end

  def handle_info(:health_check, state) do
    Logger.debug("Running health check...")
    
    services = ExMCP.Transport.Native.list_services()
    failed_services = check_service_health(services)
    
    if MapSet.size(failed_services) > 0 do
      Logger.warning("Failed services detected: #{inspect(MapSet.to_list(failed_services))}")
    end
    
    schedule_health_check()
    new_state = %{state | last_check: DateTime.utc_now(), failed_services: failed_services}
    {:noreply, new_state}
  end

  defp check_service_health(services) do
    services
    |> Enum.reduce(MapSet.new(), fn {service_name, pid, _meta}, failed ->
      if Process.alive?(pid) do
        # Service process is alive
        failed
      else
        Logger.error("Service #{service_name} process is dead")
        MapSet.put(failed, service_name)
      end
    end)
  end

  defp schedule_health_check do
    Process.send_after(self(), :health_check, @check_interval)
  end

  def get_health_status do
    GenServer.call(__MODULE__, :get_health)
  end

  def handle_call(:get_health, _from, state) do
    services = ExMCP.Transport.Native.list_services()
    health_status = %{
      total_services: length(services),
      failed_services: MapSet.size(state.failed_services),
      last_check: state.last_check,
      status: if(MapSet.size(state.failed_services) == 0, do: :healthy, else: :degraded)
    }
    {:reply, health_status, state}
  end
end
```

## Application Configuration

### Application Module

```elixir
defmodule MyApp.Application do
  @moduledoc """
  Production application with MCP services.
  """
  use Application
  require Logger

  def start(_type, _args) do
    Logger.info("Starting MyApp with MCP services...")
    
    children = [
      # Start the Registry for Native BEAM transport
      {Registry, keys: :unique, name: ExMCP.Registry},
      
      # Start telemetry and monitoring
      MyApp.Telemetry,
      
      # Start MCP services
      MyApp.MCPSupervisor,
      
      # Start health monitoring (after services)
      MyApp.HealthMonitor
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    case Supervisor.start_link(children, opts) do
      {:ok, pid} ->
        Logger.info("MyApp started successfully")
        {:ok, pid}
        
      {:error, reason} ->
        Logger.error("Failed to start MyApp: #{inspect(reason)}")
        {:error, reason}
    end
  end
end
```

### Configuration

```elixir
# config/config.exs
import Config

# Configure logging for production
config :logger,
  level: :info,
  backends: [:console, {LoggerFileBackend, :info}]

config :logger, :info,
  path: "/var/log/myapp/info.log",
  level: :info

# Configure telemetry
config :telemetry_poller, :default, []

# Configure MCP services
config :my_app, :mcp_services,
  calculator_service: [
    # Service-specific configuration
  ],
  file_service: [
    base_path: "/var/data/myapp"
  ]
```

## Monitoring and Observability

### Telemetry Integration

```elixir
defmodule MyApp.Telemetry do
  @moduledoc """
  Telemetry setup for MCP services.
  """
  use Supervisor
  require Logger

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  def init(_arg) do
    children = [
      # Start telemetry metrics
      {TelemetryMetrics.ConsoleReporter, 
       metrics: [
         # Service call metrics
         counter("mcp.service.call.count", tags: [:service, :method]),
         distribution("mcp.service.call.duration", tags: [:service, :method]),
         
         # Service health metrics
         gauge("mcp.service.count", tags: [:status]),
         last_value("mcp.service.uptime", tags: [:service])
       ]},
       
      # Telemetry poller for periodic metrics
      {TelemetryPoller,
       measurements: [
         {MyApp.Metrics, :measure_service_count, []},
         {MyApp.Metrics, :measure_service_uptime, []}
       ],
       period: 10_000}  # Every 10 seconds
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule MyApp.Metrics do
  @moduledoc """
  Custom metrics collection for MCP services.
  """
  require Logger

  def measure_service_count do
    services = ExMCP.Transport.Native.list_services()
    healthy_count = Enum.count(services, fn {_name, pid, _meta} -> Process.alive?(pid) end)
    total_count = length(services)
    
    :telemetry.execute([:mcp, :service, :count], %{total: total_count, healthy: healthy_count})
  end

  def measure_service_uptime do
    services = ExMCP.Transport.Native.list_services()
    
    for {service_name, pid, meta} <- services do
      if Process.alive?(pid) do
        registered_at = Map.get(meta, :registered_at)
        if registered_at do
          uptime = DateTime.diff(DateTime.utc_now(), registered_at)
          :telemetry.execute([:mcp, :service, :uptime], %{seconds: uptime}, %{service: service_name})
        end
      end
    end
  end
end
```

## Performance Optimization

### Service Call Patterns

```elixir
defmodule MyApp.ServiceClient do
  @moduledoc """
  Optimized client patterns for service calls.
  """
  require Logger

  @default_timeout 5_000

  def call_with_retry(service_id, method, params, opts \\ []) do
    max_retries = Keyword.get(opts, :max_retries, 3)
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    
    do_call_with_retry(service_id, method, params, timeout, max_retries, max_retries)
  end

  defp do_call_with_retry(_service_id, _method, _params, _timeout, 0, original_retries) do
    Logger.error("Service call failed after #{original_retries} retries")
    {:error, :max_retries_exceeded}
  end

  defp do_call_with_retry(service_id, method, params, timeout, retries_left, original_retries) do
    case ExMCP.Transport.Native.call(service_id, method, params, timeout: timeout) do
      {:ok, result} ->
        {:ok, result}
        
      {:error, %{"code" => -32603}} when retries_left > 1 ->
        # Service timeout, retry
        Logger.warning("Service call timeout, retrying... (#{retries_left - 1} left)")
        Process.sleep(100)  # Brief backoff
        do_call_with_retry(service_id, method, params, timeout, retries_left - 1, original_retries)
        
      {:error, reason} ->
        Logger.error("Service call failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def batch_calls(calls, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    
    # Execute calls in parallel using Task.async_stream
    calls
    |> Task.async_stream(
      fn {service_id, method, params} ->
        ExMCP.Transport.Native.call(service_id, method, params, timeout: timeout)
      end,
      max_concurrency: System.schedulers_online(),
      timeout: timeout + 1000  # Add buffer
    )
    |> Enum.map(fn
      {:ok, result} -> result
      {:exit, reason} -> {:error, {:task_exit, reason}}
    end)
  end
end
```

## Deployment Patterns

### Production Deployment

```elixir
# rel/config.exs - Release configuration
import Mix.Releases.Config

environment :prod do
  set include_erts: true
  set include_src: false
  set cookie: :"your_secret_cookie_here"
  
  set config_providers: [
    {Mix.Releases.Config.Providers.Elixir, ["${RELEASE_ROOT_DIR}/etc/config.exs"]}
  ]
  
  set overlays: [
    {:copy, "rel/files/config.exs", "etc/config.exs"}
  ]
end

release :myapp do
  set version: current_version(:my_app)
  
  set applications: [
    :runtime_tools,
    :observer,
    :my_app
  ]
end
```

### Clustering Configuration

```elixir
# config/prod.exs
import Config

# Configure clustering
config :my_app, :clustering,
  # Use your preferred clustering strategy
  strategy: MyApp.Cluster.Strategy,
  topologies: [
    k8s: [
      strategy: Cluster.Strategy.Kubernetes,
      config: [
        kubernetes_selector: "app=myapp",
        kubernetes_node_basename: "myapp"
      ]
    ]
  ]

# Configure node naming
config :my_app, :node_name, "myapp@${POD_IP}"
```

### Docker Configuration

```dockerfile
# Dockerfile
FROM hexpm/elixir:1.14.0-erlang-25.0.4-alpine-3.16.0 as builder

# Install build dependencies
RUN apk add --no-cache build-base npm git python3

WORKDIR /app

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Set environment
ENV MIX_ENV=prod

# Copy mix files
COPY mix.exs mix.lock ./
RUN mix deps.get --only prod
RUN mix deps.compile

# Copy application
COPY . .
RUN mix compile
RUN mix release

# Production image
FROM alpine:3.16.0

RUN apk add --no-cache openssl ncurses-libs

WORKDIR /opt/app

COPY --from=builder /app/_build/prod/rel/myapp ./

# Health check for MCP services
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
  CMD bin/myapp eval "MyApp.HealthMonitor.get_health_status()"

CMD ["bin/myapp", "start"]
```

## Best Practices

### Service Design

1. **Idempotent Operations**: Design tools to be idempotent when possible
2. **Error Handling**: Always return proper MCP error codes
3. **Validation**: Validate inputs according to JSON schemas
4. **Logging**: Use structured logging with service context
5. **Timeouts**: Set appropriate timeouts for different operation types

### Production Deployment

1. **Supervision**: Always use proper OTP supervision trees
2. **Monitoring**: Monitor service health and performance
3. **Clustering**: Use proper clustering for distributed deployments
4. **Secrets**: Never hardcode secrets, use runtime configuration
5. **Health Checks**: Implement comprehensive health checking

### Security

1. **Process Isolation**: Each service runs in isolated process
2. **Input Validation**: Validate all incoming requests
3. **Resource Limits**: Set appropriate timeouts and memory limits
4. **Audit Logging**: Log all service interactions
5. **Access Control**: Use OTP supervision for access control

## Troubleshooting

### Common Issues

1. **Service Not Found**: Check service registration in Registry
2. **Timeout Errors**: Increase timeout or optimize service performance
3. **Memory Issues**: Monitor process memory usage
4. **Performance**: Use telemetry to identify bottlenecks

### Debugging Tools

```elixir
# Check registered services
ExMCP.Transport.Native.list_services()

# Check service availability
ExMCP.Transport.Native.service_available?(:my_service)

# Get health status
MyApp.HealthMonitor.get_health_status()

# Check telemetry metrics
:telemetry.list_handlers([:mcp])
```

This production guide provides a comprehensive foundation for deploying Native BEAM transport services in production environments, leveraging OTP's proven patterns for reliability and scalability.