# ExMCP Library Restructuring Design Plan

## Executive Summary

This document outlines the complete restructuring of the ExMCP library to transform it from a low-level transport library into a high-level, developer-friendly framework. The goal is to reduce server boilerplate from 151 lines to under 20 lines while maintaining flexibility for advanced users through a layered architecture approach inspired by Phoenix.

## Current State Analysis

### Problems Identified

1. **Excessive Server Boilerplate**: HTTP servers require 151+ lines of code with repetitive JSON-RPC parsing, routing, and response formatting
2. **Fragile Client Synchronization**: Client code relies on `Process.sleep()` calls and unreliable process introspection
3. **Low-Level API Exposure**: Developers must handle transport details, inconsistent response formats, and manual lifecycle management
4. **Duplicated Logic**: Similar boilerplate exists across stdio, HTTP, and SSE server implementations

### Success Metrics

- **Line Count Reduction**: 151-line servers → <20 lines (85%+ reduction)
- **API Simplicity**: Client code needs no Process.sleep or connection management
- **Flexibility Maintained**: Advanced users can still embed MCP into Phoenix apps
- **Transport Agnostic**: Same server code works for stdio/HTTP/SSE
- **Documentation**: Clear migration guide and examples

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Simple Developers                       │
│                use ExMCP.Server + deftool                  │  <- Declarative, <20 lines
│                    (Magic Layer)                           │
├─────────────────────────────────────────────────────────────┤
│                   Advanced Developers                      │
│                use ExMCP.Plug directly                     │  <- Full control, Phoenix integration
│                  (Foundation Layer)                        │
├─────────────────────────────────────────────────────────────┤
│                   Transport Layer                          │
│              stdio | HTTP | SSE                            │  <- Existing implementation (working)
└─────────────────────────────────────────────────────────────┘
```

### Design Principles

1. **Layered Abstraction**: High-level declarative API built on flexible foundation
2. **Convention Over Configuration**: Sensible defaults with escape hatches
3. **Phoenix-Inspired**: Follow proven patterns from the Elixir ecosystem
4. **Transport Agnostic**: Same server code works across all transports
5. **Backwards Compatibility**: Breaking changes acceptable in pre-1.0 for better DX

## Implementation Phases

```
Phase 1 (ExMCP.Plug) - Foundation
    ↓
Phase 2 (ExMCP.Server) - Magic Layer ←─ Parallel ─→ Phase 3 (Client Fixes)
    ↓                                                     ↓
    └──────────── Phase 4 (DX Enhancements) ──────────────┘
```

---

# Phase 1: Foundation Layer (ExMCP.Plug)

## Overview

Create the core Plug that handles all MCP protocol logic, eliminating 80% of boilerplate in current servers by handling all protocol concerns automatically.

## Step 1.1: Create Core ExMCP.Plug Module

### File: `lib/ex_mcp/plug.ex`

```elixir
defmodule ExMCP.Plug do
  @moduledoc """
  A Plug that handles MCP (Model Context Protocol) requests.
  
  This plug provides the foundation for MCP servers by handling:
  - JSON-RPC 2.0 protocol parsing and validation
  - Standard MCP method routing (initialize, tools/list, tools/call)
  - Response formatting and error handling
  - Tool registry integration
  - SSE (Server-Sent Events) support
  
  ## Usage
  
  ### In a Phoenix application:
  
      plug ExMCP.Plug,
        tools: MyApp.ToolDefinitions,
        handler: MyApp.ToolHandler,
        server_info: %{name: "my-app", version: "1.0.0"}
  
  ### Standalone with Cowboy:
  
      {:ok, _} = Plug.Cowboy.http(ExMCP.Plug, [
        tools: MyApp.ToolDefinitions,
        handler: MyApp.ToolHandler
      ], port: 4000)
  """

  @behaviour Plug
  
  import Plug.Conn
  require Logger

  alias ExMCP.{Protocol, Types, ToolRegistry}

  @default_server_info %{
    name: "ex_mcp_server",
    version: "1.0.0"
  }

  @impl Plug
  def init(opts) do
    %{
      tools: Keyword.get(opts, :tools),
      handler: Keyword.get(opts, :handler),
      server_info: Keyword.get(opts, :server_info, @default_server_info),
      session_manager: Keyword.get(opts, :session_manager, ExMCP.SessionManager)
    }
  end

  @impl Plug
  def call(%Plug.Conn{method: "GET", path_info: ["sse"]} = conn, opts) do
    handle_sse_connection(conn, opts)
  end

  @impl Plug
  def call(%Plug.Conn{method: "POST"} = conn, opts) do
    handle_mcp_request(conn, opts)
  end

  @impl Plug
  def call(conn, _opts) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(404, Jason.encode!(%{error: "Not found"}))
  end

  # Private functions implementation...
end
```

### Implementation Details

#### Core Functions to Implement:

1. **`handle_mcp_request/2`**
   - Parse JSON-RPC request body
   - Validate request format and required fields
   - Route to appropriate handler based on method
   - Format and send JSON-RPC response
   - Handle errors and edge cases

2. **`route_mcp_method/3`**
   - Handle standard methods: `initialize`, `tools/list`, `tools/call`
   - Delegate tool calls to the configured handler module
   - Generate appropriate responses for each method type

3. **`handle_sse_connection/2`**
   - Establish SSE connection with proper headers
   - Manage session correlation using Mcp-Session-Id
   - Handle connection lifecycle and cleanup

4. **Error Handling**
   - JSON-RPC error responses with proper error codes
   - Logging for debugging and monitoring
   - Graceful handling of malformed requests

#### Dependencies:
- Extract JSON-RPC logic from `ExMCP.Protocol`
- Reuse session management from existing transport layer
- Integrate with new `ExMCP.ToolRegistry` module

## Step 1.2: Add SSE Support to the Plug

### SSE Implementation Requirements

```elixir
defmodule ExMCP.Plug.SSE do
  @moduledoc """
  Server-Sent Events support for ExMCP.Plug.
  """

  def handle_sse_connection(conn, opts) do
    session_id = get_session_id(conn)
    
    conn
    |> put_resp_header("content-type", "text/event-stream")
    |> put_resp_header("cache-control", "no-cache")
    |> put_resp_header("connection", "keep-alive")
    |> put_resp_header("access-control-allow-origin", "*")
    |> send_chunked(200)
    |> start_sse_loop(session_id, opts)
  end

  defp start_sse_loop(conn, session_id, opts) do
    # Register this connection with the session manager
    # Start event loop to send responses
    # Handle connection cleanup
  end
end
```

#### SSE Features to Extract from `sse_http_server.exs`:

1. **Connection Management**
   - SSE connection establishment with proper headers
   - Session ID correlation using Mcp-Session-Id header
   - Connection registry for sending responses

2. **Event Streaming**
   - Format responses as SSE events
   - Handle keep-alive messages
   - Connection cleanup on client disconnect

3. **Session Correlation**
   - Link HTTP POST requests to SSE connections
   - Route tool call responses to correct SSE stream
   - Handle multiple concurrent sessions

## Step 1.3: Create ExMCP.ToolRegistry Module

### File: `lib/ex_mcp/tool_registry.ex`

```elixir
defmodule ExMCP.ToolRegistry do
  @moduledoc """
  Registry for MCP tools with schema validation and dynamic lookup.
  
  Provides a centralized way to register, validate, and query available tools
  for MCP servers. Supports both static tool definitions and dynamic registration.
  """

  use GenServer

  @type tool_definition :: %{
    name: String.t(),
    description: String.t(),
    input_schema: map()
  }

  @type tool_registry :: %{String.t() => tool_definition()}

  # Client API
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def register_tool(name, definition) do
    GenServer.call(__MODULE__, {:register_tool, name, definition})
  end

  def list_tools() do
    GenServer.call(__MODULE__, :list_tools)
  end

  def get_tool(name) do
    GenServer.call(__MODULE__, {:get_tool, name})
  end

  def validate_tool_call(name, arguments) do
    GenServer.call(__MODULE__, {:validate_tool_call, name, arguments})
  end

  # Server implementation
  @impl GenServer
  def init(_opts) do
    {:ok, %{}}
  end

  @impl GenServer
  def handle_call({:register_tool, name, definition}, _from, state) do
    case validate_tool_definition(definition) do
      :ok ->
        new_state = Map.put(state, name, definition)
        {:reply, :ok, new_state}
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl GenServer
  def handle_call(:list_tools, _from, state) do
    tools = Enum.map(state, fn {name, definition} ->
      %{
        name: name,
        description: definition.description,
        inputSchema: definition.input_schema
      }
    end)
    {:reply, tools, state}
  end

  @impl GenServer
  def handle_call({:get_tool, name}, _from, state) do
    {:reply, Map.get(state, name), state}
  end

  @impl GenServer
  def handle_call({:validate_tool_call, name, arguments}, _from, state) do
    case Map.get(state, name) do
      nil -> 
        {:reply, {:error, :tool_not_found}, state}
      tool_def -> 
        result = validate_arguments(arguments, tool_def.input_schema)
        {:reply, result, state}
    end
  end

  # Private validation functions
  defp validate_tool_definition(definition) do
    # Validate tool definition structure
    # Check required fields: name, description, input_schema
    # Validate JSON schema format
  end

  defp validate_arguments(arguments, schema) do
    # Validate tool call arguments against JSON schema
    # Use ExJsonSchema or similar library
  end
end
```

### Integration Points

1. **With ExMCP.Plug**: Tool registry provides tools/list responses and validates tool calls
2. **With ExMCP.Server**: Automatic tool registration from `deftool` macros
3. **With Handler Modules**: Validation before delegating to `handle_tool_call/3`

---

# Phase 2: Magic Layer (ExMCP.Server)

## Overview

Create the declarative server experience that reduces server implementation from 151 lines to under 20 lines through convention-over-configuration.

## Step 2.1: Create ExMCP.Server Behaviour Module

### File: `lib/ex_mcp/server.ex`

```elixir
defmodule ExMCP.Server do
  @moduledoc """
  Behaviour and macro for creating declarative MCP servers.
  
  ## Usage
  
      defmodule MyToolServer do
        use ExMCP.Server,
          server_info: %{name: "my-server", version: "1.0.0"}
  
        deftool "say_hello" do
          description "Says hello to a given name"
          input_schema %{
            type: "object",
            properties: %{name: %{type: "string"}},
            required: ["name"]
          }
        end
  
        @impl ExMCP.Server
        def handle_tool_call("say_hello", %{"name" => name}, _state) do
          {:ok, %{content: [%{type: "text", text: "Hello, #{name}!"}]}}
        end
      end
  
      # Start the server
      MyToolServer.start_link(transport: :http, port: 8080)
  """

  @type tool_call_result :: 
    {:ok, map()} | 
    {:error, String.t()} | 
    {:error, String.t(), map()}

  @callback handle_tool_call(
    tool_name :: String.t(),
    arguments :: map(),
    state :: any()
  ) :: tool_call_result()

  defmacro __using__(opts) do
    quote do
      @behaviour ExMCP.Server
      
      import ExMCP.Server, only: [deftool: 2]
      
      @server_info unquote(opts[:server_info]) || %{name: "mcp_server", version: "1.0.0"}
      @tools []
      
      @before_compile ExMCP.Server
      
      def start_link(opts \\ []) do
        ExMCP.Server.start_link(__MODULE__, @server_info, @tools, opts)
      end
      
      def child_spec(opts) do
        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [opts]},
          type: :worker,
          restart: :permanent,
          shutdown: 500
        }
      end
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def __server_info__, do: @server_info
      def __tools__, do: Enum.reverse(@tools)
    end
  end

  def start_link(module, server_info, tools, opts) do
    transport = Keyword.get(opts, :transport, :http)
    
    case transport do
      :stdio -> start_stdio_server(module, server_info, tools, opts)
      :http -> start_http_server(module, server_info, tools, opts)
      :sse -> start_http_server(module, server_info, tools, Keyword.put(opts, :sse, true))
    end
  end

  # Transport-specific server startup functions...
end
```

### Implementation Details

#### Transport Startup Functions:

1. **`start_stdio_server/4`**
   - Create GenServer that reads from stdin
   - Handle MCP protocol over stdio
   - Delegate tool calls to handler module

2. **`start_http_server/4`**
   - Start Cowboy HTTP server with ExMCP.Plug
   - Configure ports, endpoints, and SSE options
   - Register tools with ToolRegistry

3. **Configuration Management**
   - Default port selection (4000 for HTTP)
   - Environment-based configuration
   - Development vs production settings

## Step 2.2: Implement the `deftool` Macro

### Macro Implementation

```elixir
defmodule ExMCP.Server do
  # ... previous code ...

  defmacro deftool(name, do: block) do
    quote do
      @tools [
        %{
          name: unquote(name),
          definition: ExMCP.Server.parse_tool_definition(unquote(block))
        } | @tools
      ]
    end
  end

  def parse_tool_definition(block) do
    # Parse the do block to extract:
    # - description
    # - input_schema
    # - other tool metadata
    
    # This will be expanded to handle various tool definition formats
    %{
      description: extract_description(block),
      input_schema: extract_input_schema(block)
    }
  end

  # Helper functions for parsing tool definitions...
end
```

### Tool Definition DSL

Support multiple syntax styles for tool definitions:

```elixir
# Style 1: Block syntax
deftool "say_hello" do
  description "Says hello to a given name"
  input_schema %{
    type: "object",
    properties: %{name: %{type: "string"}},
    required: ["name"]
  }
end

# Style 2: Attribute syntax (future enhancement)
@tool_description "Says hello to a given name"
@tool_schema %{type: "object", properties: %{name: %{type: "string"}}}
deftool "say_hello"

# Style 3: Inline syntax (future enhancement)
deftool "say_hello", 
  description: "Says hello",
  input_schema: %{...}
```

## Step 2.3: Add Automatic Server Lifecycle Management

### Transport Configuration

```elixir
defmodule ExMCP.Server.Transport do
  @moduledoc """
  Transport configuration and lifecycle management for ExMCP servers.
  """

  def start_stdio_server(module, server_info, tools, opts) do
    ExMCP.Server.StdioServer.start_link([
      module: module,
      server_info: server_info,
      tools: tools
    ] ++ opts)
  end

  def start_http_server(module, server_info, tools, opts) do
    port = Keyword.get(opts, :port, 4000)
    sse_enabled = Keyword.get(opts, :sse, false)
    
    plug_opts = [
      handler: module,
      server_info: server_info,
      tools: tools,
      sse_enabled: sse_enabled
    ]
    
    cowboy_opts = [
      port: port,
      dispatch: build_dispatch(plug_opts)
    ]
    
    Plug.Cowboy.http(ExMCP.Plug, plug_opts, cowboy_opts)
  end

  defp build_dispatch(plug_opts) do
    :cowboy_router.compile([
      {:_, [
        {"/", ExMCP.Plug, plug_opts},
        {"/sse", ExMCP.Plug, plug_opts ++ [sse: true]},
        {:_, ExMCP.Plug, plug_opts}
      ]}
    ])
  end
end
```

### Server Modules

1. **`ExMCP.Server.StdioServer`**
   - GenServer that handles stdio communication
   - Line-based JSON-RPC processing
   - Graceful shutdown on EOF

2. **`ExMCP.Server.HttpServer`** 
   - Wrapper around Cowboy with ExMCP.Plug
   - Automatic SSL/TLS configuration
   - Health check endpoints

## Step 2.4: Create Server Configuration System

### Configuration Schema

```elixir
defmodule ExMCP.Server.Config do
  @moduledoc """
  Configuration management for ExMCP servers.
  """

  defstruct [
    :transport,
    :port,
    :host,
    :server_info,
    :ssl_opts,
    :cors_opts,
    :session_timeout,
    :max_connections
  ]

  def load_config(module, opts) do
    base_config = Application.get_env(:ex_mcp, module, [])
    merged_opts = Keyword.merge(base_config, opts)
    
    %__MODULE__{
      transport: Keyword.get(merged_opts, :transport, :http),
      port: Keyword.get(merged_opts, :port, default_port()),
      host: Keyword.get(merged_opts, :host, "localhost"),
      server_info: Keyword.get(merged_opts, :server_info, %{}),
      ssl_opts: Keyword.get(merged_opts, :ssl_opts),
      cors_opts: Keyword.get(merged_opts, :cors_opts, default_cors()),
      session_timeout: Keyword.get(merged_opts, :session_timeout, 30_000),
      max_connections: Keyword.get(merged_opts, :max_connections, 1000)
    }
  end

  defp default_port do
    case Mix.env() do
      :test -> 4001
      _ -> 4000
    end
  end

  defp default_cors do
    %{
      allow_origin: ["*"],
      allow_methods: ["GET", "POST", "OPTIONS"],
      allow_headers: ["content-type", "mcp-session-id"]
    }
  end
end
```

---

# Phase 3: Client API Hardening

## Overview

Fix core client synchronization issues by making `start_link` fully synchronous and eliminating all `Process.sleep` dependencies.

## Step 3.1: Refactor ExMCP.Client.start_link to be Synchronous

### Current Problems

```elixir
# Current problematic pattern
{:ok, client} = ExMCP.Client.start_link(url: "http://localhost:8080")
Process.sleep(1000)  # Race condition workaround
case Process.info(client, :dictionary) do  # Unreliable state check
  nil -> {:error, :not_connected}
  _ -> ExMCP.Client.call_tool(client, "tool", %{})
end
```

### Target API

```elixir
# New synchronous pattern - no sleep needed
{:ok, client} = ExMCP.Client.start_link(url: "http://localhost:8080")
# Client is guaranteed to be ready for tool calls
{:ok, result} = ExMCP.Client.call_tool(client, "tool", %{})
```

### Implementation Strategy

#### Modify `ExMCP.Client.init/1`

```elixir
defmodule ExMCP.Client do
  # ... existing code ...

  @impl GenServer
  def init(opts) do
    # Current: Return {:ok, state} immediately, then connect asynchronously
    # New: Complete full connection and handshake before returning
    
    case connect_and_initialize_sync(opts) do
      {:ok, state} -> 
        {:ok, state}
      {:error, reason} -> 
        {:stop, reason}
    end
  end

  defp connect_and_initialize_sync(opts) do
    with {:ok, transport_state} <- connect_transport(opts),
         {:ok, negotiated_state} <- perform_mcp_handshake(transport_state, opts),
         :ok <- start_receiver_loop(negotiated_state) do
      state = build_initial_state(negotiated_state, opts)
      {:ok, state}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp perform_mcp_handshake(transport_state, opts) do
    # Send initialize request
    init_msg = build_initialize_message(opts)
    
    with {:ok, _} <- send_message_sync(init_msg, transport_state),
         {:ok, init_response} <- wait_for_response(:initialize, 10_000),
         :ok <- validate_server_capabilities(init_response),
         {:ok, _} <- send_initialized_notification(transport_state) do
      {:ok, update_state_with_handshake(transport_state, init_response)}
    end
  end

  defp wait_for_response(expected_type, timeout) do
    receive do
      {:transport_message, message} ->
        case parse_response(message, expected_type) do
          {:ok, response} -> {:ok, response}
          {:error, reason} -> {:error, reason}
        end
    after
      timeout -> {:error, :handshake_timeout}
    end
  end
end
```

#### Benefits of Synchronous Approach

1. **Eliminates Race Conditions**: No need for `Process.sleep` workarounds
2. **Clear Error Handling**: Connection failures surface immediately at startup
3. **Predictable State**: Client is always ready when `start_link` succeeds
4. **Better Testing**: Deterministic behavior makes tests more reliable

## Step 3.2: Add Proper State Checking and Lifecycle Management

### Client State Management

```elixir
defmodule ExMCP.Client.State do
  @moduledoc """
  Structured state management for MCP clients.
  """

  defstruct [
    :transport_mod,
    :transport_state,
    :server_info,
    :server_capabilities,
    :connection_status,
    :pending_requests,
    :last_activity,
    :session_id
  ]

  @type connection_status :: 
    :connecting | 
    :connected | 
    :ready | 
    :disconnected | 
    :error

  def new(opts) do
    %__MODULE__{
      connection_status: :connecting,
      pending_requests: %{},
      last_activity: System.monotonic_time(:millisecond),
      session_id: generate_session_id()
    }
  end

  def mark_ready(state, server_info, capabilities) do
    %{state | 
      connection_status: :ready,
      server_info: server_info,
      server_capabilities: capabilities,
      last_activity: System.monotonic_time(:millisecond)
    }
  end

  def is_ready?(%__MODULE__{connection_status: :ready}), do: true
  def is_ready?(_), do: false
end
```

### Health Monitoring

```elixir
defmodule ExMCP.Client do
  # ... existing code ...

  def ready?(client) when is_pid(client) do
    try do
      GenServer.call(client, :is_ready?, 1000)
    catch
      :exit, _ -> false
    end
  end

  def connection_info(client) do
    GenServer.call(client, :connection_info)
  end

  def ping(client, timeout \\ 5000) do
    # Send a ping request to verify connection health
    GenServer.call(client, {:ping, timeout})
  end

  @impl GenServer
  def handle_call(:is_ready?, _from, state) do
    {:reply, ExMCP.Client.State.is_ready?(state), state}
  end

  @impl GenServer
  def handle_call(:connection_info, _from, state) do
    info = %{
      status: state.connection_status,
      server_info: state.server_info,
      last_activity: state.last_activity,
      pending_requests: map_size(state.pending_requests)
    }
    {:reply, info, state}
  end

  @impl GenServer  
  def handle_call({:ping, timeout}, from, state) do
    # Implementation of ping/health check
    ping_id = generate_request_id()
    ping_msg = build_ping_message(ping_id)
    
    case send_message(ping_msg, state) do
      {:ok, new_state} ->
        # Track ping request with timeout
        pending = Map.put(new_state.pending_requests, ping_id, {from, :ping, timeout})
        {:noreply, %{new_state | pending_requests: pending}}
      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end
end
```

---

# Phase 4: Developer Experience Enhancements

## Overview

Add convenience functions and improve the overall developer experience with better APIs, response normalization, and error handling.

## Step 4.1: Add Convenience Client Functions

### High-Level Client API

```elixir
defmodule ExMCP.Client.Convenience do
  @moduledoc """
  High-level convenience functions for common MCP client operations.
  """

  @doc """
  Execute a tool call with automatic client lifecycle management.
  
  This function handles the complete lifecycle: connect, call, disconnect.
  Perfect for one-off tool calls or stateless operations.
  
  ## Examples
  
      {:ok, result} = ExMCP.with_client(
        url: "http://localhost:8080", 
        fn client ->
          ExMCP.Client.call_tool(client, "say_hello", %{name: "World"})
        end
      )
  """
  def with_client(connect_opts, fun) when is_function(fun, 1) do
    case ExMCP.Client.start_link(connect_opts) do
      {:ok, client} ->
        try do
          fun.(client)
        after
          ExMCP.Client.stop(client)
        end
      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Simple one-shot tool call function.
  
  ## Examples
  
      {:ok, result} = ExMCP.call("http://localhost:8080", "say_hello", %{name: "World"})
      {:ok, result} = ExMCP.call([url: "http://localhost:8080"], "tool_name", %{arg: "value"})
  """
  def call(connect_opts, tool_name, arguments) when is_binary(tool_name) do
    with_client(normalize_connect_opts(connect_opts), fn client ->
      ExMCP.Client.call_tool(client, tool_name, arguments)
    end)
  end

  @doc """
  List available tools from a server.
  
  ## Examples
  
      {:ok, tools} = ExMCP.list_tools("http://localhost:8080")
  """
  def list_tools(connect_opts) do
    with_client(normalize_connect_opts(connect_opts), fn client ->
      ExMCP.Client.list_tools(client)
    end)
  end

  @doc """
  Get server information.
  
  ## Examples
  
      {:ok, info} = ExMCP.server_info("http://localhost:8080")
  """
  def server_info(connect_opts) do
    with_client(normalize_connect_opts(connect_opts), fn client ->
      ExMCP.Client.server_info(client)
    end)
  end

  # Private helper functions
  defp normalize_connect_opts(url) when is_binary(url) do
    [url: url]
  end

  defp normalize_connect_opts(opts) when is_list(opts) do
    opts
  end

  defp normalize_connect_opts(opts) when is_map(opts) do
    Map.to_list(opts)
  end
end
```

### Auto-Configuration and Defaults

```elixir
defmodule ExMCP.Client.Config do
  @moduledoc """
  Client configuration with intelligent defaults and auto-detection.
  """

  defstruct [
    :url,
    :transport,
    :headers,
    :timeout,
    :retry_attempts,
    :ssl_opts
  ]

  def build_config(opts) do
    url = Keyword.get(opts, :url) || infer_url_from_env()
    
    %__MODULE__{
      url: url,
      transport: infer_transport(url, opts),
      headers: build_headers(opts),
      timeout: Keyword.get(opts, :timeout, 30_000),
      retry_attempts: Keyword.get(opts, :retry_attempts, 3),
      ssl_opts: build_ssl_opts(url, opts)
    }
  end

  defp infer_transport(url, opts) do
    cond do
      Keyword.has_key?(opts, :transport) -> Keyword.get(opts, :transport)
      String.starts_with?(url, "http") -> :http
      true -> :stdio
    end
  end

  defp infer_url_from_env do
    System.get_env("MCP_SERVER_URL") || "http://localhost:4000"
  end

  defp build_headers(opts) do
    base_headers = [
      {"content-type", "application/json"},
      {"user-agent", "ExMCP/#{Application.spec(:ex_mcp, :vsn)}"}
    ]
    
    custom_headers = Keyword.get(opts, :headers, [])
    Keyword.merge(base_headers, custom_headers)
  end

  defp build_ssl_opts(url, opts) do
    if String.starts_with?(url, "https") do
      Keyword.get(opts, :ssl_opts, [verify: :verify_peer])
    else
      []
    end
  end
end
```

## Step 4.2: Normalize Responses and Improve Error Handling

### Response Normalization

```elixir
defmodule ExMCP.Response do
  @moduledoc """
  Structured response types for MCP operations.
  
  Provides consistent, predictable response formats with atom keys
  and proper error handling.
  """

  defstruct [
    :content,
    :meta,
    :tool_name,
    :request_id,
    :server_info
  ]

  @type t :: %__MODULE__{
    content: [content_item()],
    meta: map() | nil,
    tool_name: String.t() | nil,
    request_id: String.t() | nil,
    server_info: map() | nil
  }

  @type content_item :: %{
    type: String.t(),
    text: String.t() | nil,
    data: any() | nil,
    annotations: map() | nil
  }

  def from_raw_response(raw_response, opts \\ []) do
    %__MODULE__{
      content: normalize_content(raw_response["content"] || raw_response[:content]),
      meta: normalize_meta(raw_response["meta"] || raw_response[:meta]),
      tool_name: Keyword.get(opts, :tool_name),
      request_id: Keyword.get(opts, :request_id),
      server_info: Keyword.get(opts, :server_info)
    }
  end

  defp normalize_content(content) when is_list(content) do
    Enum.map(content, &normalize_content_item/1)
  end

  defp normalize_content(content) when is_map(content) do
    [normalize_content_item(content)]
  end

  defp normalize_content(nil), do: []

  defp normalize_content_item(item) when is_map(item) do
    %{
      type: item["type"] || item[:type] || "text",
      text: item["text"] || item[:text],
      data: item["data"] || item[:data],
      annotations: normalize_map_keys(item["annotations"] || item[:annotations])
    }
  end

  defp normalize_meta(meta) when is_map(meta) do
    normalize_map_keys(meta)
  end

  defp normalize_meta(_), do: nil

  defp normalize_map_keys(map) when is_map(map) do
    for {key, value} <- map, into: %{} do
      atom_key = if is_binary(key), do: String.to_atom(key), else: key
      {atom_key, value}
    end
  end

  defp normalize_map_keys(other), do: other
end
```

### Enhanced Error Handling

```elixir
defmodule ExMCP.Error do
  @moduledoc """
  Structured error types for MCP operations.
  """

  defexception [:code, :message, :data, :request_id]

  @type t :: %__MODULE__{
    code: integer() | atom(),
    message: String.t(),
    data: any(),
    request_id: String.t() | nil
  }

  # JSON-RPC Error Codes
  @parse_error -32700
  @invalid_request -32600
  @method_not_found -32601
  @invalid_params -32602
  @internal_error -32603

  # MCP-specific Error Codes
  @connection_error -32001
  @timeout_error -32002
  @auth_error -32003
  @tool_error -32004

  def connection_error(message, data \\ nil) do
    %__MODULE__{
      code: @connection_error,
      message: "Connection error: #{message}",
      data: data
    }
  end

  def timeout_error(operation, timeout) do
    %__MODULE__{
      code: @timeout_error,
      message: "Operation #{operation} timed out after #{timeout}ms",
      data: %{operation: operation, timeout: timeout}
    }
  end

  def tool_error(tool_name, message, data \\ nil) do
    %__MODULE__{
      code: @tool_error,
      message: "Tool '#{tool_name}' error: #{message}",
      data: Map.merge(%{tool_name: tool_name}, data || %{})
    }
  end

  def from_json_rpc_error(error_map, request_id \\ nil) do
    %__MODULE__{
      code: error_map["code"] || error_map[:code],
      message: error_map["message"] || error_map[:message] || "Unknown error",
      data: error_map["data"] || error_map[:data],
      request_id: request_id
    }
  end
end
```

### Client API Integration

```elixir
defmodule ExMCP.Client do
  # ... existing code ...

  def call_tool(client, tool_name, arguments) do
    case GenServer.call(client, {:call_tool, tool_name, arguments}) do
      {:ok, raw_response} ->
        response = ExMCP.Response.from_raw_response(raw_response, tool_name: tool_name)
        {:ok, response}
      
      {:error, error_data} when is_map(error_data) ->
        error = ExMCP.Error.from_json_rpc_error(error_data)
        {:error, error}
      
      {:error, reason} ->
        error = ExMCP.Error.connection_error(inspect(reason))
        {:error, error}
    end
  end

  def list_tools(client) do
    case GenServer.call(client, :list_tools) do
      {:ok, tools} when is_list(tools) ->
        normalized_tools = Enum.map(tools, &normalize_tool_definition/1)
        {:ok, normalized_tools}
      
      {:error, reason} ->
        {:error, ExMCP.Error.connection_error(inspect(reason))}
    end
  end

  defp normalize_tool_definition(tool) do
    %{
      name: tool["name"] || tool[:name],
      description: tool["description"] || tool[:description],
      input_schema: tool["inputSchema"] || tool[:input_schema] || tool["input_schema"]
    }
  end
end
```

---

# Integration and Migration Plan

## Phase Integration Strategy

### Phase 1-2 Integration: Server Foundation

1. **Create ExMCP.Plug** with full MCP protocol support
2. **Build ExMCP.Server** on top of the plug
3. **Validate with current examples**: Convert `simple_http_server.exs` to use new API
4. **Measure success**: Ensure line count reduction from 151 to <20 lines

### Phase 3-4 Integration: Client Enhancement

1. **Refactor ExMCP.Client** for synchronous operation
2. **Add convenience functions** and response normalization
3. **Update hello_world.exs**: Remove all `Process.sleep` calls
4. **Test all transports**: Ensure stdio, HTTP, and SSE work with new client

## Migration Path

### Backwards Compatibility Strategy

Since the library is pre-1.0, breaking changes are acceptable. However, provide a smooth migration path:

1. **Keep existing examples working** during development
2. **Create migration guide** with before/after comparisons  
3. **Provide deprecation warnings** for old patterns
4. **Update documentation** with new recommended approaches

### Example Migration

#### Before (Current 151-line server):
```elixir
# simple_http_server.exs - 151 lines of boilerplate
defmodule SimpleHTTPServer do
  # ... 80 lines of HTTP setup ...
  # ... 50 lines of JSON-RPC parsing ...
  # ... 20 lines of tool handling ...
end
```

#### After (New declarative server):
```elixir
defmodule SimpleHTTPServer do
  use ExMCP.Server,
    server_info: %{name: "simple-http-server", version: "1.0.0"}

  deftool "say_hello" do
    description "Says hello to a given name"
    input_schema %{
      type: "object", 
      properties: %{name: %{type: "string"}},
      required: ["name"]
    }
  end

  @impl ExMCP.Server
  def handle_tool_call("say_hello", %{"name" => name}, _state) do
    content = [%{type: "text", text: "Hello, #{name}! Welcome to ExMCP via HTTP! 🌐"}]
    {:ok, %{content: content}}
  end
end

# Start with one line
SimpleHTTPServer.start_link(transport: :http, port: 8321)
```

## Testing Strategy

### Validation Tests

1. **Line Count Verification**: Automated tests to ensure new servers are <20 lines
2. **Protocol Compliance**: Verify all MCP standard methods work correctly
3. **Transport Compatibility**: Test stdio, HTTP, and SSE with same server code
4. **Client Reliability**: Ensure no `Process.sleep` needed in any examples

### Integration Tests

1. **Cross-transport Testing**: Same server code works on all transports
2. **Client-Server Communication**: Full end-to-end testing  
3. **Error Handling**: Proper error propagation and formatting
4. **Performance**: Ensure new abstractions don't degrade performance

## Documentation Updates

### Developer Guide Restructure

1. **Quick Start Guide**: Show the new 20-line server example first
2. **Transport Guide**: Explain when to use stdio vs HTTP vs SSE
3. **Advanced Usage**: Document ExMCP.Plug for Phoenix integration
4. **Migration Guide**: Step-by-step conversion from old to new API

### API Documentation

1. **ExMCP.Server**: Complete module documentation with examples
2. **ExMCP.Client**: Updated client documentation without Process.sleep
3. **ExMCP.Response**: Document the new response structures
4. **ExMCP.Error**: Comprehensive error handling documentation

---

# Native Transport Integration

## Overview

The Native Service Dispatcher provides ultra-fast BEAM-to-BEAM communication (~15μs local, ~50μs cross-node) through direct GenServer calls. This section details how to integrate Native as a first-class transport while preserving its performance characteristics.

## Architecture Changes

### Transport Behaviour Enhancement

```elixir
defmodule ExMCP.Transport do
  @type message :: map()  # Always string keys for consistency
  @type state :: any()
  
  # Synchronous request/response (covers Native and HTTP)
  @callback call(message, state, opts :: keyword()) :: 
    {:ok, response :: message, new_state} | {:error, reason}
  
  # Transport lifecycle
  @callback connect(opts :: keyword()) :: {:ok, state} | {:error, reason}
  @callback close(state) :: :ok
  
  # Future: streaming support
  # @callback stream(message, state, opts) :: {:ok, Enumerable.t(), new_state} | {:error, reason}
end
```

### Native Transport Implementation

```elixir
defmodule ExMCP.Transport.Native do
  @behaviour ExMCP.Transport
  
  defstruct [:service_name, :pid]
  
  @impl true
  def connect(opts) do
    service_name = Keyword.fetch!(opts, :name)
    
    case lookup_service_with_retry(service_name, 3) do
      {:ok, pid} -> 
        {:ok, %__MODULE__{service_name: service_name, pid: pid}}
      {:error, reason} -> 
        {:error, reason}
    end
  end
  
  @impl true
  def call(message, %{service_name: name} = state, opts) do
    # No JSON encoding needed - direct map passing!
    timeout = Keyword.get(opts, :timeout, 5_000)
    
    case ExMCP.Native.call(name, message["method"], message["params"], timeout: timeout) do
      {:ok, result} -> {:ok, result, state}
      {:error, reason} -> {:error, reason}
    end
  end
  
  @impl true
  def close(_state), do: :ok
  
  defp lookup_service_with_retry(name, attempts) do
    # Implement retry logic for service discovery
    # This handles race conditions during startup
  end
end
```

## Enhanced Service Registry

### Rich Metadata Registration

When a service uses `use ExMCP.Server`, it registers comprehensive metadata:

```elixir
%{
  name: :my_tool_server,
  server_info: %{"name" => "my-server", "version" => "1.0.0"},
  transports: [
    :native,
    {:http, [port: 4000, host: "0.0.0.0"]},
    {:sse, [port: 4000, host: "0.0.0.0"]}
  ],
  tools: [
    %{
      "name" => "say_hello",
      "description" => "Says hello to a given name"
    }
  ],
  node: node(),
  started_at: DateTime.utc_now()
}
```

### Service Discovery API

```elixir
defmodule ExMCP.Discovery do
  @moduledoc """
  Service discovery for Native transport across BEAM cluster.
  """
  
  def list_services do
    # Query Horde.Registry across cluster
    # Return list of service metadata
  end
  
  def find_service(name) do
    # Find specific service by name
    # Return metadata including available transports
  end
  
  def service_available?(name, transport \\ :any) do
    # Check if service is available on specific transport
  end
end
```

## Unified Server Implementation

### Multi-Transport Server

```elixir
defmodule MyToolServer do
  use ExMCP.Server,
    server_info: %{name: "my-server", version: "1.0.0"},
    transports: [:native, :http],  # Explicit transport selection
    
    # Transport-specific configuration
    native_name: :my_tool_server,
    http_port: 4000,
    http_host: "0.0.0.0"

  deftool "say_hello" do
    description "Says hello to a given name"
    input_schema %{
      type: "object",
      properties: %{name: %{type: "string"}},
      required: ["name"]
    }
  end

  @impl ExMCP.Server
  def handle_tool_call("say_hello", %{"name" => name}, state) do
    # Single implementation serves ALL transports!
    {:ok, %{"content" => [%{"type" => "text", "text" => "Hello, #{name}!"}]}}
  end
end
```

### Generated Server Structure

The `use ExMCP.Server` macro generates:

1. **For Native Transport**:
   - GenServer callbacks (`handle_call`, `handle_cast`)
   - Automatic Horde.Registry registration
   - Direct message handling without JSON parsing

2. **For HTTP Transport**:
   - Plug.Cowboy server startup
   - ExMCP.Plug integration
   - JSON-RPC parsing and response formatting

3. **Common Infrastructure**:
   - Tool registry management
   - Unified `handle_tool_call/3` dispatch
   - Capability negotiation

## Enhanced Client API

### Transport Preference Lists

```elixir
# Simple case - single transport
{:ok, client} = ExMCP.Client.start_link(
  transport: :native, 
  name: :my_service
)

# Advanced case - automatic fallback
{:ok, client} = ExMCP.Client.start_link(
  service_name: :my_service,
  transports: [
    {:native, []},                              # Try native first
    {:http, [url: "http://localhost:4000"]},    # Fallback to local HTTP
    {:http, [url: "http://backup:4000"]}        # Final fallback
  ]
)

# Dynamic discovery via Horde
{:ok, client} = ExMCP.Client.start_link(
  service_name: :my_service,
  discovery: :horde  # Discovers available transports automatically
)
```

### Discovery Implementation

```elixir
defmodule ExMCP.Client do
  # ... existing code ...
  
  defp connect_with_discovery(service_name, opts) do
    case ExMCP.Discovery.find_service(service_name) do
      {:ok, metadata} ->
        # Build transport list from metadata
        transports = build_transport_list(metadata.transports)
        
        # Try each transport in preference order
        connect_with_transports(transports, opts)
        
      {:error, :not_found} ->
        {:error, {:service_not_found, service_name}}
    end
  end
  
  defp build_transport_list(transport_specs) do
    # Order by preference: native > sse > http
    # Convert metadata format to client config format
  end
end
```

## Migration Path

### From ExMCP.Service to ExMCP.Server

```elixir
# Old way (current)
defmodule MyService do
  use ExMCP.Service, name: :my_service
  
  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    {:ok, %{"tools" => [...]}, state}
  end
  
  @impl true  
  def handle_mcp_request("tools/call", %{"name" => tool} = params, state) do
    # Manual routing
  end
end

# New way (proposed)
defmodule MyService do
  use ExMCP.Server,
    transports: [:native],
    native_name: :my_service
    
  deftool "my_tool" do
    description "My tool description"
    input_schema %{...}
  end
  
  @impl ExMCP.Server
  def handle_tool_call("my_tool", params, state) do
    # Automatic routing, validation, and error handling
    {:ok, %{"content" => [...]}}
  end
end
```

## Performance Characteristics

### Preserved Performance

- **Native calls**: Still ~15μs local, ~50μs cross-node
- **Zero serialization**: Maps passed directly between processes
- **Direct supervision**: OTP supervision tree unchanged
- **Cluster-aware**: Horde.Registry handles distribution

### Additional Benefits

- **Unified API**: Same server code works for all transports
- **Automatic discovery**: Clients can find services dynamically
- **Graceful degradation**: Fallback from Native to HTTP seamlessly
- **Rich introspection**: Service metadata available cluster-wide

## Key Design Decisions

1. **String Keys Everywhere**: Even Native transport uses string keys for consistency
2. **Explicit Transport Selection**: No magical switching, developer controls transports
3. **Discovery as Opt-In**: Explicit `discovery: :horde` option for dynamic environments
4. **Single Tool Implementation**: One `handle_tool_call/3` serves all transports
5. **Performance First**: Native path has zero overhead compared to current implementation

---

# Success Metrics and Validation

## Primary Success Metrics

1. **Line Count Reduction**: 151-line servers → <20 lines (85%+ reduction) ✓
2. **API Simplicity**: Client code needs no Process.sleep or connection management ✓
3. **Flexibility Maintained**: Advanced users can still embed MCP into Phoenix apps ✓
4. **Transport Agnostic**: Same server code works for Native/stdio/HTTP/SSE ✓
5. **Documentation**: Clear migration guide and examples ✓

## Acceptance Criteria

### Server-Side Success
- [ ] Convert `simple_http_server.exs` to <20 lines using `use ExMCP.Server`
- [ ] Convert `sse_http_server.exs` to same <20 lines (transport agnostic)
- [ ] Create stdio server using same declarative API
- [ ] Native services use same `use ExMCP.Server` API
- [ ] Advanced users can still use `ExMCP.Plug` in Phoenix apps

### Client-Side Success  
- [ ] Remove all `Process.sleep` calls from `hello_world.exs`
- [ ] `ExMCP.Client.start_link` is fully synchronous
- [ ] Add `ExMCP.call/3` for simple one-shot operations
- [ ] Responses use consistent atom keys or structured types

### Integration Success
- [ ] All four transport modes work with new APIs (Native/stdio/HTTP/SSE)
- [ ] Native transport maintains ~15μs local latency
- [ ] Transport preference lists enable graceful fallback
- [ ] Discovery via Horde enables dynamic service location
- [ ] Existing functionality is preserved
- [ ] Performance is maintained or improved
- [ ] Test suite passes with new implementation

## Phase 5: DSL Implementation

### 5.1 Core DSL Design
Based on MCP specification analysis and Elixir best practices:

#### Tool Definition DSL
```elixir
deftool "search_files" do
  description "Search for files matching a pattern"
  
  # Elixir-native schema DSL (compiles to JSON Schema)
  args do
    field :pattern, :string, required: true, description: "Search pattern"
    field :path, :string, default: ".", description: "Search path"
    field :recursive, :boolean, default: true
    field :max_results, :integer, min: 1, max: 1000
    
    # Nested objects
    field :options, :object do
      field :case_sensitive, :boolean, default: false
      field :include_hidden, :boolean, default: false
    end
    
    # Arrays
    field :extensions, {:array, :string}, description: "File extensions"
  end
end

# Escape hatch for complex JSON Schema
deftool "complex_tool" do
  input_schema load_json_schema("priv/schemas/complex_tool.json")
end
```

#### Resource Definition DSL
```elixir
defresource "config://app/settings" do
  name "Application Settings"
  description "Current application configuration"
  mime_type "application/json"
  
  # Optional annotations
  annotations %{
    audience: ["admin"],
    priority: 0.8
  }
end

# Pattern-based resources
defresource "file://logs/*.log" do
  name "Log Files"
  description "Application log files"
  mime_type "text/plain"
  list_pattern true
  subscribable true  # Enables subscription support
end

# Resource templates with URI patterns
defresource_template "github://repos/{owner}/{repo}/issues/{id}" do
  name "GitHub Issues"
  description "Access GitHub issue data"
  mime_type "application/json"
end
```

#### Prompt Definition DSL
```elixir
defprompt "code_review" do
  name "Code Review Assistant"
  description "Reviews code with specific focus areas"
  
  # Simpler argument definition than tools
  arguments do
    arg :code, required: true, description: "Code to review"
    arg :language, required: false, description: "Programming language"
    arg :focus, required: false, description: "Review focus"
  end
end
```

### 5.2 Content Type System

#### Type-safe Content Structs
```elixir
defprotocol ExMCP.Content do
  @spec serialize(t) :: map()
  def serialize(content)
end

defmodule ExMCP.TextContent do
  @enforce_keys [:text]
  defstruct [:text, annotations: %{}]
  
  defimpl ExMCP.Content do
    def serialize(%__MODULE__{text: text, annotations: a}) do
      %{type: "text", text: text, annotations: a}
    end
  end
end

# Smart constructors
text("Hello", audience: ["user"], priority: 0.9)

# Chainable builder pattern
image_content()
|> with_data(base64_data)
|> with_mime_type("image/png")
|> with_annotation(:priority, 0.8)

# Message constructors
user("User message")
assistant("Assistant response")
system("System prompt")
```

### 5.3 Server Patterns

#### Unified Server with Composition
```elixir
defmodule MyUnifiedServer do
  use ExMCP.Server
  
  # Mix all types in one module
  deftool "hello" do
    description "Says hello"
    args do
      field :name, :string, required: true
    end
  end
  
  defresource "config://app" do
    name "App Config"
    mime_type "application/json"
  end
  
  defprompt "greeting" do
    name "Greeting Template"
    arguments do
      arg :style, description: "Greeting style"
    end
  end
  
  # Callbacks in one place
  @impl true
  def handle_tool_call("hello", %{"name" => name}, state) do
    {:ok, %{content: [text("Hello, #{name}!")]}, state}
  end
end

# Compose from multiple modules
defmodule MyComposedServer do
  use ExMCP.Server, compose: [MyToolServer, MyResourceServer]
end
```

#### Pattern Matching in Handlers
```elixir
@impl true
def handle_tool_call("process_data", %{"type" => "csv"} = params, state) do
  # CSV-specific processing
end

def handle_tool_call("process_data", %{"type" => "json"} = params, state) do
  # JSON-specific processing
end
```

### 5.4 Advanced Features

#### Capability Auto-detection
```elixir
defmodule MyServer do
  use ExMCP.Server
  
  # Framework detects and advertises capabilities
  deftool "my_tool" do ... end        # -> tools capability
  defresource "my://resource" do ... end  # -> resources capability
  defprompt "my_prompt" do ... end     # -> prompts capability
  
  # Experimental features detected by callback pattern
  @impl true
  def handle_request("experimental/custom_feature", params, state) do
    # Framework advertises experimental.custom_feature capability
  end
  
  # Explicit capability declaration
  use ExMCP.Server, capabilities: %{
    experimental: %{
      "advanced_mode" => %{version: "1.0"}
    }
  }
end
```

#### Middleware/Interceptors
```elixir
defmodule MyServer do
  use ExMCP.Server,
    middleware: [
      ExMCP.Middleware.RateLimit,
      ExMCP.Middleware.Telemetry,
      MyApp.AuthMiddleware
    ]
end
```

#### Subscription Management
```elixir
defmodule MyResourceServer do
  use ExMCP.Server
  
  defresource "db://users/*" do
    name "User Records"
    mime_type "application/json"
    subscribable true
  end
  
  @impl true
  def handle_resource_subscribe("db://users/" <> user_id, state) do
    # Framework handles subscription tracking
    {:ok, state}
  end
  
  # Public API for pushing updates
  def user_updated(server_pid, user_id) do
    ExMCP.Server.notify_resource_update(server_pid, "db://users/#{user_id}")
  end
end
```

### 5.5 Validation Strategy

#### Compile-time Validation
- Tool/resource/prompt names are unique
- Callbacks exist for defined items
- Schema types are valid
- Required callbacks are implemented

#### Runtime Validation
- Validates incoming arguments against schemas
- Ensures content types are properly structured
- Checks annotation constraints
- Validates protocol compliance

### 5.6 Testing Support
```elixir
defmodule MyServerTest do
  use ExMCP.ServerCase, server: MyServer
  
  test "calculate_sum tool" do
    assert {:ok, result} = call_tool("calculate_sum", %{a: 1, b: 2})
    assert_content result, [text("Result: 3")]
  end
  
  test "resource subscription" do
    subscribe_to_resource("db://users/123")
    MyServer.user_updated(server_pid(), "123")
    assert_resource_update "db://users/123"
  end
end
```

### 5.7 Additional Considerations

#### Telemetry Integration
```elixir
# Automatic telemetry events
[:ex_mcp, :tool, :start]
[:ex_mcp, :tool, :stop]
[:ex_mcp, :tool, :exception]
[:ex_mcp, :resource, :read]
[:ex_mcp, :subscription, :update]
```

#### Built-in Rate Limiting
```elixir
use ExMCP.Server,
  rate_limit: [
    tools: {100, :per_minute},
    resources: {1000, :per_minute}
  ]
```

#### Registry Pattern for Discovery
```elixir
# Servers can register themselves
ExMCP.Registry.register("my_service", MyServer)

# Clients can discover servers
{:ok, server_pid} = ExMCP.Registry.lookup("my_service")
```

## Phase 6: Testing & Reliability

### 6.1 Testing Framework

#### Test Harness Design
```elixir
defmodule ExMCP.TestCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      import ExMCP.TestHelpers
      import ExMCP.Assertions
      
      setup do
        # Start test transport
        {:ok, transport} = ExMCP.Transport.Test.start_link()
        
        # Provide test context
        %{transport: transport}
      end
    end
  end
end
```

#### Mocking Strategies
```elixir
# Transport mocking
defmodule ExMCP.Transport.Mock do
  use ExMCP.Transport
  
  def send_message(state, message) do
    # Record sent messages for assertions
    TestAgent.record_message(message)
    {:ok, state}
  end
  
  def receive_message(state, timeout) do
    # Return pre-programmed responses
    TestAgent.next_response()
  end
end

# Server behavior mocking
Mox.defmock(ExMCP.ServerMock, for: ExMCP.Server.Handler)
```

#### Integration Test Patterns
```elixir
defmodule ExMCP.IntegrationTest do
  use ExMCP.TestCase
  
  describe "client-server communication" do
    setup do
      # Start real server
      {:ok, server} = TestServer.start_link()
      
      # Start client with test transport
      {:ok, client} = ExMCP.Client.start_link(
        transport: :test,
        server: server
      )
      
      %{client: client, server: server}
    end
    
    test "tool invocation round trip", %{client: client} do
      # Full integration test
      assert {:ok, result} = ExMCP.Client.call_tool(
        client,
        "test_tool",
        %{input: "test"}
      )
      
      assert result.content == [%{type: "text", text: "processed"}]
    end
  end
end
```

#### Property-based Testing
```elixir
defmodule ExMCP.PropertyTest do
  use ExUnitProperties
  
  property "all valid JSON-RPC messages can be encoded/decoded" do
    check all message <- json_rpc_message_generator() do
      encoded = ExMCP.Protocol.encode(message)
      {:ok, decoded} = ExMCP.Protocol.decode(encoded)
      assert decoded == message
    end
  end
  
  property "schema validation catches all invalid inputs" do
    check all tool_input <- tool_input_generator(),
              schema <- json_schema_generator() do
      
      result = ExMCP.Schema.validate(tool_input, schema)
      
      # If valid according to our validator, 
      # it should also be valid according to JSON Schema spec
      if result == :ok do
        assert JSONSchema.valid?(tool_input, schema)
      end
    end
  end
end
```

### 6.2 Test Helpers

#### Assertion Helpers
```elixir
defmodule ExMCP.Assertions do
  import ExUnit.Assertions
  
  def assert_content(result, expected) do
    assert length(result.content) == length(expected)
    
    Enum.zip(result.content, expected)
    |> Enum.each(fn {actual, expected} ->
      assert_content_item(actual, expected)
    end)
  end
  
  def assert_content_item(%{type: "text"} = actual, expected) do
    assert actual.text =~ expected.text
  end
  
  def assert_received_notification(method, timeout \\ 100) do
    assert_receive {:notification, %{method: ^method}}, timeout
  end
  
  def refute_received_notification(method, timeout \\ 100) do
    refute_receive {:notification, %{method: ^method}}, timeout
  end
end
```

#### Test Data Builders
```elixir
defmodule ExMCP.TestBuilders do
  def tool(name, opts \\ []) do
    %{
      name: name,
      description: opts[:description] || "Test tool",
      inputSchema: opts[:schema] || %{
        type: "object",
        properties: %{}
      }
    }
  end
  
  def resource(uri, opts \\ []) do
    %{
      uri: uri,
      name: opts[:name] || "Test resource",
      mimeType: opts[:mime_type] || "text/plain"
    }
  end
  
  def prompt(name, opts \\ []) do
    %{
      name: name,
      description: opts[:description] || "Test prompt",
      arguments: opts[:arguments] || []
    }
  end
end
```

### 6.3 Testing Strategies

#### Unit Testing
- Test each transport implementation in isolation
- Test protocol encoding/decoding
- Test capability negotiation logic
- Test individual server callbacks

#### Integration Testing
- Test full client-server communication flows
- Test transport switching and fallback
- Test error handling and recovery
- Test subscription mechanisms

#### Performance Testing
```elixir
defmodule ExMCP.PerformanceTest do
  use ExMCP.TestCase
  
  @tag :performance
  test "Native transport maintains sub-millisecond latency" do
    {:ok, server} = NativeServer.start_link()
    {:ok, client} = ExMCP.Client.start_link(transport: :native)
    
    # Warm up
    for _ <- 1..100 do
      ExMCP.Client.call_tool(client, "echo", %{})
    end
    
    # Measure
    timings = for _ <- 1..1000 do
      {time, _} = :timer.tc(fn ->
        ExMCP.Client.call_tool(client, "echo", %{})
      end)
      time
    end
    
    avg_microseconds = Enum.sum(timings) / length(timings)
    assert avg_microseconds < 50 # Should be ~15μs
  end
end
```

#### Chaos Testing
```elixir
defmodule ExMCP.ChaosTest do
  use ExMCP.TestCase
  
  @tag :chaos
  test "client recovers from transport failures" do
    {:ok, client} = ExMCP.Client.start_link(
      transport: :http,
      reconnect: true
    )
    
    # Simulate transport failure
    ChaosMonkey.kill_transport(client)
    
    # Client should reconnect
    Process.sleep(100)
    
    # Should work again
    assert {:ok, _} = ExMCP.Client.call_tool(client, "test", %{})
  end
end
```

### 6.4 Continuous Integration

#### GitHub Actions Workflow
```yaml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    
    strategy:
      matrix:
        elixir: ['1.15', '1.16']
        otp: ['25', '26']
    
    steps:
      - uses: actions/checkout@v3
      
      - uses: erlef/setup-beam@v1
        with:
          elixir-version: ${{ matrix.elixir }}
          otp-version: ${{ matrix.otp }}
      
      - run: mix deps.get
      - run: mix format --check-formatted
      - run: mix credo
      - run: mix dialyzer
      - run: mix test
      - run: mix test --tag performance
```

#### Coverage Requirements
```elixir
# mix.exs
def project do
  [
    test_coverage: [
      tool: ExCoveralls,
      minimum_coverage: 80
    ],
    preferred_cli_env: [
      coveralls: :test,
      "coveralls.html": :test,
      "coveralls.github": :test
    ]
  ]
end
```

### 6.5 Documentation Testing

#### Doctest Integration
```elixir
defmodule ExMCP.Examples do
  @moduledoc """
  Examples for ExMCP usage.
  
  ## Basic Usage
  
      iex> {:ok, server} = ExMCP.Server.start_link()
      iex> {:ok, client} = ExMCP.Client.start_link()
      iex> ExMCP.Client.connect(client, server)
      :ok
  """
end
```

#### Example Validation
```elixir
defmodule ExMCP.ExampleTest do
  use ExMCP.TestCase
  
  test "all examples in documentation work" do
    # Run all examples from docs
    for example <- find_doc_examples() do
      assert {result, _} = Code.eval_string(example)
      assert result != nil
    end
  end
  
  test "getting started guide is accurate" do
    # Follow the getting started guide programmatically
    assert {:ok, _} = run_getting_started_steps()
  end
end
```

## Implementation Timeline

1. **Phase 1**: Foundation (ExMCP.Plug + ToolRegistry)
2. **Phase 2**: Magic Layer (ExMCP.Server + deftool)
3. **Phase 3**: Client Hardening (Synchronous start_link)
4. **Phase 4**: DX Enhancements (Convenience functions + Response normalization)
5. **Phase 5**: DSL Implementation (Tools, Resources, Prompts)
6. **Phase 6**: Testing & Reliability

Each phase should be fully tested and validated before proceeding to the next phase.

---

This design plan provides comprehensive implementation details for each phase, specific code examples, integration strategies, and clear success metrics. The plan transforms ExMCP from a low-level transport library into a high-level, developer-friendly framework while maintaining the flexibility that advanced users need.