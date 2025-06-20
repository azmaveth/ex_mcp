defmodule ExMCP do
  @moduledoc """
  ExMCP - Complete Elixir implementation of the Model Context Protocol.

  ExMCP enables AI models to securely interact with local and remote resources through
  a standardized protocol. It provides both client and server implementations with
  multiple transport options.

  ## Public API

  ExMCP provides a clean, focused public API. Only use these modules in your applications:

  ### Core Modules
  - `ExMCP` - This module (convenience functions and metadata)
  - `ExMCP.Client` - MCP client implementation
  - `ExMCP.Server` - MCP server implementation
  - `ExMCP.Native` - High-performance BEAM service dispatcher
  - `ExMCP.Service` - Macro for automatic service registration
  - `ExMCP.Transport` - Transport behaviour definition

  ### Optional Features
  - `ExMCP.Authorization` - OAuth 2.1 authorization flows (MCP optional feature)

  ### Supporting Modules
  - `ExMCP.Content` - Content type helpers
  - `ExMCP.Types` - Type definitions (stable across versions)

  > #### Internal Modules {: .warning}
  >
  > All other modules under the `ExMCP` namespace are internal implementation details
  > and may change without notice. Do not depend on them directly in your applications.

  ## Quick Start

  ### Start a Client

      # Connect to stdio server
      {:ok, client} = ExMCP.start_client(
        transport: :stdio,
        command: ["python", "mcp-server.py"]
      )

      # Connect with HTTP
      {:ok, client} = ExMCP.start_client(
        transport: :http,
        url: "https://api.example.com"
      )

  ### Start a Server

      {:ok, server} = ExMCP.start_server(
        handler: MyApp.MCPHandler,
        transport: :stdio
      )

  ### Native BEAM Communication

      # High-performance service-to-service calls
      {:ok, result} = ExMCP.Native.call(:my_service, "method", %{})

  ## Protocol Versions

  ExMCP supports multiple MCP protocol versions:
  - **2024-11-05** - Base MCP features
  - **2025-03-26** - Previous stable (subscriptions, roots, logging, batch support)
  - **2025-06-18** - Current stable (structured output, OAuth 2.1, elicitation, no batch)

  See the README.md for a complete feature comparison chart.

  ## Features

  - **Tools** - Register and execute functions with parameters
  - **Resources** - List and read data from various sources
  - **Prompts** - Manage reusable prompt templates
  - **Sampling** - Direct LLM integration for response generation
  - **Roots** - URI-based resource boundaries
  - **Subscriptions** - Monitor resources for changes
  - **Progress** - Track long-running operations
  - **Notifications** - Real-time updates for changes
  - **Native BEAM** - High-performance Elixir-to-Elixir communication

  ## Transport Options

  - **stdio** - Process communication (standard MCP)
  - **HTTP/SSE** - Web-friendly transport (standard MCP)
  - **Native BEAM** - Direct Erlang process communication (ExMCP extension)

  ## Examples

  ### Basic Client Usage

      {:ok, client} = ExMCP.start_client(transport: :stdio, command: ["mcp-server"])

      # List and call tools
      {:ok, %{tools: tools}} = ExMCP.Client.list_tools(client)
      {:ok, result} = ExMCP.Client.call_tool(client, "search", %{query: "elixir"})

      # Read resources
      {:ok, content} = ExMCP.Client.read_resource(client, "file:///data.json")

  ### Basic Server Usage

      defmodule MyHandler do
        use ExMCP.Server.Handler

        @impl true
        def handle_list_tools(state) do
          tools = [%{name: "echo", description: "Echo input"}]
          {:ok, tools, state}
        end

        @impl true
        def handle_call_tool("echo", params, state) do
          {:ok, [%{type: "text", text: params["message"]}], state}
        end
      end

      {:ok, server} = ExMCP.start_server(handler: MyHandler, transport: :stdio)

  ### Native BEAM Service

      defmodule MyService do
        use ExMCP.Service, name: :my_service

        @impl true
        def handle_mcp_request("ping", _params, state) do
          {:ok, %{"response" => "pong"}, state}
        end
      end

      # Automatic registration and discovery
      {:ok, _} = MyService.start_link()
      {:ok, result} = ExMCP.Native.call(:my_service, "ping", %{})
  """

  alias ExMCP.Client
  alias ExMCP.Server

  # Convenience aliases
  alias ExMCP.{ConvenienceClient, SimpleClient}
  alias ExMCP.{Error, Response}

  @doc """
  Convenience function to start an MCP client.

  This is equivalent to `ExMCP.Client.start_link/1` but provides a simpler
  entry point for common use cases.

  ## Examples

      # stdio transport
      {:ok, client} = ExMCP.start_client(
        transport: :stdio,
        command: ["python", "mcp-server.py"]
      )

      # HTTP transport
      {:ok, client} = ExMCP.start_client(
        transport: :http,
        url: "https://api.example.com"
      )

  """
  @spec start_client(keyword()) :: {:ok, pid()} | {:error, term()}
  def start_client(opts) do
    Client.start_link(opts)
  end

  @doc """
  Convenience function to start an MCP server.

  This is equivalent to `ExMCP.Server.start_link/1` but provides a simpler
  entry point for common use cases.

  ## Examples

      {:ok, server} = ExMCP.start_server(
        handler: MyApp.Handler,
        transport: :stdio
      )

  """
  @spec start_server(keyword()) :: {:ok, pid()} | {:error, term()}
  def start_server(opts) do
    Server.start_link(opts)
  end

  @doc """
  Returns the version of the MCP protocol this library implements by default.

  ExMCP supports multiple protocol versions. This returns the default/latest stable version.
  """
  @spec protocol_version() :: String.t()
  def protocol_version do
    "2025-03-26"
  end

  @doc """
  Returns the version of the ExMCP library.
  """
  @spec version() :: String.t()
  def version do
    Application.spec(:ex_mcp, :vsn) |> to_string()
  end

  @doc """
  Returns information about supported protocol versions.
  """
  @spec supported_versions() :: [String.t()]
  def supported_versions do
    ["2024-11-05", "2025-03-26", "2025-06-18"]
  end

  # Server Convenience Macros

  @doc """
  Convenience macro that aliases `use ExMCP.Server`.

  This provides backward compatibility and a simpler import path for new users.

  ## Examples

      defmodule MyServer do
        use ExMCP.Server  # Same as `use ExMCP.Server`

        deftool "hello" do
          description "Says hello"
        end
      end
  """
  defmacro __using__(opts) do
    quote do
      use ExMCP.Server, unquote(opts)
    end
  end

  # V2 Convenience Functions

  @type v2_client :: pid()
  @type connection_spec :: String.t() | {atom(), keyword()} | [any()] | ExMCP.ClientConfig.t()

  @doc """
  Connects to an MCP server using the v2 client implementations.

  This function provides a simplified interface to the v2 clients with
  automatic client type selection and connection configuration.

  ## Options

  - `:client_type` - Force a specific client (`:simple`, `:v2`, `:convenience`)
  - `:timeout` - Connection timeout in milliseconds (default: 10_000)
  - `:retry_attempts` - Number of retry attempts (default: 3)
  - Transport-specific options (see individual client docs)

  ## Examples

      # HTTP connection using convenience client
      {:ok, client} = ExMCP.connect("http://localhost:8080")

      # Stdio connection using simple client
      {:ok, client} = ExMCP.connect({:stdio, command: "my-server"})

      # Multiple transports with fallback
      {:ok, client} = ExMCP.connect([
        "http://primary:8080",
        "http://backup:8080"
      ])

      # Force specific client type
      {:ok, client} = ExMCP.connect("http://localhost:8080", client_type: :v2)

      # Using ClientConfig for advanced configuration
      config = ExMCP.ClientConfig.new(:production)
      |> ExMCP.ClientConfig.put_transport(:http, url: "https://api.example.com")
      |> ExMCP.ClientConfig.put_auth(:bearer, token: "secret")
      |> ExMCP.ClientConfig.put_retry_policy(max_attempts: 5)
      {:ok, client} = ExMCP.connect(config)
  """
  @spec connect(connection_spec(), keyword()) :: {:ok, v2_client()} | {:error, any()}
  def connect(connection_spec, opts \\ [])

  def connect(%ExMCP.ClientConfig{} = config, opts) do
    # ClientConfig provided - convert to client options and connect
    client_type = Keyword.get(opts, :client_type, :simple)
    client_opts = ExMCP.ClientConfig.to_client_opts(config)

    # Merge any additional opts
    final_opts = Keyword.merge(client_opts, opts)

    case client_type do
      :convenience ->
        ConvenienceClient.connect(
          config.transport.url || {config.transport.type, final_opts},
          final_opts
        )

      :v2 ->
        Client.start_link(final_opts)

      :simple ->
        SimpleClient.start_link(final_opts)
    end
  end

  def connect(connection_spec, opts) when is_list(connection_spec) do
    # Multiple connections - use ConvenienceClient for fallback support
    ConvenienceClient.connect(connection_spec, opts)
  end

  def connect(connection_spec, opts) do
    client_type = Keyword.get(opts, :client_type, :simple)

    case client_type do
      :convenience ->
        ConvenienceClient.connect(connection_spec, opts)

      :v2 ->
        # Convert connection spec to Client format
        client_opts = normalize_connection_for_v2(connection_spec, opts)
        Client.start_link(client_opts)

      :simple ->
        # Convert connection spec to SimpleClient format
        client_opts = normalize_connection_for_simple(connection_spec, opts)
        SimpleClient.start_link(client_opts)
    end
  end

  @doc """
  Disconnects from an MCP server (v2 clients).
  """
  @spec disconnect(v2_client()) :: :ok
  def disconnect(client) do
    # Try to gracefully stop the client
    if function_exported?(ConvenienceClient, :disconnect, 1) do
      ConvenienceClient.disconnect(client)
    else
      GenServer.stop(client, :normal)
    end
  catch
    # Already stopped
    :exit, _ -> :ok
  end

  @doc """
  Lists available tools from the connected server (v2 clients).

  Returns a list of tool definitions with their schemas and descriptions.
  """
  @spec tools(v2_client(), keyword()) :: [map()] | {:error, any()}
  def tools(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)

    try do
      case SimpleClient.list_tools(client, timeout) do
        {:ok, tools} -> tools
        error -> error
      end
    rescue
      # Fallback to other client types
      _ ->
        try do
          case Client.list_tools(client, timeout) do
            {:ok, tools} -> tools
            error -> error
          end
        rescue
          _ -> {:error, Error.connection_error("Client not responding")}
        end
    end
  end

  @doc """
  Calls a tool on the connected server (v2 clients).

  ## Options

  - `:timeout` - Request timeout in milliseconds (default: 30_000)
  - `:normalize` - Whether to normalize the response (default: true)

  ## Examples

      # Simple call
      result = ExMCP.call(client, "calculator", %{op: "add", a: 1, b: 2})

      # With options
      result = ExMCP.call(client, "slow_tool", %{data: "..."}, timeout: 60_000)
  """
  @spec call(v2_client(), String.t(), map(), keyword()) :: any() | {:error, any()}
  def call(client, tool_name, args \\ %{}, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    normalize = Keyword.get(opts, :normalize, true)

    try do
      case SimpleClient.call_tool(client, tool_name, args, timeout) do
        {:ok, %Response{} = response} ->
          if normalize do
            Response.text_content(response) || response
          else
            response
          end

        error ->
          error
      end
    rescue
      # Fallback to other client types
      _ ->
        try do
          case Client.call_tool(client, tool_name, args, timeout) do
            {:ok, %Response{} = response} ->
              if normalize do
                Response.text_content(response) || response
              else
                response
              end

            error ->
              error
          end
        rescue
          _ -> {:error, Error.connection_error("Client not responding")}
        end
    end
  end

  @doc """
  Lists available resources from the connected server (v2 clients).
  """
  @spec resources(v2_client(), keyword()) :: [map()] | {:error, any()}
  def resources(client, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5_000)

    try do
      case SimpleClient.list_resources(client, timeout) do
        {:ok, resources} -> resources
        error -> error
      end
    rescue
      _ ->
        try do
          case Client.list_resources(client, timeout) do
            {:ok, resources} -> resources
            error -> error
          end
        rescue
          _ -> {:error, Error.connection_error("Client not responding")}
        end
    end
  end

  @doc """
  Reads a resource from the connected server (v2 clients).

  ## Options

  - `:timeout` - Request timeout in milliseconds (default: 10_000)
  - `:parse_json` - Automatically parse JSON content (default: false)

  ## Examples

      # Read text content
      content = ExMCP.read(client, "file://data.txt")

      # Read and parse JSON
      data = ExMCP.read(client, "file://config.json", parse_json: true)
  """
  @spec read(v2_client(), String.t(), keyword()) :: any() | {:error, any()}
  def read(client, uri, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 10_000)
    parse_json = Keyword.get(opts, :parse_json, false)

    try do
      case SimpleClient.read_resource(client, uri, timeout) do
        {:ok, %Response{} = response} ->
          content =
            Response.resource_content(response) || Response.text_content(response) ||
              Response.data_content(response)

          if parse_json and is_binary(content) do
            case Jason.decode(content) do
              {:ok, parsed} -> parsed
              {:error, _} -> content
            end
          else
            content
          end

        error ->
          error
      end
    rescue
      _ ->
        try do
          case Client.read_resource(client, uri, timeout) do
            {:ok, %Response{} = response} ->
              content =
                Response.resource_content(response) || Response.text_content(response) ||
                  Response.data_content(response)

              if parse_json and is_binary(content) do
                case Jason.decode(content) do
                  {:ok, parsed} -> parsed
                  {:error, _} -> content
                end
              else
                content
              end

            error ->
              error
          end
        rescue
          _ -> {:error, Error.connection_error("Client not responding")}
        end
    end
  end

  @doc """
  Gets connection status and server information (v2 clients).
  """
  @spec status(v2_client()) :: {:ok, map()} | {:error, any()}
  def status(client) do
    case SimpleClient.get_status(client) do
      {:ok, status} -> {:ok, status}
      error -> error
    end
  rescue
    _ ->
      try do
        Client.get_status(client)
      rescue
        _ -> {:error, Error.connection_error("Client not responding")}
      end
  end

  @doc """
  Tests connectivity to an MCP server without establishing a persistent connection.
  """
  @spec ping(connection_spec(), keyword()) :: :ok | {:error, any()}
  def ping(connection_spec, opts \\ []) do
    ConvenienceClient.ping(connection_spec, opts)
  end

  @doc """
  Gets library configuration and capabilities.
  """
  @spec info() :: map()
  def info do
    %{
      version: version(),
      protocol_versions: supported_versions(),
      transports: [:http, :stdio, :sse, :native],
      features: [
        :structured_responses,
        :backward_compatibility,
        :dsl_syntax,
        :automatic_reconnection,
        :transport_fallback,
        :type_safety
      ]
    }
  end

  # Private Helper Functions for V2

  defp normalize_connection_for_v2(connection_spec, opts) when is_binary(connection_spec) do
    # Parse URL and convert to transport options
    uri = URI.parse(connection_spec)

    transport_opts =
      case uri.scheme do
        "http" -> [transport: :http, url: connection_spec]
        "https" -> [transport: :http, url: connection_spec]
        _ -> [transport: :stdio, command: connection_spec]
      end

    Keyword.merge(transport_opts, Keyword.drop(opts, [:client_type]))
  end

  defp normalize_connection_for_v2({transport, transport_opts}, opts) do
    [transport: transport] ++ transport_opts ++ Keyword.drop(opts, [:client_type])
  end

  defp normalize_connection_for_v2(connection_spec, opts) do
    # For other formats, pass through
    Keyword.merge([connection: connection_spec], Keyword.drop(opts, [:client_type]))
  end

  defp normalize_connection_for_simple(connection_spec, opts) do
    # SimpleClient expects similar format to v2
    normalize_connection_for_v2(connection_spec, opts)
  end
end
