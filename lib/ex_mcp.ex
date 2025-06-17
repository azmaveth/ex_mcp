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
  - **2025-03-26** - Latest stable (subscriptions, roots, logging)
  - **draft** - Experimental features

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
    ["2024-11-05", "2025-03-26", "draft"]
  end
end
