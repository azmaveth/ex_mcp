defmodule ExMCP do
  @moduledoc """
  ExMCP - Complete Elixir implementation of the Model Context Protocol.

  ExMCP enables AI models to securely interact with local and remote resources through
  a standardized protocol. It provides both client and server implementations with
  multiple transport options.

  ## API Categories

  This library provides three categories of APIs:

  ### MCP Specification Features (Portable)

  These APIs implement the official MCP specification and are portable across all
  MCP implementations:

  - `ExMCP.Client` - Core client operations (list_tools, call_tool, etc.)
  - `ExMCP.Server` - Core server functionality
  - `ExMCP.Protocol` - Message encoding/decoding
  - `ExMCP.Types` - Standard MCP types

  ### ExMCP Extensions (Elixir-specific)

  > #### Extension Feature {: .warning}
  > These features are specific to ExMCP and not part of the official MCP specification.

  - `ExMCP.ServerManager` - Manage multiple server connections
  - `ExMCP.Discovery` - Discover available MCP servers
  - `ExMCP.Transport.Beam` - Native Erlang process transport
  - Automatic reconnection in `ExMCP.Client`
  - Batch operations in `ExMCP.Client`

  ### Draft Specification Features (Experimental)

  > #### Draft Feature {: .info}
  > These features implement the draft MCP specification and may change.

  - **Structured Tool Output** - Tools can return structured data with schema validation
    - `outputSchema` field in tool definitions
    - `structuredContent` in tool results
    - See `ExMCP.Server.Handler` for examples

  ## Protocol Features

  ExMCP implements the full MCP specification (version 2025-03-26):

  - **Tools** - Register and execute functions with parameters
  - **Resources** - List and read data from various sources
  - **Prompts** - Manage reusable prompt templates
  - **Sampling** - Direct LLM integration for response generation
  - **Roots** - URI-based resource boundaries
  - **Subscriptions** - Monitor resources for changes
  - **Progress** - Track long-running operations
  - **Notifications** - Real-time updates for changes

  ## Transport Options

  ### stdio Transport
  Process communication via standard input/output (standard MCP specification). Best for:
  - Subprocess communication
  - Cross-language integration
  - Command-line tools

  ### Streamable HTTP Transport
  HTTP with Server-Sent Events streaming (standard MCP specification). Best for:
  - Web integration
  - Firewall-friendly communication
  - RESTful architectures

  ### BEAM Transport
  Native Erlang/Elixir message passing (ExMCP extension). Best for:
  - Elixir-to-Elixir communication
  - High-performance local tools
  - Distributed Erlang clusters

  ## Quick Start

  ### Client Example

      # Connect to a stdio server
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: ["python", "mcp-server.py"]
      )

      # List available tools
      {:ok, tools} = ExMCP.Client.list_tools(client)

      # Call a tool
      {:ok, result} = ExMCP.Client.call_tool(client, "search", %{
        query: "Elixir metaprogramming"
      })

      # Read a resource
      {:ok, content} = ExMCP.Client.read_resource(client, "file:///data.json")

      # Subscribe to changes
      {:ok, _} = ExMCP.Client.subscribe_resource(client, "file:///config.json")

  ### Server Example

      defmodule MyServer do
        use ExMCP.Server.Handler

        @impl true
        def init(_args) do
          {:ok, %{}}
        end

        @impl true
        def handle_initialize(_params, state) do
          {:ok, %{
            name: "my-server",
            version: "1.0.0",
            capabilities: %{
              tools: %{},
              resources: %{subscribe: true},
              roots: %{}
            }
          }, state}
        end

        @impl true
        def handle_list_tools(state) do
          tools = [
            %{
              name: "echo",
              description: "Echoes the input",
              input_schema: %{
                type: "object",
                properties: %{
                  message: %{type: "string"}
                },
                required: ["message"]
              },
              # Tool annotations (new in 2025-03-26)
              readOnlyHint: true,
              destructiveHint: false,
              costHint: :low
            }
          ]
          {:ok, tools, state}
        end

        @impl true
        def handle_call_tool("echo", %{"message" => msg}, state) do
          {:ok, [%{type: "text", text: msg}], state}
        end

        # ... implement other callbacks
      end

      # Start the server
      {:ok, server} = ExMCP.Server.start_link(
        handler: MyServer,
        transport: :stdio
      )

  ### BEAM Transport Example

      # Server on node1
      {:ok, server} = ExMCP.Server.start_link(
        handler: ToolServer,
        transport: :beam,
        name: {:global, :tool_server}
      )

      # Client on node2
      {:ok, client} = ExMCP.Client.start_link(
        transport: :beam,
        server: {:global, :tool_server}
      )

      # Works transparently across nodes
      {:ok, result} = ExMCP.Client.call_tool(client, "process", %{})

  ## Documentation

  - [User Guide](https://github.com/azmaveth/ex_mcp/blob/master/USER_GUIDE.md) - Comprehensive guide
  - [Examples](https://github.com/azmaveth/ex_mcp/tree/master/examples) - Working examples
  - [API Docs](https://hexdocs.pm/ex_mcp) - Full API reference

  ## Protocol Compliance

  ExMCP implements MCP specification version 2025-03-26, including all latest features:
  roots capability, resource subscriptions, tool annotations, and multimodal content support.
  """

  @doc """
  Returns the version of the MCP protocol this library implements.
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
    "0.2.0"
  end
end
