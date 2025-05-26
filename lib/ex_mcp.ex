defmodule ExMCP do
  @moduledoc """
  ExMCP - Elixir implementation of the Model Context Protocol.

  This is the main entry point for the ExMCP library. For most use cases,
  you'll want to use the specific modules:

  - `ExMCP.Client` - For connecting to MCP servers
  - `ExMCP.Server` - For implementing MCP servers
  - `ExMCP.ServerManager` - For managing multiple server connections
  - `ExMCP.Discovery` - For discovering available MCP servers

  ## Transport Options

  ExMCP supports multiple transport layers:

  - **stdio** - Process communication via standard input/output
  - **SSE** - Server-Sent Events over HTTP
  - **BEAM** - Native Erlang/Elixir message passing

  ## Quick Examples

  ### Client Usage

      # Connect via stdio to an external process
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: ["npx", "-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
      )

      # Connect via BEAM transport to an Elixir server
      {:ok, client} = ExMCP.Client.start_link(
        transport: :beam,
        server: :my_server
      )

      # Connect via SSE
      {:ok, client} = ExMCP.Client.start_link(
        transport: :sse,
        url: "http://localhost:8080/sse"
      )

      # List and call tools
      {:ok, tools} = ExMCP.Client.list_tools(client)
      {:ok, result} = ExMCP.Client.call_tool(client, "read_file", %{"path" => "/tmp/test.txt"})

  ### Server Usage

      defmodule MyServer do
        use ExMCP.Server.Handler

        @impl true
        def handle_list_tools(state) do
          tools = [%{name: "hello", description: "Say hello"}]
          {:ok, tools, state}
        end

        @impl true
        def handle_call_tool("hello", %{"name" => name} = _args, state) do
          {:ok, [%{type: "text", text: "Hello, " <> name <> "!"}], state}
        end
      end

      # Start with stdio transport
      {:ok, server} = ExMCP.Server.start_link(
        handler: MyServer,
        transport: :stdio
      )

      # Or with BEAM transport for Elixir-to-Elixir communication
      {:ok, server} = ExMCP.Server.start_link(
        handler: MyServer,
        transport: :beam,
        name: :my_server  # Optional: register with a name
      )

  ### BEAM Transport Example

  The BEAM transport is ideal for building Elixir-native tool ecosystems:

      # Define a calculation server
      defmodule CalcServer do
        use ExMCP.Server.Handler
        
        @impl true
        def handle_call_tool("add", %{"a" => a, "b" => b}, state) do
          result = a + b
          {:ok, [%{type: "text", text: "Result: " <> to_string(result)}], state}
        end
      end

      # Start server
      {:ok, server} = ExMCP.Server.start_link(
        handler: CalcServer,
        transport: :beam,
        name: :calc
      )

      # Connect client (can be from different process/node)
      {:ok, client} = ExMCP.Client.start_link(
        transport: :beam,
        server: :calc
      )

      # Use it
      {:ok, result} = ExMCP.Client.call_tool(client, "add", %{"a" => 5, "b" => 3})

  For more examples, see the `examples/beam_transport/` directory.
  """

  @doc """
  Returns the version of the MCP protocol this library implements.
  """
  @spec protocol_version() :: String.t()
  def protocol_version do
    "2024-11-05"
  end

  @doc """
  Returns the version of the ExMCP library.
  """
  @spec version() :: String.t()
  def version do
    "0.2.0"
  end
end
