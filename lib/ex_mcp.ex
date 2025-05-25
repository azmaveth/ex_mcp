defmodule ExMCP do
  @moduledoc """
  ExMCP - Elixir implementation of the Model Context Protocol.

  This is the main entry point for the ExMCP library. For most use cases,
  you'll want to use the specific modules:

  - `ExMCP.Client` - For connecting to MCP servers
  - `ExMCP.Server` - For implementing MCP servers
  - `ExMCP.ServerManager` - For managing multiple server connections
  - `ExMCP.Discovery` - For discovering available MCP servers

  ## Quick Examples

  ### Client Usage

      # Connect to a filesystem MCP server
      {:ok, client} = ExMCP.Client.start_link(
        transport: :stdio,
        command: ["npx", "-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
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

      {:ok, server} = ExMCP.Server.start_link(
        handler: MyServer,
        transport: :stdio
      )
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
    "0.1.0"
  end
end