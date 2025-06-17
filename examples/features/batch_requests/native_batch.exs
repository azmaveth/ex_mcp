#!/usr/bin/env elixir

# Example: Using batch requests with Native Service Dispatcher

Mix.install([
  {:ex_mcp, path: "."}
])

defmodule BatchExampleService do
  use ExMCP.Service, name: :batch_example

  @impl true
  def init(_args) do
    {:ok, %{}}
  end

  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "echo",
        "description" => "Echoes back the message",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "message" => %{"type" => "string"}
          },
          "required" => ["message"]
        }
      }
    ]
    {:ok, %{"tools" => tools}, state}
  end

  @impl true
  def handle_mcp_request("tools/call", %{"name" => "echo", "arguments" => %{"message" => msg}}, state) do
    {:ok, %{"content" => [%{"type" => "text", "text" => msg}]}, state}
  end

  @impl true
  def handle_mcp_request("list_resources", _params, state) do
    {:ok, %{"resources" => []}, state}
  end

  @impl true
  def handle_mcp_request("list_roots", _params, state) do
    {:ok, %{"roots" => []}, state}
  end

  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
end

defmodule BatchExample do
  def run do
    # Start the service
    {:ok, _service} = BatchExampleService.start_link([])
    
    # For batch requests with Native Service Dispatcher, we need to use
    # a regular MCP client connected via stdio transport
    # Create a wrapper server that bridges to Native
    {:ok, server} = ExMCP.Server.start_link(
      transport: :stdio,
      handler: NativeBridgeHandler
    )

    # Connect client
    {:ok, client} = ExMCP.Client.start_link(
      transport: :stdio,
      server: server
    )

    # Initialize
    {:ok, _} = ExMCP.Client.initialize(client, %{
      protocolVersion: ExMCP.protocol_version(),
      clientInfo: %{name: "batch-example", version: "1.0.0"}
    })

    IO.puts("Sending individual requests...")
    start_time = System.monotonic_time(:millisecond)

    # Individual requests (slower)
    {:ok, %{tools: tools}} = ExMCP.Client.list_tools(client)
    {:ok, %{resources: resources}} = ExMCP.Client.list_resources(client)
    {:ok, %{roots: roots}} = ExMCP.Client.list_roots(client)
    {:ok, echo_result} = ExMCP.Client.call_tool(client, "echo", %{message: "Hello"})

    individual_time = System.monotonic_time(:millisecond) - start_time
    IO.puts("Individual requests took: #{individual_time}ms")

    IO.puts("\nSending batch request...")
    start_time = System.monotonic_time(:millisecond)

    # Batch request (faster)
    requests = [
      {:list_tools, []},
      {:list_resources, []},
      {:list_roots, []},
      {:call_tool, ["echo", %{message: "Hello"}]}
    ]

    {:ok, [batch_tools, batch_resources, batch_roots, batch_echo]} =
      ExMCP.Client.batch_request(client, requests)

    batch_time = System.monotonic_time(:millisecond) - start_time
    IO.puts("Batch request took: #{batch_time}ms")

    IO.puts("\nComparison:")
    IO.puts("Individual: #{individual_time}ms")
    IO.puts("Batch: #{batch_time}ms")
    IO.puts("Speedup: #{Float.round(individual_time / batch_time, 2)}x")

    :ok
  end
end

# Bridge handler that forwards to Native Service
defmodule NativeBridgeHandler do
  use ExMCP.Server.Handler

  @impl true
  def init(_args), do: {:ok, %{}}

  @impl true
  def handle_initialize(_params, state) do
    {:ok, %{
      protocolVersion: ExMCP.protocol_version(),
      serverInfo: %{name: "native-bridge", version: "1.0.0"},
      capabilities: %{tools: %{}, resources: %{}, roots: %{}}
    }, state}
  end

  @impl true
  def handle_list_tools(_cursor, state) do
    case ExMCP.Native.call(:batch_example, "list_tools", %{}) do
      {:ok, %{"tools" => tools}} -> {:ok, tools, nil, state}
      {:error, reason} -> {:error, reason, state}
    end
  end

  @impl true
  def handle_call_tool(name, arguments, state) do
    case ExMCP.Native.call(:batch_example, "tools/call", %{"name" => name, "arguments" => arguments}) do
      {:ok, %{"content" => content}} -> {:ok, content, state}
      {:error, reason} -> {:error, reason, state}
    end
  end

  @impl true
  def handle_list_resources(_cursor, state) do
    case ExMCP.Native.call(:batch_example, "list_resources", %{}) do
      {:ok, %{"resources" => resources}} -> {:ok, resources, nil, state}
      {:error, reason} -> {:error, reason, state}
    end
  end

  @impl true
  def handle_list_roots(state) do
    case ExMCP.Native.call(:batch_example, "list_roots", %{}) do
      {:ok, %{"roots" => roots}} -> {:ok, roots, state}
      {:error, reason} -> {:error, reason, state}
    end
  end

  @impl true
  def handle_read_resource(_uri, state), do: {:error, :not_found, state}
  @impl true
  def handle_list_prompts(_cursor, state), do: {:ok, [], nil, state}
  @impl true
  def handle_get_prompt(_name, _args, state), do: {:error, :not_found, state}
  @impl true
  def handle_complete(_ref, _arg, state), do: {:ok, %{completion: %{values: []}}, state}
  @impl true
  def handle_set_log_level(_level, state), do: {:ok, %{}, state}
end

# Run the example
BatchExample.run()