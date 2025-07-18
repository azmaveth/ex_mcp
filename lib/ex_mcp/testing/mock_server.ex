defmodule ExMCP.Testing.MockServer do
  @moduledoc """
  Mock MCP server implementation for testing.

  This module provides a comprehensive mock server that can simulate
  various MCP server behaviors for testing client implementations.
  It supports all MCP protocol features including tools, resources,
  prompts, and streaming.

  ## Features

  - **Full Protocol Support**: All MCP methods and notifications
  - **Configurable Responses**: Customize server behavior for testing
  - **State Management**: Stateful behavior for complex scenarios
  - **Error Simulation**: Test error handling and edge cases
  - **Performance Testing**: Latency and throughput simulation
  - **Transport Agnostic**: Works with all transport implementations

  ## Usage

      # Simple mock server
      MockServer.with_server([], fn client ->
        {:ok, result} = ExMCP.Client.list_tools(client)
        assert length(result["tools"]) == 0
      end)

      # Server with predefined tools
      MockServer.with_server([
        tools: [MockServer.sample_tool()],
        resources: [MockServer.sample_resource()]
      ], fn client ->
        # Test with mock data
      end)

      # Server with custom behavior
      MockServer.with_server([
        handler: MyCustomHandler,
        state: %{custom_data: "test"}
      ], fn client ->
        # Test with custom handler
      end)
  """

  use GenServer

  alias ExMCP.Testing.Builders

  @typedoc "Mock server configuration"
  @type mock_config :: [
          tools: [map()],
          resources: [map()],
          prompts: [map()],
          handler: module() | nil,
          state: map(),
          latency: pos_integer(),
          error_rate: float(),
          capabilities: map()
        ]

  @typedoc "Mock server state"
  @type server_state :: %{
          tools: [map()],
          resources: [map()],
          prompts: [map()],
          handler: module() | nil,
          custom_state: map(),
          latency: pos_integer(),
          error_rate: float(),
          capabilities: map(),
          call_count: map(),
          last_calls: [map()]
        }

  # Client API

  @doc """
  Starts a mock server and runs a test function with a connected client.

  ## Options

  - `:tools` - List of tool definitions to serve
  - `:resources` - List of resource definitions to serve
  - `:prompts` - List of prompt definitions to serve
  - `:handler` - Custom handler module for advanced behavior
  - `:state` - Initial custom state for the handler
  - `:latency` - Simulated response latency in milliseconds
  - `:error_rate` - Probability (0.0-1.0) of returning errors
  - `:capabilities` - Custom server capabilities

  ## Examples

      with_server([], fn client ->
        result = ExMCP.Client.list_tools(client)
        assert {:ok, %{"tools" => []}} = result
      end)

      with_server([
        tools: [sample_tool()],
        latency: 100
      ], fn client ->
        # Test with 100ms latency
      end)
  """
  @spec with_server(mock_config(), (pid() -> any())) :: any()
  def with_server(config \\ [], test_fun) when is_function(test_fun, 1) do
    # Start mock server
    {:ok, server_pid} = start_link(config)

    # Create test transport that connects to mock server
    transport_config = [
      type: :mock,
      server_pid: server_pid,
      timeout: Keyword.get(config, :timeout, 5000)
    ]

    # Start client with mock transport
    client_config = [
      transport: transport_config,
      auto_initialize: true
    ]

    {:ok, client_pid} = ExMCP.Client.start_link(client_config)

    try do
      # Run test with client
      test_fun.(client_pid)
    after
      # Clean up
      if Process.alive?(client_pid), do: GenServer.stop(client_pid)
      if Process.alive?(server_pid), do: GenServer.stop(server_pid)
    end
  end

  @doc """
  Creates a sample tool definition for testing.

  ## Examples

      tool = sample_tool()
      # %{"name" => "sample_tool", "description" => "...", "inputSchema" => %{...}}

      tool = sample_tool(name: "custom_tool", description: "Custom tool")
  """
  @spec sample_tool() :: map()
  @spec sample_tool(keyword()) :: map()
  def sample_tool(opts \\ []) do
    name = Keyword.get(opts, :name, "sample_tool")
    description = Keyword.get(opts, :description, "A sample tool for testing")

    Builders.tool(name,
      description: description,
      schema:
        Builders.object_schema(
          %{
            "input" => Builders.string_schema(description: "Input text"),
            "options" =>
              Builders.object_schema(%{
                "format" => Builders.string_schema(enum: ["text", "json"])
              })
          },
          required: ["input"]
        )
    )
  end

  @doc """
  Creates a sample resource definition for testing.

  ## Examples

      resource = sample_resource()
      # %{"uri" => "file://sample.txt", "name" => "Sample", ...}

      resource = sample_resource(uri: "https://api.example.com", name: "API")
  """
  @spec sample_resource() :: map()
  @spec sample_resource(keyword()) :: map()
  def sample_resource(opts \\ []) do
    uri = Keyword.get(opts, :uri, "file://sample_data.txt")
    name = Keyword.get(opts, :name, "Sample Resource")

    Builders.resource(uri, name,
      description: Keyword.get(opts, :description, "A sample resource for testing"),
      mime_type: Keyword.get(opts, :mime_type, "text/plain")
    )
  end

  @doc """
  Creates a sample prompt definition for testing.

  ## Examples

      prompt = sample_prompt()
      # %{"name" => "sample_prompt", "description" => "...", "arguments" => [...]}

      prompt = sample_prompt(name: "custom_prompt")
  """
  @spec sample_prompt() :: map()
  @spec sample_prompt(keyword()) :: map()
  def sample_prompt(opts \\ []) do
    name = Keyword.get(opts, :name, "sample_prompt")
    description = Keyword.get(opts, :description, "A sample prompt for testing")

    Builders.prompt(name, description,
      arguments: [
        Builders.prompt_argument("topic", "The topic to write about", required: true),
        Builders.prompt_argument("style", "Writing style", required: false)
      ]
    )
  end

  # GenServer Implementation

  @spec start_link(mock_config()) :: GenServer.on_start()
  def start_link(config \\ []) do
    GenServer.start_link(__MODULE__, config)
  end

  @impl GenServer
  def init(config) do
    state = %{
      tools: Keyword.get(config, :tools, []),
      resources: Keyword.get(config, :resources, []),
      prompts: Keyword.get(config, :prompts, []),
      handler: Keyword.get(config, :handler),
      custom_state: Keyword.get(config, :state, %{}),
      latency: Keyword.get(config, :latency, 0),
      error_rate: Keyword.get(config, :error_rate, 0.0),
      capabilities: build_capabilities(config),
      call_count: %{},
      last_calls: []
    }

    {:ok, state}
  end

  @impl GenServer
  def handle_call({:mcp_request, request}, _from, state) do
    # Simulate latency if configured
    if state.latency > 0 do
      Process.sleep(state.latency)
    end

    # Simulate errors if configured
    if should_simulate_error?(state.error_rate) do
      error_response =
        Builders.error_response(
          Map.get(request, "id"),
          -1,
          "Simulated error for testing"
        )

      {:reply, {:error, error_response}, state}
    else
      # Process request normally
      {response, new_state} = handle_mcp_request(request, state)
      {:reply, {:ok, response}, new_state}
    end
  end

  @impl GenServer
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  @impl GenServer
  def handle_call({:set_state, new_custom_state}, _from, state) do
    new_state = %{state | custom_state: new_custom_state}
    {:reply, :ok, new_state}
  end

  @impl GenServer
  def handle_call(:get_call_count, _from, state) do
    {:reply, state.call_count, state}
  end

  @impl GenServer
  def handle_call(:reset_call_count, _from, state) do
    new_state = %{state | call_count: %{}, last_calls: []}
    {:reply, :ok, new_state}
  end

  @impl GenServer
  def handle_info({:test_transport_connect, client_pid}, state) do
    # Store the connected client pid for test transport
    new_state = Map.put(state, :test_client_pid, client_pid)
    {:noreply, new_state}
  end

  @impl GenServer
  def handle_info({:transport_message, message}, state) do
    # Handle test transport messages
    case Jason.decode(message) do
      {:ok, request} ->
        # Process the request and send response back to client
        {response, new_state} = handle_mcp_request(request, state)

        # Send response back to client if connected and response is not nil (notifications don't need responses)
        if response && state[:test_client_pid] do
          client_pid = state[:test_client_pid]

          case Jason.encode(response) do
            {:ok, encoded_response} ->
              send(client_pid, {:transport_message, encoded_response})

            {:error, _} ->
              error_response = %{
                "jsonrpc" => "2.0",
                "id" => request["id"],
                "error" => %{"code" => -32603, "message" => "Failed to encode response"}
              }

              encoded_error = Jason.encode!(error_response)
              send(client_pid, {:transport_message, encoded_error})
          end
        end

        {:noreply, new_state}

      {:error, _} ->
        # Invalid JSON - send error response
        if client_pid = state[:test_client_pid] do
          error_response = %{
            "jsonrpc" => "2.0",
            "id" => nil,
            "error" => %{"code" => -32700, "message" => "Parse error"}
          }

          encoded_error = Jason.encode!(error_response)
          send(client_pid, {:transport_message, encoded_error})
        end

        {:noreply, state}
    end
  end

  @impl GenServer
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  # MCP Request Handlers

  defp handle_mcp_request(%{"method" => "initialize"} = request, state) do
    response =
      Builders.success_response(
        request["id"],
        %{
          "protocolVersion" => "2025-06-18",
          "capabilities" => state.capabilities,
          "serverInfo" => %{
            "name" => "ExMCP Mock Server",
            "version" => "1.0.0"
          }
        }
      )

    new_state = update_call_stats(state, "initialize", request)
    {response, new_state}
  end

  defp handle_mcp_request(%{"method" => "tools/list"} = request, state) do
    tools =
      if state.handler do
        state.handler.list_tools(state.custom_state)
      else
        state.tools
      end

    response = Builders.success_response(request["id"], %{"tools" => tools})
    new_state = update_call_stats(state, "tools/list", request)
    {response, new_state}
  end

  defp handle_mcp_request(%{"method" => "tools/call", "params" => params} = request, state) do
    tool_name = params["name"]
    arguments = Map.get(params, "arguments", %{})

    result =
      if state.handler do
        state.handler.call_tool(tool_name, arguments, state.custom_state)
      else
        default_tool_call(tool_name, arguments, state)
      end

    response = Builders.success_response(request["id"], result)
    new_state = update_call_stats(state, "tools/call", request)
    {response, new_state}
  end

  defp handle_mcp_request(%{"method" => "resources/list"} = request, state) do
    resources =
      if state.handler do
        state.handler.list_resources(state.custom_state)
      else
        state.resources
      end

    response = Builders.success_response(request["id"], %{"resources" => resources})
    new_state = update_call_stats(state, "resources/list", request)
    {response, new_state}
  end

  defp handle_mcp_request(%{"method" => "resources/read", "params" => params} = request, state) do
    uri = params["uri"]

    result =
      if state.handler do
        state.handler.read_resource(uri, state.custom_state)
      else
        default_resource_read(uri, state)
      end

    response = Builders.success_response(request["id"], result)
    new_state = update_call_stats(state, "resources/read", request)
    {response, new_state}
  end

  defp handle_mcp_request(%{"method" => "prompts/list"} = request, state) do
    prompts =
      if state.handler do
        state.handler.list_prompts(state.custom_state)
      else
        state.prompts
      end

    response = Builders.success_response(request["id"], %{"prompts" => prompts})
    new_state = update_call_stats(state, "prompts/list", request)
    {response, new_state}
  end

  defp handle_mcp_request(%{"method" => "prompts/get", "params" => params} = request, state) do
    prompt_name = params["name"]
    arguments = Map.get(params, "arguments", %{})

    result =
      if state.handler do
        state.handler.get_prompt(prompt_name, arguments, state.custom_state)
      else
        default_prompt_get(prompt_name, arguments, state)
      end

    response = Builders.success_response(request["id"], result)
    new_state = update_call_stats(state, "prompts/get", request)
    {response, new_state}
  end

  defp handle_mcp_request(%{"method" => "notifications/initialized"} = request, state) do
    # The initialized notification doesn't need a response
    new_state = update_call_stats(state, "notifications/initialized", request)
    # Return nil to indicate no response needed for notifications
    {nil, new_state}
  end

  defp handle_mcp_request(%{"method" => method} = request, state) do
    # Unknown method
    error_response =
      Builders.error_response(
        request["id"],
        -32601,
        "Method not found: #{method}"
      )

    new_state = update_call_stats(state, "unknown", request)
    {error_response, new_state}
  end

  # Default Handlers

  defp default_tool_call("sample_tool", arguments, _state) do
    input = Map.get(arguments, "input", "")
    format = get_in(arguments, ["options", "format"]) || "text"

    content =
      case format do
        "json" ->
          %{"result" => "Processed: #{input}", "format" => "json"}
          |> Jason.encode!()

        _ ->
          "Processed: #{input}"
      end

    Builders.tool_result(content)
  end

  defp default_tool_call(tool_name, _arguments, _state) do
    Builders.tool_result(nil, error: "Tool not found: #{tool_name}")
  end

  defp default_resource_read("file://sample_data.txt", _state) do
    Builders.resource_data("Sample file content for testing", "text/plain")
  end

  defp default_resource_read(uri, _state) do
    Builders.resource_data("Resource content for #{uri}", "text/plain", uri: uri)
  end

  defp default_prompt_get("sample_prompt", arguments, _state) do
    topic = Map.get(arguments, "topic", "general topic")
    style = Map.get(arguments, "style", "formal")

    content = "Write about #{topic} in a #{style} style."
    Builders.prompt_data(content)
  end

  defp default_prompt_get(prompt_name, _arguments, _state) do
    content = "Generated prompt for #{prompt_name}"
    Builders.prompt_data(content)
  end

  # Helper Functions

  defp build_capabilities(config) do
    default_capabilities = %{
      "tools" => %{
        "listChanged" => false
      },
      "resources" => %{
        "subscribe" => false,
        "listChanged" => false
      },
      "prompts" => %{
        "listChanged" => false
      }
    }

    custom_capabilities = Keyword.get(config, :capabilities, %{})
    Map.merge(default_capabilities, custom_capabilities)
  end

  defp should_simulate_error?(error_rate) when error_rate > 0.0 do
    :rand.uniform() < error_rate
  end

  defp should_simulate_error?(_), do: false

  defp update_call_stats(state, method, request) do
    # Update call count
    call_count = Map.update(state.call_count, method, 1, &(&1 + 1))

    # Add to recent calls (keep last 10)
    call_info = %{
      method: method,
      timestamp: DateTime.utc_now(),
      request: request
    }

    last_calls = [call_info | state.last_calls] |> Enum.take(10)

    %{state | call_count: call_count, last_calls: last_calls}
  end

  # Public Utilities for Testing

  @doc """
  Gets the current call count for a mock server.

  ## Examples

      with_server([], fn client ->
        ExMCP.Client.list_tools(client)
        ExMCP.Client.list_tools(client)

        count = MockServer.get_call_count(server_pid)
        assert count["tools/list"] == 2
      end)
  """
  @spec get_call_count(pid()) :: map()
  def get_call_count(server_pid) do
    GenServer.call(server_pid, :get_call_count)
  end

  @doc """
  Resets the call count for a mock server.
  """
  @spec reset_call_count(pid()) :: :ok
  def reset_call_count(server_pid) do
    GenServer.call(server_pid, :reset_call_count)
  end

  @doc """
  Gets the current state of a mock server.
  """
  @spec get_server_state(pid()) :: server_state()
  def get_server_state(server_pid) do
    GenServer.call(server_pid, :get_state)
  end

  @doc """
  Updates the custom state of a mock server.
  """
  @spec set_custom_state(pid(), map()) :: :ok
  def set_custom_state(server_pid, new_state) do
    GenServer.call(server_pid, {:set_state, new_state})
  end
end

# Mock Transport Implementation
defmodule ExMCP.Testing.MockTransport do
  @moduledoc """
  Mock transport implementation that connects to MockServer.

  This transport sends messages directly to a mock server process
  instead of using network communication, enabling fast and reliable
  testing of MCP protocol interactions.
  """

  @behaviour ExMCP.Transport

  defstruct [:server_pid, :timeout, :request_responses]

  @impl ExMCP.Transport
  def connect(opts) do
    server_pid = Keyword.fetch!(opts, :server_pid)
    timeout = Keyword.get(opts, :timeout, 5000)

    transport = %__MODULE__{
      server_pid: server_pid,
      timeout: timeout,
      request_responses: %{}
    }

    {:ok, transport}
  end

  @impl ExMCP.Transport
  def send_message(message, transport) do
    case GenServer.call(transport.server_pid, {:mcp_request, message}, transport.timeout) do
      {:error, error_response} -> {:error, error_response}
      _response -> {:ok, transport}
    end
  rescue
    error -> {:error, %{"error" => %{"code" => -1, "message" => inspect(error)}}}
  end

  # Compatibility method for Client
  def send(transport, message) when is_binary(message) do
    case Jason.decode(message) do
      {:ok, decoded} ->
        case GenServer.call(transport.server_pid, {:mcp_request, decoded}, transport.timeout) do
          {:ok, response} ->
            # Store the response in transport state instead of process dictionary
            request_id = decoded["id"]
            updated_responses = Map.put(transport.request_responses, request_id, response)
            updated_transport = %{transport | request_responses: updated_responses}
            {:ok, updated_transport}

          {:error, error_response} ->
            # For error responses, still store them for recv to return
            request_id = decoded["id"]
            updated_responses = Map.put(transport.request_responses, request_id, error_response)
            updated_transport = %{transport | request_responses: updated_responses}
            {:ok, updated_transport}
        end

      {:error, _} = error ->
        error
    end
  end

  @impl ExMCP.Transport
  def receive_message(_transport) do
    # Mock transport doesn't support receiving messages - it's synchronous
    {:error, "Mock transport doesn't support async receiving"}
  end

  # Compatibility method for Client
  def receive(transport, _timeout \\ 5000) do
    # Try to get the stored response from transport state
    receive do
      _ -> :ok
    after
      0 -> :ok
    end

    # Check transport state for stored responses
    case transport.request_responses do
      responses when map_size(responses) == 0 ->
        {:error, :no_response}

      responses ->
        # Get the first available response
        {request_id, response} = Enum.at(responses, 0)
        # Remove the response from state for next time
        updated_responses = Map.delete(responses, request_id)
        updated_transport = %{transport | request_responses: updated_responses}
        {:ok, Jason.encode!(response), updated_transport}
    end
  end

  # Alias for receive used by Client
  def recv(transport, timeout) do
    receive(transport, timeout)
  end

  @impl ExMCP.Transport
  def close(_transport) do
    :ok
  end
end
