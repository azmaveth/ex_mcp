defmodule ExMCP.Service do
  @moduledoc """
  Behaviour and macro for creating MCP services with automatic registration.

  This module provides a convenient way to create MCP services that automatically
  register themselves with ExMCP.Native on startup and unregister on shutdown.

  ## Usage

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
        def handle_mcp_request("tools/call", %{"name" => "ping"} = params, state) do
          {:ok, %{"content" => [%{"type" => "text", "text" => "Pong!"}]}, state}
        end

        def handle_mcp_request(method, _params, state) do
          {:error, %{"code" => -32601, "message" => "Method not found: \#{method}"}, state}
        end
      end

  ## Callbacks

  The service must implement the `handle_mcp_request/3` callback to process
  MCP method calls.
  """

  @doc """
  Handles an MCP request.

  ## Parameters

  - `method` - The MCP method name (e.g., "list_tools", "tools/call")
  - `params` - The request parameters as a map
  - `state` - The current GenServer state

  ## Returns

  - `{:ok, result, new_state}` - Success with result data
  - `{:error, error, new_state}` - Error with error details
  """
  @callback handle_mcp_request(method :: String.t(), params :: map(), state :: term()) ::
              {:ok, result :: term(), new_state :: term()}
              | {:error, error :: term(), new_state :: term()}

  defmacro __using__(opts) do
    service_name = Keyword.fetch!(opts, :name)

    quote do
      use GenServer
      @behaviour ExMCP.Service

      require Logger

      @service_name unquote(service_name)

      def start_link(init_arg \\ []) do
        GenServer.start_link(__MODULE__, init_arg, name: @service_name)
      end

      @impl GenServer
      def init(init_arg) do
        # Register the service with ExMCP.Native
        case ExMCP.Native.register_service(@service_name) do
          :ok ->
            Logger.info("MCP service started: #{@service_name}")
            {:ok, init_arg}

          {:error, reason} ->
            Logger.error("Failed to register MCP service #{@service_name}: #{inspect(reason)}")
            {:stop, reason}
        end
      end

      @impl GenServer
      def terminate(reason, _state) do
        ExMCP.Native.unregister_service(@service_name)
        Logger.info("MCP service terminated: #{@service_name} (#{inspect(reason)})")
        :ok
      end

      @impl GenServer
      def handle_call({:mcp_request, %{"method" => method, "params" => params}}, _from, state) do
        case handle_mcp_request(method, params, state) do
          {:ok, result, new_state} ->
            {:reply, {:ok, result}, new_state}

          {:error, error, new_state} ->
            {:reply, {:error, error}, new_state}
        end
      end

      def handle_call({:mcp_request, %{"method" => method} = msg}, _from, state) do
        # Handle requests without params
        params = Map.get(msg, "params", %{})

        case handle_mcp_request(method, params, state) do
          {:ok, result, new_state} ->
            {:reply, {:ok, result}, new_state}

          {:error, error, new_state} ->
            {:reply, {:error, error}, new_state}
        end
      end

      @impl GenServer
      def handle_cast({:mcp_notification, %{"method" => method, "params" => params}}, state) do
        # Handle notifications - call the same handler but ignore the result
        case handle_mcp_request(method, params, state) do
          {:ok, _result, new_state} -> {:noreply, new_state}
          {:error, _error, new_state} -> {:noreply, new_state}
        end
      end

      def handle_cast({:mcp_notification, %{"method" => method} = msg}, state) do
        # Handle notifications without params
        params = Map.get(msg, "params", %{})

        case handle_mcp_request(method, params, state) do
          {:ok, _result, new_state} -> {:noreply, new_state}
          {:error, _error, new_state} -> {:noreply, new_state}
        end
      end

      # Allow services to override init/1, terminate/2, and other GenServer callbacks
      defoverridable init: 1, terminate: 2
    end
  end
end
