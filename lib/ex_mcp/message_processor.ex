defmodule ExMCP.MessageProcessor do
  @moduledoc """
  Core message processing abstraction for ExMCP v2.

  The MessageProcessor provides a simple, composable interface for processing MCP messages.
  It follows the Plug specification pattern used throughout the Elixir ecosystem.
  """

  @type t :: module()
  @type opts :: term()
  @type conn :: %__MODULE__.Conn{}

  defmodule Conn do
    @moduledoc """
    Connection struct representing an MCP message processing context.
    """

    defstruct [
      # The incoming MCP request
      :request,
      # The outgoing MCP response (if any)
      :response,
      # Processing state
      :state,
      # User-defined assigns
      :assigns,
      # Transport information
      :transport,
      # Session identifier
      :session_id,
      # Whether processing should stop
      :halted
    ]

    @type t :: %__MODULE__{
            request: map() | nil,
            response: map() | nil,
            state: term(),
            assigns: map(),
            transport: atom(),
            session_id: String.t() | nil,
            halted: boolean()
          }
  end

  @doc """
  Callback for initializing the plug with options.
  """
  @callback init(opts) :: opts

  @doc """
  Callback for processing the connection.
  """
  @callback call(conn, opts) :: conn

  @doc """
  Creates a new connection.
  """
  @spec new(map(), keyword()) :: Conn.t()
  def new(request, opts \\ []) do
    %Conn{
      request: request,
      response: nil,
      state: nil,
      assigns: %{},
      transport: Keyword.get(opts, :transport),
      session_id: Keyword.get(opts, :session_id),
      halted: false
    }
  end

  @doc """
  Assigns a value to the connection.
  """
  @spec assign(Conn.t(), atom(), term()) :: Conn.t()
  def assign(%Conn{} = conn, key, value) do
    %{conn | assigns: Map.put(conn.assigns, key, value)}
  end

  @doc """
  Halts the plug pipeline.
  """
  @spec halt(Conn.t()) :: Conn.t()
  def halt(%Conn{} = conn) do
    %{conn | halted: true}
  end

  @doc """
  Sets the response on the connection.
  """
  @spec put_response(Conn.t(), map()) :: Conn.t()
  def put_response(%Conn{} = conn, response) do
    %{conn | response: response}
  end

  @doc """
  Runs a list of plugs on the connection.
  """
  @spec run([{module(), opts}], Conn.t()) :: Conn.t()
  def run(plugs, %Conn{} = conn) do
    Enum.reduce_while(plugs, conn, fn {plug_module, opts}, acc ->
      if acc.halted do
        {:halt, acc}
      else
        result = plug_module.call(acc, plug_module.init(opts))
        {:cont, result}
      end
    end)
  end

  @doc """
  Process an MCP request using a handler module.

  This is a convenience function that creates a connection, processes it
  through a handler, and returns the response.
  """
  @spec process(Conn.t(), map()) :: Conn.t()
  def process(%Conn{} = conn, opts) do
    handler = Map.get(opts, :handler)
    server_info = Map.get(opts, :server_info, %{})

    case handler do
      nil ->
        error_response = %{
          "jsonrpc" => "2.0",
          "error" => %{
            "code" => -32603,
            "message" => "No handler configured"
          },
          "id" => get_request_id(conn.request)
        }

        put_response(conn, error_response)

      handler_module when is_atom(handler_module) ->
        # Check if handler implements DSL Server pattern
        if function_exported?(handler_module, :start_link, 1) and
             function_exported?(handler_module, :handle_resource_read, 3) do
          process_with_dsl_server(conn, handler_module, server_info)
        else
          process_with_handler(conn, handler_module, server_info)
        end
    end
  end

  # Process request using DSL Server with temporary GenServer instance
  defp process_with_dsl_server(conn, handler_module, server_info) do
    # Start a temporary server instance for this request
    case start_temporary_server(handler_module) do
      {:ok, server_pid} ->
        try do
          process_with_server_pid(conn, server_pid, server_info)
        after
          # Clean up the temporary server
          if Process.alive?(server_pid) do
            GenServer.stop(server_pid, :normal, 1000)
          end
        end

      {:error, reason} ->
        error_response = %{
          "jsonrpc" => "2.0",
          "error" => %{
            "code" => -32603,
            "message" => "Failed to start server instance",
            "data" => %{"reason" => inspect(reason)}
          },
          "id" => get_request_id(conn.request)
        }

        put_response(conn, error_response)
    end
  end

  # Process request using running GenServer instance
  defp process_with_server_pid(conn, server_pid, _server_info) do
    request = conn.request
    method = Map.get(request, "method")
    params = Map.get(request, "params", %{})
    id = get_request_id(request)

    case method do
      "initialize" -> handle_initialize_with_server(conn, server_pid, id)
      "ping" -> handle_ping(conn, id)
      "tools/list" -> handle_tools_list_with_server(conn, server_pid, id)
      "tools/call" -> handle_tools_call_with_server(conn, server_pid, params, id)
      "resources/list" -> handle_resources_list_with_server(conn, server_pid, id)
      "resources/read" -> handle_resources_read_with_server(conn, server_pid, params, id)
      "prompts/list" -> handle_prompts_list_with_server(conn, server_pid, id)
      "prompts/get" -> handle_prompts_get_with_server(conn, server_pid, params, id)
      _ -> handle_custom_method_with_server(conn, server_pid, method, params, id)
    end
  end

  defp start_temporary_server(handler_module) do
    # Start a temporary GenServer instance
    handler_module.start_link([])
  end

  # Process request using ServerV2 handler
  defp process_with_handler(conn, handler_module, server_info) do
    request = conn.request
    method = Map.get(request, "method")
    params = Map.get(request, "params", %{})
    id = get_request_id(request)

    # Debug logging for tests
    log_method_processing(method, handler_module)

    case method do
      "initialize" -> handle_initialize(conn, handler_module, server_info, id)
      "ping" -> handle_ping(conn, id)
      "tools/list" -> handle_tools_list(conn, handler_module, id)
      "tools/call" -> handle_tools_call(conn, handler_module, params, id)
      "resources/list" -> handle_resources_list(conn, handler_module, id)
      "resources/read" -> handle_resources_read(conn, handler_module, params, id)
      "prompts/list" -> handle_prompts_list(conn, handler_module, id)
      "prompts/get" -> handle_prompts_get(conn, handler_module, params, id)
      _ -> handle_custom_method(conn, handler_module, method, params, id)
    end
  end

  defp log_method_processing(method, handler_module) do
    if Application.get_env(:ex_mcp, :debug_logging, false) do
      require Logger
      Logger.debug("Processing method: #{method} with handler: #{handler_module}")
    end
  end

  defp handle_ping(conn, id) do
    response = success_response(%{}, id)
    put_response(conn, response)
  end

  defp handle_initialize(conn, handler_module, server_info, id) do
    response = %{
      "jsonrpc" => "2.0",
      "result" => %{
        "protocolVersion" => "2024-11-05",
        "capabilities" => handler_module.get_capabilities(),
        "serverInfo" => server_info
      },
      "id" => id
    }

    put_response(conn, response)
  end

  defp handle_tools_list(conn, handler_module, id) do
    tools = handler_module.get_tools() |> Map.values()

    response = %{
      "jsonrpc" => "2.0",
      "result" => %{"tools" => tools},
      "id" => id
    }

    put_response(conn, response)
  end

  defp handle_tools_call(conn, handler_module, params, id) do
    tool_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    case handler_module.handle_tool_call(tool_name, arguments, %{}) do
      {:ok, result, _state} ->
        response = success_response(result, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Tool execution failed", reason, id)
        put_response(conn, error_response)
    end
  end

  defp handle_resources_list(conn, handler_module, id) do
    resources = handler_module.get_resources() |> Map.values()

    response = %{
      "jsonrpc" => "2.0",
      "result" => %{"resources" => resources},
      "id" => id
    }

    put_response(conn, response)
  end

  defp handle_resources_read(conn, handler_module, params, id) do
    uri = Map.get(params, "uri")

    case handler_module.handle_resource_read(uri, uri, %{}) do
      {:ok, content, _state} ->
        response = %{
          "jsonrpc" => "2.0",
          "result" => %{"contents" => content},
          "id" => id
        }

        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Resource read failed", reason, id)
        put_response(conn, error_response)
    end
  end

  defp handle_prompts_list(conn, handler_module, id) do
    prompts = handler_module.get_prompts() |> Map.values()

    response = %{
      "jsonrpc" => "2.0",
      "result" => %{"prompts" => prompts},
      "id" => id
    }

    put_response(conn, response)
  end

  defp handle_prompts_get(conn, handler_module, params, id) do
    prompt_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    case handler_module.handle_prompt_get(prompt_name, arguments, %{}) do
      {:ok, result, _state} ->
        response = success_response(result, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Prompt get failed", reason, id)
        put_response(conn, error_response)
    end
  end

  defp handle_custom_method(conn, handler_module, method, params, id) do
    if Code.ensure_loaded?(handler_module) and
         function_exported?(handler_module, :handle_request, 3) do
      handle_custom_request(conn, handler_module, method, params, id)
    else
      handle_method_not_found(conn, id)
    end
  end

  defp handle_custom_request(conn, handler_module, method, params, id) do
    case handler_module.handle_request(method, params, %{}) do
      {:reply, result, _state} ->
        response = success_response(result, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Request failed", reason, id)
        put_response(conn, error_response)

      {:noreply, _state} ->
        conn

      _ ->
        handle_method_not_found(conn, id)
    end
  end

  defp handle_method_not_found(conn, id) do
    error_response = %{
      "jsonrpc" => "2.0",
      "error" => %{
        "code" => -32601,
        "message" => "Method not found"
      },
      "id" => id
    }

    put_response(conn, error_response)
  end

  defp success_response(result, id) do
    %{
      "jsonrpc" => "2.0",
      "result" => result,
      "id" => id
    }
  end

  defp error_response(message, reason, id) do
    %{
      "jsonrpc" => "2.0",
      "error" => %{
        "code" => -32603,
        "message" => message,
        "data" => %{"reason" => inspect(reason)}
      },
      "id" => id
    }
  end

  # GenServer-based handlers
  defp handle_initialize_with_server(conn, server_pid, id) do
    server_info = GenServer.call(server_pid, :get_server_info, 5000)
    capabilities = GenServer.call(server_pid, :get_capabilities, 5000)

    response = %{
      "jsonrpc" => "2.0",
      "result" => %{
        "protocolVersion" => "2024-11-05",
        "capabilities" => capabilities,
        "serverInfo" => server_info
      },
      "id" => id
    }

    put_response(conn, response)
  rescue
    error ->
      error_response = error_response("Initialize failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_tools_list_with_server(conn, server_pid, id) do
    tools = GenServer.call(server_pid, :get_tools, 5000) |> Map.values()

    response = %{
      "jsonrpc" => "2.0",
      "result" => %{"tools" => tools},
      "id" => id
    }

    put_response(conn, response)
  rescue
    error ->
      error_response = error_response("Tools list failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_tools_call_with_server(conn, server_pid, params, id) do
    tool_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    case GenServer.call(server_pid, {:handle_tool_call, tool_name, arguments}, 10000) do
      {:ok, result, _state} ->
        response = success_response(result, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Tool execution failed", reason, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Tool call failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_resources_list_with_server(conn, server_pid, id) do
    resources = GenServer.call(server_pid, :get_resources, 5000) |> Map.values()

    response = %{
      "jsonrpc" => "2.0",
      "result" => %{"resources" => resources},
      "id" => id
    }

    put_response(conn, response)
  rescue
    error ->
      error_response = error_response("Resources list failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_resources_read_with_server(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:handle_resource_read, uri, uri}, 10000) do
      {:ok, content, _state} ->
        response = %{
          "jsonrpc" => "2.0",
          "result" => %{"contents" => content},
          "id" => id
        }

        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Resource read failed", reason, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Resource read failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_prompts_list_with_server(conn, server_pid, id) do
    prompts = GenServer.call(server_pid, :get_prompts, 5000) |> Map.values()

    response = %{
      "jsonrpc" => "2.0",
      "result" => %{"prompts" => prompts},
      "id" => id
    }

    put_response(conn, response)
  rescue
    error ->
      error_response = error_response("Prompts list failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_prompts_get_with_server(conn, server_pid, params, id) do
    prompt_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    case GenServer.call(server_pid, {:handle_prompt_get, prompt_name, arguments}, 10000) do
      {:ok, result, _state} ->
        response = success_response(result, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Prompt get failed", reason, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Prompt get failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_custom_method_with_server(conn, server_pid, method, params, id) do
    case GenServer.call(server_pid, {:handle_request, method, params}, 10000) do
      {:reply, result, _state} ->
        response = success_response(result, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Request failed", reason, id)
        put_response(conn, error_response)

      {:noreply, _state} ->
        conn

      _ ->
        handle_method_not_found(conn, id)
    end
  rescue
    error ->
      error_response = error_response("Custom method failed", error, id)
      put_response(conn, error_response)
  end

  defp get_request_id(request) when is_map(request) do
    Map.get(request, "id")
  end

  defp get_request_id(_), do: nil
end
