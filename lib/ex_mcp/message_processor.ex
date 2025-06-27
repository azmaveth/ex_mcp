defmodule ExMCP.MessageProcessor do
  @moduledoc """
  Core message processing abstraction for ExMCP.

  The MessageProcessor provides a simple, composable interface for processing MCP messages.
  It follows the Plug specification pattern used throughout the Elixir ecosystem.
  """

  alias ExMCP.Internal.MessageValidator

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
      # Progress token for long-running operations (MCP 2025-06-18)
      :progress_token,
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
            progress_token: String.t() | integer() | nil,
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
      progress_token: extract_progress_token(request),
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

  # Detect the type of server based on exported functions.
  # Returns:
  # - :dsl_server - Server uses the DSL pattern (has get_tools/0, etc.)
  # - :handler_server - Server uses the handler pattern (has handle_list_tools/2, etc.)
  # - :unknown - Cannot determine server type
  @spec detect_server_type(module()) :: :dsl_server | :handler_server | :unknown
  defp detect_server_type(handler_module) do
    cond do
      # DSL servers have getter functions
      function_exported?(handler_module, :get_tools, 0) and
        function_exported?(handler_module, :get_prompts, 0) and
          function_exported?(handler_module, :get_resources, 0) ->
        :dsl_server

      # Handler servers have handler callbacks
      function_exported?(handler_module, :handle_list_tools, 2) and
        function_exported?(handler_module, :handle_list_prompts, 2) and
          function_exported?(handler_module, :handle_list_resources, 2) ->
        :handler_server

      true ->
        :unknown
    end
  end

  @doc """
  Process an MCP request using a handler module.

  This is a convenience function that creates a connection, processes it
  through a handler, and returns the response.
  """
  @spec process(Conn.t(), map()) :: Conn.t()
  def process(%Conn{} = conn, opts) do
    # Validate based on message type (request vs notification)
    if notification?(conn.request) do
      # For notifications, use the simpler validation that doesn't require "id"
      case validate_notification(conn.request) do
        {:ok, _validated_notification} ->
          process_validated_notification(conn, opts)

        {:error, error_data} ->
          # Notifications that fail validation are just logged, no response
          require Logger
          Logger.warning("Invalid notification received: #{inspect(error_data)}")
          conn
      end
    else
      # For requests, use full request validation
      case MessageValidator.validate_request(conn.request) do
        {:ok, _validated_request} ->
          # Request is valid, proceed with processing.
          process_validated_request(conn, opts)

        {:error, error_data} ->
          # Request is invalid, construct and return an error response.
          # Note: for validation errors, the ID might be null or invalid.
          # We still try to get it to adhere to JSON-RPC, but it might be nil.
          error_response = %{
            "jsonrpc" => "2.0",
            "error" => error_data,
            "id" => get_request_id(conn.request)
          }

          put_response(conn, error_response)
      end
    end
  end

  defp process_validated_request(%Conn{} = conn, opts) do
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
        # Detect server type based on exported functions
        case detect_server_type(handler_module) do
          :dsl_server ->
            process_with_dsl_server(conn, handler_module, server_info)

          :handler_server ->
            process_with_handler_genserver(conn, handler_module, server_info)

          :unknown ->
            # Fallback to original detection for backward compatibility
            if function_exported?(handler_module, :start_link, 1) and
                 function_exported?(handler_module, :handle_resource_read, 3) do
              process_with_dsl_server(conn, handler_module, server_info)
            else
              process_with_handler(conn, handler_module, server_info)
            end
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

  # Process request using Handler Server with GenServer
  defp process_with_handler_genserver(conn, handler_module, server_info) do
    # Start the handler as a GenServer
    case GenServer.start_link(handler_module, []) do
      {:ok, server_pid} ->
        try do
          # Process the request using the handler's GenServer interface
          process_handler_request(conn, server_pid, server_info)
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
            "message" => "Failed to start handler server",
            "data" => %{"reason" => inspect(reason)}
          },
          "id" => get_request_id(conn.request)
        }

        put_response(conn, error_response)
    end
  end

  # Process handler request through GenServer calls
  defp process_handler_request(conn, server_pid, _server_info) do
    request = conn.request
    method = Map.get(request, "method")
    params = Map.get(request, "params", %{})
    id = get_request_id(request)

    case Map.get(handler_method_dispatch(), method) do
      nil ->
        handle_handler_custom_method(conn, server_pid, method, params, id)

      handler_fun ->
        handler_fun.(conn, server_pid, params, id)
    end
  end

  # Process request using running GenServer instance
  defp process_with_server_pid(conn, server_pid, _server_info) do
    request = conn.request
    method = Map.get(request, "method")
    params = Map.get(request, "params", %{})
    id = get_request_id(request)

    case Map.get(server_method_dispatch(), method) do
      nil ->
        handle_custom_method_with_server(conn, server_pid, method, params, id)

      handler_fun ->
        handler_fun.(conn, server_pid, params, id)
    end
  end

  defp start_temporary_server(handler_module) do
    # Start a temporary GenServer instance
    handler_module.start_link([])
  end

  # Process request using Server handler
  defp process_with_handler(conn, handler_module, server_info) do
    request = conn.request
    method = Map.get(request, "method")
    params = Map.get(request, "params", %{})
    id = get_request_id(request)

    # Debug logging for tests
    log_method_processing(method, handler_module)

    case Map.get(handler_direct_dispatch(), method) do
      nil ->
        handle_custom_method(conn, handler_module, method, params, id)

      handler_fun ->
        handler_fun.(conn, handler_module, server_info, params, id)
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
        "protocolVersion" => "2025-06-18",
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

  defp handle_resources_subscribe(conn, handler_module, params, id) do
    uri = Map.get(params, "uri")

    case handler_module.handle_resource_subscribe(uri, %{}) do
      {:ok, _new_state} ->
        response = success_response(%{}, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Resource subscription failed", reason, id)
        put_response(conn, error_response)
    end
  end

  defp handle_resources_unsubscribe(conn, handler_module, params, id) do
    uri = Map.get(params, "uri")

    case handler_module.handle_resource_unsubscribe(uri, %{}) do
      {:ok, _new_state} ->
        response = success_response(%{}, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Resource unsubscription failed", reason, id)
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
        "protocolVersion" => "2025-06-18",
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

  defp handle_resources_subscribe_with_server(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:handle_resource_subscribe, uri}, 10000) do
      {:ok, _new_state} ->
        response = success_response(%{}, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Resource subscription failed", reason, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Resource subscription failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_resources_unsubscribe_with_server(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:handle_resource_unsubscribe, uri}, 10000) do
      {:ok, _new_state} ->
        response = success_response(%{}, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Resource unsubscription failed", reason, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Resource unsubscription failed", error, id)
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

  # Handler-specific functions that use GenServer calls
  defp handle_handler_initialize(conn, server_pid, params, id) do
    case GenServer.call(server_pid, {:initialize, params}, 5000) do
      {:ok, result, _state} ->
        response = %{
          "jsonrpc" => "2.0",
          "result" => result,
          "id" => id
        }

        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Initialize failed", reason, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Initialize failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_handler_tools_list(conn, server_pid, params, id) do
    cursor = Map.get(params, "cursor")

    case GenServer.call(server_pid, {:list_tools, cursor}, 5000) do
      {:ok, tools, next_cursor, _state} ->
        result = %{"tools" => tools}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result

        response = %{
          "jsonrpc" => "2.0",
          "result" => result,
          "id" => id
        }

        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Tools list failed", reason, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Tools list failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_handler_tools_call(conn, server_pid, params, id) do
    tool_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    case GenServer.call(server_pid, {:call_tool, tool_name, arguments}, 10000) do
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

  defp handle_handler_resources_list(conn, server_pid, params, id) do
    cursor = Map.get(params, "cursor")

    case GenServer.call(server_pid, {:list_resources, cursor}, 5000) do
      {:ok, resources, next_cursor, _state} ->
        result = %{"resources" => resources}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result

        response = %{
          "jsonrpc" => "2.0",
          "result" => result,
          "id" => id
        }

        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Resources list failed", reason, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Resources list failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_handler_resources_read(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:read_resource, uri}, 10000) do
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

  defp handle_handler_resources_subscribe(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:subscribe_resource, uri}, 10000) do
      {:ok, _state} ->
        response = success_response(%{}, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Resource subscription failed", reason, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Resource subscription failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_handler_resources_unsubscribe(conn, server_pid, params, id) do
    uri = Map.get(params, "uri")

    case GenServer.call(server_pid, {:unsubscribe_resource, uri}, 10000) do
      {:ok, _state} ->
        response = success_response(%{}, id)
        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Resource unsubscription failed", reason, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Resource unsubscription failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_handler_prompts_list(conn, server_pid, params, id) do
    cursor = Map.get(params, "cursor")

    case GenServer.call(server_pid, {:list_prompts, cursor}, 5000) do
      {:ok, prompts, next_cursor, _state} ->
        result = %{"prompts" => prompts}
        result = if next_cursor, do: Map.put(result, "nextCursor", next_cursor), else: result

        response = %{
          "jsonrpc" => "2.0",
          "result" => result,
          "id" => id
        }

        put_response(conn, response)

      {:error, reason, _state} ->
        error_response = error_response("Prompts list failed", reason, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Prompts list failed", error, id)
      put_response(conn, error_response)
  end

  defp handle_handler_prompts_get(conn, server_pid, params, id) do
    prompt_name = Map.get(params, "name")
    arguments = Map.get(params, "arguments", %{})

    case GenServer.call(server_pid, {:get_prompt, prompt_name, arguments}, 10000) do
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

  defp handle_handler_custom_method(conn, server_pid, method, params, id) do
    case GenServer.call(server_pid, {:request, method, params}, 10000) do
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

  # Handler-specific completion complete function
  defp handle_handler_completion_complete(conn, server_pid, params, id) do
    ref = Map.get(params, "ref")
    argument = Map.get(params, "argument")

    case GenServer.call(server_pid, {:complete, ref, argument}, 10_000) do
      {:ok, result, _new_state} ->
        response = success_response(result, id)
        put_response(conn, response)

      {:error, reason, _new_state} ->
        error_response = error_response("Completion error", reason, id)
        put_response(conn, error_response)

      error ->
        error_response = error_response("Completion failed", error, id)
        put_response(conn, error_response)
    end
  rescue
    error ->
      error_response = error_response("Completion request failed", error, id)
      put_response(conn, error_response)
  end

  defp get_request_id(request) when is_map(request) do
    Map.get(request, "id")
  end

  defp get_request_id(_), do: nil

  defp handler_method_dispatch do
    %{
      "initialize" => fn conn, server_pid, params, id ->
        handle_handler_initialize(conn, server_pid, params, id)
      end,
      "ping" => fn conn, _server_pid, _params, id -> handle_ping(conn, id) end,
      "tools/list" => fn conn, server_pid, params, id ->
        handle_handler_tools_list(conn, server_pid, params, id)
      end,
      "tools/call" => fn conn, server_pid, params, id ->
        handle_handler_tools_call(conn, server_pid, params, id)
      end,
      "resources/list" => fn conn, server_pid, params, id ->
        handle_handler_resources_list(conn, server_pid, params, id)
      end,
      "resources/read" => fn conn, server_pid, params, id ->
        handle_handler_resources_read(conn, server_pid, params, id)
      end,
      "resources/subscribe" => fn conn, server_pid, params, id ->
        handle_handler_resources_subscribe(conn, server_pid, params, id)
      end,
      "resources/unsubscribe" => fn conn, server_pid, params, id ->
        handle_handler_resources_unsubscribe(conn, server_pid, params, id)
      end,
      "prompts/list" => fn conn, server_pid, params, id ->
        handle_handler_prompts_list(conn, server_pid, params, id)
      end,
      "prompts/get" => fn conn, server_pid, params, id ->
        handle_handler_prompts_get(conn, server_pid, params, id)
      end,
      "completion/complete" => fn conn, server_pid, params, id ->
        handle_handler_completion_complete(conn, server_pid, params, id)
      end
    }
  end

  defp server_method_dispatch do
    %{
      "initialize" => fn conn, server_pid, _params, id ->
        handle_initialize_with_server(conn, server_pid, id)
      end,
      "ping" => fn conn, _server_pid, _params, id ->
        handle_ping(conn, id)
      end,
      "tools/list" => fn conn, server_pid, _params, id ->
        handle_tools_list_with_server(conn, server_pid, id)
      end,
      "tools/call" => &handle_tools_call_with_server/4,
      "resources/list" => fn conn, server_pid, _params, id ->
        handle_resources_list_with_server(conn, server_pid, id)
      end,
      "resources/read" => &handle_resources_read_with_server/4,
      "resources/subscribe" => &handle_resources_subscribe_with_server/4,
      "resources/unsubscribe" => &handle_resources_unsubscribe_with_server/4,
      "prompts/list" => fn conn, server_pid, _params, id ->
        handle_prompts_list_with_server(conn, server_pid, id)
      end,
      "prompts/get" => &handle_prompts_get_with_server/4
    }
  end

  defp handler_direct_dispatch do
    %{
      "initialize" => fn conn, handler_module, server_info, _params, id ->
        handle_initialize(conn, handler_module, server_info, id)
      end,
      "ping" => fn conn, _handler_module, _server_info, _params, id ->
        handle_ping(conn, id)
      end,
      "tools/list" => fn conn, handler_module, _server_info, _params, id ->
        handle_tools_list(conn, handler_module, id)
      end,
      "tools/call" => fn conn, handler_module, _server_info, params, id ->
        handle_tools_call(conn, handler_module, params, id)
      end,
      "resources/list" => fn conn, handler_module, _server_info, _params, id ->
        handle_resources_list(conn, handler_module, id)
      end,
      "resources/read" => fn conn, handler_module, _server_info, params, id ->
        handle_resources_read(conn, handler_module, params, id)
      end,
      "resources/subscribe" => fn conn, handler_module, _server_info, params, id ->
        handle_resources_subscribe(conn, handler_module, params, id)
      end,
      "resources/unsubscribe" => fn conn, handler_module, _server_info, params, id ->
        handle_resources_unsubscribe(conn, handler_module, params, id)
      end,
      "prompts/list" => fn conn, handler_module, _server_info, _params, id ->
        handle_prompts_list(conn, handler_module, id)
      end,
      "prompts/get" => fn conn, handler_module, _server_info, params, id ->
        handle_prompts_get(conn, handler_module, params, id)
      end
    }
  end

  # Progress notification helpers for MCP 2025-06-18 compliance

  # Extracts the progress token from a request's _meta field.
  # According to MCP 2025-06-18 specification, progress tokens are sent
  # in the request metadata field and must be string or integer values.
  @spec extract_progress_token(map()) :: ExMCP.Types.progress_token() | nil
  defp extract_progress_token(%{"params" => %{"_meta" => %{"progressToken" => token}}} = _request)
       when is_binary(token) or is_integer(token) do
    token
  end

  defp extract_progress_token(_request), do: nil

  @doc """
  Starts progress tracking for a connection if it has a progress token.

  This should be called at the beginning of long-running operations.
  """
  @spec start_progress_tracking(Conn.t()) :: Conn.t()
  def start_progress_tracking(%Conn{progress_token: nil} = conn), do: conn

  def start_progress_tracking(%Conn{progress_token: token} = conn) when not is_nil(token) do
    case ExMCP.ProgressTracker.start_progress(token, self()) do
      {:ok, _state} ->
        conn

      {:error, reason} ->
        require Logger
        Logger.warning("Failed to start progress tracking", token: token, reason: reason)
        conn
    end
  end

  @doc """
  Updates progress for a connection.

  This is a helper function to send progress notifications during
  long-running operations.
  """
  @spec update_progress(Conn.t(), number(), number() | nil, String.t() | nil) :: Conn.t()
  def update_progress(%Conn{progress_token: nil} = conn, _progress, _total, _message), do: conn

  def update_progress(%Conn{progress_token: token} = conn, progress, total, message)
      when not is_nil(token) do
    case ExMCP.ProgressTracker.update_progress(token, progress, total, message) do
      :ok ->
        conn

      {:error, reason} ->
        require Logger
        Logger.warning("Failed to update progress", token: token, reason: reason)
        conn
    end
  end

  @doc """
  Completes progress tracking for a connection.

  This should be called when a long-running operation finishes,
  either successfully or with an error.
  """
  @spec complete_progress(Conn.t()) :: Conn.t()
  def complete_progress(%Conn{progress_token: nil} = conn), do: conn

  def complete_progress(%Conn{progress_token: token} = conn) when not is_nil(token) do
    case ExMCP.ProgressTracker.complete_progress(token) do
      :ok ->
        conn

      {:error, reason} ->
        require Logger
        Logger.warning("Failed to complete progress", token: token, reason: reason)
        conn
    end
  end

  # Helper functions for notification handling

  defp notification?(%{"method" => _method} = request) do
    # Notifications don't have an "id" field
    not Map.has_key?(request, "id")
  end

  defp notification?(_), do: false

  defp validate_notification(notification) do
    # Simple validation for notifications - just check required fields
    with :ok <- validate_jsonrpc_version(notification),
         :ok <- validate_notification_structure(notification) do
      {:ok, notification}
    else
      {:error, error_data} -> {:error, error_data}
    end
  end

  defp validate_jsonrpc_version(%{"jsonrpc" => "2.0"}), do: :ok

  defp validate_jsonrpc_version(_),
    do: {:error, %{"code" => -32600, "message" => "Invalid JSON-RPC version"}}

  defp validate_notification_structure(%{"method" => _method}) do
    # Notifications only require jsonrpc and method fields
    :ok
  end

  defp validate_notification_structure(_) do
    {:error, %{"code" => -32600, "message" => "Notification must have method field"}}
  end

  defp process_validated_notification(%Conn{} = conn, opts) do
    # Notifications don't generate responses, just process them
    handler = Map.get(opts, :handler)

    if handler do
      try do
        method = Map.get(conn.request, "method")
        params = Map.get(conn.request, "params", %{})

        # For notifications, we just call the handler but don't return a response
        if function_exported?(handler, :handle_mcp_request, 3) do
          handler.handle_mcp_request(method, params, %{})
        end
      rescue
        # Ignore errors in notifications
        _ -> :ok
      end
    end

    conn
  end
end
