defmodule ExMCP.MessageProcessor do
  @moduledoc """
  Core message processing abstraction for ExMCP.

  The MessageProcessor provides a simple, composable interface for processing MCP messages.
  It follows the Plug specification pattern used throughout the Elixir ecosystem.
  """

  alias ExMCP.Internal.{JSONRPC, MessageValidator}
  alias ExMCP.Protocol.ErrorCodes
  alias ExMCP.Protocol.ResponseBuilder

  # Protocol version constants used by MethodHandlers
  # @supported_protocol_versions ["2024-11-05", "2025-03-26", "2025-06-18", "2025-11-25"]
  # @default_protocol_version "2025-11-25"

  @type t :: module()
  @type opts :: term()
  @type conn :: %__MODULE__.Conn{}

  # Conn struct is defined in ExMCP.MessageProcessor.Conn
  alias __MODULE__.Conn

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

  @doc """
  Process an MCP request using a handler module.

  This is a convenience function that creates a connection, processes it
  through a handler, and returns the response.
  """
  @spec process(Conn.t(), map()) :: Conn.t()
  def process(%Conn{} = conn, opts) do
    method = Map.get(conn.request, "method")

    # Validate based on message type (request vs notification)
    result =
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
            error_response = JSONRPC.error(get_request_id(conn.request), error_data)

            put_response(conn, error_response)
        end
      end

    :telemetry.execute(
      [:ex_mcp, :server, :request, :processed],
      %{},
      %{method: method, has_response: result.response != nil}
    )

    result
  end

  defp process_validated_request(%Conn{} = conn, opts) do
    handler = Map.get(opts, :handler)
    handler_opts = Map.get(opts, :handler_opts, [])
    server = Map.get(opts, :server)
    server_info = Map.get(opts, :server_info, %{})

    cond do
      # If we have a server PID, use it directly
      is_pid(server) ->
        process_handler_request(conn, server, server_info)

      # If we have a handler module
      handler != nil ->
        case handler do
          handler_module when is_atom(handler_module) ->
            process_with_handler_genserver(conn, handler_module, server_info, handler_opts)

          _ ->
            error_response =
              JSONRPC.error(
                get_request_id(conn.request),
                ErrorCodes.internal_error(),
                "Invalid handler type"
              )

            put_response(conn, error_response)
        end

      # No handler or server configured
      true ->
        error_response =
          JSONRPC.error(
            get_request_id(conn.request),
            ErrorCodes.internal_error(),
            "No handler configured"
          )

        put_response(conn, error_response)
    end
  end

  # Process request using Handler Server with GenServer
  defp process_with_handler_genserver(conn, handler_module, server_info, handler_opts) do
    # Start the handler as a GenServer
    case GenServer.start_link(handler_module, handler_opts) do
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
        error_response =
          JSONRPC.error(
            get_request_id(conn.request),
            ErrorCodes.internal_error(),
            "Failed to start handler server",
            %{"reason" => inspect(reason)}
          )

        put_response(conn, error_response)
    end
  end

  # Process handler request through GenServer calls
  defp process_handler_request(conn, server_pid, server_info) do
    conn = assign(conn, :server_info, server_info)
    dispatch_to_method_handlers(conn, server_pid)
  end

  alias ExMCP.MessageProcessor.MethodHandlers

  defp dispatch_to_method_handlers(conn, handler) do
    request = conn.request
    method = Map.get(request, "method")
    params = Map.get(request, "params", %{})
    id = get_request_id(request)

    case method do
      "ping" -> handle_ping(conn, id)
      "initialize" -> handle_initialize_dispatch(conn, handler, params, id)
      "logging/setLevel" -> put_response(conn, ResponseBuilder.build_success_response(%{}, id))
      _ -> dispatch_method(method, conn, handler, params, id)
    end
  end

  defp dispatch_method(method, conn, handler, params, id) do
    case method do
      "tools/list" ->
        MethodHandlers.handle_tools_list(conn, handler, params, id)

      "tools/call" ->
        MethodHandlers.handle_tools_call(conn, handler, params, id)

      "resources/list" ->
        MethodHandlers.handle_resources_list(conn, handler, params, id)

      "resources/read" ->
        MethodHandlers.handle_resources_read(conn, handler, params, id)

      "resources/subscribe" ->
        MethodHandlers.handle_resources_subscribe(conn, handler, params, id)

      "resources/unsubscribe" ->
        MethodHandlers.handle_resources_unsubscribe(conn, handler, params, id)

      "prompts/list" ->
        MethodHandlers.handle_prompts_list(conn, handler, params, id)

      "prompts/get" ->
        MethodHandlers.handle_prompts_get(conn, handler, params, id)

      "completion/complete" ->
        MethodHandlers.handle_completion_complete(conn, handler, params, id)

      _ ->
        MethodHandlers.handle_custom_method(conn, handler, method, params, id)
    end
  end

  defp handle_ping(conn, id) do
    response = ResponseBuilder.build_success_response(%{}, id)
    put_response(conn, response)
  end

  defp handle_initialize_dispatch(conn, handler, params, id) do
    # server_info comes from the conn's assigns (set during process routing)
    server_info = Map.get(conn.assigns || %{}, :server_info, %{})
    MethodHandlers.handle_initialize(conn, handler, params, id, server_info)
  end

  defp get_request_id(request) when is_map(request), do: Map.get(request, "id")
  defp get_request_id(_), do: nil

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
      {:ok, _tracker} ->
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
    do:
      {:error, %{"code" => ErrorCodes.invalid_request(), "message" => "Invalid JSON-RPC version"}}

  defp validate_notification_structure(%{"method" => _method}) do
    # Notifications only require jsonrpc and method fields
    :ok
  end

  defp validate_notification_structure(_) do
    {:error,
     %{"code" => ErrorCodes.invalid_request(), "message" => "Notification must have method field"}}
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
