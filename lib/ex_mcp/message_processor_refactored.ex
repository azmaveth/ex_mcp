defmodule ExMCP.MessageProcessorRefactored do
  @moduledoc """
  Core message processing abstraction for ExMCP (Refactored Version).

  This version eliminates code duplication by using the unified Dispatcher
  and Handlers modules, reducing the codebase by ~500 lines while maintaining
  all functionality and backward compatibility.
  """

  alias ExMCP.Internal.MessageValidator
  alias ExMCP.MessageProcessor.Conn
  alias ExMCP.MessageProcessor.Dispatcher

  require Logger

  @type t :: module()
  @type opts :: term()
  @type conn :: Conn.t()

  # Re-export Conn for backward compatibility
  defmodule Conn do
    defstruct [
      :request,
      :response,
      :state,
      :assigns,
      :transport,
      :session_id,
      :progress_token,
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

  @doc """
  Process an MCP request using a handler module.

  This is a convenience function that creates a connection, processes it
  through a handler, and returns the response.
  """
  @spec process(Conn.t(), map()) :: Conn.t()
  def process(%Conn{} = conn, opts) do
    # First, validate the incoming request against the MCP spec.
    case MessageValidator.validate_request(conn.request) do
      {:ok, _validated_request} ->
        # Request is valid, proceed with processing.
        process_validated_request(conn, opts)

      {:error, error_data} ->
        # Request is invalid, construct and return an error response.
        error_response = %{
          "jsonrpc" => "2.0",
          "error" => error_data,
          "id" => get_request_id(conn.request)
        }

        put_response(conn, error_response)
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
        # Detect server type and dispatch appropriately
        mode = detect_handler_mode(handler_module)

        case mode do
          :dsl_server ->
            process_with_dsl_server(conn, handler_module, server_info)

          :handler_server ->
            process_with_handler_genserver(conn, handler_module, server_info)

          :direct ->
            # Use the unified dispatcher for direct handler calls
            Dispatcher.dispatch(conn, handler_module, :direct, server_info)
        end
    end
  end

  # Detect the handler mode based on exported functions
  defp detect_handler_mode(handler_module) do
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
        # Default to direct mode for simple handlers
        :direct
    end
  end

  # Process request using DSL Server with temporary GenServer instance
  defp process_with_dsl_server(conn, handler_module, server_info) do
    # Start a temporary server instance for this request
    case start_temporary_server(handler_module) do
      {:ok, server_pid} ->
        try do
          # Use unified dispatcher with genserver mode
          Dispatcher.dispatch(conn, server_pid, :genserver, server_info)
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
          # Use unified dispatcher with handler mode
          Dispatcher.dispatch(conn, server_pid, :handler, server_info)
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

  defp start_temporary_server(handler_module) do
    # Start a temporary GenServer instance
    handler_module.start_link([])
  end

  defp get_request_id(request) when is_map(request) do
    Map.get(request, "id")
  end

  defp get_request_id(_), do: nil

  # Progress notification helpers for MCP 2025-06-18 compliance

  # Extracts the progress token from a request's _meta field.
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
        Logger.warning("Failed to complete progress", token: token, reason: reason)
        conn
    end
  end
end
