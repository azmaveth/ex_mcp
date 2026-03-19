defmodule ExMCP.Server.SSESession do
  @moduledoc """
  Server-side SSE session manager for bidirectional MCP communication.

  Manages the lifecycle of SSE sessions where the server needs to send
  requests to the client (elicitation, sampling) and receive responses.

  ## Architecture

  Each MCP session has:
  - A POST endpoint for client→server requests
  - A GET SSE stream for server→client messages (notifications, requests)
  - A pending request tracker for correlating server requests with client responses

  ## Usage

      # Initialize session state (call once at server startup)
      SSESession.init()

      # Register a GET SSE stream for a session
      SSESession.register_sse_stream(session_id)

      # Send a request to the client and wait for response
      {:ok, result} = SSESession.send_request(session_id, "elicitation/create", params)

      # Route a client response to the waiting request handler
      SSESession.handle_response(session_id, request_id, result)
  """

  require Logger

  @ets_table :ex_mcp_sse_sessions

  @doc "Initialize the session ETS table."
  @spec init() :: :ok
  def init do
    if :ets.info(@ets_table) == :undefined do
      :ets.new(@ets_table, [:set, :public, :named_table])
    end

    :ok
  end

  @doc "Register the calling process as the SSE stream for a session."
  @spec register_sse_stream(String.t()) :: :ok
  def register_sse_stream(session_id) do
    :ets.insert(@ets_table, {"sse_pid:#{session_id}", self()})
    :ok
  end

  @doc """
  Send a JSON-RPC request to the client via the GET SSE stream.

  Blocks until the client responds or timeout is reached.
  Returns `{:ok, result}` or `{:error, reason}`.
  """
  @spec send_request(String.t(), String.t(), map(), keyword()) ::
          {:ok, map()} | {:error, term()}
  def send_request(session_id, method, params, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 10_000)
    request_id = System.unique_integer([:positive])

    case :ets.lookup(@ets_table, "sse_pid:#{session_id}") do
      [{_, sse_pid}] ->
        # Register to receive the response
        :ets.insert(@ets_table, {"pending:#{request_id}", self()})

        # Send request via SSE stream
        request = %{
          "jsonrpc" => "2.0",
          "id" => request_id,
          "method" => method,
          "params" => params
        }

        send(sse_pid, {:sse_send, request})

        # Wait for response
        receive do
          {:client_response, ^request_id, result} -> result
        after
          timeout -> {:error, :timeout}
        end

      _ ->
        {:error, {:no_sse_stream, session_id}}
    end
  end

  @doc """
  Route a client response back to the waiting request handler.

  Called when the server receives a POST with a JSON-RPC response
  (has `id` + `result`/`error`, no `method`).
  """
  @spec handle_response(integer() | String.t(), {:ok, map()} | {:error, map()}) :: boolean()
  def handle_response(request_id, result) do
    key = "pending:#{request_id}"

    case :ets.lookup(@ets_table, key) do
      [{_, pid}] ->
        :ets.delete(@ets_table, key)
        send(pid, {:client_response, request_id, result})
        true

      _ ->
        false
    end
  end

  @doc """
  Run the SSE event loop for a GET connection.

  Forwards `{:sse_send, data}` messages as SSE events.
  Returns when `:sse_close` is received or timeout expires.
  """
  @spec run_sse_loop(Plug.Conn.t(), String.t(), keyword()) :: Plug.Conn.t()
  def run_sse_loop(conn, session_id, opts \\ []) do
    timeout = Keyword.get(opts, :idle_timeout, 60_000)

    receive do
      {:sse_send, data} ->
        case Plug.Conn.chunk(conn, "event: message\ndata: #{Jason.encode!(data)}\n\n") do
          {:ok, conn} -> run_sse_loop(conn, session_id, opts)
          {:error, _} -> conn
        end

      :sse_close ->
        conn
    after
      timeout -> conn
    end
  end

  @doc "Check if a session has an active SSE stream."
  @spec has_sse_stream?(String.t()) :: boolean()
  def has_sse_stream?(session_id) do
    case :ets.lookup(@ets_table, "sse_pid:#{session_id}") do
      [{_, pid}] -> Process.alive?(pid)
      _ -> false
    end
  end

  @doc "Clean up session state."
  @spec cleanup(String.t()) :: :ok
  def cleanup(session_id) do
    :ets.delete(@ets_table, "sse_pid:#{session_id}")
    :ok
  rescue
    _ -> :ok
  end
end
