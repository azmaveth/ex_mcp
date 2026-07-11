defmodule ExMCP.Server do
  @moduledoc """
  Public server-side helpers for running MCP servers.

  Server implementations should use `ExMCP.Server.Handler` directly. For a
  declarative handler DSL, combine it with `ExMCP.Server.DSL`:

      defmodule MyServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.DSL

        tool "echo", "Echo input" do
          param :message, :string, required: true

          run fn %{message: message}, state ->
            {:ok, %{text: message}, state}
          end
        end
      end

  Use `ExMCP.Server.HandlerServer.start_link/1` when you need a transport-aware
  process for a handler module.

  > #### Deprecated API {: .warning}
  >
  > `ExMCP.Server.Tools` (and `Tools.Simplified`) are deprecated and will be
  > removed in **1.1.0**. Prefer `ExMCP.Server.DSL` for new code.
  """

  @doc """
  Sends a log message through the server.
  """
  @spec send_log_message(GenServer.server(), atom() | String.t(), String.t(), map()) :: :ok
  def send_log_message(server, level, message, data) do
    GenServer.cast(server, {:send_log_message, level, message, data})
  end

  @doc """
  Sends a ping request to the connected client.
  """
  @spec ping(GenServer.server(), timeout()) :: {:ok, map()} | {:error, any()}
  def ping(server, timeout \\ 5000) do
    GenServer.call(server, :ping, timeout)
  end

  @doc """
  Lists roots available from the connected client.
  """
  @spec list_roots(GenServer.server(), timeout()) :: {:ok, %{roots: [map()]}} | {:error, any()}
  def list_roots(server, timeout \\ 5000) do
    GenServer.call(server, {:list_roots, timeout}, timeout)
  end

  @doc """
  Notifies the client that the server's roots have changed.
  """
  @spec notify_roots_changed(GenServer.server()) :: :ok
  def notify_roots_changed(server) do
    GenServer.cast(server, :notify_roots_changed)
  end

  @doc """
  Sends a progress notification to the client.
  """
  @spec notify_progress(GenServer.server(), any(), number()) :: :ok
  def notify_progress(server, progress_token, progress) do
    GenServer.cast(server, {:notify_progress, progress_token, progress, nil})
  end

  @doc """
  Sends a progress notification with a total to the client.
  """
  @spec notify_progress(GenServer.server(), any(), number(), number()) :: :ok
  def notify_progress(server, progress_token, progress, total) do
    GenServer.cast(server, {:notify_progress, progress_token, progress, total})
  end

  @doc """
  Sends a resource update notification for subscribed clients.
  """
  @spec notify_resource_update(GenServer.server(), String.t()) :: :ok
  def notify_resource_update(server, uri) do
    GenServer.cast(server, {:notify_resource_update, uri})
  end

  @doc """
  Notifies subscribed clients that the resource list has changed.
  """
  @spec notify_resources_changed(GenServer.server()) :: :ok
  def notify_resources_changed(server) do
    GenServer.cast(server, {:notify_resources_changed})
  end

  @doc """
  Notifies subscribed clients that the tools list has changed.
  """
  @spec notify_tools_changed(GenServer.server()) :: :ok
  def notify_tools_changed(server) do
    GenServer.cast(server, {:notify_tools_changed})
  end

  @doc """
  Notifies subscribed clients that the prompts list has changed.
  """
  @spec notify_prompts_changed(GenServer.server()) :: :ok
  def notify_prompts_changed(server) do
    GenServer.cast(server, {:notify_prompts_changed})
  end

  @doc """
  Gets the list of pending request IDs on the server.
  """
  @spec get_pending_requests(GenServer.server()) :: [ExMCP.Types.request_id()]
  def get_pending_requests(server) do
    GenServer.call(server, :get_pending_requests)
  end

  @doc """
  Sends a cancellation notification to the server.
  """
  @spec cancel_request(GenServer.server(), ExMCP.Types.request_id(), String.t() | nil) :: :ok
  def cancel_request(server, request_id, reason \\ nil) do
    params = %{"requestId" => request_id}
    params = if reason, do: Map.put(params, "reason", reason), else: params
    GenServer.cast(server, {:notification, "notifications/cancelled", params})
  end

  @doc """
  Sends a `sampling/createMessage` request to the connected client.
  """
  @spec create_message(GenServer.server(), map()) :: {:ok, map()} | {:error, term()}
  def create_message(server, params) do
    case GenServer.call(server, {:create_message, params}) do
      {:error, {:unknown_call, _request}} -> {:error, :not_implemented}
      result -> result
    end
  end
end
