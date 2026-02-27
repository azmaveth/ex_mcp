defmodule ExMCP.ACP.Client.DefaultHandler do
  @moduledoc """
  Default ACP handler that collects events and auto-allows permissions.

  Collects all session updates in a list (newest first). Permission requests
  are auto-approved using the first available option. File access is denied.

  Useful for testing and simple use cases. For production, implement
  `ExMCP.ACP.Client.Handler` with custom logic.
  """

  @behaviour ExMCP.ACP.Client.Handler

  @impl true
  def init(_opts) do
    {:ok, %{events: []}}
  end

  @impl true
  def handle_session_update(_session_id, update, state) do
    {:ok, %{state | events: [update | state.events]}}
  end

  @impl true
  def handle_permission_request(_session_id, _tool_call, options, state) do
    # Auto-allow: pick the first option (usually "allow" or "allow once")
    outcome =
      case options do
        [first | _] -> %{"optionId" => first["id"]}
        [] -> %{"optionId" => "allow"}
      end

    {:ok, outcome, state}
  end

  @impl true
  def handle_file_read(_session_id, _path, _opts, state) do
    {:error, "File read denied by default handler", state}
  end

  @impl true
  def handle_file_write(_session_id, _path, _content, state) do
    {:error, "File write denied by default handler", state}
  end

  @impl true
  def terminate(_reason, _state), do: :ok
end
