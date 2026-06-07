defmodule ExMCP.ACP.Client.DefaultHandler do
  @moduledoc """
  Default ACP handler that collects events and denies permissions.

  Collects all session updates in a list (newest first). Permission requests
  are denied by default. File access is denied.

  Useful for testing and simple use cases. For production, implement
  `ExMCP.ACP.Client.Handler` with custom logic.

  Set `auto_approve_permissions: true` in handler opts only for trusted local
  tests or demos that intentionally approve the first allow option.
  """

  @behaviour ExMCP.ACP.Client.Handler

  @impl true
  def init(opts) do
    {:ok,
     %{events: [], auto_approve_permissions: Keyword.get(opts, :auto_approve_permissions, false)}}
  end

  @impl true
  def handle_session_update(_session_id, update, state) do
    {:ok, %{state | events: [update | state.events]}}
  end

  @impl true
  def handle_permission_request(_session_id, _tool_call, options, state) do
    outcome =
      if state.auto_approve_permissions do
        option =
          Enum.find(options, &(Map.get(&1, "kind") in ["allow_once", "allow_always"])) ||
            List.first(options)

        case option do
          nil -> %{"outcome" => "cancelled"}
          option -> %{"outcome" => "selected", "optionId" => option["optionId"]}
        end
      else
        option =
          Enum.find(options, &(Map.get(&1, "kind") in ["reject_once", "reject_always"]))

        case option do
          nil -> %{"outcome" => "cancelled"}
          option -> %{"outcome" => "selected", "optionId" => option["optionId"]}
        end
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
