defmodule ExMCP.ACP.Client.DefaultHandler do
  @moduledoc """
  Default ACP handler that collects events and denies permissions.

  Collects all session updates in a list (newest first). Permission requests
  are denied by default. File access is denied.

  Useful for testing and simple use cases. For production, implement
  `ExMCP.ACP.Client.Handler` with custom logic.

  Set `auto_approve_permissions: true` in handler opts only for trusted local
  tests or demos that intentionally approve the first allow option.

  Event retention is bounded by `:max_events` (1,000) and
  `:max_event_bytes` (1 MiB).
  """

  @behaviour ExMCP.ACP.Client.Handler
  @default_max_events 1_000
  @default_max_event_bytes 1_048_576

  @impl true
  def init(opts) do
    max_events =
      case Keyword.get(opts, :max_events, @default_max_events) do
        value when is_integer(value) and value > 0 -> value
        _invalid -> @default_max_events
      end

    max_event_bytes =
      case Keyword.get(opts, :max_event_bytes, @default_max_event_bytes) do
        value when is_integer(value) and value > 0 -> value
        _invalid -> @default_max_event_bytes
      end

    {:ok,
     %{
       events: [],
       event_bytes: 0,
       max_events: max_events,
       max_event_bytes: max_event_bytes,
       auto_approve_permissions: Keyword.get(opts, :auto_approve_permissions, false)
     }}
  end

  @impl true
  def handle_session_update(_session_id, update, state) do
    {events, event_bytes} =
      [update | state.events]
      |> Enum.take(state.max_events)
      |> Enum.reduce({[], 0}, fn event, {retained, total} ->
        size = event_size(event)

        if total + size <= state.max_event_bytes do
          {[event | retained], total + size}
        else
          {retained, total}
        end
      end)

    {:ok, %{state | events: Enum.reverse(events), event_bytes: event_bytes}}
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

  defp event_size(event) do
    case Jason.encode(event) do
      {:ok, encoded} -> byte_size(encoded)
      {:error, _reason} -> :erlang.external_size(event)
    end
  end
end
