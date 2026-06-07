defmodule ExMCP.ACP.AdapterEvents do
  @moduledoc """
  Pure ACP event builders for agent adapters.
  """

  alias ExMCP.ACP.Meta

  @spec session_update(String.t(), map()) :: map()
  def session_update(session_id, update) do
    %{
      "jsonrpc" => "2.0",
      "method" => "session/update",
      "params" => %{
        "sessionId" => session_id || "default",
        "update" => update
      }
    }
  end

  @spec status_update(String.t(), String.t(), String.t(), map()) :: map()
  def status_update(session_id, adapter, status, extra \\ %{}) do
    session_update(session_id, %{
      "sessionUpdate" => "session_info_update",
      "_meta" => %{
        "ex_mcp" => Map.merge(%{"adapter" => adapter, "status" => status}, extra)
      }
    })
  end

  @spec prompt_response(any(), String.t(), keyword()) :: map()
  def prompt_response(id, stop_reason, opts \\ []) do
    result =
      %{"stopReason" => stop_reason}
      |> maybe_put("usage", Keyword.get(opts, :usage))
      |> Meta.put_ex_mcp(Keyword.get(opts, :meta, %{}))

    %{"jsonrpc" => "2.0", "id" => id, "result" => result}
  end

  @spec maybe_put(map(), any(), any()) :: map()
  def maybe_put(map, _key, nil), do: map
  def maybe_put(map, key, value), do: Map.put(map, key, value)
end
