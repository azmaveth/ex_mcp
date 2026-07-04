defmodule ExMCP.ACP.AdapterEvents do
  @moduledoc """
  Pure ACP event builders for agent adapters.
  """

  alias ExMCP.ACP.{Envelope, Maps, Meta}

  @spec session_update(String.t(), map()) :: map()
  def session_update(session_id, update) do
    Envelope.notification("session/update", %{
      "sessionId" => session_id || "default",
      "update" => update
    })
  end

  @spec agent_message_chunk(String.t(), String.t(), keyword()) :: map()
  def agent_message_chunk(session_id, text, opts \\ []) do
    text_chunk(session_id, "agent_message_chunk", text, opts)
  end

  @spec agent_thought_chunk(String.t(), String.t(), keyword()) :: map()
  def agent_thought_chunk(session_id, text, opts \\ []) do
    text_chunk(session_id, "agent_thought_chunk", text, opts)
  end

  @spec user_message_chunk(String.t(), String.t(), keyword()) :: map()
  def user_message_chunk(session_id, text, opts \\ []) do
    text_chunk(session_id, "user_message_chunk", text, opts)
  end

  @spec content_chunk(String.t(), String.t(), map(), keyword() | map()) :: map()
  def content_chunk(session_id, type, content, opts \\ []) do
    attrs =
      %{"content" => content}
      |> Maps.put_present("messageId", message_id_option(opts))

    session_update_type(session_id, type, attrs, opts)
  end

  @spec resource_link_chunk(String.t(), String.t(), keyword()) :: map()
  def resource_link_chunk(session_id, uri, opts \\ []) do
    content =
      %{"type" => "resource_link", "uri" => uri}
      |> Maps.put_present("name", Keyword.get(opts, :name))

    content_chunk(session_id, "agent_message_chunk", content, opts)
  end

  @spec current_mode_update(String.t(), String.t()) :: map()
  def current_mode_update(session_id, mode_id) do
    session_update_type(session_id, "current_mode_update", %{"currentModeId" => mode_id})
  end

  @spec available_commands_update(String.t(), [map()]) :: map()
  def available_commands_update(session_id, commands) do
    session_update_type(session_id, "available_commands_update", %{
      "availableCommands" => commands
    })
  end

  @spec plan(String.t(), [map()]) :: map()
  def plan(session_id, entries) do
    session_update_type(session_id, "plan", %{"entries" => entries})
  end

  @spec config_option_update(String.t(), [map()]) :: map()
  def config_option_update(session_id, options) do
    session_update_type(session_id, "config_option_update", %{"configOptions" => options})
  end

  @spec session_info_update(String.t(), map()) :: map()
  def session_info_update(session_id, attrs \\ %{}) do
    session_update_type(session_id, "session_info_update", attrs)
  end

  @spec tool_call(String.t(), map()) :: map()
  def tool_call(session_id, attrs) do
    session_update_type(session_id, "tool_call", attrs)
  end

  @spec tool_call_update(String.t(), map()) :: map()
  def tool_call_update(session_id, attrs) do
    session_update_type(session_id, "tool_call_update", attrs)
  end

  @spec session_update_type(String.t(), String.t(), map(), keyword()) :: map()
  def session_update_type(session_id, type, attrs \\ %{}, opts \\ []) do
    update =
      attrs
      |> Map.put("sessionUpdate", type)
      |> Maps.put_present("_meta", Keyword.get(opts, :meta))

    session_update(session_id, update)
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
      |> Maps.put_present("usage", Keyword.get(opts, :usage))
      |> Meta.put_ex_mcp(Keyword.get(opts, :meta, %{}))

    Envelope.response(id, result)
  end

  @spec maybe_put(map(), any(), any()) :: map()
  defdelegate maybe_put(map, key, value), to: Maps, as: :put_present

  defp text_chunk(session_id, type, text, opts) do
    content_chunk(session_id, type, %{"type" => "text", "text" => text}, opts)
  end

  defp message_id_option(opts) when is_list(opts) do
    Keyword.get(opts, :message_id) || Keyword.get(opts, :messageId)
  end

  defp message_id_option(opts) when is_map(opts) do
    Map.get(opts, "messageId") || Map.get(opts, :message_id) || Map.get(opts, :messageId)
  end

  defp message_id_option(_opts), do: nil
end
