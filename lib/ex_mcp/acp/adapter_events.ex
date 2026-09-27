defmodule ExMCP.ACP.AdapterEvents do
  @moduledoc """
  Builders for the ACP messages an adapter emits from `c:ExMCP.ACP.Adapter.translate_inbound/2`.

  Each function returns a complete JSON-RPC map (a `session/update`
  notification, or a `session/prompt` response) ready to go in a
  `{:messages, [...], state}` reply. They are pure and do no validation beyond
  shaping the envelope.

      def translate_inbound(line, state) do
        case Jason.decode(line) do
          {:ok, %{"type" => "delta", "text" => text}} ->
            {:messages, [AdapterEvents.agent_message_chunk(state.session_id, text)], state}

          {:ok, %{"type" => "done"}} ->
            {:messages, [AdapterEvents.prompt_response(state.prompt_id, "end_turn")], state}

          _ ->
            {:skip, state}
        end
      end

  A `nil` session id is sent as `"default"`. Options accepted by the chunk
  builders:

    * `:message_id` - sets the update's `messageId`, grouping chunks of one message
    * `:meta` - a map placed in the update's `_meta`
  """

  alias ExMCP.ACP.{Envelope, Maps, Meta}

  @doc "Wraps an already-built `update` map in a `session/update` notification."
  @spec session_update(String.t() | nil, map()) :: map()
  def session_update(session_id, update) do
    Envelope.notification("session/update", %{
      "sessionId" => session_id || "default",
      "update" => update
    })
  end

  @doc "An `agent_message_chunk` update carrying `text`."
  @spec agent_message_chunk(String.t(), String.t(), keyword()) :: map()
  def agent_message_chunk(session_id, text, opts \\ []) do
    text_chunk(session_id, "agent_message_chunk", text, opts)
  end

  @doc "An `agent_thought_chunk` update carrying `text`."
  @spec agent_thought_chunk(String.t(), String.t(), keyword()) :: map()
  def agent_thought_chunk(session_id, text, opts \\ []) do
    text_chunk(session_id, "agent_thought_chunk", text, opts)
  end

  @doc "A `user_message_chunk` update carrying `text`, e.g. when replaying history."
  @spec user_message_chunk(String.t(), String.t(), keyword()) :: map()
  def user_message_chunk(session_id, text, opts \\ []) do
    text_chunk(session_id, "user_message_chunk", text, opts)
  end

  @doc """
  A chunk update of `type` (such as `"agent_message_chunk"`) carrying an
  arbitrary ACP content block. `opts` may be a keyword list or a map.
  """
  @spec content_chunk(String.t() | nil, String.t(), map(), keyword() | map()) :: map()
  def content_chunk(session_id, type, content, opts \\ []) do
    attrs =
      %{"content" => content}
      |> Maps.put_present("messageId", message_id_option(opts))

    session_update_type(session_id, type, attrs, meta: meta_option(opts))
  end

  @doc "An `agent_message_chunk` carrying a `resource_link` to `uri`; accepts `:name`."
  @spec resource_link_chunk(String.t(), String.t(), keyword()) :: map()
  def resource_link_chunk(session_id, uri, opts \\ []) do
    content =
      %{"type" => "resource_link", "uri" => uri}
      |> Maps.put_present("name", Keyword.get(opts, :name))

    content_chunk(session_id, "agent_message_chunk", content, opts)
  end

  @doc "A `current_mode_update` announcing the session's active mode."
  @spec current_mode_update(String.t(), String.t()) :: map()
  def current_mode_update(session_id, mode_id) do
    session_update_type(session_id, "current_mode_update", %{"currentModeId" => mode_id})
  end

  @doc "An `available_commands_update` listing the agent's slash commands."
  @spec available_commands_update(String.t(), [map()]) :: map()
  def available_commands_update(session_id, commands) do
    session_update_type(session_id, "available_commands_update", %{
      "availableCommands" => commands
    })
  end

  @doc "A `plan` update with the given plan `entries`."
  @spec plan(String.t(), [map()]) :: map()
  def plan(session_id, entries) do
    session_update_type(session_id, "plan", %{"entries" => entries})
  end

  @doc "A `config_option_update` carrying the session's current config options."
  @spec config_option_update(String.t(), [map()]) :: map()
  def config_option_update(session_id, options) do
    session_update_type(session_id, "config_option_update", %{"configOptions" => options})
  end

  @doc "A `session_info_update` with `attrs` (for example `\"title\"`)."
  @spec session_info_update(String.t(), map()) :: map()
  def session_info_update(session_id, attrs \\ %{}) do
    session_update_type(session_id, "session_info_update", attrs)
  end

  @doc "A `tool_call` update announcing a new tool call; `attrs` holds the ACP fields."
  @spec tool_call(String.t(), map()) :: map()
  def tool_call(session_id, attrs) do
    session_update_type(session_id, "tool_call", attrs)
  end

  @doc "A `tool_call_update` reporting progress or completion of a tool call."
  @spec tool_call_update(String.t(), map()) :: map()
  def tool_call_update(session_id, attrs) do
    session_update_type(session_id, "tool_call_update", attrs)
  end

  @doc """
  A `session/update` whose `sessionUpdate` is `type`, with `attrs` merged in.
  The other builders are shorthands for this. Accepts `:meta`.
  """
  @spec session_update_type(String.t(), String.t(), map(), keyword()) :: map()
  def session_update_type(session_id, type, attrs \\ %{}, opts \\ []) do
    update =
      attrs
      |> Map.put("sessionUpdate", type)
      |> Maps.put_present("_meta", Keyword.get(opts, :meta))

    session_update(session_id, update)
  end

  @doc """
  A `session_info_update` reporting adapter `status` under `_meta.ex_mcp`,
  with `extra` merged into that map.
  """
  @spec status_update(String.t(), String.t(), String.t(), map()) :: map()
  def status_update(session_id, adapter, status, extra \\ %{}) do
    session_update(session_id, %{
      "sessionUpdate" => "session_info_update",
      "_meta" => %{
        "ex_mcp" => Map.merge(%{"adapter" => adapter, "status" => status}, extra)
      }
    })
  end

  @doc """
  The JSON-RPC response to a `session/prompt` request `id`. Accepts `:usage`
  and `:meta` (placed under `_meta.ex_mcp`).
  """
  @spec prompt_response(any(), String.t(), keyword()) :: map()
  def prompt_response(id, stop_reason, opts \\ []) do
    result =
      %{"stopReason" => stop_reason}
      |> Maps.put_present("usage", Keyword.get(opts, :usage))
      |> Meta.put_ex_mcp(Keyword.get(opts, :meta, %{}))

    Envelope.response(id, result)
  end

  # Map helper that leaked into this module's API; kept callable through 1.x.
  @doc false
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

  defp meta_option(opts) when is_list(opts), do: Keyword.get(opts, :meta)
  defp meta_option(opts) when is_map(opts), do: Map.get(opts, :meta)
  defp meta_option(_opts), do: nil
end
