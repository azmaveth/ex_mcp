defmodule ExMCP.ACP.Adapters.Pi.RPC do
  @moduledoc """
  Pure native RPC protocol helpers for the Pi ACP adapter.

  Owns NDJSON envelope construction, command type names, inbound response
  classification, and request-id correlation shapes. The root adapter retains
  process ownership, Port I/O, and lifecycle state.
  """

  @type request_id :: String.t()
  @type command :: String.t()
  @type control_kind :: atom()

  @type control_entry :: %{
          required(:rpc_id) => request_id(),
          required(:kind) => control_kind(),
          required(:group_id) => String.t()
        }

  @type inbound ::
          {:response, request_id(), {:ok, map()}}
          | {:response, request_id(), {:error, map()}}
          | {:event, String.t(), map()}
          | :unknown

  @methods %{
    new_session: "new_session",
    switch_session: "switch_session",
    get_state: "get_state",
    get_messages: "get_messages",
    get_available_models: "get_available_models",
    get_commands: "get_commands",
    prompt: "prompt",
    abort: "abort",
    set_thinking_level: "set_thinking_level",
    set_model: "set_model",
    set_auto_compaction: "set_auto_compaction",
    set_auto_retry: "set_auto_retry",
    set_steering_mode: "set_steering_mode",
    set_follow_up_mode: "set_follow_up_mode",
    compact: "compact",
    export_html: "export_html",
    get_session_stats: "get_session_stats",
    set_session_name: "set_session_name",
    extension_ui_response: "extension_ui_response"
  }

  @spec method(atom()) :: String.t()
  def method(name) when is_map_key(@methods, name), do: Map.fetch!(@methods, name)

  @spec methods() :: %{atom() => String.t()}
  def methods, do: @methods

  @spec request(request_id(), String.t(), map()) :: map()
  def request(id, type, fields \\ %{}) when is_binary(id) and is_binary(type) do
    fields
    |> compact()
    |> Map.put("type", type)
    |> Map.put("id", id)
  end

  @spec notification(String.t(), map()) :: map()
  def notification(type, fields \\ %{}) when is_binary(type) do
    fields
    |> compact()
    |> Map.put("type", type)
  end

  @spec line(map()) :: String.t()
  def line(message), do: Jason.encode!(compact(message)) <> "\n"

  @spec encode_request(request_id(), String.t(), map()) :: String.t()
  def encode_request(id, type, fields \\ %{}), do: line(request(id, type, fields))

  @spec encode_notification(String.t(), map()) :: String.t()
  def encode_notification(type, fields \\ %{}), do: line(notification(type, fields))

  @spec encode_many([map()]) :: binary()
  def encode_many(messages), do: messages |> Enum.map(&line/1) |> IO.iodata_to_binary()

  @spec decode_line(String.t()) :: inbound()
  def decode_line(line) when is_binary(line) do
    trimmed = String.trim(line)

    if trimmed == "" do
      :unknown
    else
      case Jason.decode(trimmed) do
        {:ok, msg} -> classify_inbound(msg)
        {:error, _} -> :unknown
      end
    end
  end

  @spec classify_inbound(term()) :: inbound()
  def classify_inbound(%{"type" => "response", "id" => id} = event) when is_binary(id) do
    if event["success"] == false do
      {:response, id, {:error, event}}
    else
      {:response, id, {:ok, event}}
    end
  end

  def classify_inbound(%{"type" => type} = event) when is_binary(type) do
    {:event, type, event}
  end

  def classify_inbound(_msg), do: :unknown

  @spec rpc_id(integer()) :: request_id()
  def rpc_id(n) when is_integer(n), do: "pi-#{n}"

  @spec prompt_id(integer()) :: request_id()
  def prompt_id(n) when is_integer(n), do: "msg-#{n}"

  @spec next_prompt_id(integer()) :: {request_id(), integer()}
  def next_prompt_id(counter) when is_integer(counter) do
    next = counter + 1
    {prompt_id(next), next}
  end

  @spec control_entry(request_id(), control_kind(), String.t()) :: control_entry()
  def control_entry(rpc_id, kind, group_id)
      when is_binary(rpc_id) and is_atom(kind) and is_binary(group_id) do
    %{rpc_id: rpc_id, kind: kind, group_id: group_id}
  end

  @spec compact(map() | term()) :: map() | term()
  def compact(map) when is_map(map) do
    map
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Map.new()
  end

  def compact(value), do: value
end
