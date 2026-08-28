defmodule ExMCP.ACP.Adapters.Codex.Protocol do
  @moduledoc """
  Pure native app-server protocol helpers for the Codex ACP adapter.

  Owns envelope construction, method names, inbound response classification,
  and pending-request correlation shapes. The root adapter retains process
  ownership, Port I/O, and lifecycle state.
  """

  alias ExMCP.Internal.Maps

  @type request_id :: integer() | String.t()
  @type request_type :: atom()
  @type request_entry :: %{
          required(:type) => request_type(),
          required(:acp_id) => term(),
          required(:meta) => map()
        }

  @type inbound ::
          {:response, request_id(), {:ok, term()}}
          | {:response, request_id(), {:error, term()}}
          | {:request, request_id(), String.t(), map()}
          | {:notification, String.t(), map()}
          | :unknown

  @methods %{
    initialize: "initialize",
    initialized: "initialized",
    model_list: "model/list",
    account_login_start: "account/login/start",
    account_logout: "account/logout",
    account_login_cancel: "account/login/cancel",
    thread_start: "thread/start",
    thread_resume: "thread/resume",
    thread_list: "thread/list",
    thread_unsubscribe: "thread/unsubscribe",
    thread_archive: "thread/archive",
    thread_compact_start: "thread/compact/start",
    turn_start: "turn/start",
    turn_interrupt: "turn/interrupt",
    review_start: "review/start"
  }

  @spec method(atom()) :: String.t()
  def method(name) when is_map_key(@methods, name), do: Map.fetch!(@methods, name)

  @spec methods() :: %{atom() => String.t()}
  def methods, do: @methods

  @spec request(request_id(), String.t(), map() | nil) :: map()
  def request(id, method, params) do
    %{"id" => id, "method" => method, "params" => params || %{}}
  end

  @spec response(request_id(), term()) :: map()
  def response(id, result), do: %{"id" => id, "result" => result}

  @spec error(request_id(), integer(), String.t()) :: map()
  def error(id, code, message) do
    %{"id" => id, "error" => %{"code" => code, "message" => message}}
  end

  @spec notification(String.t(), map() | nil) :: map()
  def notification(method, params \\ nil) do
    %{"method" => method} |> maybe_put("params", params)
  end

  @spec line(map()) :: iodata()
  def line(message), do: [Jason.encode!(message), "\n"]

  @spec encode_request(request_id(), String.t(), map() | nil) :: iodata()
  def encode_request(id, method, params), do: line(request(id, method, params))

  @spec encode_response(request_id(), term()) :: iodata()
  def encode_response(id, result), do: line(response(id, result))

  @spec encode_error(request_id(), integer(), String.t()) :: iodata()
  def encode_error(id, code, message), do: line(error(id, code, message))

  @spec encode_notification(String.t(), map() | nil) :: iodata()
  def encode_notification(method, params \\ nil), do: line(notification(method, params))

  @spec decode_line(String.t()) :: inbound()
  def decode_line(line) do
    case Jason.decode(line) do
      {:ok, msg} -> classify_inbound(msg)
      {:error, _} -> :unknown
    end
  end

  @spec classify_inbound(map()) :: inbound()
  def classify_inbound(%{"id" => id, "result" => result}), do: {:response, id, {:ok, result}}
  def classify_inbound(%{"id" => id, "error" => error}), do: {:response, id, {:error, error}}

  def classify_inbound(%{"id" => id, "method" => method, "params" => params})
      when is_binary(method) do
    {:request, id, method, params || %{}}
  end

  def classify_inbound(%{"method" => method, "params" => params}) when is_binary(method) do
    {:notification, method, params || %{}}
  end

  def classify_inbound(%{"method" => method}) when is_binary(method) do
    {:notification, method, %{}}
  end

  def classify_inbound(_msg), do: :unknown

  @spec request_entry(request_type(), term(), map()) :: request_entry()
  def request_entry(type, acp_id, meta \\ %{}) do
    %{type: type, acp_id: acp_id, meta: meta}
  end

  @spec next_id(integer()) :: {integer(), integer()}
  def next_id(id) when is_integer(id), do: {id, id + 1}

  defp maybe_put(map, key, value), do: Maps.put_non_empty(map, key, value)
end
