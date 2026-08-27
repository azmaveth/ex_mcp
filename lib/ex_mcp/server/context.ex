defmodule ExMCP.Server.Context do
  @moduledoc """
  Access to the validated context of the currently executing server callback.

  This is primarily useful for MRTR-aware handlers that retain their existing
  callback arity. The value is scoped to the callback invocation and must not
  be read later from a spawned process.

  `cancelled?/0` reports whether the current request id has been cancelled.
  """

  alias ExMCP.Internal.Protocol
  alias ExMCP.Server.{Cancellation, RequestContext}

  @key {__MODULE__, :current}
  @log_levels ~w(debug info notice warning error critical alert emergency)

  @spec current() :: RequestContext.t() | nil
  def current, do: Process.get(@key)

  @doc """
  Returns true if the current request id has been cancelled.

  Safe to call from inside a running handler: cancel is recorded out of
  band when `notifications/cancelled` is accepted, so this does not wait
  for the server GenServer to finish the current callback.

  Returns `false` when there is no request context or the current id has
  not been cancelled. The server MAY stop work when this is true; ExMCP
  does not automatically abort the JSON-RPC request.
  """
  @spec cancelled?() :: boolean()
  def cancelled? do
    case current() do
      %RequestContext{request_id: request_id} when not is_nil(request_id) ->
        Cancellation.cancelled?(request_id)

      _other ->
        false
    end
  end

  @spec input_responses() :: map() | nil
  def input_responses do
    case current() do
      %RequestContext{input_responses: responses} -> responses
      nil -> nil
    end
  end

  @spec request_state() :: term()
  def request_state do
    case current() do
      %RequestContext{request_state: state} -> state
      nil -> nil
    end
  end

  @spec progress_token() :: ExMCP.Types.progress_token() | nil
  def progress_token do
    case current() do
      %RequestContext{progress_token: token} -> token
      nil -> nil
    end
  end

  @doc """
  Reports progress for the currently executing request callback.

  Modern streamable-HTTP handlers use this request-scoped helper instead of a
  connection-wide server process. The request must include
  `_meta.progressToken`, and its HTTP response must be an active SSE stream.
  Delivery is acknowledged before this function returns, which guarantees the
  notification is written before the request's final JSON-RPC response.
  """
  @spec report_progress(number(), number() | nil, String.t() | nil) ::
          :ok
          | {:error,
             :no_request_context
             | :progress_not_requested
             | :request_not_streaming
             | :stream_closed
             | :stream_timeout}
  def report_progress(progress, total \\ nil, message \\ nil) when is_number(progress) do
    case current() do
      %RequestContext{progress_token: nil} ->
        {:error, :progress_not_requested}

      %RequestContext{notification_target: target, progress_token: token} when is_pid(target) ->
        deliver(target, Protocol.encode_progress(token, progress, total, message))

      %RequestContext{} ->
        {:error, :request_not_streaming}

      nil ->
        {:error, :no_request_context}
    end
  end

  @doc """
  Sends a log notification on the currently executing request's HTTP stream.

  Request-scoped log delivery is available only while that request owns an
  active SSE response. It never falls back to another request or subscription
  stream.

  MCP protocol Logging is deprecated as of 2026-07-28 and retained throughout
  ExMCP 1.x. Prefer stderr for stdio diagnostics or OpenTelemetry for new
  structured-observability integrations.
  """
  @spec send_log_message(atom() | String.t(), String.t(), map()) ::
          :ok
          | {:error,
             :no_request_context
             | :logging_not_requested
             | :invalid_log_level
             | :request_not_streaming
             | :stream_closed
             | :stream_timeout}
  def send_log_message(level, message, data \\ %{}) when is_binary(message) and is_map(data) do
    level = to_string(level)

    case current() do
      %RequestContext{log_level: nil} ->
        {:error, :logging_not_requested}

      %RequestContext{log_level: requested, notification_target: target} ->
        cond do
          level not in @log_levels or requested not in @log_levels ->
            {:error, :invalid_log_level}

          not log_level_enabled?(level, requested) ->
            :ok

          not is_pid(target) ->
            {:error, :request_not_streaming}

          true ->
            data =
              if map_size(data) == 0,
                do: message,
                else: Map.put_new(data, "message", message)

            notification = %{
              "jsonrpc" => "2.0",
              "method" => "notifications/message",
              "params" => %{
                "level" => level,
                "logger" => "ExMCP.Server",
                "data" => data
              }
            }

            deliver(target, notification)
        end

      nil ->
        {:error, :no_request_context}
    end
  end

  defp log_level_enabled?(level, requested) do
    Enum.find_index(@log_levels, &(&1 == level)) >=
      Enum.find_index(@log_levels, &(&1 == requested))
  end

  @doc false
  def with_context(%RequestContext{} = context, fun) when is_function(fun, 0) do
    previous = Process.put(@key, context)

    try do
      fun.()
    after
      restore(previous)
      Cancellation.clear(context.request_id)
    end
  end

  defp deliver(target, notification) do
    ref = make_ref()
    monitor = Process.monitor(target)
    send(target, {:ex_mcp_request_notification, self(), ref, notification})

    receive do
      {:ex_mcp_request_notification_ack, ^ref, :ok} ->
        Process.demonitor(monitor, [:flush])
        :ok

      {:ex_mcp_request_notification_ack, ^ref, {:error, _reason}} ->
        Process.demonitor(monitor, [:flush])
        {:error, :stream_closed}

      {:DOWN, ^monitor, :process, ^target, _reason} ->
        {:error, :stream_closed}
    after
      5_000 ->
        Process.demonitor(monitor, [:flush])
        {:error, :stream_timeout}
    end
  end

  defp restore(nil), do: Process.delete(@key)
  defp restore(previous), do: Process.put(@key, previous)
end
