defmodule ExMCP.Transport.HTTP.ModernStreamClient do
  @moduledoc false

  use GenServer

  alias ExMCP.Internal.{Headers, SSE}

  @httpc_profiles [
    :ex_mcp_modern_stream_0,
    :ex_mcp_modern_stream_1,
    :ex_mcp_modern_stream_2,
    :ex_mcp_modern_stream_3,
    :ex_mcp_modern_stream_4,
    :ex_mcp_modern_stream_5,
    :ex_mcp_modern_stream_6,
    :ex_mcp_modern_stream_7,
    :ex_mcp_modern_stream_8,
    :ex_mcp_modern_stream_9,
    :ex_mcp_modern_stream_10,
    :ex_mcp_modern_stream_11,
    :ex_mcp_modern_stream_12,
    :ex_mcp_modern_stream_13,
    :ex_mcp_modern_stream_14,
    :ex_mcp_modern_stream_15
  ]

  defstruct [
    :parent,
    :parent_monitor,
    :request_id,
    :stream_kind,
    :url,
    :headers,
    :body,
    :http_options,
    :httpc_profile,
    :request_ref,
    :idle_timeout,
    :idle_timer,
    :auth_provider,
    :auth_provider_state,
    auth_attempts: 0,
    buffer: "",
    completed?: false,
    failed?: false,
    cancelled?: false,
    close_notified?: false
  ]

  @spec start(keyword()) :: GenServer.on_start()
  def start(opts), do: GenServer.start(__MODULE__, opts)

  @spec cancel(pid()) :: :ok
  def cancel(pid) do
    GenServer.call(pid, :cancel, 1_000)
  catch
    :exit, _reason -> :ok
  end

  @impl true
  def init(opts) do
    parent = Keyword.fetch!(opts, :parent)
    profile = httpc_profile()
    ensure_httpc_profile!(profile)

    state = %__MODULE__{
      parent: parent,
      parent_monitor: Process.monitor(parent),
      request_id: Keyword.fetch!(opts, :request_id),
      stream_kind: Keyword.fetch!(opts, :stream_kind),
      url: Keyword.fetch!(opts, :url),
      headers: Keyword.fetch!(opts, :headers),
      body: Keyword.fetch!(opts, :body),
      http_options: Keyword.fetch!(opts, :http_options),
      httpc_profile: profile,
      idle_timeout: Keyword.fetch!(opts, :idle_timeout),
      auth_provider: Keyword.get(opts, :auth_provider),
      auth_provider_state: Keyword.get(opts, :auth_provider_state)
    }

    {:ok, state, {:continue, :open}}
  end

  @impl true
  def handle_continue(:open, state) do
    case open_request(state) do
      {:ok, request_ref} ->
        {:noreply, %{state | request_ref: request_ref}}

      {:error, reason} ->
        {:stop, :normal, notify_closed(state, reason)}
    end
  end

  @impl true
  def handle_call(:cancel, _from, state) do
    cancel_request(state)
    {:stop, :normal, :ok, %{state | cancelled?: true}}
  end

  @impl true
  def handle_info({:http, {ref, :stream_start, _headers}}, %{request_ref: ref} = state) do
    {:noreply, reset_idle_timer(state)}
  end

  def handle_info({:http, {ref, :stream, chunk}}, %{request_ref: ref} = state) do
    buffer = state.buffer <> to_binary(chunk)
    {events, remaining} = SSE.parse_stream(buffer)

    state =
      Enum.reduce_while(events, %{reset_idle_timer(state) | buffer: remaining}, fn event, acc ->
        state = deliver_event(event, acc)

        if state.completed? or state.failed?, do: {:halt, state}, else: {:cont, state}
      end)

    cond do
      state.completed? ->
        cancel_request(state)
        send(state.parent, {:modern_http_stream_finished, self(), state.request_id})
        {:stop, :normal, cancel_idle_timer(state)}

      state.failed? ->
        cancel_request(state)
        {:stop, :normal, cancel_idle_timer(state)}

      true ->
        {:noreply, state}
    end
  end

  def handle_info({:http, {ref, :stream_end, _headers}}, %{request_ref: ref} = state) do
    state = cancel_idle_timer(state)

    if state.completed? do
      send(state.parent, {:modern_http_stream_finished, self(), state.request_id})
      {:stop, :normal, state}
    else
      {:stop, :normal, notify_closed(state, :stream_ended)}
    end
  end

  def handle_info(
        {:http, {ref, {{_version, status, _reason}, headers, body}}},
        %{request_ref: ref} = state
      ) do
    state = cancel_idle_timer(state)
    body = to_binary(body)

    case maybe_retry_with_auth(status, headers, state) do
      {:retry, state} ->
        case open_request(%{state | request_ref: nil}) do
          {:ok, request_ref} ->
            {:noreply, %{state | request_ref: request_ref}}

          {:error, reason} ->
            {:stop, :normal, notify_closed(state, {:auth_retry_failed, reason})}
        end

      :no_retry ->
        state = handle_complete_response(status, body, state)

        if state.completed? do
          send(state.parent, {:modern_http_stream_finished, self(), state.request_id})
        end

        {:stop, :normal, state}

      {:error, reason, state} ->
        {:stop, :normal, notify_closed(state, {:authentication_failed, reason})}
    end
  end

  def handle_info({:http, {ref, {:error, reason}}}, %{request_ref: ref} = state) do
    {:stop, :normal, state |> cancel_idle_timer() |> notify_closed(reason)}
  end

  def handle_info(:idle_timeout, state) do
    cancel_request(state)
    {:stop, :normal, notify_closed(state, :stream_idle_timeout)}
  end

  def handle_info(
        {:DOWN, ref, :process, parent, _reason},
        %{parent_monitor: ref, parent: parent} = state
      ) do
    cancel_request(state)
    {:stop, :normal, %{state | cancelled?: true}}
  end

  @impl true
  def terminate(_reason, state) do
    cancel_idle_timer(state)

    unless state.cancelled? or state.completed? or state.close_notified? do
      send(state.parent, {:modern_http_stream_closed, self(), state.request_id, :stream_stopped})
    end

    :ok
  end

  defp open_request(state) do
    request = {
      String.to_charlist(state.url),
      Enum.map(state.headers, fn {name, value} ->
        {String.to_charlist(name), String.to_charlist(value)}
      end),
      ~c"application/json",
      state.body
    }

    :httpc.request(
      :post,
      request,
      state.http_options,
      [sync: false, stream: :self, body_format: :binary],
      state.httpc_profile
    )
  end

  defp deliver_event(%{"data" => data}, state) when is_binary(data) do
    case Jason.decode(data) do
      {:ok, message} ->
        handle_stream_message(message, state)

      {:error, _reason} ->
        state
        |> notify_closed(:invalid_sse_json)
        |> Map.put(:failed?, true)
    end
  end

  defp deliver_event(_comment_or_empty_event, state), do: state

  defp handle_stream_message(message, state) do
    case validate_message(message, state.request_id, state.stream_kind) do
      :ok ->
        deliver_message(message, state)

        if final_response?(message),
          do: %{state | completed?: true},
          else: state

      {:error, reason} ->
        state
        |> notify_closed(reason)
        |> Map.put(:failed?, true)
    end
  end

  defp deliver_message(message, state) do
    send(
      state.parent,
      {:modern_http_stream_message, self(), state.request_id, message}
    )
  end

  defp handle_complete_response(status, body, state) when status in 200..299 do
    case Jason.decode(body) do
      {:ok, message} ->
        case validate_message(message, state.request_id, state.stream_kind) do
          :ok ->
            if final_response?(message) do
              deliver_message(message, state)
              %{state | completed?: true}
            else
              notify_closed(state, :final_response_required)
            end

          {:error, reason} ->
            notify_closed(state, reason)
        end

      {:error, _reason} ->
        notify_closed(state, :invalid_sse_json)
    end
  end

  defp handle_complete_response(status, _body, state) do
    notify_closed(state, {:http_error, status})
  end

  # A POST-owned stream gets non-2xx responses as complete httpc responses,
  # rather than stream_start/stream_end messages. Retry at most the same two
  # challenge steps as the ordinary HTTP path: an initial 401 followed by an
  # optional 403 insufficient-scope step-up.
  defp maybe_retry_with_auth(status, response_headers, state)
       when status in [401, 403] and not is_nil(state.auth_provider) and
              state.auth_attempts < 2 do
    callback = if status == 401, do: :handle_unauthorized, else: :handle_forbidden
    www_auth = Headers.get(response_headers, "www-authenticate")
    scopes = extract_scopes(www_auth)

    case apply(state.auth_provider, callback, [www_auth, scopes, state.auth_provider_state]) do
      {:ok, token, provider_state} when is_binary(token) ->
        headers = put_bearer_header(state.headers, token)

        send(
          state.parent,
          {:modern_http_stream_auth_updated, self(), state.request_id,
           %{access_token: token, auth_provider_state: provider_state}}
        )

        {:retry,
         %{
           state
           | headers: headers,
             auth_provider_state: provider_state,
             auth_attempts: state.auth_attempts + 1,
             buffer: "",
             close_notified?: false,
             failed?: false,
             completed?: false
         }}

      {:error, reason, _provider_state} ->
        {:error, reason, state}

      other ->
        {:error, {:invalid_provider_response, other}, state}
    end
  end

  defp maybe_retry_with_auth(_status, _response_headers, _state), do: :no_retry

  defp extract_scopes(www_auth) when is_binary(www_auth) do
    case Regex.run(~r/scope="([^"]+)"/, www_auth) do
      [_, scopes] -> String.split(scopes, " ", trim: true)
      _other -> []
    end
  end

  defp extract_scopes(_www_auth), do: []

  defp put_bearer_header(headers, token) do
    headers
    |> Headers.delete("authorization")
    |> List.insert_at(0, {"Authorization", "Bearer #{token}"})
  end

  defp reset_idle_timer(state) do
    state = cancel_idle_timer(state)
    %{state | idle_timer: Process.send_after(self(), :idle_timeout, state.idle_timeout)}
  end

  defp cancel_idle_timer(%{idle_timer: nil} = state), do: state

  defp cancel_idle_timer(state) do
    Process.cancel_timer(state.idle_timer, async: false, info: false)
    %{state | idle_timer: nil}
  end

  defp cancel_request(%{request_ref: nil}), do: :ok

  defp cancel_request(state) do
    :httpc.cancel_request(state.request_ref, state.httpc_profile)
    :ok
  end

  defp notify_closed(%{close_notified?: true} = state, _reason), do: state

  defp notify_closed(state, reason) do
    send(state.parent, {:modern_http_stream_closed, self(), state.request_id, reason})
    %{state | close_notified?: true}
  end

  @doc false
  @spec validate_message(map(), ExMCP.Types.request_id(), :request | :subscription) ::
          :ok | {:error, atom()}
  def validate_message(message, request_id, stream_kind) when is_map(message) do
    cond do
      final_response?(message) ->
        if Map.get(message, "id") == request_id,
          do: :ok,
          else: {:error, :response_id_mismatch}

      notification?(message) and allowed_notification?(message["method"], stream_kind) ->
        :ok

      true ->
        {:error, :invalid_stream_message}
    end
  end

  def validate_message(_message, _request_id, _stream_kind),
    do: {:error, :invalid_stream_message}

  defp final_response?(%{"jsonrpc" => "2.0", "id" => _id} = message) do
    not Map.has_key?(message, "method") and
      Map.has_key?(message, "result") != Map.has_key?(message, "error")
  end

  defp final_response?(_message), do: false

  defp notification?(%{"jsonrpc" => "2.0", "method" => method} = message)
       when is_binary(method) do
    not Map.has_key?(message, "id") and not Map.has_key?(message, "result") and
      not Map.has_key?(message, "error")
  end

  defp notification?(_message), do: false

  defp allowed_notification?(method, :request),
    do: method in ["notifications/progress", "notifications/message"]

  defp allowed_notification?(method, :subscription) do
    method in [
      "notifications/subscriptions/acknowledged",
      "notifications/tools/list_changed",
      "notifications/prompts/list_changed",
      "notifications/resources/list_changed",
      "notifications/resources/updated",
      "notifications/tasks"
    ]
  end

  defp allowed_notification?(_method, _stream_kind), do: false

  defp to_binary(value) when is_binary(value), do: value
  defp to_binary(value) when is_list(value), do: List.to_string(value)

  defp httpc_profile do
    index = rem(:erlang.unique_integer([:positive, :monotonic]), length(@httpc_profiles))
    Enum.at(@httpc_profiles, index)
  end

  defp ensure_httpc_profile!(profile) do
    case :inets.start(:httpc, [{:profile, profile}]) do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> :ok
    end
  end
end
