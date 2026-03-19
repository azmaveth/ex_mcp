defmodule ExMCP.Transport.SSEClient do
  @moduledoc """
  Server-Sent Events client for the Streamable HTTP transport.

  This internal module implements the SSE portion of the Streamable HTTP
  transport as defined in the MCP specification. It provides robust connection
  handling with keep-alive, reconnection, and retry logic.

  Features:
  - Automatic reconnection with exponential backoff
  - Keep-alive/heartbeat mechanism
  - Proper handling of SSE retry suggestions
  - Connection health monitoring

  Note: This is an internal implementation detail of the Streamable HTTP transport.
  """

  use GenServer
  require Logger

  @initial_retry_delay 1_000
  @max_retry_delay 60_000
  @heartbeat_interval 30_000
  @connection_timeout 30_000

  defstruct [
    :url,
    :headers,
    :ssl_opts,
    :parent,
    :ref,
    :buffer,
    :retry_delay,
    :retry_count,
    :heartbeat_ref,
    :last_event_id,
    :reconnect_timer,
    :connect_timeout,
    :idle_timeout,
    :max_retry_delay,
    :httpc_profile
  ]

  @type t :: %__MODULE__{
          url: String.t(),
          headers: [{String.t(), String.t()}],
          ssl_opts: keyword(),
          parent: pid(),
          ref: reference() | nil,
          buffer: String.t(),
          retry_delay: non_neg_integer(),
          retry_count: non_neg_integer(),
          heartbeat_ref: reference() | nil,
          last_event_id: String.t() | nil,
          reconnect_timer: reference() | nil,
          connect_timeout: non_neg_integer(),
          idle_timeout: non_neg_integer(),
          max_retry_delay: non_neg_integer(),
          httpc_profile: atom() | nil
        }

  # Client API

  @doc """
  Starts an SSE client connected to the given URL.

  Options:
  - `:url` - The SSE endpoint URL (required)
  - `:headers` - Additional HTTP headers
  - `:ssl_opts` - SSL options for HTTPS connections
  - `:parent` - Process to send events to (defaults to caller)
  - `:initial_retry_delay` - Initial reconnection delay in ms (default: #{@initial_retry_delay})
  - `:max_retry_delay` - Maximum reconnection delay in ms (default: #{@max_retry_delay})
  - `:connect_timeout` - Connection timeout in ms (default: #{@connection_timeout})
  - `:idle_timeout` - Idle/heartbeat timeout in ms (default: #{@heartbeat_interval})
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Stops the SSE client gracefully.
  """
  @spec stop(GenServer.server()) :: :ok
  def stop(client) do
    GenServer.stop(client)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    url = Keyword.fetch!(opts, :url)
    headers = Keyword.get(opts, :headers, [])
    ssl_opts = Keyword.get(opts, :ssl_opts, [])
    parent = Keyword.get(opts, :parent, self())
    # Accept configurable timeouts with fallback to defaults
    connect_timeout = Keyword.get(opts, :connect_timeout, @connection_timeout)
    idle_timeout = Keyword.get(opts, :idle_timeout, @heartbeat_interval)

    # Use a separate httpc profile to avoid connection pool conflicts
    # with POST requests on the default profile
    profile = String.to_atom("sse_#{:erlang.unique_integer([:positive])}")
    {:ok, _} = :inets.start(:httpc, [{:profile, profile}])

    # Use server-specified retry delay if provided (from POST SSE response)
    initial_delay = Keyword.get(opts, :initial_retry_delay) || @initial_retry_delay
    max_delay = Keyword.get(opts, :max_retry_delay, @max_retry_delay)

    state = %__MODULE__{
      url: url,
      headers: headers,
      ssl_opts: ssl_opts,
      parent: parent,
      buffer: "",
      retry_delay: initial_delay,
      retry_count: 0,
      connect_timeout: connect_timeout,
      idle_timeout: idle_timeout,
      max_retry_delay: max_delay,
      httpc_profile: profile
    }

    {:ok, state, {:continue, :connect}}
  end

  @impl true
  def handle_continue(:connect, state) do
    case connect_sse(state) do
      {:ok, ref} ->
        # Store the reference but don't send connected message yet
        # We'll send it when we receive :stream_start
        # Don't reset retry_delay here - only reset when stream actually starts
        new_state = %{
          state
          | ref: ref
        }

        {:noreply, new_state}

      {:error, reason} ->
        Logger.warning("SSE connection failed: #{inspect(reason)}")
        schedule_reconnect(state)
    end
  end

  @impl true
  def handle_info({:http, {ref, :stream_start, headers}}, %{ref: ref} = state) do
    # Connection is now established, send notification
    send(state.parent, {:sse_connected, self()})

    # Start heartbeat monitoring using configurable idle timeout
    heartbeat_ref = Process.send_after(self(), :check_heartbeat, state.idle_timeout)

    # Process headers for retry suggestions
    retry_after = get_retry_after(headers)

    # Only override retry_delay if server provides Retry-After header.
    # Preserve current retry_delay (may be set from SSE retry field or initial config).
    new_state =
      if retry_after do
        %{state | retry_delay: retry_after * 1000, heartbeat_ref: heartbeat_ref, retry_count: 0}
      else
        %{state | heartbeat_ref: heartbeat_ref, retry_count: 0}
      end

    {:noreply, new_state}
  end

  def handle_info({:http, {ref, :stream, chunk}}, %{ref: ref} = state) do
    # Reset heartbeat timer on data received
    if state.heartbeat_ref do
      Process.cancel_timer(state.heartbeat_ref)
    end

    heartbeat_ref = Process.send_after(self(), :check_heartbeat, state.idle_timeout)

    # Process the chunk
    buffer = state.buffer <> chunk
    {events, remaining} = parse_events(buffer)

    # Debug logging
    if length(events) > 0 do
      Logger.debug("SSE Client parsed #{length(events)} events from chunk")
    end

    # Process events — extract retry field and send data events to parent
    new_state =
      Enum.reduce(events, %{state | buffer: remaining, heartbeat_ref: heartbeat_ref}, fn event,
                                                                                         acc ->
        # Check for retry field (SSE spec: sets reconnection delay)
        acc =
          case Map.get(event, "retry") do
            nil ->
              acc

            retry_str ->
              case Integer.parse(retry_str) do
                {ms, _} when ms >= 0 ->
                  Logger.debug("SSE retry field set to #{ms}ms")
                  %{acc | retry_delay: ms}

                _ ->
                  acc
              end
          end

        # Track last event ID for resumption
        acc =
          case Map.get(event, "id") do
            nil -> acc
            id -> %{acc | last_event_id: id}
          end

        # Forward data events to parent
        if Map.has_key?(event, "data") do
          process_event(event, acc)
        end

        acc
      end)

    {:noreply, new_state}
  end

  def handle_info({:http, {ref, :stream_end, _headers}}, %{ref: ref} = state) do
    Logger.info("SSE stream ended, reconnecting...")
    send(state.parent, {:sse_closed, self()})
    schedule_reconnect(state)
  end

  # Non-streaming HTTP response (e.g., 405 Method Not Allowed)
  def handle_info({:http, {ref, {{_, 405, _}, _headers, _body}}}, %{ref: ref} = state) do
    Logger.info("SSE: server returned 405 — SSE not supported, disabling")
    send(state.parent, {:sse_not_supported, self()})
    {:noreply, %{state | ref: nil}}
  end

  def handle_info({:http, {ref, {{_, status, _}, _headers, _body}}}, %{ref: ref} = state)
      when status >= 400 do
    Logger.warning("SSE: server returned HTTP #{status}")
    send(state.parent, {:sse_error, self(), {:http_error, status}})
    schedule_reconnect(state)
  end

  def handle_info({:http, {ref, {:error, reason}}}, %{ref: ref} = state) do
    Logger.error("SSE error: #{inspect(reason)}")
    send(state.parent, {:sse_error, self(), reason})
    schedule_reconnect(state)
  end

  def handle_info(:check_heartbeat, state) do
    # No data received within heartbeat interval, assume connection is dead
    Logger.warning("SSE heartbeat timeout, reconnecting...")

    if state.ref do
      :httpc.cancel_request(state.ref, state.httpc_profile)
    end

    schedule_reconnect(state)
  end

  def handle_info(:reconnect, state) do
    {:noreply, state, {:continue, :connect}}
  end

  # Force reconnect from parent (e.g., POST SSE response closed without result).
  # Delay briefly to let pending :stream messages drain first so we have
  # the latest last_event_id and retry_delay before reconnecting.
  def handle_info(:force_reconnect, state) do
    Logger.info("SSE forced reconnect requested, draining pending messages")
    Process.send_after(self(), :do_force_reconnect, 100)
    {:noreply, state}
  end

  def handle_info(:do_force_reconnect, state) do
    if state.ref do
      :httpc.cancel_request(state.ref, state.httpc_profile)
    end

    if state.heartbeat_ref do
      Process.cancel_timer(state.heartbeat_ref)
    end

    schedule_reconnect(%{state | ref: nil, heartbeat_ref: nil})
  end

  # Update retry delay from parent (e.g., from POST SSE retry field)
  def handle_info({:update_retry_delay, delay}, state) when is_integer(delay) do
    {:noreply, %{state | retry_delay: delay}}
  end

  # Update last event ID from parent (e.g., from SSE event received before force_reconnect)
  def handle_info({:update_last_event_id, id}, state) when is_binary(id) do
    {:noreply, %{state | last_event_id: id}}
  end

  def handle_info({:change_parent, new_parent}, state) do
    {:noreply, %{state | parent: new_parent}}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, state) do
    if state.ref do
      :httpc.cancel_request(state.ref, state.httpc_profile)
    end

    if state.heartbeat_ref do
      Process.cancel_timer(state.heartbeat_ref)
    end

    if state.reconnect_timer do
      Process.cancel_timer(state.reconnect_timer)
    end

    # Stop the dedicated httpc profile
    if state.httpc_profile do
      :inets.stop(:httpc, state.httpc_profile)
    end

    :ok
  end

  # Private functions

  defp connect_sse(state) do
    headers = build_headers(state)

    request = {
      String.to_charlist(state.url),
      Enum.map(headers, fn {k, v} ->
        {String.to_charlist(k), String.to_charlist(v)}
      end)
    }

    # For SSE connections, :timeout is the total request time — we use infinity
    # since SSE connections are long-lived. The SSEClient's heartbeat mechanism
    # handles idle detection. :connect_timeout limits just the TCP connection.
    http_opts =
      case URI.parse(state.url).scheme do
        "https" ->
          [
            {:ssl, state.ssl_opts},
            {:timeout, :infinity},
            {:connect_timeout, state.connect_timeout}
          ]

        _ ->
          [{:timeout, :infinity}, {:connect_timeout, state.connect_timeout}]
      end

    :httpc.request(
      :get,
      request,
      http_opts,
      [{:sync, false}, {:stream, :self}],
      state.httpc_profile
    )
  end

  defp build_headers(state) do
    base_headers = [
      {"accept", "text/event-stream"},
      {"cache-control", "no-cache"},
      {"connection", "keep-alive"}
    ]

    # Add Last-Event-ID if we have one (for resumption)
    headers_with_id =
      if state.last_event_id do
        [{"last-event-id", state.last_event_id} | base_headers]
      else
        base_headers
      end

    # Merge with user-provided headers
    Enum.uniq_by(state.headers ++ headers_with_id, fn {k, _} -> k end)
  end

  defp parse_events(buffer) do
    lines = String.split(buffer, "\n")
    parse_events(lines, [], %{}, [])
  end

  defp parse_events([], events, current_event, acc) do
    # Return accumulated events and any incomplete data
    buffer =
      if map_size(current_event) > 0 do
        # Reconstruct incomplete event
        current_event
        |> Enum.map_join("\n", fn {k, v} -> "#{k}: #{v}" end)
      else
        Enum.join(acc, "\n")
      end

    {Enum.reverse(events), buffer}
  end

  defp parse_events(["" | rest], events, current_event, _acc) when map_size(current_event) > 0 do
    # Empty line marks end of event
    parse_events(rest, [current_event | events], %{}, [])
  end

  defp parse_events([line | rest], events, current_event, acc) do
    case parse_field(line) do
      {:ok, key, value} ->
        updated_event =
          Map.update(current_event, key, value, fn existing ->
            existing <> "\n" <> value
          end)

        parse_events(rest, events, updated_event, [])

      :ignore ->
        parse_events(rest, events, current_event, [])

      :incomplete ->
        # Line doesn't contain a complete field, accumulate it
        parse_events(rest, events, current_event, [line | acc])
    end
  end

  defp parse_field(":" <> _comment), do: :ignore
  defp parse_field(""), do: :ignore

  defp parse_field(line) do
    case String.split(line, ":", parts: 2) do
      [field, value] ->
        # Remove leading space from value if present
        value = String.trim_leading(value)
        {:ok, field, value}

      _ ->
        :incomplete
    end
  end

  defp process_event(event, state) do
    # Extract event data
    data = Map.get(event, "data", "")
    event_type = Map.get(event, "event", "message")
    id = Map.get(event, "id")

    Logger.debug(
      "SSE Client processing event: type=#{event_type}, id=#{inspect(id)}, data_size=#{byte_size(data)}"
    )

    # Update last event ID if provided
    if id do
      GenServer.cast(self(), {:update_last_id, id})
    end

    # Send to parent
    send(
      state.parent,
      {:sse_event, self(),
       %{
         type: event_type,
         data: data,
         id: id
       }}
    )
  end

  defp schedule_reconnect(state) do
    # Cancel existing timers
    if state.heartbeat_ref do
      Process.cancel_timer(state.heartbeat_ref)
    end

    if state.ref do
      :httpc.cancel_request(state.ref)
    end

    # Use server-specified retry delay, or exponential backoff
    delay = state.retry_delay
    # Only apply backoff if using the initial default (not server-specified)
    new_delay =
      if delay == @initial_retry_delay do
        min(delay * 2, state.max_retry_delay)
      else
        delay
      end

    # Add small buffer to ensure we never reconnect early per spec requirement.
    # The MCP spec says client MUST wait at least the retry time.
    buffered_delay = delay + 50
    Logger.info("Scheduling SSE reconnection in #{buffered_delay}ms (retry: #{delay}ms)")

    reconnect_timer = Process.send_after(self(), :reconnect, buffered_delay)

    new_state = %{
      state
      | ref: nil,
        heartbeat_ref: nil,
        reconnect_timer: reconnect_timer,
        retry_delay: new_delay,
        retry_count: state.retry_count + 1
    }

    {:noreply, new_state}
  end

  defp get_retry_after(headers) do
    headers
    |> Enum.find(fn {name, _} ->
      String.downcase(to_string(name)) == "retry-after"
    end)
    |> case do
      {_, value} ->
        case Integer.parse(to_string(value)) do
          {seconds, _} -> seconds
          _ -> nil
        end

      _ ->
        nil
    end
  end

  @impl true
  def handle_cast({:update_last_id, id}, state) do
    {:noreply, %{state | last_event_id: id}}
  end
end
