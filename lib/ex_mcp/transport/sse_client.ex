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
    :idle_timeout
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
          idle_timeout: non_neg_integer()
        }

  # Client API

  @doc """
  Starts an SSE client connected to the given URL.

  Options:
  - `:url` - The SSE endpoint URL (required)
  - `:headers` - Additional HTTP headers
  - `:ssl_opts` - SSL options for HTTPS connections
  - `:parent` - Process to send events to (defaults to caller)
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

    state = %__MODULE__{
      url: url,
      headers: headers,
      ssl_opts: ssl_opts,
      parent: parent,
      buffer: "",
      retry_delay: @initial_retry_delay,
      retry_count: 0,
      connect_timeout: connect_timeout,
      idle_timeout: idle_timeout
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

    new_state =
      if retry_after do
        %{state | retry_delay: retry_after * 1000, heartbeat_ref: heartbeat_ref, retry_count: 0}
      else
        %{state | retry_delay: @initial_retry_delay, heartbeat_ref: heartbeat_ref, retry_count: 0}
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

    # Send events to parent
    Enum.each(events, fn event ->
      process_event(event, state)
    end)

    {:noreply, %{state | buffer: remaining, heartbeat_ref: heartbeat_ref}}
  end

  def handle_info({:http, {ref, :stream_end, _headers}}, %{ref: ref} = state) do
    Logger.info("SSE stream ended, reconnecting...")
    send(state.parent, {:sse_closed, self()})
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
      :httpc.cancel_request(state.ref)
    end

    schedule_reconnect(state)
  end

  def handle_info(:reconnect, state) do
    {:noreply, state, {:continue, :connect}}
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
      :httpc.cancel_request(state.ref)
    end

    if state.heartbeat_ref do
      Process.cancel_timer(state.heartbeat_ref)
    end

    if state.reconnect_timer do
      Process.cancel_timer(state.reconnect_timer)
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

    http_opts =
      case URI.parse(state.url).scheme do
        "https" -> [{:ssl, state.ssl_opts} | [{:timeout, state.connect_timeout}]]
        _ -> [{:timeout, state.connect_timeout}]
      end

    :httpc.request(:get, request, http_opts, [{:sync, false}, {:stream, :self}])
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

    # Calculate delay with exponential backoff
    delay = state.retry_delay
    new_delay = min(delay * 2, @max_retry_delay)

    Logger.info("Scheduling SSE reconnection in #{delay}ms")

    reconnect_timer = Process.send_after(self(), :reconnect, delay)

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
