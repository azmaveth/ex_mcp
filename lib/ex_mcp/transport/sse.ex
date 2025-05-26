defmodule ExMCP.Transport.SSE do
  @moduledoc """
  Server-Sent Events (SSE) transport for MCP.

  This transport uses HTTP SSE for server-to-client messages
  and HTTP POST for client-to-server messages.
  """

  @behaviour ExMCP.Transport

  defstruct [
    :base_url,
    :headers,
    :http_client,
    :sse_pid,
    :endpoint
  ]

  @type t :: %__MODULE__{
          base_url: String.t(),
          headers: [{String.t(), String.t()}],
          http_client: module(),
          sse_pid: pid() | nil,
          endpoint: String.t()
        }

  @default_endpoint "/mcp/v1"

  @impl true
  def connect(config) do
    base_url = Keyword.fetch!(config, :url)
    headers = Keyword.get(config, :headers, [])
    http_client = Keyword.get(config, :http_client, :httpc)
    endpoint = Keyword.get(config, :endpoint, @default_endpoint)

    state = %__MODULE__{
      base_url: base_url,
      headers: headers,
      http_client: http_client,
      endpoint: endpoint
    }

    # Start SSE connection
    case start_sse(state) do
      {:ok, sse_pid} ->
        {:ok, %{state | sse_pid: sse_pid}}

      error ->
        error
    end
  end

  @impl true
  def send_message(message, %__MODULE__{} = state) do
    url = build_url(state, "/messages")
    headers = [{"content-type", "application/json"} | state.headers]

    case Jason.encode(message) do
      {:ok, body} ->
        request = {
          String.to_charlist(url),
          Enum.map(headers, fn {k, v} -> {String.to_charlist(k), String.to_charlist(v)} end),
          String.to_charlist("application/json"),
          body
        }

        case :httpc.request(:post, request, [], []) do
          {:ok, {{_, 200, _}, _, _response_body}} ->
            :ok

          {:ok, {{_, status, _}, _, body}} ->
            {:error, {:http_error, status, body}}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, {:encoding_error, reason}}
    end
  end

  @impl true
  def receive_message(%__MODULE__{sse_pid: sse_pid} = state) do
    receive do
      {:sse_event, ^sse_pid, event} ->
        case parse_sse_event(event) do
          {:ok, message} ->
            {:ok, message, state}

          error ->
            error
        end

      {:sse_error, ^sse_pid, reason} ->
        {:error, {:sse_error, reason}}

      {:sse_closed, ^sse_pid} ->
        {:error, :connection_closed}
    end
  end

  @impl true
  def close(%__MODULE__{sse_pid: sse_pid}) when is_pid(sse_pid) do
    Process.exit(sse_pid, :normal)
    :ok
  end

  def close(%__MODULE__{}), do: :ok

  # Private functions

  defp start_sse(state) do
    parent = self()
    url = build_url(state, "/sse")

    sse_pid =
      spawn_link(fn ->
        sse_loop(parent, url, state.headers)
      end)

    # Wait for connection confirmation
    receive do
      {:sse_connected, ^sse_pid} ->
        {:ok, sse_pid}

      {:sse_error, ^sse_pid, reason} ->
        {:error, reason}
    after
      5000 ->
        Process.exit(sse_pid, :kill)
        {:error, :connection_timeout}
    end
  end

  defp sse_loop(parent, url, headers) do
    # This is a simplified SSE client - in production you'd want
    # to use a proper SSE client library
    case connect_sse(url, headers) do
      {:ok, ref} ->
        send(parent, {:sse_connected, self()})
        receive_sse_loop(parent, ref, "")

      {:error, reason} ->
        send(parent, {:sse_error, self(), reason})
    end
  end

  defp connect_sse(url, headers) do
    headers = [
      {"accept", "text/event-stream"},
      {"cache-control", "no-cache"} | headers
    ]

    request = {
      String.to_charlist(url),
      Enum.map(headers, fn {k, v} -> {String.to_charlist(k), String.to_charlist(v)} end)
    }

    case :httpc.request(:get, request, [], [{:sync, false}, {:stream, :self}]) do
      {:ok, ref} ->
        {:ok, ref}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp receive_sse_loop(parent, ref, buffer) do
    receive do
      {:http, {^ref, :stream_start, _headers}} ->
        receive_sse_loop(parent, ref, buffer)

      {:http, {^ref, :stream, chunk}} ->
        new_buffer = buffer <> chunk
        {events, remaining} = parse_sse_chunk(new_buffer)

        Enum.each(events, fn event ->
          send(parent, {:sse_event, self(), event})
        end)

        receive_sse_loop(parent, ref, remaining)

      {:http, {^ref, :stream_end, _headers}} ->
        send(parent, {:sse_closed, self()})

      {:http, {^ref, {:error, reason}}} ->
        send(parent, {:sse_error, self(), reason})
    end
  end

  defp parse_sse_chunk(buffer) do
    # Split but keep the last incomplete line
    case String.split(buffer, "\n") do
      lines when length(lines) > 1 ->
        {complete_lines, [last_line]} = Enum.split(lines, -1)

        # Check if the buffer ended with a newline
        if String.ends_with?(buffer, "\n") do
          # Last line is complete
          {events, _} = parse_sse_lines(lines, %{}, [])
          {Enum.reverse(events), ""}
        else
          # Last line is incomplete
          {events, _} = parse_sse_lines(complete_lines, %{}, [])
          {Enum.reverse(events), last_line}
        end

      [single_line] ->
        # No complete lines yet
        {[], single_line}
    end
  end

  defp parse_sse_lines([line | rest], current_event, events) do
    case line do
      "" ->
        # Empty line signals end of event
        if map_size(current_event) > 0 do
          parse_sse_lines(rest, %{}, [current_event | events])
        else
          parse_sse_lines(rest, current_event, events)
        end

      ":" <> _comment ->
        # Comment, ignore
        parse_sse_lines(rest, current_event, events)

      _ ->
        case String.split(line, ":", parts: 2) do
          [field, value] ->
            value = String.trim_leading(value)
            updated_event = Map.put(current_event, field, value)
            parse_sse_lines(rest, updated_event, events)

          _ ->
            # Invalid line, skip
            parse_sse_lines(rest, current_event, events)
        end
    end
  end

  defp parse_sse_lines([], _current_event, events) do
    # Return events and empty remaining buffer
    {events, ""}
  end

  defp parse_sse_event(%{"data" => data}) do
    case Jason.decode(data) do
      {:ok, message} ->
        {:ok, Jason.encode!(message)}

      {:error, reason} ->
        {:error, {:parse_error, reason}}
    end
  end

  defp parse_sse_event(_), do: {:error, :invalid_sse_event}

  defp build_url(%__MODULE__{base_url: base_url, endpoint: endpoint}, path) do
    base_url
    |> URI.parse()
    |> Map.put(:path, endpoint <> path)
    |> URI.to_string()
  end
end
