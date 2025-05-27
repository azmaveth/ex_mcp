defmodule ExMCP.Test.MockSSEServer do
  # NOTE: This mock server is not currently used because SSE transport uses httpc
  # which requires a full HTTP server implementation. Keeping for future use.
  @moduledoc """
  Mock SSE server for testing SSE transport functionality.
  """
  use GenServer
  require Logger

  defstruct [:port, :socket, :clients, :test_pid]

  @doc """
  Starts a mock SSE server on a random port.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Gets the URL of the running mock server.
  """
  def get_url do
    GenServer.call(__MODULE__, :get_url)
  end

  @doc """
  Sends an SSE event to all connected clients.
  """
  def send_event(event_type, data) do
    GenServer.cast(__MODULE__, {:send_event, event_type, data})
  end

  @doc """
  Gets the list of connected clients.
  """
  def get_clients do
    GenServer.call(__MODULE__, :get_clients)
  end

  @doc """
  Gets the last request headers received.
  """
  def get_last_headers do
    GenServer.call(__MODULE__, :get_last_headers)
  end

  @doc """
  Stops the mock server.
  """
  def stop do
    GenServer.stop(__MODULE__)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    test_pid = opts[:test_pid] || self()

    # Start listening on a random port
    {:ok, socket} =
      :gen_tcp.listen(0, [
        :binary,
        packet: :raw,
        active: false,
        reuseaddr: true
      ])

    {:ok, port} = :inet.port(socket)

    # Start acceptor
    spawn_link(fn -> accept_loop(socket, self()) end)

    {:ok,
     %__MODULE__{
       port: port,
       socket: socket,
       clients: [],
       test_pid: test_pid
     }}
  end

  @impl true
  def handle_call(:get_url, _from, state) do
    {:reply, "http://localhost:#{state.port}", state}
  end

  @impl true
  def handle_call(:get_clients, _from, state) do
    {:reply, state.clients, state}
  end

  @impl true
  def handle_call(:get_last_headers, _from, state) do
    # Return headers from the most recent client
    case state.clients do
      [{_socket, headers} | _] -> {:reply, headers, state}
      [] -> {:reply, nil, state}
    end
  end

  @impl true
  def handle_cast({:send_event, event_type, data}, state) do
    event = format_sse_event(event_type, data)

    Enum.each(state.clients, fn {socket, _headers} ->
      :gen_tcp.send(socket, event)
    end)

    {:noreply, state}
  end

  @impl true
  def handle_info({:new_client, socket, headers}, state) do
    # Send SSE headers
    response = [
      "HTTP/1.1 200 OK\r\n",
      "Content-Type: text/event-stream\r\n",
      "Cache-Control: no-cache\r\n",
      "Connection: keep-alive\r\n",
      "Access-Control-Allow-Origin: *\r\n",
      "\r\n"
    ]

    :gen_tcp.send(socket, Enum.join(response))

    # Add to clients list
    clients = [{socket, headers} | state.clients]

    # Notify test process
    send(state.test_pid, {:sse_client_connected, socket})

    {:noreply, %{state | clients: clients}}
  end

  @impl true
  def handle_info({:client_disconnected, socket}, state) do
    clients = Enum.reject(state.clients, fn {s, _} -> s == socket end)

    # Notify test process
    send(state.test_pid, {:sse_client_disconnected, socket})

    {:noreply, %{state | clients: clients}}
  end

  @impl true
  def terminate(_reason, state) do
    :gen_tcp.close(state.socket)
    :ok
  end

  # Private functions

  defp accept_loop(socket, server_pid) do
    case :gen_tcp.accept(socket) do
      {:ok, client_socket} ->
        spawn_link(fn -> handle_client(client_socket, server_pid) end)
        accept_loop(socket, server_pid)

      {:error, :closed} ->
        :ok
    end
  end

  defp handle_client(socket, server_pid) do
    # Read HTTP request
    case read_http_request(socket) do
      {:ok, headers} ->
        send(server_pid, {:new_client, socket, headers})
        # Keep the connection alive
        keep_alive_loop(socket, server_pid)

      {:error, _reason} ->
        :gen_tcp.close(socket)
    end
  end

  defp read_http_request(socket) do
    case :gen_tcp.recv(socket, 0, 5000) do
      {:ok, data} ->
        # Parse headers (simplified)
        lines = String.split(data, "\r\n")
        headers = parse_headers(lines)
        {:ok, headers}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp parse_headers(lines) do
    lines
    # Skip request line
    |> Enum.drop(1)
    |> Enum.take_while(&(&1 != ""))
    |> Enum.map(fn line ->
      case String.split(line, ": ", parts: 2) do
        [key, value] -> {key, value}
        _ -> nil
      end
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp keep_alive_loop(socket, server_pid) do
    # Just wait for the socket to close
    case :gen_tcp.recv(socket, 0, :infinity) do
      {:error, :closed} ->
        send(server_pid, {:client_disconnected, socket})

      _ ->
        keep_alive_loop(socket, server_pid)
    end
  end

  defp format_sse_event(event_type, data) do
    encoded = Jason.encode!(data)
    "event: #{event_type}\ndata: #{encoded}\n\n"
  end
end
