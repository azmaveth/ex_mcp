defmodule ExMCP.Transport.Test do
  import Kernel, except: [send: 2]

  @moduledoc """
  In-memory transport for testing purposes.

  This transport allows direct communication between client and server processes
  within the same Elixir VM, without requiring external subprocesses or network
  connections. It's designed specifically for unit and integration testing.

  ## Usage

      # Start a server with test transport
      {:ok, server} = ExMCP.Server.start_link(
        transport: :test,
        handler: MyHandler
      )

      # Start a client connected to that server
      {:ok, client} = ExMCP.Client.start_link(
        transport: :test,
        server: server
      )
  """

  @behaviour ExMCP.Transport

  alias ExMCP.Transport.Error

  # State for server side (when acting as server transport)
  defstruct [:peer_pid, :role]

  @impl true
  def connect(opts) do
    server_pid = Keyword.get(opts, :server)

    if server_pid do
      # Client connecting to server
      if is_pid(server_pid) && Process.alive?(server_pid) do
        state = %__MODULE__{
          peer_pid: server_pid,
          role: :client
        }

        # Tell the server who this client is
        Kernel.send(server_pid, {:test_transport_connect, self()})

        {:ok, state}
      else
        # Server not available or not a valid PID
        Error.connection_error(:server_not_available)
      end
    else
      # Server listening (no server option means this is the server)
      state = %__MODULE__{
        peer_pid: nil,
        role: :server
      }

      {:ok, state}
    end
  end

  @impl true
  def receive_message(%__MODULE__{} = state) do
    receive_message(state, 5000)
  end

  # Client API expects receive_message/2 with timeout
  def receive_message(%__MODULE__{} = state, timeout) do
    case Error.validate_connection(state, &connected?/1) do
      :ok ->
        receive do
          {:transport_message, message} ->
            {:ok, message, state}

          {:transport_error, reason} ->
            Error.transport_error(reason)
        after
          timeout ->
            Error.timeout_error(:receive_timeout)
        end

      error ->
        error
    end
  end

  @impl true
  def close(_state) do
    :ok
  end

  @impl true
  def connected?(%__MODULE__{peer_pid: peer_pid}) do
    peer_pid != nil && Process.alive?(peer_pid)
  end

  @impl true
  def send_message(message, %__MODULE__{peer_pid: peer_pid} = state) when peer_pid != nil do
    case Error.validate_connection(state, &connected?/1) do
      :ok ->
        Kernel.send(peer_pid, {:transport_message, message})
        {:ok, state}

      error ->
        error
    end
  end

  @impl true
  def send_message(_message, %__MODULE__{peer_pid: nil} = state) do
    # Server doesn't have a peer yet, this will happen during initialization
    {:ok, state}
  end

  # Compatibility functions for client expectations
  # Use explicit module reference to avoid conflict with Kernel.send/2
  def send(state, message) do
    case __MODULE__.send_message(message, state) do
      {:ok, new_state} -> {:ok, new_state}
      error -> error
    end
  end

  def recv(state, timeout \\ 5_000) do
    receive_message(state, timeout)
  end

  # Client receiver task expects this function
  def receive(state) do
    receive_message(state, 5_000)
  end
end
