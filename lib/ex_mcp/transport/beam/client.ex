defmodule ExMCP.Transport.Beam.Client do
  @moduledoc """
  BEAM transport client with ranch integration.

  This module provides a high-level client interface for connecting to
  BEAM transport servers using the new architecture with Connection processes.

  ## Features

  - Automatic connection management and reconnection
  - Registry-based request correlation for high performance
  - ETF serialization for maximum BEAM efficiency
  - Built-in authentication and security
  - Connection pooling and load balancing

  ## Usage

      # Connect to a BEAM transport server
      {:ok, client} = ExMCP.Transport.Beam.Client.connect([
        host: "localhost",
        port: 9999,
        auth_config: %{type: :bearer, token: "secret"}
      ])
      
      # Send a request with automatic correlation
      {:ok, response} = ExMCP.Transport.Beam.Client.call(client, %{
        "method" => "ping",
        "params" => %{}
      })
      
      # Send a notification (fire-and-forget)
      :ok = ExMCP.Transport.Beam.Client.notify(client, %{
        "method" => "log",
        "params" => %{"message" => "Hello"}
      })
  """

  use GenServer
  require Logger

  alias ExMCP.Transport.Beam.{Connection, Observability, Batch}

  @default_host ~c"localhost"
  @default_port 9999
  @default_timeout 30_000
  @reconnect_interval 5_000

  defstruct [
    :connection_pid,
    :host,
    :port,
    :auth_config,
    :reconnect_timer,
    :options,
    :batch_manager,
    :batching_config,
    connected: false,
    reconnect_attempts: 0,
    batching_enabled: false
  ]

  @type t :: %__MODULE__{}
  @type client_opts :: [
          host: charlist() | binary(),
          port: non_neg_integer(),
          auth_config: map() | nil,
          timeout: non_neg_integer(),
          auto_reconnect: boolean(),
          batching: map() | nil,
          zero_copy: boolean()
        ]

  # Client API

  @doc """
  Connects to a BEAM transport server.
  """
  @spec connect(client_opts()) :: {:ok, pid()} | {:error, term()}
  def connect(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Sends a request and waits for a response.
  """
  @spec call(pid(), map(), keyword()) :: {:ok, term()} | {:error, term()}
  def call(client, message, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    GenServer.call(client, {:call, message, opts}, timeout + 1000)
  end

  @doc """
  Sends a notification (fire-and-forget).
  """
  @spec notify(pid(), map()) :: :ok | {:error, term()}
  def notify(client, message) do
    GenServer.call(client, {:notify, message})
  end

  @doc """
  Gets the connection status.
  """
  @spec status(pid()) :: :connected | :connecting | :disconnected
  def status(client) do
    GenServer.call(client, :status)
  end

  @doc """
  Gets connection statistics.
  """
  @spec get_stats(pid()) :: map()
  def get_stats(client) do
    GenServer.call(client, :get_stats)
  end

  @doc """
  Closes the connection.
  """
  @spec close(pid()) :: :ok
  def close(client) do
    GenServer.call(client, :close)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    host = normalize_host(Keyword.get(opts, :host, @default_host))
    port = Keyword.get(opts, :port, @default_port)
    auth_config = Keyword.get(opts, :auth_config)
    auto_reconnect = Keyword.get(opts, :auto_reconnect, true)
    batching_config = Keyword.get(opts, :batching, %{enabled: false})

    # Check if batching is enabled
    batching_enabled =
      case batching_config do
        %{enabled: true} -> true
        %{"enabled" => true} -> true
        _ -> false
      end

    state = %__MODULE__{
      host: host,
      port: port,
      auth_config: auth_config,
      batching_config: batching_config,
      batching_enabled: batching_enabled,
      options: %{
        auto_reconnect: auto_reconnect,
        timeout: Keyword.get(opts, :timeout, @default_timeout)
      }
    }

    # Start connection immediately
    send(self(), :connect)

    {:ok, state}
  end

  @impl true
  def handle_call({:call, message, opts}, _from, state) do
    if state.connected and state.connection_pid do
      timeout = Keyword.get(opts, :timeout, @default_timeout)

      # Record start time for latency measurement
      start_time = System.monotonic_time(:millisecond)

      # Send directly through connection for now
      # Batching optimization will be handled at the transport layer
      result = Connection.send_message(state.connection_pid, message, timeout: timeout)

      case result do
        {:ok, response} ->
          # Calculate and record latency
          latency_ms = System.monotonic_time(:millisecond) - start_time
          Observability.record_metric(:successful_requests, 1)
          Observability.record_metric(:average_latency, latency_ms)
          {:reply, {:ok, response}, state}

        {:error, reason} ->
          Observability.record_error(:request_failed)
          {:reply, {:error, reason}, state}
      end
    else
      Observability.record_error(:not_connected)
      {:reply, {:error, :not_connected}, state}
    end
  end

  def handle_call({:notify, message}, _from, state) do
    if state.connected and state.connection_pid do
      case Connection.send_notification(state.connection_pid, message) do
        :ok ->
          {:reply, :ok, state}

        {:error, reason} ->
          {:reply, {:error, reason}, state}
      end
    else
      {:reply, {:error, :not_connected}, state}
    end
  end

  def handle_call(:status, _from, state) do
    status =
      cond do
        state.connected -> :connected
        state.connection_pid -> :connecting
        true -> :disconnected
      end

    {:reply, status, state}
  end

  def handle_call(:get_stats, _from, state) do
    stats =
      if state.connected and state.connection_pid do
        stats = Connection.get_stats(state.connection_pid)

        Map.merge(stats, %{
          client_connected: true,
          reconnect_attempts: state.reconnect_attempts
        })
      else
        %{client_connected: false, reconnect_attempts: state.reconnect_attempts}
      end

    {:reply, stats, state}
  end

  def handle_call(:close, _from, state) do
    new_state = disconnect(state)
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_info(:connect, state) do
    Logger.info(
      "BEAM client attempting to connect to #{state.host}:#{state.port} (attempt #{state.reconnect_attempts + 1})"
    )

    case establish_connection(state) do
      {:ok, connection_pid} ->
        Logger.info(
          "BEAM client established connection to #{state.host}:#{state.port} (pid: #{inspect(connection_pid)})"
        )

        new_state = %{
          state
          | connection_pid: connection_pid,
            # Will be set true when connection_ready is received
            connected: false,
            reconnect_attempts: 0
        }

        # Monitor the connection process
        monitor_ref = Process.monitor(connection_pid)

        Logger.debug(
          "BEAM client monitoring connection #{inspect(connection_pid)} with ref #{inspect(monitor_ref)}"
        )

        {:noreply, new_state}

      {:error, reason} ->
        Logger.warning("Failed to connect to #{state.host}:#{state.port}: #{inspect(reason)}")

        if state.options.auto_reconnect do
          # Schedule reconnection
          timer = Process.send_after(self(), :connect, @reconnect_interval)

          new_state = %{
            state
            | reconnect_timer: timer,
              reconnect_attempts: state.reconnect_attempts + 1
          }

          {:noreply, new_state}
        else
          {:noreply, state}
        end
    end
  end

  def handle_info({:connection_ready, connection_pid}, state)
      when connection_pid == state.connection_pid do
    Logger.debug(
      "BEAM client connection ready from #{inspect(connection_pid)} (matches current: #{connection_pid == state.connection_pid})"
    )

    # Start batch manager if batching is enabled
    new_state =
      if state.batching_enabled do
        case start_batch_manager(connection_pid, state.batching_config) do
          {:ok, batch_manager} ->
            Logger.debug("Started batch manager: #{inspect(batch_manager)}")
            %{state | batch_manager: batch_manager, connected: true}

          {:error, reason} ->
            Logger.warning("Failed to start batch manager: #{inspect(reason)}")
            %{state | connected: true}
        end
      else
        %{state | connected: true}
      end

    {:noreply, new_state}
  end

  def handle_info({:connection_ready, connection_pid}, state) do
    Logger.warning(
      "BEAM client received connection_ready from unexpected PID #{inspect(connection_pid)} (current: #{inspect(state.connection_pid)})"
    )

    {:noreply, state}
  end

  def handle_info({:connection_closed, connection_pid}, state) do
    Logger.warning(
      "BEAM client received connection_closed from #{inspect(connection_pid)} (current: #{inspect(state.connection_pid)})"
    )

    # Only handle disconnection if this is from our current connection
    if connection_pid == state.connection_pid do
      Logger.warning("BEAM client connection closed - handling disconnection")
      new_state = handle_disconnection(state)
      {:noreply, new_state}
    else
      # This is from an old connection, ignore it
      Logger.debug("Ignoring connection_closed from old connection #{inspect(connection_pid)}")
      {:noreply, state}
    end
  end

  def handle_info({:DOWN, _ref, :process, pid, reason}, state) when pid == state.connection_pid do
    Logger.warning(
      "BEAM client connection process #{inspect(pid)} died: #{inspect(reason)} (current connection: #{inspect(state.connection_pid)})"
    )

    new_state = handle_disconnection(state)
    {:noreply, new_state}
  end

  def handle_info({:DOWN, _ref, :process, pid, reason}, state) do
    Logger.debug(
      "BEAM client received DOWN for unrelated process #{inspect(pid)}: #{inspect(reason)}"
    )

    {:noreply, state}
  end

  def handle_info(msg, state) do
    Logger.debug("Unexpected message in BEAM client: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def terminate(reason, state) do
    Logger.debug("BEAM client terminating: #{inspect(reason)}")
    disconnect(state)
    :ok
  end

  # Private helper functions

  defp establish_connection(state) do
    peer_info = %{
      host: state.host,
      port: state.port
    }

    connection_opts = [
      peer_info: peer_info,
      auth_config: state.auth_config,
      format: :etf,
      owner: self()
    ]

    Connection.start_link(connection_opts)
  end

  defp handle_disconnection(state) do
    Logger.info("Handling disconnection - auto_reconnect: #{state.options.auto_reconnect}")
    new_state = %{state | connected: false, connection_pid: nil}

    if state.options.auto_reconnect do
      # Schedule reconnection
      Logger.info(
        "Scheduling reconnection in #{@reconnect_interval}ms (attempt #{state.reconnect_attempts + 1})"
      )

      timer = Process.send_after(self(), :connect, @reconnect_interval)
      %{new_state | reconnect_timer: timer, reconnect_attempts: state.reconnect_attempts + 1}
    else
      Logger.info("Auto-reconnect disabled, not scheduling reconnection")
      new_state
    end
  end

  defp disconnect(state) do
    # Cancel reconnect timer if active
    if state.reconnect_timer do
      Process.cancel_timer(state.reconnect_timer)
    end

    # Stop batch manager if active
    if state.batch_manager && Process.alive?(state.batch_manager) do
      GenServer.stop(state.batch_manager, :normal)
    end

    # Close connection if active
    if state.connection_pid && Process.alive?(state.connection_pid) do
      Connection.close(state.connection_pid)
    end

    %{state | connected: false, connection_pid: nil, reconnect_timer: nil, batch_manager: nil}
  end

  defp normalize_host(host) when is_binary(host) do
    String.to_charlist(host)
  end

  defp normalize_host(host) when is_list(host) do
    host
  end

  defp normalize_host(host) do
    host
  end

  defp start_batch_manager(connection_pid, batching_config) do
    # Handle both atom and string keys
    max_batch_size =
      Map.get(batching_config, :max_batch_size) ||
        Map.get(batching_config, "max_batch_size", 50)

    max_batch_bytes =
      Map.get(batching_config, :max_batch_bytes) ||
        Map.get(batching_config, "max_batch_bytes", 32_768)

    batch_timeout =
      Map.get(batching_config, :batch_timeout) ||
        Map.get(batching_config, "batch_timeout", 10)

    batch_opts = [
      connection: connection_pid,
      max_batch_size: max_batch_size,
      max_batch_bytes: max_batch_bytes,
      batch_timeout: batch_timeout
    ]

    Batch.start_link(batch_opts)
  end
end
