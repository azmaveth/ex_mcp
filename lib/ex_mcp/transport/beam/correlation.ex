defmodule ExMCP.Transport.Beam.Correlation do
  @moduledoc """
  Request-response correlation using Registry for efficient lookups.

  Provides automatic cleanup when processes die and supports distributed scenarios.
  Uses Registry's built-in process linking for automatic cleanup.

  ## Features
  - Automatic cleanup when processes terminate
  - Efficient O(1) lookup performance
  - Distributed operation support
  - Timeout handling with automatic cleanup
  - Request tracking and metrics

  ## Usage

      # Register a pending request
      correlation_id = Correlation.register_request(self(), %{timeout: 5000})

      # Send request with correlation ID
      send_request(message, correlation_id)

      # Wait for response
      case Correlation.wait_for_response(correlation_id, 5000) do
        {:ok, response} -> handle_response(response)
        {:error, :timeout} -> handle_timeout()
      end
  """

  use GenServer
  require Logger

  @registry_name ExMCP.RequestRegistry
  @correlation_timeout_table :correlation_timeouts

  @type correlation_id :: reference()
  @type request_info :: %{
          requester_pid: pid(),
          started_at: integer(),
          timeout: non_neg_integer(),
          metadata: map()
        }

  defstruct [:timeout_table, stats: %{active_requests: 0, total_requests: 0, timeouts: 0}]

  @doc """
  Starts the correlation manager.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Registers a new request and returns a correlation ID.

  ## Examples

      iex> correlation_id = Correlation.register_request(self())
      #Reference<0.123.456.789>

      iex> correlation_id = Correlation.register_request(self(), %{timeout: 10000})
      #Reference<0.123.456.790>
  """
  @spec register_request(pid(), map()) :: correlation_id()
  def register_request(requester_pid, metadata \\ %{}) do
    correlation_id = make_ref()
    timeout = Map.get(metadata, :timeout, 30_000)

    request_info = %{
      requester_pid: requester_pid,
      started_at: System.monotonic_time(:millisecond),
      timeout: timeout,
      metadata: metadata
    }

    # Register in Registry for efficient lookup
    case Registry.register(@registry_name, correlation_id, request_info) do
      {:ok, _} ->
        # Set up timeout if specified
        if timeout > 0 do
          timer_ref = Process.send_after(self(), {:correlation_timeout, correlation_id}, timeout)
          :ets.insert(@correlation_timeout_table, {correlation_id, timer_ref})
        end

        GenServer.cast(__MODULE__, {:request_registered, correlation_id})
        correlation_id

      {:error, {:already_registered, _}} ->
        # Extremely rare collision, generate new ID
        register_request(requester_pid, metadata)
    end
  end

  @doc """
  Sends a response to a pending request.

  ## Examples

      iex> Correlation.send_response(correlation_id, {:ok, result})
      :ok

      iex> Correlation.send_response(invalid_id, {:error, reason})
      {:error, :not_found}
  """
  @spec send_response(correlation_id(), term()) :: :ok | {:error, :not_found}
  def send_response(correlation_id, response) do
    case Registry.lookup(@registry_name, correlation_id) do
      [{requester_pid, request_info}] ->
        # Send response to requester
        send(requester_pid, {:mcp_response, correlation_id, response})

        # Clean up the registration
        Registry.unregister(@registry_name, correlation_id)
        cleanup_timeout(correlation_id)

        # Update stats
        GenServer.cast(__MODULE__, {:response_sent, correlation_id, request_info})
        :ok

      [] ->
        {:error, :not_found}
    end
  end

  @doc """
  Waits for a response to a specific correlation ID.

  ## Examples

      iex> Correlation.wait_for_response(correlation_id, 5000)
      {:ok, %{result: "success"}}

      iex> Correlation.wait_for_response(correlation_id, 1000)
      {:error, :timeout}
  """
  @spec wait_for_response(correlation_id(), non_neg_integer()) ::
          {:ok, term()} | {:error, :timeout | :not_found}
  def wait_for_response(correlation_id, timeout \\ 30_000) do
    receive do
      {:mcp_response, ^correlation_id, response} ->
        {:ok, response}
    after
      timeout ->
        # Clean up on timeout
        Registry.unregister(@registry_name, correlation_id)
        cleanup_timeout(correlation_id)
        GenServer.cast(__MODULE__, {:request_timeout, correlation_id})
        {:error, :timeout}
    end
  end

  @doc """
  Cancels a pending request.

  ## Examples

      iex> Correlation.cancel_request(correlation_id)
      :ok

      iex> Correlation.cancel_request(invalid_id)
      {:error, :not_found}
  """
  @spec cancel_request(correlation_id()) :: :ok | {:error, :not_found}
  def cancel_request(correlation_id) do
    # Check if request exists before attempting to unregister
    case Registry.lookup(@registry_name, correlation_id) do
      [] ->
        {:error, :not_found}

      [_] ->
        Registry.unregister(@registry_name, correlation_id)
        cleanup_timeout(correlation_id)
        GenServer.cast(__MODULE__, {:request_cancelled, correlation_id})
        :ok
    end
  end

  @doc """
  Lists all active request correlation IDs.
  """
  @spec list_active_requests() :: [correlation_id()]
  def list_active_requests do
    Registry.select(@registry_name, [{{:"$1", :_, :_}, [], [:"$1"]}])
  end

  @doc """
  Gets information about a specific request.
  """
  @spec get_request_info(correlation_id()) :: {:ok, request_info()} | {:error, :not_found}
  def get_request_info(correlation_id) do
    case Registry.lookup(@registry_name, correlation_id) do
      [{_pid, request_info}] -> {:ok, request_info}
      [] -> {:error, :not_found}
    end
  end

  @doc """
  Gets correlation statistics.
  """
  @spec get_stats() :: map()
  def get_stats do
    GenServer.call(__MODULE__, :get_stats)
  end

  # GenServer callbacks

  @impl true
  def init(_opts) do
    # Create timeout tracking table
    :ets.new(@correlation_timeout_table, [:named_table, :set, :public])

    # Start the Registry if it doesn't exist
    case Registry.start_link(keys: :unique, name: @registry_name) do
      {:ok, _} -> :ok
      {:error, {:already_started, _}} -> :ok
    end

    {:ok, %__MODULE__{timeout_table: @correlation_timeout_table}}
  end

  @impl true
  def handle_call(:get_stats, _from, state) do
    active_count = Registry.count(@registry_name)
    updated_stats = %{state.stats | active_requests: active_count}

    {:reply, updated_stats, %{state | stats: updated_stats}}
  end

  @impl true
  def handle_cast({:request_registered, _correlation_id}, state) do
    updated_stats = %{
      state.stats
      | total_requests: state.stats.total_requests + 1,
        active_requests: state.stats.active_requests + 1
    }

    {:noreply, %{state | stats: updated_stats}}
  end

  def handle_cast({:response_sent, _correlation_id, request_info}, state) do
    # Log performance metrics
    duration = System.monotonic_time(:millisecond) - request_info.started_at
    Logger.debug("Request completed in #{duration}ms")

    updated_stats = %{
      state.stats
      | active_requests: max(0, state.stats.active_requests - 1)
    }

    {:noreply, %{state | stats: updated_stats}}
  end

  def handle_cast({:request_timeout, correlation_id}, state) do
    Logger.warning("Request timeout for correlation_id: #{inspect(correlation_id)}")

    updated_stats = %{
      state.stats
      | timeouts: state.stats.timeouts + 1,
        active_requests: max(0, state.stats.active_requests - 1)
    }

    {:noreply, %{state | stats: updated_stats}}
  end

  def handle_cast({:request_cancelled, _correlation_id}, state) do
    updated_stats = %{
      state.stats
      | active_requests: max(0, state.stats.active_requests - 1)
    }

    {:noreply, %{state | stats: updated_stats}}
  end

  @impl true
  def handle_info({:correlation_timeout, correlation_id}, state) do
    # Handle automatic timeout
    case Registry.lookup(@registry_name, correlation_id) do
      [{requester_pid, _request_info}] ->
        send(requester_pid, {:mcp_response, correlation_id, {:error, :timeout}})
        Registry.unregister(@registry_name, correlation_id)
        cleanup_timeout(correlation_id)
        GenServer.cast(__MODULE__, {:request_timeout, correlation_id})

      [] ->
        # Already cleaned up
        :ok
    end

    {:noreply, state}
  end

  def handle_info({:DOWN, _ref, :process, _pid, _reason}, state) do
    # Registry automatically cleans up when processes die
    # We just need to update our active count
    active_count = Registry.count(@registry_name)
    updated_stats = %{state.stats | active_requests: active_count}

    {:noreply, %{state | stats: updated_stats}}
  end

  def handle_info(msg, state) do
    Logger.debug("Unexpected message in Correlation: #{inspect(msg)}")
    {:noreply, state}
  end

  # Private helper functions

  defp cleanup_timeout(correlation_id) do
    case :ets.lookup(@correlation_timeout_table, correlation_id) do
      [{^correlation_id, timer_ref}] ->
        Process.cancel_timer(timer_ref)
        :ets.delete(@correlation_timeout_table, correlation_id)

      [] ->
        :ok
    end
  end
end
