defmodule ExMCP.Reliability.CircuitBreaker do
  @moduledoc """
  Circuit breaker implementation for MCP operations.

  Provides fault tolerance by preventing cascading failures when
  downstream services are unavailable. The circuit breaker has three states:

  - **Closed**: Normal operation, requests pass through
  - **Open**: Failure threshold exceeded, requests fail fast
  - **Half-Open**: Testing if service has recovered

  ## Usage

      # Create a circuit breaker
      {:ok, breaker} = CircuitBreaker.start_link(
        name: :my_breaker,
        failure_threshold: 5,
        success_threshold: 2,
        timeout: 30_000,
        reset_timeout: 60_000
      )
      
      # Use with a function
      CircuitBreaker.call(breaker, fn ->
        # Your potentially failing operation
        ExMCP.SimpleClient.call_tool(client, "tool", %{})
      end)

  ## Configuration

  - `:failure_threshold` - Number of failures before opening circuit (default: 5)
  - `:success_threshold` - Number of successes in half-open before closing (default: 2)
  - `:timeout` - Operation timeout in ms (default: 5000)
  - `:reset_timeout` - Time before trying half-open in ms (default: 60000)
  - `:error_filter` - Function to determine if error should trip breaker (default: all errors)
  """

  use GenServer
  require Logger

  @default_failure_threshold 5
  @default_success_threshold 2
  @default_timeout 5000
  @default_reset_timeout 60000

  defstruct [
    :name,
    :failure_threshold,
    :success_threshold,
    :timeout,
    :reset_timeout,
    :error_filter,
    state: :closed,
    failure_count: 0,
    success_count: 0,
    last_failure_time: nil,
    stats: %{
      total_calls: 0,
      successful_calls: 0,
      failed_calls: 0,
      rejected_calls: 0
    }
  ]

  @type state :: :closed | :open | :half_open

  @type t :: %__MODULE__{
          name: atom(),
          failure_threshold: pos_integer(),
          success_threshold: pos_integer(),
          timeout: pos_integer(),
          reset_timeout: pos_integer(),
          error_filter: (any() -> boolean()) | nil,
          state: state(),
          failure_count: non_neg_integer(),
          success_count: non_neg_integer(),
          last_failure_time: integer() | nil,
          stats: map()
        }

  @type call_result :: {:ok, any()} | {:error, any()}

  ## Client API

  @doc """
  Starts a circuit breaker process.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    name = Keyword.get(opts, :name)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Executes a function through the circuit breaker.

  Returns `{:ok, result}` if successful, `{:error, reason}` if failed,
  or `{:error, :circuit_open}` if circuit is open.
  """
  @spec call(GenServer.server(), function(), keyword()) :: call_result()
  def call(breaker, fun, opts \\ []) when is_function(fun, 0) do
    timeout = Keyword.get(opts, :timeout)
    GenServer.call(breaker, {:call, fun, timeout})
  end

  @doc """
  Gets the current state of the circuit breaker.
  """
  @spec get_state(GenServer.server()) :: t()
  def get_state(breaker) do
    GenServer.call(breaker, :get_state)
  end

  @doc """
  Gets statistics for the circuit breaker.
  """
  @spec get_stats(GenServer.server()) :: map()
  def get_stats(breaker) do
    GenServer.call(breaker, :get_stats)
  end

  @doc """
  Manually resets the circuit breaker to closed state.
  """
  @spec reset(GenServer.server()) :: :ok
  def reset(breaker) do
    GenServer.call(breaker, :reset)
  end

  @doc """
  Manually trips the circuit breaker to open state.
  """
  @spec trip(GenServer.server()) :: :ok
  def trip(breaker) do
    GenServer.call(breaker, :trip)
  end

  ## Server Callbacks

  @impl GenServer
  def init(opts) do
    state = %__MODULE__{
      name: Keyword.get(opts, :name, :circuit_breaker),
      failure_threshold: Keyword.get(opts, :failure_threshold, @default_failure_threshold),
      success_threshold: Keyword.get(opts, :success_threshold, @default_success_threshold),
      timeout: Keyword.get(opts, :timeout, @default_timeout),
      reset_timeout: Keyword.get(opts, :reset_timeout, @default_reset_timeout),
      error_filter: Keyword.get(opts, :error_filter)
    }

    {:ok, state}
  end

  @impl GenServer
  def handle_call({:call, fun, timeout}, _from, state) do
    {result, new_state} = execute_with_breaker(fun, timeout || state.timeout, state)
    {:reply, result, new_state}
  end

  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  def handle_call(:get_stats, _from, state) do
    {:reply, state.stats, state}
  end

  def handle_call(:reset, _from, state) do
    new_state = %{
      state
      | state: :closed,
        failure_count: 0,
        success_count: 0,
        last_failure_time: nil
    }

    Logger.info("Circuit breaker #{state.name} manually reset to closed")
    {:reply, :ok, new_state}
  end

  def handle_call(:trip, _from, state) do
    new_state = %{state | state: :open, last_failure_time: System.monotonic_time(:millisecond)}
    Logger.warning("Circuit breaker #{state.name} manually tripped to open")
    {:reply, :ok, new_state}
  end

  @impl GenServer
  def handle_info({:check_recovery, from_time}, state) do
    # Check if we should transition from open to half-open
    if state.state == :open and state.last_failure_time == from_time do
      new_state = %{state | state: :half_open, success_count: 0}
      Logger.info("Circuit breaker #{state.name} transitioning to half-open")
      {:noreply, new_state}
    else
      {:noreply, state}
    end
  end

  ## Private Functions

  defp execute_with_breaker(fun, timeout, %{state: :closed} = state) do
    case execute_function(fun, timeout) do
      {:ok, result} ->
        new_state = %{state | failure_count: 0, stats: update_stats(state.stats, :success)}
        {{:ok, result}, new_state}

      {:error, reason} = error ->
        if should_count_error?(reason, state.error_filter) do
          handle_failure(error, state)
        else
          # Don't count this error towards circuit breaker
          new_state = %{state | stats: update_stats(state.stats, :success)}
          {error, new_state}
        end
    end
  end

  defp execute_with_breaker(fun, timeout, %{state: :open} = state) do
    # Check if enough time has passed to try half-open
    now = System.monotonic_time(:millisecond)
    time_since_failure = now - (state.last_failure_time || 0)

    if time_since_failure >= state.reset_timeout do
      # Transition to half-open and try
      new_state = %{state | state: :half_open, success_count: 0}
      Logger.info("Circuit breaker #{state.name} attempting recovery (half-open)")
      execute_with_breaker(fun, timeout, new_state)
    else
      # Still open, reject call
      new_state = %{state | stats: update_stats(state.stats, :rejected)}
      {{:error, :circuit_open}, new_state}
    end
  end

  defp execute_with_breaker(fun, timeout, %{state: :half_open} = state) do
    case execute_function(fun, timeout) do
      {:ok, result} ->
        new_success_count = state.success_count + 1

        if new_success_count >= state.success_threshold do
          # Recovered! Close the circuit
          new_state = %{
            state
            | state: :closed,
              failure_count: 0,
              success_count: 0,
              last_failure_time: nil,
              stats: update_stats(state.stats, :success)
          }

          Logger.info("Circuit breaker #{state.name} recovered (closed)")
          {{:ok, result}, new_state}
        else
          # Still testing
          new_state = %{
            state
            | success_count: new_success_count,
              stats: update_stats(state.stats, :success)
          }

          {{:ok, result}, new_state}
        end

      {:error, reason} = error ->
        if should_count_error?(reason, state.error_filter) do
          # Failed again, reopen
          new_state = %{
            state
            | state: :open,
              failure_count: 0,
              success_count: 0,
              last_failure_time: System.monotonic_time(:millisecond),
              stats: update_stats(state.stats, :failure)
          }

          # Schedule check for recovery
          Process.send_after(
            self(),
            {:check_recovery, new_state.last_failure_time},
            state.reset_timeout
          )

          Logger.warning("Circuit breaker #{state.name} reopened after half-open failure")
          {error, new_state}
        else
          # Don't count this error
          new_state = %{state | stats: update_stats(state.stats, :success)}
          {error, new_state}
        end
    end
  end

  defp handle_failure(error, state) do
    new_failure_count = state.failure_count + 1

    if new_failure_count >= state.failure_threshold do
      # Trip the circuit breaker
      new_state = %{
        state
        | state: :open,
          failure_count: 0,
          last_failure_time: System.monotonic_time(:millisecond),
          stats: update_stats(state.stats, :failure)
      }

      # Schedule check for recovery
      Process.send_after(
        self(),
        {:check_recovery, new_state.last_failure_time},
        state.reset_timeout
      )

      Logger.error("Circuit breaker #{state.name} opened after #{new_failure_count} failures")
      {error, new_state}
    else
      # Increment failure count
      new_state = %{
        state
        | failure_count: new_failure_count,
          stats: update_stats(state.stats, :failure)
      }

      {error, new_state}
    end
  end

  defp execute_function(fun, timeout) do
    task = Task.async(fun)

    case Task.yield(task, timeout) || Task.shutdown(task) do
      {:ok, result} -> result
      nil -> {:error, :timeout}
      {:exit, reason} -> {:error, {:exit, reason}}
    end
  rescue
    e -> {:error, e}
  end

  defp should_count_error?(_reason, nil), do: true

  defp should_count_error?(reason, filter) when is_function(filter, 1) do
    filter.(reason)
  end

  defp update_stats(stats, :success) do
    %{stats | total_calls: stats.total_calls + 1, successful_calls: stats.successful_calls + 1}
  end

  defp update_stats(stats, :failure) do
    %{stats | total_calls: stats.total_calls + 1, failed_calls: stats.failed_calls + 1}
  end

  defp update_stats(stats, :rejected) do
    %{stats | total_calls: stats.total_calls + 1, rejected_calls: stats.rejected_calls + 1}
  end
end
