defmodule ExMCP.Reliability.CircuitBreaker do
  @moduledoc """
  Circuit breaker GenServer wrapper for ExMCP.Transport.Beam.CircuitBreaker.

  Provides a GenServer-based interface to the circuit breaker pattern for
  protecting MCP services from cascading failures.
  """

  use GenServer

  alias ExMCP.Transport.Beam.CircuitBreaker, as: CB

  @type options :: [
          failure_threshold: non_neg_integer(),
          success_threshold: non_neg_integer(),
          timeout: non_neg_integer(),
          reset_timeout: non_neg_integer(),
          error_filter: (any() -> boolean())
        ]

  @doc """
  Starts a circuit breaker process.
  """
  @spec start_link(options()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    {gen_opts, cb_opts} = split_options(opts)
    GenServer.start_link(__MODULE__, cb_opts, gen_opts)
  end

  @doc """
  Executes a function through the circuit breaker.
  """
  @spec call(GenServer.server(), (-> any())) :: any()
  def call(server, fun) do
    call(server, fun, 5000)
  end

  @doc """
  Executes a function through the circuit breaker with timeout.
  """
  @spec call(GenServer.server(), (-> any()), timeout()) :: any()
  def call(server, fun, timeout) do
    GenServer.call(server, {:execute, fun}, timeout)
  end

  @doc """
  Gets the current state of the circuit breaker.
  """
  @spec get_state(GenServer.server()) :: CB.state()
  def get_state(server) do
    GenServer.call(server, :get_state)
  end

  @doc """
  Gets circuit breaker statistics.
  """
  @spec get_stats(GenServer.server()) :: map()
  def get_stats(server) do
    GenServer.call(server, :get_stats)
  end

  @doc """
  Manually opens the circuit breaker.
  """
  @spec open(GenServer.server()) :: :ok
  def open(server) do
    GenServer.cast(server, :open)
  end

  @doc """
  Manually closes the circuit breaker.
  """
  @spec close(GenServer.server()) :: :ok
  def close(server) do
    GenServer.cast(server, :close)
  end

  @doc """
  Resets the circuit breaker to initial state.
  """
  @spec reset(GenServer.server()) :: :ok
  def reset(server) do
    GenServer.cast(server, :reset)
  end

  @doc """
  Manually trips (opens) the circuit breaker.
  Alias for open/1 to match test expectations.
  """
  @spec trip(GenServer.server()) :: :ok
  def trip(server) do
    open(server)
  end

  # GenServer callbacks

  @impl GenServer
  def init(opts) do
    {error_filter, cb_opts} = Keyword.pop(opts, :error_filter, fn _ -> true end)
    circuit_breaker = CB.new(Map.new(cb_opts))

    state = %{
      circuit_breaker: circuit_breaker,
      error_filter: error_filter
    }

    {:ok, state}
  end

  @impl GenServer
  def handle_call({:execute, fun}, _from, state) do
    if CB.allow_request?(state.circuit_breaker) do
      execute_with_circuit_breaker(fun, state)
    else
      {:reply, {:error, :circuit_open}, state}
    end
  end

  def handle_call(:get_state, _from, state) do
    # Get fresh stats which includes state transitions
    stats = CB.get_stats(state.circuit_breaker)
    {:reply, stats, state}
  end

  def handle_call(:get_stats, _from, state) do
    stats = CB.get_stats(state.circuit_breaker)
    {:reply, stats, state}
  end

  def handle_call(:open, _from, state) do
    updated_cb = CB.force_state(state.circuit_breaker, :open)
    {:reply, :ok, %{state | circuit_breaker: updated_cb}}
  end

  def handle_call(:close, _from, state) do
    updated_cb = CB.force_state(state.circuit_breaker, :closed)
    {:reply, :ok, %{state | circuit_breaker: updated_cb}}
  end

  def handle_call(:reset, _from, state) do
    updated_cb = CB.reset(state.circuit_breaker)
    {:reply, :ok, %{state | circuit_breaker: updated_cb}}
  end

  defp execute_with_circuit_breaker(fun, state) do
    # credo:disable-for-next-line Credo.Check.Readability.PreferImplicitTry
    try do
      result = execute_with_timeout(fun, state.circuit_breaker.config)
      handle_execution_result(result, state)
    rescue
      error ->
        handle_execution_error(error, state)
    catch
      :throw, value ->
        updated_cb = CB.record_failure(state.circuit_breaker)
        {:reply, {:error, {:throw, value}}, %{state | circuit_breaker: updated_cb}}

      :exit, reason ->
        updated_cb = CB.record_failure(state.circuit_breaker)
        {:reply, {:error, {:exit, reason}}, %{state | circuit_breaker: updated_cb}}
    end
  end

  defp execute_with_timeout(fun, config) do
    timeout = Map.get(config, :timeout, :infinity)

    if timeout != :infinity do
      task = Task.async(fun)

      case Task.yield(task, timeout) || Task.shutdown(task) do
        {:ok, task_result} -> task_result
        nil -> {:error, :timeout}
      end
    else
      fun.()
    end
  end

  defp handle_execution_result({:error, :timeout}, state) do
    updated_cb = CB.record_failure(state.circuit_breaker)
    {:reply, {:error, :timeout}, %{state | circuit_breaker: updated_cb}}
  end

  defp handle_execution_result({:error, error_reason}, state) do
    should_count_error = state.error_filter.(error_reason)

    updated_cb =
      if should_count_error do
        CB.record_failure(state.circuit_breaker)
      else
        state.circuit_breaker
      end

    {:reply, {:error, error_reason}, %{state | circuit_breaker: updated_cb}}
  end

  defp handle_execution_result(success_result, state) do
    updated_cb = CB.record_success(state.circuit_breaker)
    {:reply, success_result, %{state | circuit_breaker: updated_cb}}
  end

  defp handle_execution_error(error, state) do
    should_count_error = state.error_filter.(error)

    updated_cb =
      if should_count_error do
        CB.record_failure(state.circuit_breaker)
      else
        state.circuit_breaker
      end

    {:reply, {:error, error}, %{state | circuit_breaker: updated_cb}}
  end

  @impl GenServer
  def handle_cast(:open, state) do
    updated_cb = CB.force_state(state.circuit_breaker, :open)
    {:noreply, %{state | circuit_breaker: updated_cb}}
  end

  def handle_cast(:close, state) do
    updated_cb = CB.force_state(state.circuit_breaker, :closed)
    {:noreply, %{state | circuit_breaker: updated_cb}}
  end

  def handle_cast(:reset, state) do
    updated_cb = CB.reset(state.circuit_breaker)
    {:noreply, %{state | circuit_breaker: updated_cb}}
  end

  # Private helpers

  defp split_options(opts) do
    {gen_opts, cb_opts} = Keyword.split(opts, [:name])
    {gen_opts, cb_opts}
  end
end
