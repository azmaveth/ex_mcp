defmodule ExMCP.Transport.Beam.CircuitBreaker do
  @moduledoc """
  Circuit breaker implementation for MCP service protection in BEAM transport clustering.

  Implements the circuit breaker pattern to protect services from cascading failures.
  Tracks failure rates and automatically opens circuits when thresholds are exceeded,
  preventing requests from reaching failing services.

  ## Circuit States

  - `:closed` - Normal operation, requests flow through
  - `:open` - Circuit is open, requests are failed fast
  - `:half_open` - Testing if service has recovered

  ## Configuration Options

  - `failure_threshold` - Number of failures before opening circuit
  - `success_threshold` - Number of successes needed to close circuit from half-open
  - `timeout` - Time to wait before transitioning from open to half-open
  - `failure_rate_threshold` - Percentage of failures that triggers circuit opening
  - `minimum_throughput` - Minimum requests before considering failure rate

  ## Example Usage

      # Create a circuit breaker
      circuit_breaker = CircuitBreaker.new(%{
        failure_threshold: 5,
        timeout: 60000,
        success_threshold: 3
      })
      
      # Record failures and successes
      updated_cb = CircuitBreaker.record_failure(circuit_breaker)
      updated_cb = CircuitBreaker.record_success(circuit_breaker)
      
      # Check if requests should be allowed
      case CircuitBreaker.allow_request?(updated_cb) do
        true -> # Make request
        false -> # Fail fast
      end
  """

  defstruct [
    :state,
    :failure_count,
    :success_count,
    :last_failure_time,
    :last_success_time,
    :opened_at,
    :config,
    :stats
  ]

  @type state :: :closed | :open | :half_open
  @type t :: %__MODULE__{
          state: state(),
          failure_count: non_neg_integer(),
          success_count: non_neg_integer(),
          last_failure_time: integer() | nil,
          last_success_time: integer() | nil,
          opened_at: integer() | nil,
          config: map(),
          stats: map()
        }

  @type config :: %{
          failure_threshold: non_neg_integer(),
          success_threshold: non_neg_integer(),
          timeout: non_neg_integer(),
          failure_rate_threshold: float(),
          minimum_throughput: non_neg_integer(),
          reset_timeout: non_neg_integer()
        }

  @default_config %{
    failure_threshold: 5,
    success_threshold: 3,
    timeout: 60000,
    failure_rate_threshold: 0.5,
    minimum_throughput: 10,
    reset_timeout: 300_000
  }

  @doc """
  Creates a new circuit breaker with the given configuration.
  """
  @spec new(config() | map()) :: t()
  def new(config \\ %{}) do
    full_config = Map.merge(@default_config, config)

    %__MODULE__{
      state: :closed,
      failure_count: 0,
      success_count: 0,
      last_failure_time: nil,
      last_success_time: nil,
      opened_at: nil,
      config: full_config,
      stats: init_stats()
    }
  end

  @doc """
  Checks if a request should be allowed based on the current circuit state.
  """
  @spec allow_request?(t()) :: boolean()
  def allow_request?(%__MODULE__{} = circuit_breaker) do
    updated_cb = check_state_transitions(circuit_breaker)

    case updated_cb.state do
      :closed -> true
      :half_open -> true
      :open -> false
    end
  end

  @doc """
  Records a successful operation and updates the circuit breaker state.
  """
  @spec record_success(t()) :: t()
  def record_success(%__MODULE__{} = circuit_breaker) do
    current_time = System.system_time(:millisecond)

    updated_cb = %{
      circuit_breaker
      | success_count: circuit_breaker.success_count + 1,
        last_success_time: current_time,
        stats: update_stats(circuit_breaker.stats, :total_successes, 1)
    }

    # Check if we should close the circuit
    case updated_cb.state do
      :half_open ->
        if updated_cb.success_count >= updated_cb.config.success_threshold do
          close_circuit(updated_cb)
        else
          updated_cb
        end

      :open ->
        # Reset success count when circuit is open
        %{updated_cb | success_count: 0}

      :closed ->
        updated_cb
    end
  end

  @doc """
  Records a failed operation and updates the circuit breaker state.
  """
  @spec record_failure(t()) :: t()
  def record_failure(%__MODULE__{} = circuit_breaker) do
    current_time = System.system_time(:millisecond)

    updated_cb = %{
      circuit_breaker
      | failure_count: circuit_breaker.failure_count + 1,
        last_failure_time: current_time,
        stats: update_stats(circuit_breaker.stats, :total_failures, 1)
    }

    # Check if we should open the circuit
    case should_open_circuit?(updated_cb) do
      true ->
        open_circuit(updated_cb)

      false ->
        updated_cb
    end
  end

  @doc """
  Forces the circuit breaker to a specific state.
  """
  @spec force_state(t(), state()) :: t()
  def force_state(%__MODULE__{} = circuit_breaker, new_state) do
    current_time = System.system_time(:millisecond)

    updated_cb =
      case new_state do
        :open ->
          %{
            circuit_breaker
            | state: :open,
              opened_at: current_time,
              stats: update_stats(circuit_breaker.stats, :manual_opens, 1)
          }

        :closed ->
          %{
            circuit_breaker
            | state: :closed,
              failure_count: 0,
              success_count: 0,
              opened_at: nil,
              stats: update_stats(circuit_breaker.stats, :manual_closes, 1)
          }

        :half_open ->
          %{
            circuit_breaker
            | state: :half_open,
              success_count: 0,
              stats: update_stats(circuit_breaker.stats, :manual_half_opens, 1)
          }
      end

    updated_cb
  end

  @doc """
  Gets the current state of the circuit breaker.
  """
  @spec get_state(t()) :: state()
  def get_state(%__MODULE__{} = circuit_breaker) do
    updated_cb = check_state_transitions(circuit_breaker)
    updated_cb.state
  end

  @doc """
  Gets circuit breaker statistics.
  """
  @spec get_stats(t()) :: map()
  def get_stats(%__MODULE__{} = circuit_breaker) do
    total_requests = circuit_breaker.stats.total_successes + circuit_breaker.stats.total_failures

    failure_rate =
      if total_requests > 0 do
        circuit_breaker.stats.total_failures / total_requests
      else
        0.0
      end

    Map.merge(circuit_breaker.stats, %{
      current_state: circuit_breaker.state,
      failure_count: circuit_breaker.failure_count,
      success_count: circuit_breaker.success_count,
      total_requests: total_requests,
      failure_rate: failure_rate,
      last_failure_time: circuit_breaker.last_failure_time,
      last_success_time: circuit_breaker.last_success_time,
      opened_at: circuit_breaker.opened_at
    })
  end

  @doc """
  Resets the circuit breaker to its initial state.
  """
  @spec reset(t()) :: t()
  def reset(%__MODULE__{} = circuit_breaker) do
    %{
      circuit_breaker
      | state: :closed,
        failure_count: 0,
        success_count: 0,
        last_failure_time: nil,
        last_success_time: nil,
        opened_at: nil,
        stats: update_stats(circuit_breaker.stats, :resets, 1)
    }
  end

  # Private helper functions

  defp check_state_transitions(%__MODULE__{state: :open} = circuit_breaker) do
    current_time = System.system_time(:millisecond)

    if circuit_breaker.opened_at != nil and
         current_time - circuit_breaker.opened_at >= circuit_breaker.config.timeout do
      # Transition to half-open
      %{
        circuit_breaker
        | state: :half_open,
          success_count: 0,
          stats: update_stats(circuit_breaker.stats, :automatic_half_opens, 1)
      }
    else
      circuit_breaker
    end
  end

  defp check_state_transitions(%__MODULE__{state: :half_open} = circuit_breaker) do
    # Check if we should reset due to timeout
    current_time = System.system_time(:millisecond)

    if circuit_breaker.last_success_time != nil and
         current_time - circuit_breaker.last_success_time >= circuit_breaker.config.reset_timeout do
      # Reset to closed state due to inactivity
      close_circuit(circuit_breaker)
    else
      circuit_breaker
    end
  end

  defp check_state_transitions(circuit_breaker), do: circuit_breaker

  defp should_open_circuit?(%__MODULE__{} = circuit_breaker) do
    # Check failure threshold
    failure_threshold_exceeded =
      circuit_breaker.failure_count >= circuit_breaker.config.failure_threshold

    # Check failure rate
    total_requests = circuit_breaker.stats.total_successes + circuit_breaker.stats.total_failures
    minimum_throughput_met = total_requests >= circuit_breaker.config.minimum_throughput

    failure_rate =
      if total_requests > 0 do
        circuit_breaker.stats.total_failures / total_requests
      else
        0.0
      end

    failure_rate_exceeded =
      minimum_throughput_met and
        failure_rate >= circuit_breaker.config.failure_rate_threshold

    failure_threshold_exceeded or failure_rate_exceeded
  end

  defp open_circuit(%__MODULE__{} = circuit_breaker) do
    current_time = System.system_time(:millisecond)

    %{
      circuit_breaker
      | state: :open,
        opened_at: current_time,
        stats: update_stats(circuit_breaker.stats, :circuit_opens, 1)
    }
  end

  defp close_circuit(%__MODULE__{} = circuit_breaker) do
    %{
      circuit_breaker
      | state: :closed,
        failure_count: 0,
        success_count: 0,
        opened_at: nil,
        stats: update_stats(circuit_breaker.stats, :circuit_closes, 1)
    }
  end

  defp init_stats do
    %{
      total_successes: 0,
      total_failures: 0,
      circuit_opens: 0,
      circuit_closes: 0,
      manual_opens: 0,
      manual_closes: 0,
      manual_half_opens: 0,
      automatic_half_opens: 0,
      resets: 0,
      created_at: System.system_time(:millisecond)
    }
  end

  defp update_stats(stats, metric, increment) do
    current_value = Map.get(stats, metric, 0)
    Map.put(stats, metric, current_value + increment)
  end
end
