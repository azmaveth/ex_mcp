defmodule ExMCP.Resilience do
  @moduledoc """
  Optional resilience patterns for ExMCP.Native service calls.

  This module provides composable resilience patterns for services that need 
  additional fault tolerance beyond OTP's built-in supervision and monitoring.

  ## Trade-offs

  Using these patterns adds latency and complexity to service calls. In trusted
  Elixir clusters, OTP supervision typically provides better fault tolerance.
  Use these patterns only when you need specific resilience behaviors.

  ## Examples

      # Simple retry with exponential backoff
      {:ok, result} = ExMCP.Resilience.call_with_retry(
        :my_service, 
        "method", 
        %{}, 
        max_attempts: 3,
        backoff: :exponential
      )

      # Circuit breaker pattern (requires :fuse dependency)
      {:ok, result} = ExMCP.Resilience.call_with_breaker(
        :my_service,
        "method",
        %{},
        circuit_name: :my_service_circuit
      )

      # Timeout with fallback
      result = ExMCP.Resilience.call_with_fallback(
        :my_service,
        "method", 
        %{},
        timeout: 1000,
        fallback: fn -> {:ok, %{"status" => "unavailable"}} end
      )
  """

  require Logger

  @doc """
  Calls a service with retry logic and exponential backoff.

  ## Options

  - `:max_attempts` - Maximum number of attempts (default: 3)
  - `:backoff` - `:linear` or `:exponential` (default: :exponential)
  - `:base_delay` - Base delay in milliseconds (default: 100)
  - `:max_delay` - Maximum delay in milliseconds (default: 5000)
  - `:retry_on` - List of error reasons to retry on (default: [:timeout, :service_unavailable])

  ## Examples

      {:ok, result} = ExMCP.Resilience.call_with_retry(
        :flaky_service,
        "process_data",
        %{"input" => "data"},
        max_attempts: 5,
        base_delay: 200
      )
  """
  @spec call_with_retry(atom(), String.t(), map(), keyword()) ::
          {:ok, term()} | {:error, term()}
  def call_with_retry(service_id, method, params, opts \\ []) do
    max_attempts = Keyword.get(opts, :max_attempts, 3)
    backoff = Keyword.get(opts, :backoff, :exponential)
    base_delay = Keyword.get(opts, :base_delay, 100)
    max_delay = Keyword.get(opts, :max_delay, 5000)
    retry_on = Keyword.get(opts, :retry_on, [:timeout, :service_unavailable])

    native_opts =
      Keyword.drop(opts, [:max_attempts, :backoff, :base_delay, :max_delay, :retry_on])

    do_retry(
      service_id,
      method,
      params,
      native_opts,
      1,
      max_attempts,
      backoff,
      base_delay,
      max_delay,
      retry_on
    )
  end

  @doc """
  Calls a service with a fallback function if the call fails.

  The fallback function is called with no arguments and should return
  `{:ok, result}` or `{:error, reason}`.

  ## Examples

      result = ExMCP.Resilience.call_with_fallback(
        :unreliable_service,
        "get_data",
        %{},
        fallback: fn -> 
          {:ok, %{"data" => "cached_value", "source" => "fallback"}} 
        end
      )
  """
  @spec call_with_fallback(atom(), String.t(), map(), keyword()) ::
          {:ok, term()} | {:error, term()}
  def call_with_fallback(service_id, method, params, opts) do
    {fallback_fn, native_opts} = Keyword.pop(opts, :fallback)

    case ExMCP.Native.call(service_id, method, params, native_opts) do
      {:ok, result} ->
        {:ok, result}

      {:error, _reason} when is_function(fallback_fn, 0) ->
        Logger.warning("Service call failed, using fallback: #{service_id}.#{method}")
        fallback_fn.()

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Calls a service through a circuit breaker pattern.

  Requires the `:fuse` library to be added to your dependencies.
  Install with: `{:fuse, "~> 2.4"}` in mix.exs

  ## Options

  - `:circuit_name` - Name for the circuit breaker (default: service_id)
  - `:fuse_options` - Options passed to :fuse.install/2

  ## Examples

      # First, install the fuse in your application startup:
      :fuse.install(:my_service_circuit, {{:standard, 5, 60_000}, {:reset, 30_000}})

      # Then use it:
      {:ok, result} = ExMCP.Resilience.call_with_breaker(
        :my_service,
        "method",
        %{},
        circuit_name: :my_service_circuit
      )
  """
  @spec call_with_breaker(atom(), String.t(), map(), keyword()) ::
          {:ok, term()} | {:error, term()}
  def call_with_breaker(service_id, method, params, opts \\ []) do
    circuit_name = Keyword.get(opts, :circuit_name, service_id)
    native_opts = Keyword.drop(opts, [:circuit_name, :fuse_options])

    if Code.ensure_loaded?(:fuse) do
      case :fuse.ask(circuit_name, :sync) do
        :ok ->
          case ExMCP.Native.call(service_id, method, params, native_opts) do
            {:ok, result} ->
              {:ok, result}

            {:error, _reason} = error ->
              :fuse.melt(circuit_name)
              error
          end

        :blown ->
          {:error, {:circuit_breaker_open, circuit_name}}

        {:error, :not_found} ->
          Logger.warning(
            "Circuit breaker not found: #{circuit_name}. Install with :fuse.install/2"
          )

          ExMCP.Native.call(service_id, method, params, native_opts)
      end
    else
      Logger.warning("Circuit breaker requested but :fuse library not available")
      ExMCP.Native.call(service_id, method, params, native_opts)
    end
  end

  # Private functions

  defp do_retry(
         _service_id,
         _method,
         _params,
         _opts,
         attempt,
         max_attempts,
         _backoff,
         _base_delay,
         _max_delay,
         _retry_on
       )
       when attempt > max_attempts do
    {:error, :max_retries_exceeded}
  end

  defp do_retry(
         service_id,
         method,
         params,
         opts,
         attempt,
         max_attempts,
         backoff,
         base_delay,
         max_delay,
         retry_on
       ) do
    case ExMCP.Native.call(service_id, method, params, opts) do
      {:ok, result} ->
        if attempt > 1 do
          Logger.info("Service call succeeded after #{attempt} attempts: #{service_id}.#{method}")
        end

        {:ok, result}

      {:error, reason} when attempt < max_attempts ->
        if reason in retry_on do
          delay = calculate_delay(attempt, backoff, base_delay, max_delay)

          Logger.warning(
            "Service call failed (attempt #{attempt}/#{max_attempts}), retrying in #{delay}ms: #{service_id}.#{method} - #{inspect(reason)}"
          )

          Process.sleep(delay)

          do_retry(
            service_id,
            method,
            params,
            opts,
            attempt + 1,
            max_attempts,
            backoff,
            base_delay,
            max_delay,
            retry_on
          )
        else
          if attempt > 1 do
            Logger.error(
              "Service call failed after #{attempt} attempts: #{service_id}.#{method} - #{inspect(reason)}"
            )
          end

          {:error, reason}
        end

      {:error, reason} ->
        if attempt > 1 do
          Logger.error(
            "Service call failed after #{attempt} attempts: #{service_id}.#{method} - #{inspect(reason)}"
          )
        end

        {:error, reason}
    end
  end

  defp calculate_delay(attempt, :linear, base_delay, max_delay) do
    min(base_delay * attempt, max_delay)
  end

  defp calculate_delay(attempt, :exponential, base_delay, max_delay) do
    delay = base_delay * :math.pow(2, attempt - 1)
    min(trunc(delay), max_delay)
  end
end
