defmodule ExMCP.Reliability.Supervisor do
  @moduledoc """
  Supervisor for reliability components in ExMCP.

  Manages circuit breakers, health checks, and provides
  integrated reliability features for MCP clients and servers.

  ## Usage

      # Add to your supervision tree
      children = [
        {ExMCP.Reliability.Supervisor, name: MyApp.Reliability}
      ]

      # Or start manually
      {:ok, sup} = ExMCP.Reliability.Supervisor.start_link()

      # Create reliability-enhanced client
      {:ok, client} = ExMCP.Reliability.Supervisor.create_reliable_client(
        sup,
        transport: :stdio,
        circuit_breaker: [
          failure_threshold: 5,
          reset_timeout: 30_000
        ],
        retry: [
          max_attempts: 3,
          backoff_factor: 2
        ],
        health_check: [
          check_interval: 60_000,
          failure_threshold: 3
        ]
      )
  """

  use Supervisor

  alias ExMCP.Reliability.{HealthCheck, Retry}

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name)
    Supervisor.start_link(__MODULE__, opts, name: name)
  end

  @impl Supervisor
  def init(opts) do
    # Create unique names based on a unique identifier
    unique_id = :erlang.unique_integer([:positive])
    supervisor_id = Keyword.get(opts, :name, :"#{__MODULE__}_#{unique_id}")

    children = [
      # Dynamic supervisor for circuit breakers
      {DynamicSupervisor,
       strategy: :one_for_one, name: circuit_breaker_supervisor(supervisor_id)},

      # Dynamic supervisor for health checks
      {DynamicSupervisor, strategy: :one_for_one, name: health_check_supervisor(supervisor_id)},

      # Registry for tracking reliability components
      {Registry, keys: :unique, name: reliability_registry(supervisor_id)}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end

  @doc """
  Creates a reliability-enhanced MCP client.

  This wraps a standard MCP client with circuit breaker, retry logic,
  and health monitoring.

  ## Options

  - `:transport` - Transport configuration for the client
  - `:circuit_breaker` - Circuit breaker options (optional)
  - `:retry` - Retry configuration (optional)
  - `:health_check` - Health check configuration (optional)
  """
  @spec create_reliable_client(Supervisor.supervisor(), keyword()) ::
          {:ok, pid()} | {:error, term()}
  def create_reliable_client(supervisor \\ __MODULE__, opts) do
    client_id = Keyword.get(opts, :id, generate_client_id())
    transport_opts = Keyword.take(opts, [:transport, :transports])

    case ExMCP.Client.start_link(transport_opts) do
      {:ok, client_pid} ->
        case maybe_start_circuit_breaker(supervisor, client_id, opts) do
          {:ok, breaker_pid} ->
            case maybe_start_health_check(supervisor, client_id, client_pid, opts) do
              {:ok, health_pid} ->
                create_wrapper_and_cleanup(
                  supervisor,
                  client_id,
                  client_pid,
                  breaker_pid,
                  health_pid,
                  opts
                )

              error ->
                # health check failed
                cleanup_pids([client_pid, breaker_pid])
                error
            end

          error ->
            # circuit breaker failed
            cleanup_pids([client_pid])
            error
        end

      error ->
        # client failed
        error
    end
  end

  defp create_wrapper_and_cleanup(
         supervisor,
         client_id,
         client_pid,
         breaker_pid,
         health_pid,
         opts
       ) do
    wrapper_spec = %{
      id: {:reliable_client_wrapper, client_id},
      start:
        {__MODULE__.ClientWrapper, :start_link,
         [
           [
             client: client_pid,
             circuit_breaker: breaker_pid,
             retry_opts: Keyword.get(opts, :retry, []),
             health_check: health_pid
           ]
         ]},
      restart: :temporary
    }

    case DynamicSupervisor.start_child(find_circuit_breaker_supervisor(supervisor), wrapper_spec) do
      {:ok, wrapper_pid} ->
        {:ok, wrapper_pid}

      error ->
        # Clean up on wrapper start failure
        cleanup_pids([client_pid, breaker_pid, health_pid])
        error
    end
  end

  defp cleanup_pids(pids) do
    Enum.each(pids, fn pid ->
      if pid && is_pid(pid) do
        GenServer.stop(pid)
      end
    end)
  end

  defp maybe_start_circuit_breaker(supervisor, _client_id, opts) do
    case Keyword.get(opts, :circuit_breaker) do
      nil ->
        {:ok, nil}

      breaker_opts ->
        breaker_opts =
          Keyword.put(
            breaker_opts,
            :name,
            {:via, Registry,
             {find_reliability_registry(supervisor),
              {:circuit_breaker, Keyword.get(opts, :id, generate_client_id())}}}
          )

        spec = %{
          id: {:circuit_breaker, :erlang.unique_integer()},
          start: {ExMCP.Reliability.CircuitBreaker, :start_link, [breaker_opts]},
          restart: :temporary
        }

        case DynamicSupervisor.start_child(find_circuit_breaker_supervisor(supervisor), spec) do
          {:ok, pid} -> {:ok, pid}
          error -> error
        end
    end
  end

  defp maybe_start_health_check(supervisor, client_id, target_pid, opts) do
    case Keyword.get(opts, :health_check) do
      nil ->
        {:ok, nil}

      health_opts ->
        health_opts =
          health_opts
          |> Keyword.put(
            :name,
            {:via, Registry, {find_reliability_registry(supervisor), {:health_check, client_id}}}
          )
          |> Keyword.put(:target, target_pid)
          |> Keyword.put_new(:check_fn, HealthCheck.mcp_client_check_fn())

        spec = %{
          id: {:health_check, :erlang.unique_integer()},
          start: {HealthCheck, :start_link, [health_opts]},
          restart: :temporary
        }

        case DynamicSupervisor.start_child(find_health_check_supervisor(supervisor), spec) do
          {:ok, pid} -> {:ok, pid}
          error -> error
        end
    end
  end

  defp circuit_breaker_supervisor(supervisor_name \\ __MODULE__) do
    Module.concat([supervisor_name, CircuitBreakerSupervisor])
  end

  defp health_check_supervisor(supervisor_name \\ __MODULE__) do
    Module.concat([supervisor_name, HealthCheckSupervisor])
  end

  defp reliability_registry(supervisor_name \\ __MODULE__) do
    Module.concat([supervisor_name, Registry])
  end

  # Helper functions to find child supervisor names from a running supervisor
  defp find_circuit_breaker_supervisor(supervisor) do
    children = Supervisor.which_children(supervisor)

    cb_supervisor =
      Enum.find_value(children, fn
        {name, _pid, :supervisor, [DynamicSupervisor]} when is_atom(name) ->
          name_str = Atom.to_string(name)
          if String.contains?(name_str, "CircuitBreakerSupervisor"), do: name

        _ ->
          nil
      end)

    # Fallback to default if not found (backwards compatibility)
    cb_supervisor || circuit_breaker_supervisor()
  end

  defp find_health_check_supervisor(supervisor) do
    children = Supervisor.which_children(supervisor)

    hc_supervisor =
      Enum.find_value(children, fn
        {name, _pid, :supervisor, [DynamicSupervisor]} when is_atom(name) ->
          name_str = Atom.to_string(name)
          if String.contains?(name_str, "HealthCheckSupervisor"), do: name

        _ ->
          nil
      end)

    # Fallback to default if not found (backwards compatibility)
    hc_supervisor || health_check_supervisor()
  end

  defp find_reliability_registry(supervisor) do
    children = Supervisor.which_children(supervisor)

    registry =
      Enum.find_value(children, fn
        {name, _pid, :worker, [Registry]} when is_atom(name) ->
          name_str = Atom.to_string(name)
          if String.contains?(name_str, "Registry"), do: name

        _ ->
          nil
      end)

    # Fallback to default if not found (backwards compatibility)
    registry || reliability_registry()
  end

  defp generate_client_id do
    "client_#{:erlang.unique_integer([:positive])}"
  end
end

defmodule ExMCP.Reliability.Supervisor.ClientWrapper do
  @moduledoc """
  Wrapper process that integrates reliability features for MCP clients.

  This process intercepts calls to the underlying client and applies:
  - Circuit breaker protection
  - Retry logic with exponential backoff
  - Health monitoring integration
  """

  use GenServer

  alias ExMCP.Reliability.Retry

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl GenServer
  def init(opts) do
    state = %{
      client: Keyword.fetch!(opts, :client),
      circuit_breaker: Keyword.get(opts, :circuit_breaker),
      retry_opts: Keyword.get(opts, :retry_opts, []),
      health_check: Keyword.get(opts, :health_check)
    }

    # Monitor the underlying client
    Process.monitor(state.client)

    {:ok, state}
  end

  @impl GenServer
  def handle_call({:call_tool, tool_name, args, opts}, from, state) do
    execute_with_reliability(
      fn -> ExMCP.Client.call_tool(state.client, tool_name, args, opts) end,
      from,
      state
    )
  end

  def handle_call({:list_tools, opts}, from, state) do
    execute_with_reliability(
      fn -> ExMCP.Client.list_tools(state.client, opts) end,
      from,
      state
    )
  end

  def handle_call({:list_resources, opts}, from, state) do
    execute_with_reliability(
      fn -> ExMCP.Client.list_resources(state.client, opts) end,
      from,
      state
    )
  end

  def handle_call({:read_resource, uri, opts}, from, state) do
    execute_with_reliability(
      fn -> ExMCP.Client.read_resource(state.client, uri, opts) end,
      from,
      state
    )
  end

  def handle_call({:list_prompts, opts}, from, state) do
    execute_with_reliability(
      fn -> ExMCP.Client.list_prompts(state.client, opts) end,
      from,
      state
    )
  end

  def handle_call({:get_prompt, name, args, opts}, from, state) do
    execute_with_reliability(
      fn -> ExMCP.Client.get_prompt(state.client, name, args, opts) end,
      from,
      state
    )
  end

  # Forward other calls directly
  def handle_call(request, from, state) do
    execute_with_reliability(
      fn -> GenServer.call(state.client, request) end,
      from,
      state
    )
  end

  @impl GenServer
  def handle_info({:DOWN, _ref, :process, pid, reason}, %{client: pid} = state) do
    # Client process died
    {:stop, {:client_died, reason}, state}
  end

  defp execute_with_reliability(fun, from, state) do
    Task.start(fn ->
      # Execute with retry logic
      result = Retry.with_retry(fun, Retry.mcp_defaults(state.retry_opts))

      GenServer.reply(from, result)
    end)

    {:noreply, state}
  end
end

defmodule ExMCP.Reliability do
  @moduledoc """
  Convenience functions for reliability features.

  This module provides easy access to reliability patterns
  without manual setup.

  ## Examples

      # Wrap a function with retry logic
      ExMCP.Reliability.with_retry(fn ->
        ExMCP.Client.call_tool(client, "risky_tool", %{})
      end)

      # Create a circuit-breaker protected function
      protected_call = ExMCP.Reliability.protect(fn ->
        external_service_call()
      end, failure_threshold: 5)

      # Use the protected function
      protected_call.()
  """

  alias ExMCP.Reliability.Retry

  defdelegate with_retry(fun, opts \\ []), to: Retry

  @doc """
  Creates a circuit breaker and retry-protected version of a function.

  The function will be protected by a circuit breaker and retried
  according to the specified options on failure.

  ## Options

  - `:name` - Name for the circuit breaker process
  - `:failure_threshold` - Number of failures before circuit opens (default: 5)
  - `:success_threshold` - Number of successes to close circuit (default: 2)
  - `:timeout` - Timeout for each call (default: 5000)
  - `:reset_timeout` - Time before attempting to close open circuit (default: 30000)
  - Plus all retry options from `ExMCP.Reliability.Retry.mcp_defaults/1`
  """
  @spec protect(function(), keyword()) :: function()
  def protect(fun, opts \\ []) when is_function(fun) do
    # Extract circuit breaker specific options
    {cb_opts, retry_opts} =
      Keyword.split(opts, [
        :name,
        :failure_threshold,
        :success_threshold,
        :timeout,
        :reset_timeout
      ])

    # Create or get circuit breaker
    breaker_pid =
      case cb_opts[:name] do
        nil ->
          # Start anonymous circuit breaker
          {:ok, pid} =
            DynamicSupervisor.start_child(
              ExMCP.Reliability.Supervisor.CircuitBreakerSupervisor,
              {ExMCP.Reliability.CircuitBreaker, cb_opts}
            )

          pid

        name ->
          # Try to get existing breaker or start new one
          case Process.whereis(name) do
            nil ->
              case DynamicSupervisor.start_child(
                     ExMCP.Reliability.Supervisor.CircuitBreakerSupervisor,
                     {ExMCP.Reliability.CircuitBreaker, Keyword.put(cb_opts, :name, name)}
                   ) do
                {:ok, pid} -> pid
                {:error, {:already_started, pid}} -> pid
              end

            pid ->
              pid
          end
      end

    # Return a function that uses both circuit breaker and retry
    fn args ->
      cb_timeout = Keyword.get(cb_opts, :timeout, 5000)

      # Execute through circuit breaker
      case ExMCP.Reliability.CircuitBreaker.call(
             breaker_pid,
             fn ->
               # Inside circuit breaker, apply retry logic
               Retry.with_retry(fn -> apply(fun, args) end, Retry.mcp_defaults(retry_opts))
             end,
             cb_timeout
           ) do
        {:error, :circuit_open} = error ->
          error

        other ->
          other
      end
    end
  end
end
