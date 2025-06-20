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

  alias ExMCP.Reliability.{CircuitBreaker, HealthCheck, Retry}

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name)
    Supervisor.start_link(__MODULE__, opts, name: name)
  end

  @impl Supervisor
  def init(_opts) do
    children = [
      # Dynamic supervisor for circuit breakers
      {DynamicSupervisor, strategy: :one_for_one, name: circuit_breaker_supervisor()},

      # Dynamic supervisor for health checks
      {DynamicSupervisor, strategy: :one_for_one, name: health_check_supervisor()},

      # Registry for tracking reliability components
      {Registry, keys: :unique, name: reliability_registry()}
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

    with {:ok, client_pid} <- ExMCP.SimpleClient.start_link(transport_opts),
         {:ok, breaker_pid} <- maybe_start_circuit_breaker(supervisor, client_id, opts),
         {:ok, health_pid} <- maybe_start_health_check(supervisor, client_id, client_pid, opts) do
      # Create wrapper process that integrates all components
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

      case DynamicSupervisor.start_child(circuit_breaker_supervisor(), wrapper_spec) do
        {:ok, wrapper_pid} ->
          {:ok, wrapper_pid}

        error ->
          # Clean up on failure
          if is_pid(client_pid), do: GenServer.stop(client_pid)
          if is_pid(breaker_pid), do: GenServer.stop(breaker_pid)
          if is_pid(health_pid), do: GenServer.stop(health_pid)
          error
      end
    end
  end

  defp maybe_start_circuit_breaker(_supervisor, _client_id, opts) do
    case Keyword.get(opts, :circuit_breaker) do
      nil ->
        {:ok, nil}

      breaker_opts ->
        breaker_opts =
          Keyword.put(
            breaker_opts,
            :name,
            {:via, Registry,
             {reliability_registry(),
              {:circuit_breaker, Keyword.get(opts, :id, generate_client_id())}}}
          )

        spec = %{
          id: {:circuit_breaker, :erlang.unique_integer()},
          start: {CircuitBreaker, :start_link, [breaker_opts]},
          restart: :temporary
        }

        case DynamicSupervisor.start_child(circuit_breaker_supervisor(), spec) do
          {:ok, pid} -> {:ok, pid}
          error -> error
        end
    end
  end

  defp maybe_start_health_check(_supervisor, client_id, target_pid, opts) do
    case Keyword.get(opts, :health_check) do
      nil ->
        {:ok, nil}

      health_opts ->
        health_opts =
          health_opts
          |> Keyword.put(
            :name,
            {:via, Registry, {reliability_registry(), {:health_check, client_id}}}
          )
          |> Keyword.put(:target, target_pid)
          |> Keyword.put_new(:check_fn, HealthCheck.mcp_client_check_fn())

        spec = %{
          id: {:health_check, :erlang.unique_integer()},
          start: {HealthCheck, :start_link, [health_opts]},
          restart: :temporary
        }

        case DynamicSupervisor.start_child(health_check_supervisor(), spec) do
          {:ok, pid} -> {:ok, pid}
          error -> error
        end
    end
  end

  defp circuit_breaker_supervisor, do: Module.concat(__MODULE__, CircuitBreakerSupervisor)
  defp health_check_supervisor, do: Module.concat(__MODULE__, HealthCheckSupervisor)
  defp reliability_registry, do: Module.concat(__MODULE__, Registry)

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

  alias ExMCP.Reliability.{CircuitBreaker, Retry}

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
      fn -> ExMCP.SimpleClient.call_tool(state.client, tool_name, args, opts) end,
      from,
      state
    )
  end

  def handle_call({:list_tools, opts}, from, state) do
    execute_with_reliability(
      fn -> ExMCP.SimpleClient.list_tools(state.client, opts) end,
      from,
      state
    )
  end

  def handle_call({:list_resources, opts}, from, state) do
    execute_with_reliability(
      fn -> ExMCP.SimpleClient.list_resources(state.client, opts) end,
      from,
      state
    )
  end

  def handle_call({:read_resource, uri, opts}, from, state) do
    execute_with_reliability(
      fn -> ExMCP.SimpleClient.read_resource(state.client, uri, opts) end,
      from,
      state
    )
  end

  def handle_call({:list_prompts, opts}, from, state) do
    execute_with_reliability(
      fn -> ExMCP.SimpleClient.list_prompts(state.client, opts) end,
      from,
      state
    )
  end

  def handle_call({:get_prompt, name, args, opts}, from, state) do
    execute_with_reliability(
      fn -> ExMCP.SimpleClient.get_prompt(state.client, name, args, opts) end,
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
      result =
        if state.circuit_breaker do
          # Execute through circuit breaker with retry
          CircuitBreaker.call(state.circuit_breaker, fn ->
            Retry.with_retry(fun, Retry.mcp_defaults(state.retry_opts))
          end)
        else
          # Just retry without circuit breaker
          Retry.with_retry(fun, Retry.mcp_defaults(state.retry_opts))
        end

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
        ExMCP.SimpleClient.call_tool(client, "risky_tool", %{})
      end)

      # Create a circuit-breaker protected function
      protected_call = ExMCP.Reliability.protect(fn ->
        external_service_call()
      end, failure_threshold: 5)

      # Use the protected function
      protected_call.()
  """

  alias ExMCP.Reliability.{CircuitBreaker, Retry}

  defdelegate with_retry(fun, opts \\ []), to: Retry

  @doc """
  Creates a circuit-breaker protected version of a function.

  The circuit breaker is created on first use and reused for
  subsequent calls.
  """
  @spec protect(function(), keyword()) :: function()
  def protect(fun, opts \\ []) when is_function(fun) do
    breaker_name = Keyword.get(opts, :name, {:protect, :erlang.unique_integer()})

    fn args ->
      breaker =
        case Process.whereis(breaker_name) do
          nil ->
            {:ok, pid} = CircuitBreaker.start_link(Keyword.put(opts, :name, breaker_name))
            pid

          pid ->
            pid
        end

      CircuitBreaker.call(breaker, fn -> apply(fun, args) end)
    end
  end
end
