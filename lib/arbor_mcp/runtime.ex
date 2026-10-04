defmodule Arbor.MCP.Server.Runtime do
  @moduledoc """
  Supervises one independently owned MCP server and its bounded callback work.

  Stateful callbacks execute serially by default. Set `execution: :stateless`
  explicitly before increasing `max_concurrency`; stateless callbacks cannot
  change their initialized handler state.

  Runtime references survive child restarts. Requests are reserved against
  count and byte budgets before their payload enters the scheduler mailbox.

  `request/3` uses a temporary process alias: a finite `await_timeout` stops
  waiting without leaving late replies in the caller mailbox. It does not
  cancel accepted work; use `timeout` for the server deadline or `cancel/4` for
  explicit cancellation. Low-level `submit/3` and `await/2` deliver to the
  configured reply target. An `await/2` timeout leaves that delivery active,
  so the same token can be awaited again; its caller must monitor the runtime
  when abrupt runtime death also needs to end its wait.

  Low-level `submit/3` reply targets must be local process IDs or local process
  aliases. Erlang cannot validate whether a reference is an active alias:
  invalid references and deactivated aliases discard delivery without undoing
  accepted work or its state commit. `request/3` creates and manages its own
  alias, overriding any supplied `reply_to` option.

  `stop/2` starts one overall shutdown budget, then forcefully cleans explicitly
  registered children and callback tasks. A parent supervisor applies the same
  finite budget through this runtime's child specification. Forced cleanup may
  interrupt handler or store termination hooks; it does not promise persisted
  store data or cleanup of arbitrary processes spawned outside owned children.
  This stops the current runtime instance. A parent still applies its child
  restart policy, so a managed endpoint must be removed with
  `Supervisor.terminate_child(parent, child_id)` when it should remain stopped.
  Dynamically created descendants of custom store supervisors must register
  with the runtime shutdown guard before running work.
  """

  use Supervisor

  alias Arbor.MCP.Server.Runtime.{
    Admission,
    CallbackContext,
    Config,
    ExecutionSupervisor,
    Ref,
    ShutdownGuard
  }

  @type server :: pid() | atom() | {:global, term()} | {:via, module(), term()} | Ref.t()

  @spec start_link(keyword()) :: Supervisor.on_start()
  def start_link(opts) do
    with {:ok, config} <- Config.new(opts) do
      supervisor_opts = if Keyword.get(opts, :name), do: [name: opts[:name]], else: []
      Supervisor.start_link(__MODULE__, {opts, config}, supervisor_opts)
    end
  end

  def child_spec(opts) do
    %{
      id: Keyword.get(opts, :id, __MODULE__),
      start: {__MODULE__, :start_link, [opts]},
      type: :supervisor,
      shutdown: Keyword.get(opts, :shutdown_timeout_ms, 5_000)
    }
  end

  @impl true
  def init({opts, config}) do
    table = :ets.new(__MODULE__, [:set, :public, read_concurrency: true, write_concurrency: true])
    {:ok, guard} = ShutdownGuard.start(self(), table, config)
    :ets.insert(table, {:shutdown_guard, guard})
    Process.put({__MODULE__, :reference}, Ref.new(self(), table))
    runtime_opts = [table: table, supervisor: self(), config: config]

    children =
      [
        {Admission, runtime_opts}
      ] ++
        Enum.map(Keyword.get(opts, :store_children, []), &ShutdownGuard.owned_spec(&1, table)) ++
        [
          {ExecutionSupervisor, runtime_opts}
        ]

    Supervisor.init(children, strategy: :rest_for_one)
  end

  @spec ref(server()) :: {:ok, Ref.t()} | {:error, :runtime_unavailable}
  def ref(server) do
    case Ref.validate(server) do
      {:ok, runtime} -> {:ok, runtime}
      {:error, _reason} -> resolve_reference(server)
    end
  end

  defp resolve_reference(server) do
    with {:ok, address} <- Ref.address(server),
         pid when is_pid(pid) <- GenServer.whereis(address),
         true <- node(pid) == node(),
         {:supervisor, __MODULE__, 1} <- :proc_lib.translate_initial_call(pid),
         {:dictionary, dictionary} <- Process.info(pid, :dictionary),
         {{__MODULE__, :reference}, runtime} <-
           List.keyfind(dictionary, {__MODULE__, :reference}, 0) do
      Ref.validate(runtime)
    else
      _ -> {:error, :runtime_unavailable}
    end
  catch
    :exit, _reason -> {:error, :runtime_unavailable}
  end

  @spec stop(server(), term()) :: :ok | {:error, :runtime_unavailable}
  def stop(server, reason \\ :normal) do
    with {:ok, runtime} <- ref(server) do
      Process.unlink(Ref.supervisor(runtime))
      ShutdownGuard.stop(Ref.table(runtime), reason)
    end
  end

  @doc false
  def submit(server, request, opts \\ []) when is_map(request) do
    with {:ok, runtime} <- ref(server),
         {:ok, route, reservation} <- Admission.reserve(runtime, request, opts) do
      opts = [runtime: runtime, dispatch_opts: Keyword.get(opts, :dispatch_opts, [])]
      send(route.scheduler, {:submit, route.generation, reservation.token, request, opts})
      {:ok, reservation.token}
    end
  end

  @doc false
  def request(server, request, opts \\ []) do
    with :ok <- validate_await_timeout(Keyword.get(opts, :await_timeout, :infinity)),
         {:ok, runtime} <- ref(server) do
      monitor = Process.monitor(Ref.supervisor(runtime))
      reply_alias = :erlang.alias()

      try do
        request_with_alias(runtime, request, opts, monitor, reply_alias)
      after
        :erlang.unalias(reply_alias)
        Process.demonitor(monitor, [:flush])
      end
    end
  end

  @doc false
  def await(token, timeout \\ :infinity) do
    receive do
      {:arbor_mcp_runtime, ^token, result} -> result
    after
      timeout -> {:error, :await_timeout}
    end
  end

  @doc false
  def cancel(server, scope, request_id, opts \\ []) do
    with {:ok, runtime} <- ref(server),
         {:ok, route} <- Admission.route(Ref.table(runtime)) do
      key = Admission.key(scope, request_id, Keyword.get(opts, :direction, :inbound))
      control = {{:cancel_control, key}, self()}

      if :ets.member(Ref.table(runtime), {:request, key}) and
           :ets.insert_new(Ref.table(runtime), control) do
        try do
          GenServer.call(route.scheduler, {:cancel, route.generation, key}, 5_000)
        after
          :ets.delete_object(Ref.table(runtime), control)
        end
      else
        :ok
      end
    end
  rescue
    ArgumentError -> {:error, :runtime_unavailable}
  catch
    :exit, _reason -> {:error, :runtime_unavailable}
  end

  @doc false
  def stats(server) do
    with {:ok, runtime} <- ref(server),
         {:ok, route} <- Admission.route(Ref.table(runtime)) do
      scheduler = GenServer.call(route.scheduler, :stats)
      Map.merge(scheduler, Admission.stats(Ref.table(runtime)))
    end
  catch
    :exit, _reason -> {:error, :runtime_unavailable}
  end

  @doc false
  def cancelled?, do: CallbackContext.cancelled?()

  defp request_with_alias(runtime, request, opts, monitor, reply_alias) do
    with {:ok, token} <- submit(runtime, request, Keyword.put(opts, :reply_to, reply_alias)) do
      try do
        receive do
          {:arbor_mcp_runtime, ^token, result} -> result
          {:DOWN, ^monitor, :process, _pid, _reason} -> {:error, :runtime_unavailable}
        after
          Keyword.get(opts, :await_timeout, :infinity) -> {:error, :await_timeout}
        end
      after
        :erlang.unalias(reply_alias)

        receive do
          {:arbor_mcp_runtime, ^token, _late_reply} -> :ok
        after
          0 -> :ok
        end
      end
    end
  end

  defp validate_await_timeout(:infinity), do: :ok
  defp validate_await_timeout(timeout) when is_integer(timeout) and timeout >= 0, do: :ok
  defp validate_await_timeout(_timeout), do: {:error, :invalid_await_timeout}
end
