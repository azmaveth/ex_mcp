defmodule ExMCP.ACP.Agent.Transport.Memory do
  @moduledoc """
  In-memory ACP transport for tests and local integration.

  The same module can be used by `ExMCP.ACP.Agent` with `role: :agent` and by
  `ExMCP.ACP.Client` with `role: :client`.
  """

  use GenServer

  @behaviour ExMCP.Transport

  defstruct [:peer, :role]

  @roles [:client, :agent]

  @doc "Starts a shared in-memory transport endpoint."
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc "Creates a shared endpoint for a client/agent pair."
  @spec new_pair(keyword()) :: GenServer.on_start()
  def new_pair(opts \\ []), do: start_link(opts)

  @impl true
  def init(_opts) do
    {:ok,
     %{
       queues: %{client: :queue.new(), agent: :queue.new()},
       waiters: %{client: :queue.new(), agent: :queue.new()},
       closed?: false
     }}
  end

  @impl true
  def connect(opts) do
    peer = Keyword.fetch!(opts, :peer)
    role = Keyword.get(opts, :role, :client)

    unless role in @roles do
      raise ArgumentError, "expected :role to be :client or :agent"
    end

    {:ok, %__MODULE__{peer: peer, role: role}}
  end

  @impl true
  def send_message(message, %__MODULE__{peer: peer, role: role} = state) do
    case GenServer.call(peer, {:send, role, message}) do
      :ok -> {:ok, state}
      {:error, reason} -> {:error, reason}
    end
  end

  @impl true
  def receive_message(%__MODULE__{peer: peer, role: role} = state) do
    case GenServer.call(peer, {:receive, role}, :infinity) do
      {:ok, message} -> {:ok, message, state}
      {:error, reason} -> {:error, reason}
    end
  end

  @impl true
  def close(%__MODULE__{peer: peer}) do
    GenServer.call(peer, :close)
  catch
    :exit, _ -> :ok
  end

  @impl true
  def connected?(%__MODULE__{peer: peer}) do
    is_pid(peer) and Process.alive?(peer)
  end

  @impl true
  def handle_call({:send, _role, _message}, _from, %{closed?: true} = state) do
    {:reply, {:error, :closed}, state}
  end

  def handle_call({:send, role, message}, _from, state) do
    target = opposite(role)

    case :queue.out(state.waiters[target]) do
      {{:value, waiter}, rest} ->
        GenServer.reply(waiter, {:ok, message})
        waiters = Map.put(state.waiters, target, rest)
        {:reply, :ok, %{state | waiters: waiters}}

      {:empty, _} ->
        queues = Map.update!(state.queues, target, &:queue.in(message, &1))
        {:reply, :ok, %{state | queues: queues}}
    end
  end

  def handle_call({:receive, _role}, _from, %{closed?: true} = state) do
    {:reply, {:error, :closed}, state}
  end

  def handle_call({:receive, role}, from, state) do
    case :queue.out(state.queues[role]) do
      {{:value, message}, rest} ->
        queues = Map.put(state.queues, role, rest)
        {:reply, {:ok, message}, %{state | queues: queues}}

      {:empty, _} ->
        waiters = Map.update!(state.waiters, role, &:queue.in(from, &1))
        {:noreply, %{state | waiters: waiters}}
    end
  end

  def handle_call(:close, _from, state) do
    reply_waiters(state.waiters.client)
    reply_waiters(state.waiters.agent)
    {:reply, :ok, %{state | closed?: true}}
  end

  defp opposite(:client), do: :agent
  defp opposite(:agent), do: :client

  defp reply_waiters(waiters) do
    case :queue.out(waiters) do
      {{:value, waiter}, rest} ->
        GenServer.reply(waiter, {:error, :closed})
        reply_waiters(rest)

      {:empty, _} ->
        :ok
    end
  end
end
