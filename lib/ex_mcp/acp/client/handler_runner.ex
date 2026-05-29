defmodule ExMCP.ACP.Client.HandlerRunner do
  @moduledoc false

  use GenServer

  require Logger

  defstruct [:handler_mod, :handler_state, :owner]

  def start_link(handler_mod, handler_opts, owner) do
    GenServer.start_link(__MODULE__, {handler_mod, handler_opts, owner})
  end

  def session_update(pid, session_id, update) do
    GenServer.cast(pid, {:session_update, session_id, update})
  end

  def permission_request(pid, ref, session_id, tool_call, options) do
    GenServer.cast(pid, {:permission_request, ref, session_id, tool_call, options})
  end

  def file_read(pid, ref, session_id, path, opts) do
    GenServer.cast(pid, {:file_read, ref, session_id, path, opts})
  end

  def file_write(pid, ref, session_id, path, content) do
    GenServer.cast(pid, {:file_write, ref, session_id, path, content})
  end

  def terminal_request(pid, ref, method, params, id) do
    GenServer.cast(pid, {:terminal_request, ref, method, params, id})
  end

  @impl true
  def init({handler_mod, handler_opts, owner}) do
    Process.flag(:trap_exit, true)

    case safe_call(fn -> handler_mod.init(handler_opts) end) do
      {:ok, {:ok, handler_state}} ->
        {:ok, %__MODULE__{handler_mod: handler_mod, handler_state: handler_state, owner: owner}}

      {:ok, {:error, reason}} ->
        {:stop, {:handler_init_failed, reason}}

      {:ok, other} ->
        {:stop, {:handler_init_failed, {:invalid_return, other}}}

      {:error, reason} ->
        {:stop, {:handler_init_failed, reason}}
    end
  end

  @impl true
  def handle_cast({:session_update, session_id, update}, state) do
    case safe_call(fn ->
           state.handler_mod.handle_session_update(session_id, update, state.handler_state)
         end) do
      {:ok, {:ok, handler_state}} ->
        {:noreply, %{state | handler_state: handler_state}}

      {:ok, other} ->
        Logger.warning("ACP handler returned invalid session update result: #{inspect(other)}")
        {:noreply, state}

      {:error, reason} ->
        Logger.warning("ACP handler session update failed: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  def handle_cast({:permission_request, ref, session_id, tool_call, options}, state) do
    {result, state} =
      case safe_call(fn ->
             state.handler_mod.handle_permission_request(
               session_id,
               tool_call,
               options,
               state.handler_state
             )
           end) do
        {:ok, {:ok, outcome, handler_state}} ->
          {{:ok, outcome}, %{state | handler_state: handler_state}}

        {:ok, {:error, reason, handler_state}} ->
          {{:error, reason}, %{state | handler_state: handler_state}}

        {:ok, other} ->
          {{:error, {:invalid_return, other}}, state}

        {:error, reason} ->
          {{:error, reason}, state}
      end

    send(state.owner, {:acp_handler_result, ref, {:permission, result}})
    {:noreply, state}
  end

  def handle_cast({:file_read, ref, session_id, path, opts}, state) do
    {result, state} =
      case safe_call(fn ->
             state.handler_mod.handle_file_read(session_id, path, opts, state.handler_state)
           end) do
        {:ok, {:ok, content, handler_state}} ->
          {{:ok, content}, %{state | handler_state: handler_state}}

        {:ok, {:error, reason, handler_state}} ->
          {{:error, reason}, %{state | handler_state: handler_state}}

        {:ok, other} ->
          {{:error, {:invalid_return, other}}, state}

        {:error, reason} ->
          {{:error, reason}, state}
      end

    send(state.owner, {:acp_handler_result, ref, {:file_read, result}})
    {:noreply, state}
  end

  def handle_cast({:file_write, ref, session_id, path, content}, state) do
    {result, state} =
      case safe_call(fn ->
             state.handler_mod.handle_file_write(session_id, path, content, state.handler_state)
           end) do
        {:ok, {:ok, handler_state}} ->
          {:ok, %{state | handler_state: handler_state}}

        {:ok, {:error, reason, handler_state}} ->
          {{:error, reason}, %{state | handler_state: handler_state}}

        {:ok, other} ->
          {{:error, {:invalid_return, other}}, state}

        {:error, reason} ->
          {{:error, reason}, state}
      end

    send(state.owner, {:acp_handler_result, ref, {:file_write, result}})
    {:noreply, state}
  end

  def handle_cast({:terminal_request, ref, method, params, id}, state) do
    {result, state} =
      case safe_call(fn ->
             state.handler_mod.handle_terminal_request(method, params, id, state.handler_state)
           end) do
        {:ok, {:ok, response, handler_state}} ->
          {{:ok, response}, %{state | handler_state: handler_state}}

        {:ok, {:error, reason, handler_state}} ->
          {{:error, reason}, %{state | handler_state: handler_state}}

        {:ok, other} ->
          {{:error, {:invalid_return, other}}, state}

        {:error, reason} ->
          {{:error, reason}, state}
      end

    send(state.owner, {:acp_handler_result, ref, {:terminal, result}})
    {:noreply, state}
  end

  @impl true
  def terminate(reason, state) do
    if function_exported?(state.handler_mod, :terminate, 2) do
      _ = safe_call(fn -> state.handler_mod.terminate(reason, state.handler_state) end)
    end

    :ok
  end

  defp safe_call(fun) do
    {:ok, fun.()}
  catch
    kind, reason ->
      {:error, {kind, reason, __STACKTRACE__}}
  end
end
