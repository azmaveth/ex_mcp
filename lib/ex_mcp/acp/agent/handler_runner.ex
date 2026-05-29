defmodule ExMCP.ACP.Agent.HandlerRunner do
  @moduledoc false

  use GenServer

  require Logger

  defstruct [:handler_mod, :handler_state, :owner]

  def start_link(handler_mod, handler_opts, owner) do
    GenServer.start_link(__MODULE__, {handler_mod, handler_opts, owner})
  end

  def initialize(pid, ref, params, ctx) do
    GenServer.cast(pid, {:callback, ref, :handle_initialize, [params, ctx]})
  end

  def authenticate(pid, ref, params, ctx) do
    GenServer.cast(pid, {:callback, ref, :handle_authenticate, [params, ctx]})
  end

  def logout(pid, ref, ctx) do
    GenServer.cast(pid, {:callback, ref, :handle_logout, [ctx]})
  end

  def new_session(pid, ref, params, ctx) do
    GenServer.cast(pid, {:callback, ref, :handle_new_session, [params, ctx]})
  end

  def load_session(pid, ref, params, ctx) do
    GenServer.cast(pid, {:callback, ref, :handle_load_session, [params, ctx]})
  end

  def list_sessions(pid, ref, params, ctx) do
    GenServer.cast(pid, {:callback, ref, :handle_list_sessions, [params, ctx]})
  end

  def resume_session(pid, ref, params, ctx) do
    GenServer.cast(pid, {:callback, ref, :handle_resume_session, [params, ctx]})
  end

  def close_session(pid, ref, session_id, ctx) do
    GenServer.cast(pid, {:callback, ref, :handle_close_session, [session_id, ctx]})
  end

  def prompt(pid, ref, session_id, prompt, ctx) do
    GenServer.cast(pid, {:callback, ref, :handle_prompt, [session_id, prompt, ctx]})
  end

  def set_mode(pid, ref, session_id, mode_id, ctx) do
    GenServer.cast(pid, {:callback, ref, :handle_set_mode, [session_id, mode_id, ctx]})
  end

  def set_config_option(pid, ref, session_id, config_id, value, ctx) do
    GenServer.cast(
      pid,
      {:callback, ref, :handle_set_config_option, [session_id, config_id, value, ctx]}
    )
  end

  def cancel(pid, ref, session_id, ctx) do
    GenServer.cast(pid, {:callback, ref, :handle_cancel, [session_id, ctx]})
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
  def handle_cast({:callback, ref, callback, args}, state) do
    {result, state} =
      state.handler_mod
      |> invoke(callback, args, state.handler_state)
      |> normalize_callback_result(state)

    send(state.owner, {:acp_agent_handler_result, ref, result})
    {:noreply, state}
  end

  @impl true
  def terminate(reason, state) do
    if function_exported?(state.handler_mod, :terminate, 2) do
      _ = safe_call(fn -> state.handler_mod.terminate(reason, state.handler_state) end)
    end

    :ok
  end

  defp invoke(handler_mod, callback, args, handler_state) do
    if function_exported?(handler_mod, callback, length(args) + 1) do
      safe_call(fn -> apply(handler_mod, callback, args ++ [handler_state]) end)
    else
      {:error, {:missing_callback, callback}}
    end
  end

  defp normalize_callback_result({:ok, {:reply, result, handler_state}}, state) do
    {{:reply, result}, %{state | handler_state: handler_state}}
  end

  defp normalize_callback_result({:ok, {:ok, result, handler_state}}, state) do
    {{:reply, result}, %{state | handler_state: handler_state}}
  end

  defp normalize_callback_result({:ok, {:ok, handler_state}}, state) do
    {{:reply, %{}}, %{state | handler_state: handler_state}}
  end

  defp normalize_callback_result({:ok, {:noreply, handler_state}}, state) do
    {:noreply, %{state | handler_state: handler_state}}
  end

  defp normalize_callback_result({:ok, {:error, reason, handler_state}}, state) do
    {{:error, reason}, %{state | handler_state: handler_state}}
  end

  defp normalize_callback_result({:ok, other}, state) do
    Logger.warning("ACP agent handler returned invalid result: #{inspect(other)}")
    {{:error, {:invalid_return, other}}, state}
  end

  defp normalize_callback_result({:error, reason}, state) do
    {{:error, reason}, state}
  end

  defp safe_call(fun) do
    {:ok, fun.()}
  catch
    kind, reason ->
      {:error, {kind, reason, __STACKTRACE__}}
  end
end
