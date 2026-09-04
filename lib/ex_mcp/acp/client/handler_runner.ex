defmodule ExMCP.ACP.Client.HandlerRunner do
  @moduledoc false

  use GenServer

  require Logger

  defstruct [:handler_mod, :handler_state, :owner]

  def start_link(handler_mod, handler_opts, owner) do
    GenServer.start_link(__MODULE__, {handler_mod, handler_opts, owner})
  end

  # rc.6 compatibility: unbounded enqueue used Client defaults after security harden.
  @default_max_update_queue 32
  @default_max_update_queue_bytes 8_388_608

  @doc false
  def session_update(pid, session_id, update) do
    _ =
      session_update(
        pid,
        session_id,
        update,
        @default_max_update_queue,
        @default_max_update_queue_bytes
      )

    :ok
  end

  def session_update(pid, session_id, update, max_queue, max_queue_bytes) do
    session_update(pid, session_id, update, max_queue, max_queue_bytes, nil)
  end

  # `message` is the decoded JSON-RPC envelope, passed only when the handler
  # exports the context-aware callback (the Client checks that). It embeds the
  # update, so its size is the retained size when present.
  def session_update(pid, session_id, update, max_queue, max_queue_bytes, message) do
    incoming_bytes = update_size(update, message)

    if update_mailbox_below_limits?(pid, max_queue, max_queue_bytes, incoming_bytes) do
      GenServer.cast(pid, {:session_update, session_id, update, message})
      :ok
    else
      :dropped
    end
  end

  defp update_mailbox_below_limits?(pid, count_limit, byte_limit, incoming_bytes) do
    with {:message_queue_len, length} when length < count_limit <-
           Process.info(pid, :message_queue_len),
         {:messages, messages} when length(messages) < count_limit <-
           Process.info(pid, :messages) do
      queued_bytes = queued_update_bytes(messages, byte_limit)
      incoming_bytes <= byte_limit - queued_bytes
    else
      _full_or_closed -> false
    end
  end

  defp queued_update_bytes(messages, limit) do
    Enum.reduce_while(messages, 0, fn message, total ->
      total = total + queued_update_size(message)
      if total >= limit, do: {:halt, total}, else: {:cont, total}
    end)
  end

  defp queued_update_size({:"$gen_cast", {:session_update, _session_id, update, message}}),
    do: update_size(update, message)

  defp queued_update_size(_message), do: 0

  defp update_size(update, nil), do: :erlang.external_size(update)
  defp update_size(_update, message), do: :erlang.external_size(message)

  def permission_request(pid, ref, session_id, tool_call, options, message) do
    GenServer.cast(pid, {:permission_request, ref, session_id, tool_call, options, message})
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

  def elicitation_request(pid, ref, mode, params) do
    GenServer.cast(pid, {:elicitation_request, ref, mode, params})
  end

  def elicitation_complete(pid, elicitation_id) do
    GenServer.cast(pid, {:elicitation_complete, elicitation_id})
  end

  # Each of these has a legacy arity and a context-aware arity that also takes
  # the decoded JSON-RPC message. Both are optional on the behaviour so a
  # handler can implement just one, but it must export at least one of each
  # or updates and permission requests would fail at first dispatch.
  @callback_pairs [handle_session_update: [3, 4], handle_permission_request: [4, 5]]

  @impl true
  def init({handler_mod, handler_opts, owner}) do
    Process.flag(:trap_exit, true)

    case missing_callback(handler_mod) do
      nil -> init_handler(handler_mod, handler_opts, owner)
      callback -> {:stop, {:handler_init_failed, {:missing_callback, callback}}}
    end
  end

  defp init_handler(handler_mod, handler_opts, owner) do
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

  defp missing_callback(handler_mod) do
    Code.ensure_loaded(handler_mod)

    Enum.find_value(@callback_pairs, fn {callback, arities} ->
      if not Enum.any?(arities, &function_exported?(handler_mod, callback, &1)), do: callback
    end)
  end

  @impl true
  def handle_cast({:session_update, session_id, update, message}, state) do
    case safe_call(fn ->
           call_with_context(state, :handle_session_update, [session_id, update], message)
         end) do
      {:ok, {:ok, handler_state}} ->
        {:noreply, %{state | handler_state: handler_state}}

      {:ok, other} ->
        Logger.warning("ACP handler returned invalid session update result",
          return_shape: return_shape(other)
        )

        {:noreply, state}

      {:error, reason} ->
        Logger.warning("ACP handler session update failed", error_class: error_class(reason))
        {:noreply, state}
    end
  end

  def handle_cast({:permission_request, ref, session_id, tool_call, options, message}, state) do
    {result, state} =
      case safe_call(fn ->
             call_with_context(
               state,
               :handle_permission_request,
               [session_id, tool_call, options],
               message
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

  def handle_cast({:elicitation_request, ref, mode, params}, state) do
    callback = if mode == "form", do: :handle_form_elicitation, else: :handle_url_elicitation

    {result, state} =
      case safe_call(fn -> apply(state.handler_mod, callback, [params, state.handler_state]) end) do
        {:ok, {:ok, response, handler_state}} ->
          {{:ok, response}, %{state | handler_state: handler_state}}

        {:ok, {:error, reason, handler_state}} ->
          {{:error, reason}, %{state | handler_state: handler_state}}

        {:ok, other} ->
          {{:error, {:invalid_return, other}}, state}

        {:error, reason} ->
          {{:error, reason}, state}
      end

    send(state.owner, {:acp_handler_result, ref, {:elicitation, result}})
    {:noreply, state}
  end

  def handle_cast({:elicitation_complete, elicitation_id}, state) do
    state =
      case safe_call(fn ->
             state.handler_mod.handle_elicitation_complete(elicitation_id, state.handler_state)
           end) do
        {:ok, {:ok, handler_state}} -> %{state | handler_state: handler_state}
        _invalid_or_failed -> state
      end

    {:noreply, state}
  end

  @impl true
  def terminate(reason, state) do
    if function_exported?(state.handler_mod, :terminate, 2) do
      _ = safe_call(fn -> state.handler_mod.terminate(reason, state.handler_state) end)
    end

    :ok
  end

  # The Client passes `message` only when the handler exports the context
  # variant, so its presence selects the arity.
  defp call_with_context(state, callback, args, nil),
    do: apply(state.handler_mod, callback, args ++ [state.handler_state])

  defp call_with_context(state, callback, args, message),
    do: apply(state.handler_mod, callback, args ++ [message, state.handler_state])

  defp safe_call(fun) do
    {:ok, fun.()}
  catch
    kind, reason ->
      {:error, {kind, reason, __STACKTRACE__}}
  end

  defp return_shape(value) when is_tuple(value), do: {:tuple, tuple_size(value)}
  defp return_shape(value) when is_map(value), do: :map
  defp return_shape(value) when is_list(value), do: :list
  defp return_shape(value) when is_atom(value), do: :atom
  defp return_shape(_value), do: :other

  defp error_class({kind, reason, _stack}) when kind in [:error, :exit, :throw],
    do: {kind, exception_module(reason)}

  defp exception_module(%{__struct__: module}) when is_atom(module), do: module
  defp exception_module(_reason), do: :non_exception
end
