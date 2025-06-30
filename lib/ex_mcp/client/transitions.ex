defmodule ExMCP.Client.Transitions do
  @moduledoc """
  State transition logic and guards for the ExMCP client state machine.

  This module centralizes all state transition logic, including:
  - Guards to prevent invalid transitions
  - Data transformation functions
  - Error handling and cleanup
  """

  require Logger

  alias ExMCP.Client.States
  alias ExMCP.Client.States.{Connecting, Disconnected, Handshaking, Ready, Reconnecting}

  # Guards

  @doc """
  Checks if the client can connect from the current state.
  """
  def can_connect?(%Disconnected{retry_count: count}) when count > 10 do
    {:error, :too_many_retries}
  end

  def can_connect?(%Disconnected{}), do: true
  def can_connect?(_), do: {:error, :already_connected}

  @doc """
  Checks if a request can be sent in the current state.
  """
  def can_send_request?(%Ready{}), do: true
  def can_send_request?(_), do: false

  # State Transitions

  @doc """
  Transitions from disconnected to connecting state.
  """
  def to_connecting(%Disconnected{common: common, retry_count: count}) do
    %Connecting{
      common: common,
      # Will be set when supervisor starts
      supervisor_pid: nil,
      start_time: System.monotonic_time(:millisecond),
      attempt_number: count + 1
    }
  end

  @doc """
  Transitions from connecting to handshaking state.
  """
  def to_handshaking(%Connecting{common: common}, transport_info) do
    client_info = build_client_info()

    %Handshaking{
      common: common,
      # {module, state, receiver_pid}
      transport: transport_info,
      # Deprecated, kept for compatibility
      transport_state: nil,
      client_info: client_info,
      handshake_start_time: System.monotonic_time(:millisecond),
      from_reconnecting: nil
    }
  end

  @doc """
  Transitions from handshaking to ready state.
  """
  def to_ready(%Handshaking{common: common, transport: transport_info}, server_info) do
    server_name =
      case server_info["serverInfo"] do
        %{"name" => name} -> name
        _ -> "unknown"
      end

    Logger.info("Connected to MCP server: #{server_name}")

    # Extract transport components
    {_transport_module, transport_state, _receiver_pid} = transport_info

    %Ready{
      common: common,
      transport: transport_info,
      # For compatibility
      transport_state: transport_state,
      server_info: server_info,
      capabilities: server_info["capabilities"] || %{},
      pending_requests: %{},
      next_request_id: 1,
      progress_callbacks: %{},
      initialized_capability: nil
    }
    |> maybe_call_initialize_callback()
  end

  @doc """
  Transitions to disconnected state from any state.
  """
  def to_disconnected(current_state, reason) do
    common = States.get_common(current_state)

    # Clean up resources
    cleanup_state(current_state)

    # Call disconnect callback if provided
    maybe_call_disconnect_callback(common, reason)

    %Disconnected{
      common: common,
      last_error: extract_error(reason),
      retry_count: increment_retry_count(current_state),
      disconnect_reason: reason
    }
  end

  @doc """
  Transitions from ready to reconnecting state.
  """
  def to_reconnecting(
        %Ready{common: common, transport: transport, server_info: server_info},
        reason
      ) do
    Logger.info("Connection lost, attempting to reconnect: #{inspect(reason)}")

    # Get initial backoff from config or default to 1000ms
    initial_backoff = common.config[:reconnect_backoff_ms] || 1000

    %Reconnecting{
      common: common,
      last_transport: transport,
      last_server_info: server_info,
      backoff_ms: initial_backoff,
      attempt_number: 1,
      max_attempts: get_max_reconnect_attempts(common.config),
      reconnect_timer: nil
    }
  end

  # Request Management

  @doc """
  Adds a pending request to the ready state.
  """
  def add_request(
        %Ready{
          pending_requests: pending,
          next_request_id: id,
          progress_callbacks: progress_callbacks
        } = state,
        from,
        method,
        params,
        opts
      ) do
    request = %{
      id: id,
      method: method,
      params: params,
      from: from,
      timeout: opts[:timeout] || 5000,
      start_time: System.monotonic_time(:millisecond),
      progress_token: opts[:progress_token]
    }

    # Handle progress tracking if callback and token are provided
    new_progress_callbacks =
      case {opts[:progress_callback], opts[:progress_token]} do
        {callback, token} when is_function(callback) and not is_nil(token) ->
          # Register with ProgressTracker for token management and rate limiting
          # We pass a dummy pid since we handle notifications through transport, not direct messages
          case ExMCP.ProgressTracker.start_progress(token, spawn(fn -> :ok end)) do
            {:ok, _progress_state} ->
              # Store callback for this token
              Map.put(progress_callbacks, token, callback)

            {:error, :token_exists} ->
              # Token already in use - log warning but still store callback
              require Logger
              Logger.warning("Progress token already in use: #{token}")
              Map.put(progress_callbacks, token, callback)

            {:error, reason} ->
              require Logger
              Logger.error("Failed to start progress tracking: #{inspect(reason)}")
              progress_callbacks
          end

        _ ->
          progress_callbacks
      end

    new_state = %{
      state
      | pending_requests: Map.put(pending, id, request),
        next_request_id: id + 1,
        progress_callbacks: new_progress_callbacks
    }

    {:ok, new_state, id}
  end

  def add_request(_state, _from, _method, _params, _opts) do
    {:error, :not_ready}
  end

  @doc """
  Marks a request as failed and replies to the caller.
  """
  def fail_request(
        %Ready{pending_requests: pending, progress_callbacks: progress_callbacks} = state,
        request_id,
        reason
      ) do
    case Map.get(pending, request_id) do
      nil ->
        state

      request ->
        # Cancel timeout timer if present
        case Map.get(request, :timer_ref) do
          nil -> :ok
          timer_ref -> Process.cancel_timer(timer_ref)
        end

        # Calculate request duration
        duration = System.monotonic_time(:millisecond) - request.start_time

        # Emit telemetry for failed request
        :telemetry.execute(
          [:ex_mcp, :client, :request, :error],
          %{count: 1, duration: duration},
          %{method: request.method, request_id: request_id, reason: reason}
        )

        # Reply to the caller
        GenStateMachine.reply(request.from, {:error, reason})

        # Clean up progress tracking if this request had a progress token
        new_progress_callbacks =
          cleanup_progress_tracking(progress_callbacks, request.progress_token)

        # Remove from pending
        %{
          state
          | pending_requests: Map.delete(pending, request_id),
            progress_callbacks: new_progress_callbacks
        }
    end
  end

  @doc """
  Completes a request with a successful response.
  """
  def complete_request(
        %Ready{pending_requests: pending, progress_callbacks: progress_callbacks} = state,
        request_id,
        result
      ) do
    case Map.get(pending, request_id) do
      nil ->
        state

      request ->
        # Cancel timeout timer if present
        case Map.get(request, :timer_ref) do
          nil -> :ok
          timer_ref -> Process.cancel_timer(timer_ref)
        end

        # Calculate request duration
        duration = System.monotonic_time(:millisecond) - request.start_time

        # Emit telemetry for successful request
        :telemetry.execute(
          [:ex_mcp, :client, :request, :success],
          %{count: 1, duration: duration},
          %{method: request.method, request_id: request_id}
        )

        # Reply to the caller
        GenStateMachine.reply(request.from, {:ok, result})

        # Clean up progress tracking if this request had a progress token
        new_progress_callbacks =
          cleanup_progress_tracking(progress_callbacks, request.progress_token)

        # Remove from pending
        %{
          state
          | pending_requests: Map.delete(pending, request_id),
            progress_callbacks: new_progress_callbacks
        }
    end
  end

  # Private Helper Functions

  defp build_client_info do
    %{
      "name" => "ExMCP Client",
      "version" => "0.6.0",
      "protocolVersion" => "2025-03-26",
      "capabilities" => %{
        "sampling" => true,
        "tools" => true,
        "resources" => true,
        "prompts" => true,
        "roots" => true
      }
    }
  end

  defp cleanup_state(%Ready{pending_requests: pending}) do
    # Reply with errors to all pending requests
    Enum.each(pending, fn {_id, request} ->
      GenStateMachine.reply(request.from, {:error, :disconnected})
    end)
  end

  defp cleanup_state(%Reconnecting{reconnect_timer: timer}) when is_reference(timer) do
    Process.cancel_timer(timer)
  end

  defp cleanup_state(_), do: :ok

  defp extract_error({:error, reason}), do: reason
  defp extract_error(reason), do: reason

  defp increment_retry_count(%Disconnected{retry_count: count}), do: count + 1
  defp increment_retry_count(%Reconnecting{attempt_number: num}), do: num
  defp increment_retry_count(_), do: 0

  defp get_max_reconnect_attempts(config) do
    config[:max_reconnect_attempts] || 10
  end

  defp maybe_call_initialize_callback(
         %Ready{common: %{callbacks: %{on_initialize: callback}}} = state
       )
       when is_function(callback) do
    case callback.(state.server_info) do
      {:ok, capability_name} ->
        %{state | initialized_capability: capability_name}

      _ ->
        state
    end
  end

  defp maybe_call_initialize_callback(state), do: state

  defp maybe_call_disconnect_callback(%{callbacks: %{on_disconnect: callback}}, reason)
       when is_function(callback) do
    callback.(reason)
  end

  defp maybe_call_disconnect_callback(_, _), do: :ok

  # Progress tracking cleanup
  defp cleanup_progress_tracking(progress_callbacks, nil), do: progress_callbacks

  defp cleanup_progress_tracking(progress_callbacks, progress_token) do
    # Complete progress tracking in ProgressTracker
    case ExMCP.ProgressTracker.complete_progress(progress_token) do
      :ok ->
        require Logger
        Logger.debug("Progress tracking completed for token: #{progress_token}")

      {:error, :not_found} ->
        require Logger
        Logger.debug("Progress token not found in ProgressTracker: #{progress_token}")
    end

    # Remove callback from state
    Map.delete(progress_callbacks, progress_token)
  end
end
