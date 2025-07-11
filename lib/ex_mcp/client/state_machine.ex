defmodule ExMCP.Client.StateMachine do
  @moduledoc """
  GenStateMachine implementation for ExMCP.Client.

  This module formalizes the client's state transitions and reduces complexity
  by using state-specific data structures instead of a single monolithic state.

  ## States

  - `:disconnected` - No active connection
  - `:connecting` - Transport connection being established
  - `:handshaking` - MCP protocol handshake in progress
  - `:ready` - Connected and ready to handle requests
  - `:reconnecting` - Connection lost, attempting to reconnect

  ## State Transitions

  ```
  disconnected -> connecting -> handshaking -> ready
        ^                                        |
        |                                        |
        +---------- reconnecting <---------------+
  ```
  """

  use GenStateMachine, callback_mode: [:handle_event_function, :state_enter]

  require Logger

  alias ExMCP.Client.States
  alias ExMCP.Client.Transitions

  @type state_name :: :disconnected | :connecting | :handshaking | :ready | :reconnecting

  # Client API

  @doc """
  Starts a new client state machine.
  """
  def start_link(config, opts \\ []) do
    GenStateMachine.start_link(__MODULE__, {config, opts}, opts)
  end

  @doc """
  Initiates connection to the MCP server.
  """
  def connect(client) do
    GenStateMachine.call(client, :connect)
  end

  @doc """
  Disconnects from the MCP server.
  """
  def disconnect(client) do
    GenStateMachine.call(client, :disconnect)
  end

  @doc """
  Sends a request to the MCP server.
  """
  def request(client, method, params, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5000)
    # Add buffer to GenStateMachine call timeout to allow proper request timeout handling
    call_timeout = timeout + 1000
    GenStateMachine.call(client, {:request, method, params, opts}, call_timeout)
  end

  @doc """
  Gets the current state of the client.
  """
  def get_state(client) do
    GenStateMachine.call(client, :get_state)
  end

  @doc """
  Gets the internal state of the client (for adapter use).
  """
  def get_internal_state(client) do
    GenStateMachine.call(client, :get_internal_state)
  end

  # GenStateMachine callbacks

  @impl true
  def init({config, opts}) do
    Logger.info("Initializing ExMCP client state machine")

    # Start in disconnected state with initial data
    data = States.Disconnected.new(config, opts)

    {:ok, :disconnected, data}
  end

  # State enter callbacks

  @impl true
  def handle_event(:enter, old_state, new_state, _data) do
    Logger.debug("State transition: #{old_state} -> #{new_state}")

    # Emit telemetry event for state transitions
    :telemetry.execute(
      [:ex_mcp, :client, :state_transition],
      %{count: 1},
      %{from_state: old_state, to_state: new_state}
    )

    :keep_state_and_data
  end

  # Handle connect request in disconnected state
  def handle_event({:call, from}, :connect, :disconnected, data) do
    case Transitions.can_connect?(data) do
      true ->
        new_data = Transitions.to_connecting(data)
        # Schedule transport start after state transition
        Process.send(self(), :start_transport_internal, [:nosuspend])
        actions = [{:reply, from, :ok}]
        {:next_state, :connecting, new_data, actions}

      {:error, reason} ->
        {:keep_state_and_data, [{:reply, from, {:error, reason}}]}
    end
  end

  # Handle connect request in other states
  def handle_event({:call, from}, :connect, _state, _data) do
    {:keep_state_and_data, [{:reply, from, {:error, :already_connected}}]}
  end

  # Handle disconnect request
  def handle_event({:call, from}, :disconnect, state, data) when state != :disconnected do
    new_data = Transitions.to_disconnected(data, :user_disconnect)
    actions = [{:reply, from, :ok}]
    {:next_state, :disconnected, new_data, actions}
  end

  def handle_event({:call, from}, :disconnect, :disconnected, _data) do
    {:keep_state_and_data, [{:reply, from, :ok}]}
  end

  # Handle request in ready state
  def handle_event({:call, from}, {:request, method, params, opts}, :ready, data) do
    # Emit telemetry for request start
    :telemetry.execute(
      [:ex_mcp, :client, :request, :start],
      %{count: 1},
      %{method: method, state: :ready}
    )

    case Transitions.add_request(data, from, method, params, opts) do
      {:ok, new_data, request_id} ->
        # Send request to transport
        actions = [{:next_event, :internal, {:send_request, request_id}}]
        {:keep_state, new_data, actions}

      {:error, reason} ->
        # Emit telemetry for request error
        :telemetry.execute(
          [:ex_mcp, :client, :request, :error],
          %{count: 1},
          %{method: method, reason: reason, state: :ready}
        )

        {:keep_state_and_data, [{:reply, from, {:error, reason}}]}
    end
  end

  # Handle request in non-ready states
  def handle_event({:call, from}, {:request, _method, _params, _opts}, state, _data) do
    {:keep_state_and_data, [{:reply, from, {:error, {:not_connected, state}}}]}
  end

  # Handle get_state request
  def handle_event({:call, from}, :get_state, state, data) do
    state_info = %{
      state: state,
      data_type: data.__struct__,
      connected: state == :ready
    }

    {:keep_state_and_data, [{:reply, from, state_info}]}
  end

  # Handle get_internal_state request (for adapter)
  def handle_event({:call, from}, :get_internal_state, state, data) do
    # Return the full internal state for the adapter
    internal_state =
      case {state, data} do
        {:ready, %States.Ready{server_info: server_info, pending_requests: pending_requests}} ->
          %{
            state: state,
            server_info: server_info,
            pending_requests: pending_requests,
            capabilities: extract_capabilities(server_info),
            protocol_version: (server_info || %{})["protocolVersion"] || nil
          }

        _ ->
          %{
            state: state,
            server_info: nil,
            pending_requests: %{},
            capabilities: %{},
            protocol_version: nil
          }
      end

    {:keep_state_and_data, [{:reply, from, {:ok, internal_state}}]}
  end

  # Internal events via info messages

  # Start transport in connecting state
  def handle_event(:info, :start_transport_internal, :connecting, data) do
    case start_transport(data) do
      {:ok, transport_info} ->
        new_data = Transitions.to_handshaking(data, transport_info)
        # Schedule handshake start after state transition
        Process.send(self(), :start_handshake_internal, [:nosuspend])
        {:next_state, :handshaking, new_data}

      {:error, reason} ->
        new_data = Transitions.to_disconnected(data, {:transport_error, reason})
        {:next_state, :disconnected, new_data}
    end
  end

  # Start handshake in handshaking state
  def handle_event(:info, :start_handshake_internal, :handshaking, data) do
    Logger.debug("Received start_handshake_internal event")

    case perform_handshake(data) do
      {:ok, server_info} ->
        new_data = Transitions.to_ready(data, server_info)
        {:next_state, :ready, new_data}

      {:error, reason} ->
        # Check if we're in a reconnection flow
        case data do
          %{from_reconnecting: %{} = reconnect_info} ->
            # We were reconnecting, go back to reconnecting state
            reconnecting_data = %States.Reconnecting{
              common: data.common,
              last_transport: nil,
              last_server_info: nil,
              attempt_number: reconnect_info.attempt_number,
              max_attempts: reconnect_info.max_attempts,
              backoff_ms: reconnect_info.backoff_ms,
              reconnect_timer: nil
            }

            # Let handle_reconnect_failure decide what to do
            handle_reconnect_failure(reconnecting_data, reason)

          _ ->
            # Normal handshake failure, go to disconnected
            new_data = Transitions.to_disconnected(data, {:handshake_error, reason})
            {:next_state, :disconnected, new_data}
        end
    end
  end

  # Send request in ready state
  def handle_event(:internal, {:send_request, request_id}, :ready, data) do
    case send_request_to_transport(data, request_id) do
      :ok ->
        # Set up timeout timer for the request
        case Map.get(data.pending_requests, request_id) do
          %{timeout: timeout} ->
            timer_ref = Process.send_after(self(), {:request_timeout, request_id}, timeout)
            # Store timer ref in request data
            updated_request = Map.put(data.pending_requests[request_id], :timer_ref, timer_ref)
            new_pending = Map.put(data.pending_requests, request_id, updated_request)
            new_data = %{data | pending_requests: new_pending}
            {:keep_state, new_data}

          _ ->
            :keep_state_and_data
        end

      {:error, reason} ->
        # Handle request error
        new_data = Transitions.fail_request(data, request_id, reason)
        {:keep_state, new_data}
    end
  end

  # Transport messages
  def handle_event(:info, {:transport_connected, transport}, :connecting, data) do
    # Emit telemetry for connection success
    :telemetry.execute(
      [:ex_mcp, :client, :connection, :success],
      %{count: 1},
      %{state: :connecting}
    )

    new_data = Transitions.to_handshaking(data, transport)
    # Schedule handshake start after state transition
    Process.send(self(), :start_handshake_internal, [:nosuspend])
    {:next_state, :handshaking, new_data}
  end

  def handle_event(:info, {:transport_error, reason}, state, data) when state != :disconnected do
    # Emit telemetry for transport error
    :telemetry.execute(
      [:ex_mcp, :client, :transport, :error],
      %{count: 1},
      %{reason: reason, state: state}
    )

    new_data = Transitions.to_disconnected(data, {:transport_error, reason})
    {:next_state, :disconnected, new_data}
  end

  def handle_event(:info, {:transport_closed, reason}, :ready, data) do
    # Emit telemetry for transport closure with reconnection
    :telemetry.execute(
      [:ex_mcp, :client, :transport, :closed],
      %{count: 1},
      %{reason: reason, state: :ready, action: :reconnect}
    )

    # Transport closed while ready - attempt reconnection
    new_data = Transitions.to_reconnecting(data, {:transport_closed, reason})
    # Schedule reconnection attempt with configured backoff
    timer_ref = Process.send_after(self(), :attempt_reconnect, new_data.backoff_ms)
    new_data_with_timer = %{new_data | reconnect_timer: timer_ref}
    {:next_state, :reconnecting, new_data_with_timer}
  end

  def handle_event(:info, {:transport_closed, reason}, state, data) when state != :disconnected do
    # Emit telemetry for transport closure with disconnection
    :telemetry.execute(
      [:ex_mcp, :client, :transport, :closed],
      %{count: 1},
      %{reason: reason, state: state, action: :disconnect}
    )

    # Transport closed in other states - disconnect
    new_data = Transitions.to_disconnected(data, {:transport_closed, reason})
    {:next_state, :disconnected, new_data}
  end

  def handle_event(:info, {:transport_message, message}, :ready, data) do
    new_data = handle_transport_message(data, message)
    {:keep_state, new_data}
  end

  # Request timeout handling
  def handle_event(:info, {:request_timeout, request_id}, :ready, data) do
    # Check if request is still pending
    case Map.get(data.pending_requests, request_id) do
      nil ->
        # Request already completed, ignore timeout
        :keep_state_and_data

      _request ->
        # Request timed out
        new_data = Transitions.fail_request(data, request_id, :timeout)
        {:keep_state, new_data}
    end
  end

  # Reconnection logic
  def handle_event(:info, :attempt_reconnect, :reconnecting, data) do
    # Emit telemetry for reconnection attempt
    :telemetry.execute(
      [:ex_mcp, :client, :reconnect, :attempt],
      %{count: 1, attempt_number: data.attempt_number},
      %{backoff_ms: data.backoff_ms}
    )

    case start_transport(data) do
      {:ok, transport_info} ->
        # Emit telemetry for successful reconnection
        :telemetry.execute(
          [:ex_mcp, :client, :reconnect, :success],
          %{count: 1, attempt_number: data.attempt_number},
          %{backoff_ms: data.backoff_ms}
        )

        # Transport connected, move to handshaking
        new_data = %States.Handshaking{
          common: data.common,
          transport: transport_info,
          transport_state: nil,
          client_info: build_client_info(),
          handshake_start_time: System.monotonic_time(:millisecond),
          from_reconnecting: %{
            attempt_number: data.attempt_number,
            max_attempts: data.max_attempts,
            backoff_ms: data.backoff_ms
          }
        }

        # Schedule handshake
        Process.send(self(), :start_handshake_internal, [:nosuspend])
        {:next_state, :handshaking, new_data}

      {:error, reason} ->
        # Emit telemetry for failed reconnection attempt
        :telemetry.execute(
          [:ex_mcp, :client, :reconnect, :error],
          %{count: 1, attempt_number: data.attempt_number},
          %{reason: reason, backoff_ms: data.backoff_ms}
        )

        # Reconnection failed
        handle_reconnect_failure(data, reason)
    end
  end

  def handle_event(:info, :reconnect_timeout, :reconnecting, data) do
    # Emit telemetry for reconnection timeout
    :telemetry.execute(
      [:ex_mcp, :client, :reconnect, :timeout],
      %{count: 1, attempt_number: data.attempt_number},
      %{max_attempts: data.max_attempts}
    )

    # Reconnection timeout - give up
    new_data = Transitions.to_disconnected(data, :reconnect_timeout)
    {:next_state, :disconnected, new_data}
  end

  # Progress notification from ProgressTracker (for internal tracking)
  def handle_event(:info, {:progress_notification, notification}, :ready, data) do
    # Process progress notifications from ProgressTracker
    # These are sent when ProgressTracker validates and rate-limits updates
    case notification do
      %{token: token, progress: progress, total: total, message: message} ->
        # Find the callback for this token
        case Map.get(data.progress_callbacks, token) do
          nil ->
            # No callback registered, ignore
            :keep_state_and_data

          callback ->
            # Call the user's progress callback
            params = %{
              "progressToken" => token,
              "progress" => progress,
              "total" => total,
              "message" => message
            }

            # Emit telemetry for progress update
            :telemetry.execute(
              [:ex_mcp, :client, :progress, :update],
              %{count: 1, progress: progress, total: total || 100},
              %{token: token, message: message}
            )

            callback.(params)
            :keep_state_and_data
        end

      _ ->
        # Unknown notification format, ignore
        :keep_state_and_data
    end
  end

  # Ignore progress notifications in non-ready states
  def handle_event(:info, {:progress_notification, _notification}, _state, _data) do
    :keep_state_and_data
  end

  # Handle Task async/await messages (from internal async operations)
  def handle_event(:info, {ref, _result}, _state, _data) when is_reference(ref) do
    # This is a Task async result - ignore it as it's handled by Task.await
    :keep_state_and_data
  end

  def handle_event(:info, {:DOWN, ref, :process, _pid, _reason}, _state, _data)
      when is_reference(ref) do
    # This is a Task process exit - normal behavior for completed tasks
    :keep_state_and_data
  end

  # Catch-all for unhandled events
  def handle_event(event_type, event_content, state, _data) do
    Logger.warning("Unhandled event in state #{state}: #{event_type} #{inspect(event_content)}")
    :keep_state_and_data
  end

  # Private functions

  defp start_transport(%{common: common}) do
    config = common.config
    transport_module = common.transport_module

    # Prepare transport configuration
    transport_opts = build_transport_opts(config) ++ [client_pid: self()]

    # Connect to transport
    case transport_module.connect(transport_opts) do
      {:ok, transport_state} ->
        # Start receiver process
        case start_receiver_task(self(), transport_module, transport_state) do
          {:ok, receiver_pid} ->
            {:ok, {transport_module, transport_state, receiver_pid}}
        end

      {:error, reason} ->
        {:error, {:transport_connect_failed, reason}}
    end
  end

  defp perform_handshake(%{
         transport: {transport_module, transport_state, _receiver},
         client_info: client_info
       }) do
    Logger.debug("Starting handshake with transport: #{inspect(transport_module)}")

    # Emit telemetry for handshake start
    :telemetry.execute(
      [:ex_mcp, :client, :handshake, :start],
      %{count: 1},
      %{transport: transport_module}
    )

    # Send initialize request
    initialize_request = %{
      "jsonrpc" => "2.0",
      "method" => "initialize",
      "params" => %{
        "protocolVersion" => client_info["protocolVersion"],
        "capabilities" => client_info["capabilities"],
        "clientInfo" => %{
          "name" => client_info["name"],
          "version" => client_info["version"]
        }
      },
      "id" => "init_#{System.unique_integer([:positive])}"
    }

    Logger.debug("Sending initialize request: #{inspect(initialize_request)}")

    case send_and_receive_response(transport_module, transport_state, initialize_request) do
      {:ok, %{"result" => result}} ->
        # Send initialized notification
        initialized_notification = %{
          "jsonrpc" => "2.0",
          "method" => "notifications/initialized",
          "params" => %{}
        }

        case transport_module.send_message(
               Jason.encode!(initialized_notification),
               transport_state
             ) do
          {:ok, _new_state} ->
            # Emit telemetry for successful handshake
            :telemetry.execute(
              [:ex_mcp, :client, :handshake, :success],
              %{count: 1},
              %{transport: transport_module}
            )

            Logger.debug("Initialize result: #{inspect(result)}")
            {:ok, result}

          {:error, reason} ->
            # Emit telemetry for handshake error
            :telemetry.execute(
              [:ex_mcp, :client, :handshake, :error],
              %{count: 1},
              %{transport: transport_module, reason: {:initialized_send_failed, reason}}
            )

            {:error, {:initialized_send_failed, reason}}
        end

      {:ok, %{"error" => error}} ->
        # Emit telemetry for handshake error
        :telemetry.execute(
          [:ex_mcp, :client, :handshake, :error],
          %{count: 1},
          %{transport: transport_module, reason: {:initialize_failed, error}}
        )

        {:error, {:initialize_failed, error}}

      {:error, reason} ->
        # Emit telemetry for handshake error
        :telemetry.execute(
          [:ex_mcp, :client, :handshake, :error],
          %{count: 1},
          %{transport: transport_module, reason: {:transport_error, reason}}
        )

        {:error, {:transport_error, reason}}
    end
  end

  defp send_request_to_transport(
         %{transport: {transport_module, transport_state, _}} = data,
         request_id
       ) do
    case Map.get(data.pending_requests, request_id) do
      nil ->
        {:error, :request_not_found}

      request ->
        message = %{
          "jsonrpc" => "2.0",
          "method" => request.method,
          "params" => request.params,
          "id" => request_id
        }

        case transport_module.send_message(Jason.encode!(message), transport_state) do
          {:ok, _new_transport_state} ->
            :ok

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp handle_transport_message(data, message) when is_binary(message) do
    case Jason.decode(message) do
      {:ok, decoded} ->
        handle_transport_message(data, decoded)

      {:error, _reason} ->
        Logger.warning("Failed to decode transport message: #{inspect(message)}")
        data
    end
  end

  defp handle_transport_message(data, %{"id" => id} = message) when is_integer(id) do
    # Response to a request
    case Map.get(data.pending_requests, id) do
      nil ->
        Logger.warning("Received response for unknown request ID: #{id}")
        data

      _request ->
        if Map.has_key?(message, "result") do
          Transitions.complete_request(data, id, message["result"])
        else
          Transitions.fail_request(data, id, message["error"])
        end
    end
  end

  defp handle_transport_message(data, %{"method" => method, "params" => params} = message) do
    # Server notification or request
    Logger.debug("Received server message: #{method}")

    cond do
      # Server notifications
      String.starts_with?(method, "notifications/") ->
        handle_server_notification(data, method, params)

      # Server requests (need to send response)
      Map.has_key?(message, "id") ->
        handle_server_request(data, method, params, message["id"])

      # Other notifications
      true ->
        Logger.warning("Unhandled server notification: #{method}")
        data
    end
  end

  defp handle_transport_message(data, message) do
    Logger.warning("Unhandled transport message: #{inspect(message)}")
    data
  end

  # Helper functions

  defp build_transport_opts(config) do
    # Extract transport-specific options from config
    base_opts =
      [
        url: config[:url],
        command: config[:command],
        args: config[:args],
        # Include test_pid if present
        test_pid: config[:test_pid],
        # Include test_mode if present (for TestTransport)
        test_mode: config[:test_mode]
      ]
      |> Enum.reject(fn {_k, v} -> is_nil(v) end)

    Keyword.merge(base_opts, config[:transport_opts] || [])
  end

  defp start_receiver_task(parent, transport_module, transport_state) do
    # For test transport, we don't need a receiver task since messages are sent directly
    if transport_module == ExMCP.TestHelpers.TestTransport do
      # Return a dummy task that doesn't interfere
      task = Task.async(fn -> :ok end)
      {:ok, task.pid}
    else
      task =
        Task.async(fn ->
          receiver_loop(parent, transport_module, transport_state)
        end)

      {:ok, task.pid}
    end
  end

  defp receiver_loop(parent, transport_module, transport_state) do
    case transport_module.receive_message(transport_state) do
      {:ok, message, new_state} ->
        send(parent, {:transport_message, message})
        receiver_loop(parent, transport_module, new_state)

      {:error, :closed} ->
        send(parent, {:transport_closed, :normal})

      {:error, reason} ->
        send(parent, {:transport_error, reason})
    end
  end

  defp send_and_receive_response(transport_module, transport_state, request) do
    # Send the request
    encoded = Jason.encode!(request)
    Logger.debug("Sending message via transport: #{encoded}")

    case transport_module.send_message(encoded, transport_state) do
      {:ok, _new_state} ->
        Logger.debug("Message sent, waiting for response...")
        # Wait for response with matching ID
        receive do
          {:transport_message, message} ->
            case Jason.decode(message) do
              {:ok, %{"id" => id} = decoded} ->
                if id == request["id"] do
                  {:ok, decoded}
                else
                  # Not our response, keep waiting
                  send(self(), {:transport_message, message})
                  {:error, :timeout}
                end

              _ ->
                # Failed to decode, keep waiting
                send(self(), {:transport_message, message})
                {:error, :timeout}
            end

          {:transport_error, reason} ->
            # Emit telemetry for transport error immediately
            :telemetry.execute(
              [:ex_mcp, :client, :transport, :error],
              %{count: 1},
              %{reason: reason, state: :handshaking}
            )

            # Re-send the error to be handled by the state machine
            send(self(), {:transport_error, reason})
            {:error, {:transport_error, reason}}

          {:transport_closed, reason} ->
            # Re-send the closure to be handled by the state machine
            send(self(), {:transport_closed, reason})
            {:error, {:transport_closed, reason}}
        after
          5000 ->
            {:error, :timeout}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Server notification handlers

  defp handle_server_notification(data, "notifications/resources/changed", _params) do
    Logger.info("Resources changed notification received")
    # TODO: Notify subscribers
    data
  end

  defp handle_server_notification(data, "notifications/tools/changed", _params) do
    Logger.info("Tools changed notification received")
    # TODO: Notify subscribers
    data
  end

  defp handle_server_notification(data, "notifications/prompts/changed", _params) do
    Logger.info("Prompts changed notification received")
    # TODO: Notify subscribers
    data
  end

  defp handle_server_notification(data, "notifications/resource/updated", %{"uri" => uri}) do
    Logger.info("Resource updated: #{uri}")
    # TODO: Notify subscribers
    data
  end

  defp handle_server_notification(data, "notifications/progress", params) do
    progress_token = params["progressToken"]
    progress_value = params["progress"]
    total = params["total"]
    message = params["message"]

    case Map.get(data.progress_callbacks, progress_token) do
      nil ->
        # Emit telemetry for unknown progress token
        :telemetry.execute(
          [:ex_mcp, :client, :progress, :unknown_token],
          %{count: 1},
          %{token: progress_token}
        )

        Logger.warning("Progress notification for unknown token: #{progress_token}")
        data

      callback ->
        # Update progress through ProgressTracker for validation and rate limiting
        # Try to update progress in ProgressTracker, handle case where it's not running
        update_result =
          try do
            ExMCP.ProgressTracker.update_progress(progress_token, progress_value, total, message)
          catch
            :exit, {:noproc, _} ->
              # ProgressTracker not running, continue without it
              :ok
          end

        case update_result do
          :ok ->
            # Emit telemetry for successful progress update
            :telemetry.execute(
              [:ex_mcp, :client, :progress, :update],
              %{count: 1, progress: progress_value, total: total || 100},
              %{token: progress_token, message: message}
            )

            # Call the progress callback if ProgressTracker validation passed
            callback.(params)
            data

          {:error, :not_found} ->
            # Emit telemetry for progress update without tracker
            :telemetry.execute(
              [:ex_mcp, :client, :progress, :untracked],
              %{count: 1, progress: progress_value},
              %{token: progress_token}
            )

            # Token not found in ProgressTracker, but we have a callback - call it anyway
            Logger.debug(
              "Progress token not in ProgressTracker, calling callback directly: #{progress_token}"
            )

            callback.(params)
            data

          {:error, :rate_limited} ->
            # Emit telemetry for rate limited progress
            :telemetry.execute(
              [:ex_mcp, :client, :progress, :rate_limited],
              %{count: 1},
              %{token: progress_token}
            )

            Logger.debug("Progress notification rate limited for token: #{progress_token}")
            data

          {:error, :not_increasing} ->
            # Emit telemetry for non-increasing progress
            :telemetry.execute(
              [:ex_mcp, :client, :progress, :not_increasing],
              %{count: 1, progress: progress_value},
              %{token: progress_token}
            )

            Logger.warning(
              "Progress value not increasing for token #{progress_token}: #{progress_value}"
            )

            data
        end
    end
  end

  defp handle_server_notification(data, method, _params) do
    Logger.warning("Unhandled server notification: #{method}")
    data
  end

  # Server request handlers

  defp handle_server_request(
         %{transport: {transport_module, transport_state, _}} = data,
         "ping",
         _params,
         id
       ) do
    # Respond with pong
    response = %{
      "jsonrpc" => "2.0",
      "result" => %{},
      "id" => id
    }

    case transport_module.send_message(Jason.encode!(response), transport_state) do
      {:ok, _} ->
        Logger.debug("Responded to ping request")

      {:error, reason} ->
        Logger.error("Failed to send pong response: #{inspect(reason)}")
    end

    data
  end

  defp handle_server_request(data, method, _params, id) do
    Logger.warning("Unhandled server request: #{method} (id: #{id})")
    # TODO: Send error response
    data
  end

  # Helper functions for state extraction

  defp extract_capabilities(nil), do: %{}
  defp extract_capabilities(%{"capabilities" => caps}), do: caps
  defp extract_capabilities(_), do: %{}

  # Reconnection helpers

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

  defp handle_reconnect_failure(%States.Reconnecting{} = data, reason) do
    if data.attempt_number >= data.max_attempts and data.max_attempts != :infinity do
      # Max attempts reached, give up
      Logger.error(
        "Reconnection failed after #{data.attempt_number} attempts: #{inspect(reason)}"
      )

      new_data = Transitions.to_disconnected(data, {:max_reconnect_attempts, reason})
      {:next_state, :disconnected, new_data}
    else
      # Schedule next attempt with exponential backoff
      # Cap at 1 minute
      new_backoff = min(data.backoff_ms * 2, 60_000)
      timer_ref = Process.send_after(self(), :attempt_reconnect, new_backoff)

      Logger.info(
        "Reconnection attempt #{data.attempt_number} failed, retrying in #{new_backoff}ms"
      )

      new_data = %{
        data
        | attempt_number: data.attempt_number + 1,
          backoff_ms: new_backoff,
          reconnect_timer: timer_ref
      }

      {:keep_state, new_data}
    end
  end
end
