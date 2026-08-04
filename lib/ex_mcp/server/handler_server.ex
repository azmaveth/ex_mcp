defmodule ExMCP.Server.HandlerServer do
  @moduledoc """
  Transport-aware process for `ExMCP.Server.Handler` modules.

  This module runs a handler module behind the MCP transports that need a
  server process, including the in-memory test transport.

  ## Usage

      # Handler module implementing ExMCP.Server.Handler
      defmodule MyHandler do
        use ExMCP.Server.Handler

        @impl true
        def handle_initialize(params, state) do
          {:ok, %{
            protocolVersion: "2025-03-26",
            serverInfo: %{name: "test-server", version: "1.0.0"},
            capabilities: %{tools: %{}}
          }, state}
        end

        @impl true
        def handle_list_tools(_cursor, state) do
          tools = [
            %{
              name: "ping",
              description: "Simple ping tool",
              inputSchema: %{type: "object", properties: %{}}
            }
          ]
          {:ok, tools, nil, state}
        end
      end

      # Start the server
      {:ok, server} = ExMCP.Server.HandlerServer.start_link(transport: :test, handler: MyHandler)
  """

  use GenServer
  require Logger

  alias ExMCP.Internal.{JSONRPC, VersionRegistry}
  alias ExMCP.Protocol.ErrorCodes
  alias ExMCP.Server.{CancellationTracker, Dispatch, RequestContext, RequestState}
  alias ExMCP.Transport.{Local, Test}

  # JSON-RPC batches were removed from the spec in 2025-06-18 and have not
  # come back since, so every version from 2025-06-18 onwards rejects them.
  @batch_removed_in "2025-06-18"

  @type handler_module :: module()
  @type state :: %{
          handler_module: handler_module(),
          handler_state: any(),
          transport: any(),
          transport_state: any(),
          protocol_version: String.t() | nil,
          protocol_mode: ExMCP.Internal.VersionRegistry.protocol_mode() | nil,
          connection_era: :legacy | :modern | nil,
          instructions: String.t() | nil,
          request_state: keyword() | nil,
          endpoint: String.t() | nil,
          principal_id: String.t() | nil,
          tenant_id: String.t() | nil,
          replay_cache: module() | {module(), keyword()} | nil,
          require_replay_protection: boolean(),
          pending_requests: map(),
          cancelled_requests: MapSet.t(),
          cancellation_tracker: module()
        }

  @doc """
  Starts a handler-based server.

  ## Options

  * `:handler` - Module implementing `ExMCP.Server.Handler` behaviour (required)
  * `:transport` - Transport type (`:test`, `:stdio`, `:http`, etc.)
  * `:handler_args` - Optional term passed to `handler.init/1` (default: `[]`)
  * `:cancellation_tracker` - Module implementing
    `ExMCP.Server.CancellationTracker` used to propagate
    `notifications/cancelled` into handler state
    (default: `ExMCP.Server.CancellationTracker.Default`)
  * Other options are passed to the transport
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts) do
    genserver_opts =
      case Keyword.get(opts, :name) do
        nil -> []
        name -> [name: name]
      end

    GenServer.start_link(__MODULE__, opts, genserver_opts)
  end

  @impl GenServer
  def init(opts) do
    case validate_mrtr_configuration(opts) do
      :ok -> do_init(opts)
      {:error, reason} -> {:stop, {:mrtr_configuration_error, reason}}
    end
  end

  defp do_init(opts) do
    handler_module = Keyword.fetch!(opts, :handler)
    transport_type = Keyword.get(opts, :transport, :test)

    handler_args = Keyword.get(opts, :handler_args, [])
    cancellation_tracker = Keyword.get(opts, :cancellation_tracker, CancellationTracker.Default)

    case handler_module.init(handler_args) do
      {:ok, handler_state} ->
        # Connect to the transport
        case connect_transport(transport_type, opts) do
          {:ok, {transport_mod, transport_state}} ->
            state = %{
              handler_module: handler_module,
              handler_state: handler_state,
              transport: transport_mod,
              transport_state: transport_state,
              protocol_version: nil,
              protocol_mode: Keyword.get(opts, :protocol_mode),
              connection_era: nil,
              instructions: Keyword.get(opts, :instructions),
              request_state: Keyword.get(opts, :request_state),
              endpoint: Keyword.get(opts, :endpoint),
              principal_id: Keyword.get(opts, :principal_id),
              tenant_id: Keyword.get(opts, :tenant_id),
              replay_cache: Keyword.get(opts, :replay_cache),
              require_replay_protection: Keyword.get(opts, :require_replay_protection, false),
              pending_requests: %{},
              cancelled_requests: MapSet.new(),
              cancellation_tracker: cancellation_tracker
            }

            {:ok, state}

          {:error, reason} ->
            {:stop, {:transport_error, reason}}
        end

      {:error, reason} ->
        {:stop, {:handler_init_error, reason}}
    end
  end

  defp validate_mrtr_configuration(opts) do
    if Keyword.get(opts, :mrtr, false) do
      RequestState.validate_configuration(request_state: Keyword.get(opts, :request_state))
    else
      :ok
    end
  end

  @impl GenServer
  def handle_info({:transport_message, message}, state) do
    case decode_transport_message(message) do
      {:ok, requests} when is_list(requests) ->
        handle_batch_request(requests, state)

      {:ok, message_data} when is_map(message_data) ->
        # Check if this is a response (has "result" or "error" but no "method")
        if Map.has_key?(message_data, "result") or Map.has_key?(message_data, "error") do
          # This is a response from client
          handle_client_response(message_data, state)
        else
          method = Map.get(message_data, "method")

          :telemetry.execute(
            [:ex_mcp, :server, :request, :received],
            %{},
            %{method: method}
          )

          # This is a request from client
          case process_mcp_request(message_data, state) do
            {:response, response, new_state} ->
              case send_message(response, new_state) do
                {:ok, final_state} ->
                  :telemetry.execute(
                    [:ex_mcp, :server, :request, :completed],
                    %{},
                    %{method: method}
                  )

                  {:noreply, final_state}

                {:error, _reason} ->
                  {:noreply, new_state}
              end

            {:notification, new_state} ->
              # Single notification received, no response needed.
              {:noreply, new_state}
          end
        end

      {:error, error} ->
        Logger.error("Failed to decode message: #{inspect(error)}")
        {:noreply, state}
    end
  end

  def handle_info({:transport_error, reason}, state) do
    Logger.error("Transport error: #{inspect(reason)}")
    {:noreply, state}
  end

  def handle_info({:test_transport_connect, client_pid}, state) do
    new_transport_state =
      case state.transport do
        Test -> %{state.transport_state | peer_pid: client_pid}
        Local -> %{state.transport_state | server_pid: client_pid, connected: true}
        _ -> state.transport_state
      end

    {:noreply, %{state | transport_state: new_transport_state}}
  end

  def handle_info({:transport_closed}, state) do
    Logger.info("Transport closed")
    {:stop, :normal, state}
  end

  def handle_info({:cancelled, request_id}, state) do
    # Handle cancellation notifications from clients
    Logger.debug("Received cancellation for request: #{request_id}")

    # Check if this request is still pending and cancel it
    case Map.get(state.pending_requests, request_id) do
      nil ->
        # Request not found - either completed, cancelled, or never existed
        Logger.debug("Cancellation for unknown request: #{request_id}")
        {:noreply, state}

      _pending_request ->
        # Cancel the pending request
        new_pending = Map.delete(state.pending_requests, request_id)
        new_state = %{state | pending_requests: new_pending}
        Logger.debug("Cancelled pending request: #{request_id}")
        {:noreply, new_state}
    end
  end

  def handle_info({:request_timeout, request_id}, state) do
    # Handle timeout for server->client requests
    case Map.get(state.pending_requests, request_id) do
      nil ->
        # Request already completed or doesn't exist
        {:noreply, state}

      {from, :server_request} ->
        # Request timed out, reply with timeout error
        GenServer.reply(from, {:error, :timeout})

        # Remove from pending requests
        new_pending_requests = Map.delete(state.pending_requests, request_id)
        new_state = %{state | pending_requests: new_pending_requests}
        {:noreply, new_state}

      _ ->
        # Not a server request, ignore
        {:noreply, state}
    end
  end

  defp decode_transport_message(message) when is_binary(message), do: Jason.decode(message)
  defp decode_transport_message(message) when is_map(message), do: {:ok, message}
  defp decode_transport_message(messages) when is_list(messages), do: {:ok, messages}
  defp decode_transport_message(_message), do: {:error, :invalid_message}

  # Handle batch requests according to JSON-RPC 2.0 specification
  defp handle_batch_request([], state) do
    # Empty batch is invalid according to JSON-RPC 2.0
    send_error_response(-32600, "Invalid Request", nil, state)
  end

  defp handle_batch_request(requests, state) when is_list(requests) do
    if batch_supported?(state.protocol_version) do
      process_batch(requests, state)
    else
      send_error_response(
        -32600,
        "Batch requests are not supported in protocol version #{state.protocol_version}",
        nil,
        state
      )
    end
  end

  # Batches are allowed up to (but not including) the version that removed
  # them. Ordering comes from VersionRegistry (newest first) instead of a
  # string equality check, so newer versions such as 2025-11-25 also reject
  # batches (audit M7).
  defp batch_supported?(nil), do: true

  defp batch_supported?(version) do
    versions = VersionRegistry.supported_versions()
    removed_index = Enum.find_index(versions, &(&1 == @batch_removed_in))
    version_index = Enum.find_index(versions, &(&1 == version))

    cond do
      is_nil(removed_index) -> true
      is_nil(version_index) -> false
      true -> version_index > removed_index
    end
  end

  defp process_batch(requests, state) when is_list(requests) do
    # Process each request in the batch
    {responses, final_state} = process_batch_requests(requests, state)

    # Filter out nils from notifications
    non_nil_responses = Enum.reject(responses, &is_nil/1)

    # Only send response if we have any (notifications don't generate responses)
    if not Enum.empty?(non_nil_responses) do
      case send_message(non_nil_responses, final_state) do
        {:ok, new_state} -> {:noreply, new_state}
        {:error, _reason} -> {:noreply, final_state}
      end
    else
      {:noreply, final_state}
    end
  end

  defp send_error_response(code, message, id, state) do
    error_response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "error" => %{"code" => code, "message" => message}
    }

    case send_message(error_response, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, _reason} -> {:noreply, state}
    end
  end

  defp process_batch_requests(requests, state) do
    Enum.map_reduce(requests, state, fn request, acc_state ->
      case process_mcp_request(request, acc_state) do
        {:response, response, new_state} ->
          {response, new_state}

        {:notification, new_state} ->
          # Notifications don't get responses
          {nil, new_state}
      end
    end)
  end

  @impl GenServer
  def handle_call(:ping, from, state) do
    # Send ping to client via transport and wait for response
    request_id = System.unique_integer([:positive])

    ping_request = %{
      "jsonrpc" => "2.0",
      "id" => request_id,
      "method" => "ping",
      "params" => %{}
    }

    case send_message(ping_request, state) do
      {:ok, new_state} ->
        # Store the request in pending_requests to wait for client response
        pending_requests =
          Map.put(new_state.pending_requests, request_id, {from, :server_request})

        final_state = %{new_state | pending_requests: pending_requests}

        # Set up timeout
        Process.send_after(self(), {:request_timeout, request_id}, 5000)
        {:noreply, final_state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:list_roots, timeout}, from, state) do
    # Send list_roots request to client via transport with custom timeout
    request_id = System.unique_integer([:positive])

    list_roots_request = %{
      "jsonrpc" => "2.0",
      "id" => request_id,
      "method" => "roots/list",
      "params" => %{}
    }

    case send_message(list_roots_request, state) do
      {:ok, new_state} ->
        # Store the request in pending_requests to wait for client response
        pending_requests =
          Map.put(new_state.pending_requests, request_id, {from, :server_request})

        final_state = %{new_state | pending_requests: pending_requests}

        # Set up timeout
        Process.send_after(self(), {:request_timeout, request_id}, timeout)
        {:noreply, final_state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(:list_roots, from, state) do
    # Default timeout of 5 seconds
    handle_call({:list_roots, 5000}, from, state)
  end

  def handle_call({:create_message, params}, from, state) do
    # Send sampling/createMessage request to client via transport
    request_id = System.unique_integer([:positive])

    create_message_request = %{
      "jsonrpc" => "2.0",
      "id" => request_id,
      "method" => "sampling/createMessage",
      "params" => params
    }

    case send_message(create_message_request, state) do
      {:ok, new_state} ->
        # Store the request in pending_requests to wait for client response
        pending_requests =
          Map.put(new_state.pending_requests, request_id, {from, :server_request})

        final_state = %{new_state | pending_requests: pending_requests}

        # Set up timeout
        Process.send_after(self(), {:request_timeout, request_id}, 5000)
        {:noreply, final_state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call(request, from, state) do
    # Forward unknown calls to the handler if it supports GenServer calls
    if function_exported?(state.handler_module, :handle_call, 3) do
      case state.handler_module.handle_call(request, from, state.handler_state) do
        {:reply, reply, new_handler_state} ->
          new_state = %{state | handler_state: new_handler_state}
          {:reply, reply, new_state}

        other ->
          other
      end
    else
      {:reply, {:error, {:unknown_call, request}}, state}
    end
  end

  @impl GenServer
  def handle_cast({:send_log_message, level, message, data}, state) do
    # Send log notification to client
    log_notification = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/message",
      "params" => %{
        "level" => level,
        "logger" => "ExMCP.Server",
        "data" => data || %{},
        "message" => message
      }
    }

    case send_message(log_notification, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, _reason} -> {:noreply, state}
    end
  end

  def handle_cast({:notify_progress, progress_token, progress, total}, state) do
    # Send progress notification to client
    progress_notification = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/progress",
      "params" => %{
        "progressToken" => progress_token,
        "progress" => progress,
        "total" => total
      }
    }

    case send_message(progress_notification, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, _reason} -> {:noreply, state}
    end
  end

  def handle_cast({:notify_resource_update, uri}, state) do
    # Send resource update notification to client
    update_notification = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/resources/updated",
      "params" => %{
        "uri" => uri
      }
    }

    case send_message(update_notification, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, _reason} -> {:noreply, state}
    end
  end

  def handle_cast(:notify_roots_changed, state) do
    # Send roots changed notification to client
    roots_notification = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/roots/list_changed",
      "params" => %{}
    }

    case send_message(roots_notification, state) do
      {:ok, new_state} -> {:noreply, new_state}
      {:error, _reason} -> {:noreply, state}
    end
  end

  def handle_cast({:notification, "notifications/cancelled", params}, state) do
    # Handle cancellation notifications from clients
    handle_cancellation_notification(params, state)
  end

  def handle_cast(request, state) do
    # Forward unknown casts to the handler if it supports GenServer casts
    if function_exported?(state.handler_module, :handle_cast, 2) do
      case state.handler_module.handle_cast(request, state.handler_state) do
        {:noreply, new_handler_state} ->
          new_state = %{state | handler_state: new_handler_state}
          {:noreply, new_state}

        other ->
          other
      end
    else
      {:noreply, state}
    end
  end

  @impl GenServer
  def terminate(reason, state) do
    if function_exported?(state.handler_module, :terminate, 2) do
      state.handler_module.terminate(reason, state.handler_state)
    end
  end

  # Private functions

  # Handle responses from clients to server requests
  defp handle_client_response(%{"id" => request_id} = response, state) do
    case Map.get(state.pending_requests, request_id) do
      nil ->
        Logger.warning("Received response for unknown request ID: #{request_id}")
        {:noreply, state}

      {from, :server_request} ->
        # This is a response to a server->client request
        if Map.has_key?(response, "result") do
          GenServer.reply(from, {:ok, response["result"]})
        else
          error = response["error"]
          GenServer.reply(from, {:error, error})
        end

        # Remove from pending requests
        new_pending_requests = Map.delete(state.pending_requests, request_id)
        new_state = %{state | pending_requests: new_pending_requests}
        {:noreply, new_state}

      _ ->
        Logger.warning(
          "Received response for request with unexpected pending state: #{request_id}"
        )

        {:noreply, state}
    end
  end

  defp handle_client_response(response, state) do
    Logger.warning("Received response without ID: #{inspect(response)}")
    {:noreply, state}
  end

  # Handle cancellation notifications from clients
  defp handle_cancellation_notification(%{"requestId" => request_id} = params, state) do
    reason = Map.get(params, "reason", "Request cancelled by client")
    Logger.debug("Received cancellation for request #{request_id}: #{reason}")

    # Mark the request as cancelled and let the configured tracker propagate
    # it into the handler's own state.
    new_state = %{
      state
      | cancelled_requests: MapSet.put(state.cancelled_requests, request_id),
        handler_state: state.cancellation_tracker.mark_cancelled(request_id, state.handler_state)
    }

    # If the request is still pending, remove it and reply with a cancellation
    # error so the caller does not wait for a reply that will never come.
    case Map.get(state.pending_requests, request_id) do
      nil ->
        {:noreply, new_state}

      :sync_call ->
        # Synchronous tool calls have no GenServer.from to reply to.
        {:noreply, drop_pending(new_state, request_id)}

      from ->
        GenServer.reply(from, {:error, :cancelled})
        {:noreply, drop_pending(new_state, request_id)}
    end
  end

  defp handle_cancellation_notification(params, state) do
    Logger.warning("Invalid cancellation notification: #{inspect(params)}")
    {:noreply, state}
  end

  defp drop_pending(state, request_id) do
    %{state | pending_requests: Map.delete(state.pending_requests, request_id)}
  end

  # Check if a request has been cancelled
  defp request_cancelled?(request_id, state) do
    MapSet.member?(state.cancelled_requests, request_id)
  end

  # Add a pending request to tracking
  defp track_pending_request(request_id, from, state) do
    new_pending_requests = Map.put(state.pending_requests, request_id, from)
    %{state | pending_requests: new_pending_requests}
  end

  # Remove a pending request from tracking (when completed)
  defp complete_pending_request(request_id, state) do
    new_pending_requests = Map.delete(state.pending_requests, request_id)
    %{state | pending_requests: new_pending_requests}
  end

  defp connect_transport(:test, opts) do
    case Test.connect(opts) do
      {:ok, transport_state} -> {:ok, {Test, transport_state}}
      error -> error
    end
  end

  defp connect_transport(:beam, opts) do
    case Local.connect(opts) do
      {:ok, transport_state} -> {:ok, {Local, transport_state}}
      error -> error
    end
  end

  defp connect_transport(transport_type, _opts) do
    {:error, {:unsupported_transport, transport_type}}
  end

  # Process a single MCP request or notification.
  #
  # Method coverage and result/error shaping live in ExMCP.Server.Dispatch so
  # that every transport answers the same set of methods identically (audit
  # M9). Only the pieces that are specific to this process — protocol version
  # capture, telemetry, and cancellation bookkeeping — stay here.
  defp process_mcp_request(%{"method" => "initialize"} = request, state) do
    case dispatch(request, state) do
      {:response, %{"result" => result} = response, new_state} ->
        emit_initialize_telemetry(result)

        {:response, response,
         %{new_state | protocol_version: protocol_version_from_result(result)}}

      other ->
        other
    end
  end

  defp process_mcp_request(%{"method" => "tools/call"} = request, state) do
    id = Map.get(request, "id")
    params = Map.get(request, "params", %{})

    if request_cancelled?(id, state) do
      response =
        JSONRPC.error(id, ErrorCodes.request_cancelled(), "Request was cancelled")

      {:response, response, state}
    else
      # Track the request so a notifications/cancelled can find it, and expose
      # the request id (plus any _meta) to cancellation-aware handlers.
      tracked_state = track_pending_request(id, :sync_call, state)

      enhanced_arguments =
        params
        |> Map.get("arguments", %{})
        |> Map.put("_request_id", id)
        |> put_meta(Map.get(params, "_meta"))

      enhanced_params = Map.put(params, "arguments", enhanced_arguments)
      enhanced_request = Map.put(request, "params", enhanced_params)

      case dispatch(enhanced_request, tracked_state) do
        {:response, response, new_state} ->
          {:response, response, complete_pending_request(id, new_state)}

        other ->
          other
      end
    end
  end

  defp process_mcp_request(%{"method" => "notifications/cancelled"} = request, state) do
    # Handle cancellation notifications from clients
    params = Map.get(request, "params", %{})
    {_, new_state} = handle_cancellation_notification(params, state)
    {:notification, new_state}
  end

  defp process_mcp_request(%{"method" => _method} = request, state) do
    dispatch(request, state)
  end

  defp process_mcp_request(_invalid_request, state) do
    # Invalid request format (e.g. not a map)
    response = JSONRPC.error(nil, ErrorCodes.invalid_request(), "Invalid Request")
    {:response, response, state}
  end

  # Runs the shared dispatcher against the handler module and folds the new
  # handler state back into the server state.
  defp dispatch(request, state) do
    state = maybe_pin_connection_era(request, state)

    dispatch_opts = [
      protocol_mode: effective_protocol_mode(state),
      instructions: state.instructions,
      request_state: state.request_state,
      endpoint: state.endpoint,
      principal_id: state.principal_id,
      tenant_id: state.tenant_id,
      replay_cache: state.replay_cache,
      require_replay_protection: state.require_replay_protection
    ]

    case Dispatch.dispatch(request, state.handler_module, state.handler_state, dispatch_opts) do
      {:response, response, handler_state} ->
        {:response, response, %{state | handler_state: handler_state}}

      {:notification, handler_state} ->
        {:notification, %{state | handler_state: handler_state}}
    end
  end

  defp maybe_pin_connection_era(request, %{connection_era: nil} = state) do
    with {:ok, context} <- RequestContext.from_message(request),
         era when era in [:legacy, :modern] <- pin_candidate(context),
         true <- mode_allows_era?(state.protocol_mode, era) do
      %{state | connection_era: era}
    else
      _other -> state
    end
  end

  defp maybe_pin_connection_era(_request, state), do: state

  defp pin_candidate(%RequestContext{era: :modern}), do: :modern
  defp pin_candidate(%RequestContext{era: :legacy, method: "initialize"}), do: :legacy
  defp pin_candidate(_context), do: nil

  defp mode_allows_era?(:modern_only, :legacy), do: false
  defp mode_allows_era?(:legacy_only, :modern), do: false
  defp mode_allows_era?(_mode, _era), do: true

  defp effective_protocol_mode(%{protocol_mode: mode})
       when mode in [:legacy_only, :modern_only],
       do: mode

  defp effective_protocol_mode(%{connection_era: :legacy}), do: :legacy_only
  defp effective_protocol_mode(%{connection_era: :modern}), do: :modern_only
  defp effective_protocol_mode(state), do: state.protocol_mode

  defp put_meta(arguments, nil), do: arguments
  defp put_meta(arguments, meta), do: Map.put(arguments, "_meta", meta)

  defp emit_initialize_telemetry(result) do
    server_name =
      case result do
        %{"serverInfo" => %{"name" => name}} -> name
        %{serverInfo: %{name: name}} -> name
        _ -> "unknown"
      end

    :telemetry.execute(
      [:ex_mcp, :server, :initialize, :completed],
      %{},
      %{server_name: server_name}
    )
  end

  defp protocol_version_from_result(%{"protocolVersion" => version}), do: version
  defp protocol_version_from_result(%{protocolVersion: version}), do: version
  defp protocol_version_from_result(_result), do: nil

  defp send_message(message, state) do
    outbound_message =
      if state.transport == ExMCP.Transport.Local do
        message
      else
        Jason.encode!(message)
      end

    case state.transport.send_message(outbound_message, state.transport_state) do
      {:ok, new_transport_state} ->
        {:ok, %{state | transport_state: new_transport_state}}

      # Transports may also answer with an immediate response payload.
      {:ok, new_transport_state, _response} ->
        {:ok, %{state | transport_state: new_transport_state}}

      error ->
        error
    end
  end
end
