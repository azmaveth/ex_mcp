defmodule ExMCP.Client.RequestHandler do
  @moduledoc """
  Request/response processing for ExMCP client.

  This module handles all request processing, batch operations, message parsing,
  and response handling for MCP clients.
  """

  require Logger
  alias ExMCP.Internal.{JSONRPC, Maps, Protocol}
  alias ExMCP.Protocol.ResponseBuilder

  # Extra time allowed past a caller-enforced timeout before the client
  # cleans up its own pending-request bookkeeping.
  @caller_timeout_grace 1_000

  @doc """
  Handles individual MCP requests.

  Processes a single MCP request and returns the appropriate GenServer response.

  The optional `meta` map controls request timeout enforcement:

  - `%{timeout: nil}` - the caller did not specify a timeout, so the client
    process enforces its own `default_timeout` by scheduling a
    `{:request_timeout, id}` message for pending requests
  - `%{timeout: integer}` - the caller enforces the timeout on its side
    (via the `GenServer.call/3` timeout), so no timer is scheduled
  - `%{timeout: :caller_enforced}` - legacy `{:request, method, params}`
    calls; no timer is scheduled
  """
  def handle_request(method, params, from, state, meta \\ %{timeout: :caller_enforced}) do
    id = Protocol.generate_id()
    request = build_request(method, params, id)

    case send_message(request, state) do
      {:ok, updated_state, response_data} ->
        # Non-SSE HTTP returns response immediately
        case Protocol.parse_message(response_data) do
          {:result, result, _id} ->
            :telemetry.execute(
              [:ex_mcp, :client, :request, :completed],
              %{},
              %{method: method, request_id: id}
            )

            {:reply, {:ok, result}, updated_state}

          {:error, error_data, _id} ->
            :telemetry.execute(
              [:ex_mcp, :client, :request, :completed],
              %{},
              %{method: method, request_id: id}
            )

            {:reply, {:error, error_data}, updated_state}

          _ ->
            {:reply, {:error, :invalid_response}, updated_state}
        end

      {:ok, updated_state} ->
        # SSE and streaming transports - track pending request
        maybe_schedule_request_timeout(id, meta, updated_state)
        pending_requests = Map.put(updated_state.pending_requests, id, {from, :single})
        new_state = %{updated_state | pending_requests: pending_requests}
        {:noreply, new_state}

      {:error, :not_connected} ->
        {:reply, {:error, :not_connected}, state}

      {:error, reason} ->
        response =
          {:error,
           %{type: :transport_error, message: "Failed to send request: #{inspect(reason)}"}}

        {:reply, response, state}
    end
  end

  # When the caller did not provide an explicit timeout, the client process
  # enforces its configured default (resolved from its own state — no extra
  # GenServer round-trips). Stale timers for completed requests are ignored
  # by the {:request_timeout, id} handler.
  defp maybe_schedule_request_timeout(id, %{timeout: nil}, state) do
    timeout = state.default_timeout || 5_000
    Process.send_after(self(), {:request_timeout, id}, timeout)
    :ok
  end

  # An explicit timeout is enforced by the caller's own `GenServer.call/3`,
  # but the client still has to drop its bookkeeping for a request the caller
  # abandoned: a leaked pending entry keeps the client looking permanently
  # busy and suppresses idle health checks. The grace period keeps the
  # caller-side timeout authoritative.
  defp maybe_schedule_request_timeout(id, %{timeout: caller_timeout}, _state)
       when is_integer(caller_timeout) do
    Process.send_after(self(), {:request_timeout, id}, caller_timeout + @caller_timeout_grace)
    :ok
  end

  defp maybe_schedule_request_timeout(_id, _meta, _state), do: :ok

  @doc """
  Handles batch MCP requests.

  Processes multiple MCP requests in a single batch operation.
  """
  def handle_batch_request(requests, from, state) do
    requests_with_ids =
      Enum.map(requests, fn request ->
        case request do
          # Handle {method, params} tuple format
          {method, params} ->
            id = Protocol.generate_id()
            {id, build_request(method, params, id)}

          # Handle pre-formatted JSON-RPC request map
          %{"method" => _method, "params" => _params, "id" => id} ->
            {id, request}

          # Handle pre-formatted request without ID
          %{"method" => _method, "params" => _params} ->
            id = Protocol.generate_id()
            request_with_id = Map.put(request, "id", id)
            {id, request_with_id}

          # Handle pre-formatted request without params
          %{"method" => _method} = req_map ->
            id = Map.get(req_map, "id", Protocol.generate_id())
            params = Map.get(req_map, "params", %{})
            request_with_id = req_map |> Map.put("id", id) |> Map.put("params", params)
            {id, request_with_id}
        end
      end)

    ordered_ids = Enum.map(requests_with_ids, &elem(&1, 0))
    protocol_requests = Enum.map(requests_with_ids, &elem(&1, 1))

    case send_message(protocol_requests, state) do
      {:ok, updated_state} ->
        batch_id = Protocol.generate_id()
        batch_info = {from, :batch, ordered_ids, %{}}

        new_pending_requests =
          Enum.reduce(ordered_ids, updated_state.pending_requests, fn req_id, acc ->
            Map.put(acc, req_id, batch_id)
          end)
          |> Map.put(batch_id, batch_info)

        new_state = %{updated_state | pending_requests: new_pending_requests}
        {:noreply, new_state}

      {:error, reason} ->
        response =
          {:error,
           %{
             type: :transport_error,
             message: "Failed to send batch request: #{inspect(reason)}"
           }}

        {:reply, response, state}
    end
  end

  @doc """
  Sends a protocol-level `ping` on behalf of the client's idle health check.

  The ping is deliberately kept out of `pending_requests`: it has no caller to
  reply to and must not show up in `ExMCP.Client.get_pending_requests/1`.

  Returns:

  - `{:ok, request_id, state}` - ping sent, the pong will arrive asynchronously
  - `{:ok, nil, state}` - a synchronous transport answered inline, so the
    connection is already proven alive
  - `{:error, reason}` - the ping could not be sent
  """
  @spec send_ping(map()) :: {:ok, term() | nil, map()} | {:error, any()}
  def send_ping(state) do
    id = Protocol.generate_id()

    case send_message(build_request("ping", %{}, id), state) do
      {:ok, updated_state, _response_data} -> {:ok, nil, updated_state}
      {:ok, updated_state} -> {:ok, id, updated_state}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Parses a message from the transport.

  This function is intended to be called from the client's `handle_info/2` callback.
  It decodes the message and delegates to the appropriate response handler.
  """
  def parse_transport_message(message, state) do
    case Protocol.parse_message(message) do
      {:result, result, id} ->
        handle_single_response({:result, result, id}, state)

      {:error, error, id} ->
        handle_single_response({:error, error, id}, state)

      {:notification, "notifications/cancelled", params} ->
        Logger.info("Received notification: notifications/cancelled")
        handle_cancellation_notification(params, state)

      {:notification, method, _params} ->
        Logger.info("Received notification: #{method}")
        {:noreply, state}

      {:request, method, params, id} ->
        handle_server_request(method, params, id, state)

      {:batch, responses} ->
        # NOTE: Batch support is deprecated in protocol version 2025-06-18
        # but maintained for backward compatibility with older versions
        parsed_responses = Protocol.parse_batch_response(responses)
        handle_batch_response(parsed_responses, state)

      {:error, reason} ->
        Logger.error("Failed to parse transport message: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  @doc """
  Handles a batch of responses from the transport.
  """
  def handle_batch_response(responses, state) do
    Enum.reduce(responses, {:noreply, state}, fn response, {:noreply, current_state} ->
      handle_single_response(response, current_state)
    end)
  end

  @doc """
  Handles a single response from the transport.
  """
  def handle_single_response({:result, result, response_id}, state) do
    handle_response_by_id(response_id, {:ok, result}, state)
  end

  def handle_single_response({:error, error, response_id}, state) do
    # Keep raw error data - let format handling in make_request decide how to format it
    handle_response_by_id(response_id, {:error, error}, state)
  end

  def handle_single_response(other, state) do
    Logger.warning("Received unexpected response format: #{inspect(other)}")
    {:noreply, state}
  end

  # Reply to the client's own health-check ping. Any answer — result or
  # error — proves the connection is alive, so the outstanding ping is simply
  # cleared. These ids are never in pending_requests.
  defp handle_response_by_id(response_id, _response_data, %{health_check_id: ping_id} = state)
       when not is_nil(ping_id) and response_id == ping_id do
    {:noreply, %{state | health_check_id: nil, last_activity: System.system_time(:second)}}
  end

  defp handle_response_by_id(response_id, response_data, state) do
    if is_nil(response_id) do
      # Check if this is a batch validation error - if we have any pending batch requests,
      # route the error to the first one (batch errors apply to the entire batch)
      case find_pending_batch_request(state.pending_requests) do
        {batch_id, {from, :batch, ordered_ids, _received_responses}} ->
          GenServer.reply(from, response_data)
          # Clean up all individual request IDs and the batch ID
          new_pending_requests =
            Enum.reduce(ordered_ids, state.pending_requests, &Map.delete(&2, &1))
            |> Map.delete(batch_id)

          new_state = %{state | pending_requests: new_pending_requests}
          {:noreply, new_state}

        nil ->
          Logger.warning("Received response without an ID: #{inspect(response_data)}")
          {:noreply, state}
      end
    else
      pending_requests = state.pending_requests

      new_state =
        case get_request_info(pending_requests, response_id) do
          {:ok, {from, :single}} ->
            :telemetry.execute(
              [:ex_mcp, :client, :request, :completed],
              %{},
              %{method: nil, request_id: response_id}
            )

            GenServer.reply(from, response_data)
            new_pending_requests = Map.delete(pending_requests, response_id)
            %{state | pending_requests: new_pending_requests}

          {:ok, {:batch, batch_id}} ->
            handle_batch_response_item(response_data, response_id, batch_id, state)

          :error ->
            Logger.warning("Received response for unknown request ID: #{response_id}")
            state
        end

      {:noreply, new_state}
    end
  end

  defp get_request_info(pending_requests, response_id) do
    case Map.get(pending_requests, response_id) do
      nil ->
        :error

      {_from, :single} = single_request_info ->
        {:ok, single_request_info}

      batch_id ->
        {:ok, {:batch, batch_id}}
    end
  end

  defp handle_batch_response_item(response_data, response_id, batch_id, state) do
    pending_requests = state.pending_requests

    case Map.get(pending_requests, batch_id) do
      {from, :batch, ordered_ids, received_responses} ->
        # response_data is already parsed: {:ok, result} or {:error, error}
        new_received = Map.put(received_responses, response_id, response_data)

        if map_size(new_received) == length(ordered_ids) do
          # Batch complete
          final_responses = Enum.map(ordered_ids, &new_received[&1])
          GenServer.reply(from, {:ok, final_responses})

          # Clean up
          new_pending_requests =
            Enum.reduce(ordered_ids, pending_requests, &Map.delete(&2, &1))
            |> Map.delete(batch_id)

          %{state | pending_requests: new_pending_requests}
        else
          # Batch not yet complete
          new_batch_info = {from, :batch, ordered_ids, new_received}
          new_pending_requests = Map.put(pending_requests, batch_id, new_batch_info)
          %{state | pending_requests: new_pending_requests}
        end

      _ ->
        Logger.error(
          "Inconsistent state: found batch_id #{inspect(batch_id)} for request #{response_id}, but no batch info."
        )

        state
    end
  end

  @doc """
  Handles a notification to be sent to the server.
  """
  def handle_cast_notification(method, params, state) do
    # A notification is a request object without an "id" member.
    # We assume build_request handles a nil id by omitting it.
    notification = build_request(method, params, nil)

    case send_message(notification, state) do
      {:ok, updated_state, _response_data} ->
        # Non-SSE HTTP returns response but we ignore it for notifications
        {:noreply, updated_state}

      {:ok, updated_state} ->
        {:noreply, updated_state}

      {:error, :not_connected} ->
        # This is expected in tests when clients are disconnected
        Logger.debug("Cannot send notification: client not connected")
        {:noreply, state}

      {:error, reason} ->
        Logger.error("Failed to send notification: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  @doc """
  Encodes and sends a message via the transport.
  """
  def send_message(message, state) do
    %{transport_mod: transport_mod, transport_state: transport_state} = state

    # Check if transport is available
    if transport_mod == nil or transport_state == nil do
      {:error, :not_connected}
    else
      with {:ok, outbound_message} <- encode_for_transport(transport_mod, message) do
        case transport_mod.send_message(outbound_message, transport_state) do
          {:ok, new_transport_state, response_data} ->
            # Non-SSE HTTP returns response immediately
            {:ok, %{state | transport_state: new_transport_state}, response_data}

          {:ok, new_transport_state} ->
            # SSE and other streaming transports return 2-tuple
            {:ok, %{state | transport_state: new_transport_state}}

          {:error, reason} ->
            {:error, reason}
        end
      end
    end
  end

  defp encode_for_transport(ExMCP.Transport.Local, message), do: {:ok, message}
  defp encode_for_transport(_transport_mod, message), do: Protocol.encode_to_string(message)

  defp find_pending_batch_request(pending_requests) do
    Enum.find(pending_requests, fn
      {_id, {_from, :batch, _ordered_ids, _received_responses}} -> true
      _ -> false
    end)
  end

  defp build_request(method, params, id) do
    method
    |> JSONRPC.request(params || %{})
    |> Maps.put_present("id", id)
  end

  @doc """
  Handles server-to-client requests by routing them to the appropriate handler callback.
  """
  def handle_server_request(method, params, request_id, state) do
    case method do
      "ping" ->
        handle_ping_request(params, request_id, state)

      "roots/list" ->
        handle_roots_list_request(params, request_id, state)

      "sampling/createMessage" ->
        handle_create_message_request(params, request_id, state)

      "elicitation/create" ->
        handle_elicitation_create_request(params, request_id, state)

      _ ->
        # Try generic handler, then fall back to method not found
        handle_generic_server_request(method, params, request_id, state)
    end
  end

  defp handle_ping_request(_params, request_id, state) do
    {handler, handler_state, state} = ensure_client_handler(state)

    if handler != :none && function_exported?(handler, :handle_ping, 1) do
      case handler.handle_ping(handler_state) do
        {:ok, result, new_handler_state} ->
          state = update_handler_state(state, new_handler_state)
          send_response(build_success_response(result, request_id), state)

        {:error, error, new_handler_state} ->
          state = update_handler_state(state, new_handler_state)
          send_response(handler_error_response(error, request_id), state)
      end
    else
      # Ping is a protocol-level operation - always respond with success
      # regardless of whether a client handler exists
      response = build_success_response(%{}, request_id)
      send_response(response, state)
    end
  end

  defp handle_roots_list_request(_params, request_id, state) do
    {handler, handler_state, state} = ensure_client_handler(state)

    if handler != :none && function_exported?(handler, :handle_list_roots, 1) do
      case handler.handle_list_roots(handler_state) do
        {:ok, roots, new_handler_state} ->
          state = update_handler_state(state, new_handler_state)
          send_response(build_success_response(%{"roots" => roots}, request_id), state)

        {:error, error, new_handler_state} ->
          state = update_handler_state(state, new_handler_state)
          send_response(handler_error_response(error, request_id), state)
      end
    else
      # Handler doesn't implement handle_list_roots or no handler configured
      error_response = build_error_response(-32601, "Method not found", request_id)
      send_response(error_response, state)
    end
  end

  defp handle_create_message_request(params, request_id, state) do
    {handler, handler_state, state} = ensure_client_handler(state)

    if handler != :none && function_exported?(handler, :handle_create_message, 2) do
      run_handler_async(
        :create_message,
        fn -> handler.handle_create_message(params, handler_state) end,
        request_id,
        state
      )
    else
      error_response = build_error_response(-32601, "Method not found", request_id)
      send_response(error_response, state)
    end
  end

  defp handle_elicitation_create_request(params, request_id, state) do
    {handler, handler_state, state} = ensure_client_handler(state)

    if handler != :none && function_exported?(handler, :handle_elicitation_create, 3) do
      message = Map.get(params, "message", "")
      requested_schema = Map.get(params, "requestedSchema", %{})

      run_handler_async(
        :elicitation,
        fn -> handler.handle_elicitation_create(message, requested_schema, handler_state) end,
        request_id,
        state
      )
    else
      # No custom handler — check if client declared elicitation capability
      capabilities = Keyword.get(state.transport_opts, :capabilities, %{})
      elicitation_cap = capabilities["elicitation"] || capabilities[:elicitation]

      if elicitation_cap do
        # Client declared elicitation support — use default handler (decline or auto-accept)
        message = Map.get(params, "message", "")
        requested_schema = Map.get(params, "requestedSchema", %{})

        result = ExMCP.Client.ElicitationHandler.handle(message, requested_schema)
        response = build_success_response(result, request_id)
        send_response(response, state)
      else
        # Client did not declare elicitation capability — method not found
        error_response = build_error_response(-32601, "Method not found", request_id)
        send_response(error_response, state)
      end
    end
  end

  # Extract module and handler args from the handler option.
  # The handler can be specified as just a module or as {module, args}.
  defp extract_handler_info(state) do
    raw_handler = Keyword.get(state.transport_opts, :handler)
    default_handler_state = Keyword.get(state.transport_opts, :handler_state, [])

    case raw_handler do
      {module, args} when is_atom(module) and is_list(args) ->
        Code.ensure_loaded(module)
        {module, args}

      module when is_atom(module) and not is_nil(module) ->
        Code.ensure_loaded(module)
        {module, default_handler_state}

      _ ->
        {nil, default_handler_state}
    end
  end

  defp handle_cancellation_notification(params, state) do
    request_id = Map.get(params, "requestId")

    if request_id do
      # Mark request as cancelled
      updated_state = %{
        state
        | cancelled_requests: MapSet.put(state.cancelled_requests, request_id)
      }

      # Check if this request is still pending and complete it with :cancelled error
      case Map.get(state.pending_requests, request_id) do
        nil ->
          # Request already completed or doesn't exist
          {:noreply, updated_state}

        {from, :single} ->
          # Reply with cancelled error and remove from pending
          GenServer.reply(from, {:error, :cancelled})
          new_pending = Map.delete(state.pending_requests, request_id)
          {:noreply, %{updated_state | pending_requests: new_pending}}

        _ ->
          # Other types of requests (batch, etc.)
          {:noreply, updated_state}
      end
    else
      Logger.warning("Received cancellation notification without requestId")
      {:noreply, state}
    end
  end

  defp build_success_response(result, request_id) do
    ResponseBuilder.build_success_response(result, request_id)
  end

  defp build_error_response(code, message, request_id) do
    ResponseBuilder.build_error_response(code, message, nil, request_id)
  end

  defp handle_generic_server_request(method, params, request_id, state) do
    {handler, handler_state, state} = ensure_client_handler(state)

    if handler != :none &&
         function_exported?(handler, :handle_server_request, 3) do
      run_handler_async(
        :generic,
        fn -> handler.handle_server_request(method, params, handler_state) end,
        request_id,
        state
      )
    else
      error_response = build_error_response(-32601, "Method not found", request_id)
      send_response(error_response, state)
    end
  end

  # -- Client handler lifecycle -------------------------------------------

  # Initializes the configured client handler once and memoizes it in the
  # client state; subsequent callbacks reuse (and update) the same handler
  # state, so stateful client handlers work.
  defp ensure_client_handler(%{client_handler: {module, handler_state}} = state) do
    {module, handler_state, state}
  end

  defp ensure_client_handler(%{client_handler: :none} = state) do
    {:none, %{}, state}
  end

  defp ensure_client_handler(state) do
    case extract_handler_info(state) do
      {nil, _opts} ->
        {:none, %{}, %{state | client_handler: :none}}

      {module, opts} ->
        handler_state =
          case module.init(opts) do
            {:ok, initial_state} -> initial_state
            _ -> %{}
          end

        {module, handler_state, %{state | client_handler: {module, handler_state}}}
    end
  end

  defp update_handler_state(%{client_handler: {module, _old}} = state, new_handler_state) do
    %{state | client_handler: {module, new_handler_state}}
  end

  defp update_handler_state(state, _new_handler_state), do: state

  # Runs a potentially slow client handler callback (LLM sampling, user
  # elicitation, custom methods) in an unlinked monitored process so it
  # cannot head-of-line-block the client loop. The result is delivered via
  # `handle_server_request_completion/3`; crashes via
  # `handle_server_request_down/3`.
  defp run_handler_async(kind, fun, request_id, state) do
    parent = self()

    {pid, ref} =
      spawn_monitor(fn ->
        outcome =
          try do
            {:ok, fun.()}
          rescue
            error -> {:handler_raised, error, __STACKTRACE__}
          catch
            thrown_kind, value -> {:handler_caught, {thrown_kind, value}}
          end

        send(parent, {:server_request_result, self(), outcome})
      end)

    tasks = Map.put(state.server_request_tasks || %{}, pid, {ref, request_id, kind})
    {:noreply, %{state | server_request_tasks: tasks}}
  end

  @doc false
  # Completion of an async server-request handler task.
  def handle_server_request_completion(task_pid, outcome, state) do
    case Map.pop(state.server_request_tasks || %{}, task_pid) do
      {nil, _tasks} ->
        {:noreply, state}

      {{ref, request_id, kind}, tasks} ->
        Process.demonitor(ref, [:flush])
        state = %{state | server_request_tasks: tasks}

        case outcome do
          {:ok, callback_return} ->
            {response, state} = map_callback_return(kind, callback_return, request_id, state)
            send_response(response, state)

          {:handler_raised, error, stacktrace} ->
            Logger.error(
              "Client handler for #{kind} raised: " <>
                Exception.format(:error, error, stacktrace)
            )

            send_response(internal_handler_error(request_id), state)

          {:handler_caught, detail} ->
            Logger.error("Client handler for #{kind} exited: #{inspect(detail)}")
            send_response(internal_handler_error(request_id), state)
        end
    end
  end

  @doc false
  # An async handler task died without delivering a result.
  def handle_server_request_down(task_pid, reason, state) do
    case Map.pop(state.server_request_tasks || %{}, task_pid) do
      {nil, _tasks} ->
        {:noreply, state}

      {{_ref, request_id, kind}, tasks} ->
        state = %{state | server_request_tasks: tasks}
        Logger.error("Client handler task for #{kind} exited: #{inspect(reason)}")
        send_response(internal_handler_error(request_id), state)
    end
  end

  defp map_callback_return(kind, callback_return, request_id, state) do
    case callback_return do
      {:ok, result, new_handler_state} ->
        state = update_handler_state(state, new_handler_state)
        {build_success_response(wrap_result(kind, result), request_id), state}

      {:error, error, new_handler_state} ->
        state = update_handler_state(state, new_handler_state)
        {kind_error_response(kind, error, request_id), state}

      other ->
        Logger.error("Client handler for #{kind} returned unexpected value: #{inspect(other)}")
        {internal_handler_error(request_id), state}
    end
  end

  defp wrap_result(:roots, roots), do: %{"roots" => roots}
  defp wrap_result(_kind, result), do: result

  defp kind_error_response(:create_message, error, request_id) do
    # Sampling errors may carry an explicit JSON-RPC code/message.
    case error do
      %{"code" => code, "message" => message} when is_integer(code) and is_binary(message) ->
        build_error_response(code, message, request_id)

      _ ->
        handler_error_response(error, request_id)
    end
  end

  defp kind_error_response(_kind, error, request_id) do
    handler_error_response(error, request_id)
  end

  # Builds a -32603 response from a handler-provided error without leaking
  # arbitrary internal terms to the server: binaries pass through, anything
  # else is logged and replaced with a generic message.
  defp handler_error_response(error, request_id) when is_binary(error) do
    build_error_response(-32603, error, request_id)
  end

  defp handler_error_response(error, request_id) do
    Logger.error("Client handler error: #{inspect(error)}")
    internal_handler_error(request_id)
  end

  defp internal_handler_error(request_id) do
    build_error_response(-32603, "Internal error in client handler", request_id)
  end

  defp send_response(response, state) do
    case send_message(response, state) do
      {:ok, updated_state} ->
        {:noreply, updated_state}

      {:ok, updated_state, response_data} ->
        # Synchronous transports (non-SSE HTTP) may return a body inline even
        # for replies to server-initiated requests. Any payload is delivered
        # through the normal parse path instead of crashing on the 3-tuple.
        case response_data do
          data when data in [nil, ""] ->
            {:noreply, updated_state}

          data ->
            parse_transport_message(data, updated_state)
        end

      {:error, reason} ->
        Logger.error("Failed to send response to server: #{inspect(reason)}")
        {:noreply, state}
    end
  end
end
