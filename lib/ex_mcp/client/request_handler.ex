defmodule ExMCP.Client.RequestHandler do
  @moduledoc """
  Request/response processing for ExMCP client.

  This module handles all request processing, batch operations, message parsing,
  and response handling for MCP clients.
  """

  require Logger
  alias ExMCP.Internal.Protocol

  @doc """
  Handles individual MCP requests.

  Processes a single MCP request and returns the appropriate GenServer response.
  """
  def handle_request(method, params, from, state) do
    id = Protocol.generate_id()
    request = build_request(method, params, id)

    case send_message(request, state) do
      {:ok, updated_state, response_data} ->
        # Non-SSE HTTP returns response immediately
        case Protocol.parse_message(response_data) do
          {:result, result, _id} ->
            {:reply, {:ok, result}, updated_state}

          {:error, error_data, _id} ->
            {:reply, {:error, error_data}, updated_state}

          _ ->
            {:reply, {:error, :invalid_response}, updated_state}
        end

      {:ok, updated_state} ->
        # SSE and streaming transports - track pending request
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
      with {:ok, encoded_message} <- Protocol.encode_to_string(message) do
        case transport_mod.send_message(encoded_message, transport_state) do
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

  defp find_pending_batch_request(pending_requests) do
    Enum.find(pending_requests, fn
      {_id, {_from, :batch, _ordered_ids, _received_responses}} -> true
      _ -> false
    end)
  end

  defp build_request(method, params, id) do
    %{
      "jsonrpc" => "2.0",
      "method" => method,
      "params" => params || %{}
    }
    |> then(fn req ->
      if id do
        Map.put(req, "id", id)
      else
        req
      end
    end)
  end

  @doc """
  Handles server-to-client requests by routing them to the appropriate handler callback.
  """
  def handle_server_request(method, params, request_id, state) do
    case method do
      "roots/list" ->
        handle_roots_list_request(params, request_id, state)

      _ ->
        # Return method not found error for unsupported requests
        error_response = build_error_response(-32601, "Method not found", request_id)
        send_response(error_response, state)
    end
  end

  defp handle_roots_list_request(_params, request_id, state) do
    # Extract handler from transport_opts
    client_handler = Keyword.get(state.transport_opts, :handler)
    handler_state_opts = Keyword.get(state.transport_opts, :handler_state, [])

    if client_handler && function_exported?(client_handler, :handle_list_roots, 1) do
      # Initialize handler if needed
      handler_state =
        case client_handler.init(handler_state_opts) do
          {:ok, initial_state} -> initial_state
          _ -> %{}
        end

      case client_handler.handle_list_roots(handler_state) do
        {:ok, roots, _new_handler_state} ->
          result = %{"roots" => roots}
          response = build_success_response(result, request_id)
          send_response(response, state)

        {:error, error, _new_handler_state} ->
          error_response = build_error_response(-32603, error, request_id)
          send_response(error_response, state)
      end
    else
      # Handler doesn't implement handle_list_roots or no handler configured
      error_response = build_error_response(-32601, "Method not found", request_id)
      send_response(error_response, state)
    end
  end

  defp build_success_response(result, request_id) do
    %{
      "jsonrpc" => "2.0",
      "result" => result,
      "id" => request_id
    }
  end

  defp build_error_response(code, message, request_id) do
    %{
      "jsonrpc" => "2.0",
      "error" => %{
        "code" => code,
        "message" => message
      },
      "id" => request_id
    }
  end

  defp send_response(response, state) do
    case send_message(response, state) do
      {:ok, updated_state} ->
        {:noreply, updated_state}

      {:error, reason} ->
        Logger.error("Failed to send response to server: #{inspect(reason)}")
        {:noreply, state}
    end
  end
end
