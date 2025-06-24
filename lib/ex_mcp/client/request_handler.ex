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
      {:ok, updated_state} ->
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
      Enum.map(requests, fn {method, params} ->
        id = Protocol.generate_id()
        {id, build_request(method, params, id)}
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
        Logger.debug("Received notification: #{method}")
        {:noreply, state}

      {:request, method, _params, _id} ->
        Logger.warning("Received unexpected request from server: #{method}")
        {:noreply, state}

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
    handle_response_by_id(response_id, {:error, error}, state)
  end

  def handle_single_response(other, state) do
    Logger.warning("Received unexpected response format: #{inspect(other)}")
    {:noreply, state}
  end

  defp handle_response_by_id(response_id, response_data, state) do
    if is_nil(response_id) do
      Logger.warning("Received response without an ID: #{inspect(response_data)}")
      {:noreply, state}
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
          {:ok, new_transport_state} ->
            # SSE mode or other transports that only return state
            {:ok, %{state | transport_state: new_transport_state}}

          {:ok, new_transport_state, _response_data} ->
            # HTTP mode that returns response data (we ignore it for send_message)
            {:ok, %{state | transport_state: new_transport_state}}

          {:error, reason} ->
            {:error, reason}
        end
      end
    end
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
end
