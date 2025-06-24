defmodule ExMCP.Client.ConnectionManager do
  @moduledoc """
  Connection lifecycle management for ExMCP client.

  This module handles all aspects of connection establishment, transport management,
  health checks, and message receiving for MCP clients.
  """

  require Logger
  # alias ExMCP.TransportManager  # Not using full manager for now
  alias ExMCP.Internal.Protocol
  alias ExMCP.Transport.{HTTP, Local, SSE, Stdio, Test}

  @doc """
  Establishes connection using the provided options and updates client state.

  Takes the current client state and connection options, establishes the connection,
  and returns the updated state with connection information.
  """
  def establish_connection(state, opts) do
    with {:ok, transport_manager_opts} <- prepare_transport_config(opts),
         {:ok, {transport_mod, transport_state}} <- connect_transport(transport_manager_opts),
         {:ok, result, state_after_handshake} <- do_handshake(transport_mod, transport_state),
         {:ok, state_after_initialized} <-
           send_initialized(transport_mod, state_after_handshake, result),
         {:ok, receiver_task} <-
           start_receiver_task(self(), transport_mod, state_after_initialized) do
      new_state =
        state
        |> Map.put(:transport_mod, transport_mod)
        |> Map.put(:transport_state, state_after_initialized)
        |> Map.put(:receiver_task, receiver_task)
        |> Map.put(:server_capabilities, result["capabilities"])

      {:ok, new_state}
    else
      {:error, reason} ->
        {:error, reason}

      error ->
        {:error, "Unexpected error during connection: #{inspect(error)}"}
    end
  end

  @doc """
  The message receiving loop.

  This function is intended to be run in a separate process (e.g., a Task).
  It continuously receives messages from the transport and forwards them to the parent process.
  """
  def receive_loop(parent, transport_mod, transport_state) do
    case transport_mod.receive_message(transport_state) do
      {:ok, message, new_state} ->
        send(parent, {:transport_message, message})
        receive_loop(parent, transport_mod, new_state)

      {:error, :closed} ->
        send(parent, {:transport_closed, :normal})
        :ok

      {:error, reason} ->
        Logger.error("Transport error in receive loop: #{inspect(reason)}")
        send(parent, {:transport_closed, reason})
        :ok
    end
  end

  # Private Functions

  defp connect_transport(transport_manager_opts) do
    # For now, just connect to the first transport directly
    case Keyword.get(transport_manager_opts, :transports) do
      [{transport_mod, transport_opts} | _] ->
        case transport_mod.connect(transport_opts) do
          {:ok, transport_state} -> {:ok, {transport_mod, transport_state}}
          error -> error
        end

      [] ->
        {:error, "No transports configured"}

      nil ->
        # Single transport specified
        case Keyword.get(transport_manager_opts, :transports) do
          [{transport_mod, transport_opts}] ->
            case transport_mod.connect(transport_opts) do
              {:ok, transport_state} -> {:ok, {transport_mod, transport_state}}
              error -> error
            end

          _ ->
            {:error, "No transport specified"}
        end
    end
  end

  def prepare_transport_config(opts) do
    cond do
      Keyword.has_key?(opts, :transports) ->
        # Multiple transports specified
        transport_manager_opts =
          Keyword.take(opts, [
            :transports,
            :fallback_strategy,
            :max_retries,
            :retry_interval
          ])

        normalized_transports =
          Enum.map(transport_manager_opts[:transports], &normalize_transport_spec(&1, opts))

        {:ok, Keyword.put(transport_manager_opts, :transports, normalized_transports)}

      Keyword.has_key?(opts, :transport) ->
        # Single transport specified
        transport_spec = Keyword.get(opts, :transport)
        {:ok, [transports: [normalize_transport_spec(transport_spec, opts)]]}

      true ->
        {:error, "No transport specified. Please provide :transport or :transports option."}
    end
  end

  defp normalize_transport_spec(transport, opts) when is_atom(transport) do
    {transport_mod, mode_opts} =
      case transport do
        :native -> {Local, [mode: :native]}
        :beam -> {Local, [mode: :beam]}
        :stdio -> {Stdio, []}
        :http -> {HTTP, []}
        :sse -> {SSE, []}
        :test -> {Test, []}
        mod when is_atom(mod) -> {mod, []}
      end

    {transport_mod, Keyword.merge(mode_opts, opts)}
  end

  defp normalize_transport_spec({transport, transport_opts}, _opts) do
    normalize_transport_spec(transport, transport_opts)
  end

  defp do_handshake(transport_mod, transport_state) do
    raw_terms_enabled = check_transport_capabilities(transport_mod, transport_state)

    case send_initialize_request(transport_mod, transport_state, raw_terms_enabled) do
      {:ok, state_after_send, response_data} ->
        # Non-SSE HTTP mode - response came back immediately
        parse_handshake_response(response_data, state_after_send)

      {:ok, state_after_send} ->
        # SSE mode or other transports - need to receive separately
        with {:ok, response_data, state_after_receive} <-
               receive_handshake_message(transport_mod, state_after_send) do
          parse_handshake_response(response_data, state_after_receive)
        end

      error ->
        error
    end
  end

  defp check_transport_capabilities(transport_mod, transport_state) do
    function_exported?(transport_mod, :supports_raw_terms?, 1) and
      transport_mod.supports_raw_terms?(transport_state)
  end

  defp send_initialize_request(transport_mod, transport_state, raw_terms_enabled) do
    client_info = %{
      "name" => "ExMCP",
      "version" => "0.8.0"
    }

    capabilities = %{"experimental" => %{"rawTerms" => raw_terms_enabled}}
    request = Protocol.encode_initialize(client_info, capabilities, nil)

    # Encode the request to JSON string before sending
    with {:ok, encoded_request} <- Protocol.encode_to_string(request) do
      case transport_mod.send_message(encoded_request, transport_state) do
        {:ok, new_state, response_data} ->
          # Non-SSE HTTP mode returns response immediately
          {:ok, new_state, response_data}

        {:ok, new_state} ->
          # SSE mode or other transports
          {:ok, new_state}

        error ->
          error
      end
    end
  end

  defp receive_handshake_message(transport_mod, transport_state) do
    # Note: Transport behaviour doesn't support timeout parameter
    case transport_mod.receive_message(transport_state) do
      {:ok, message, new_state} ->
        {:ok, message, new_state}

      {:error, reason} ->
        {:error, "Failed to receive handshake response: #{inspect(reason)}"}
    end
  end

  defp parse_handshake_response(response_data, transport_state) do
    case Protocol.parse_message(response_data) do
      {:result, result, _id} ->
        {:ok, result, transport_state}

      {:error, error_details, _id} ->
        Logger.debug("Handshake error details: #{inspect(error_details)}")

        # Extract error code for cleaner error reporting
        error_code = error_details["code"]
        error_message = error_details["message"] || "Unknown error"

        case error_code do
          -32600 -> {:error, :invalid_request}
          -32601 -> {:error, {:method_not_found, error_message}}
          _ -> {:error, "Handshake failed: #{error_message}"}
        end

      {:error, :invalid_message} ->
        {:error, "Failed to parse handshake response: invalid message format"}

      other ->
        {:error, "Unexpected handshake response: #{inspect(other)}"}
    end
  end

  defp send_initialized(transport_mod, transport_state, _result) do
    notification = Protocol.encode_initialized()

    # Encode the notification to JSON string before sending
    with {:ok, encoded_notification} <- Protocol.encode_to_string(notification) do
      case transport_mod.send_message(encoded_notification, transport_state) do
        {:ok, new_state, _response_data} ->
          # Non-SSE HTTP mode may return response (ignore it for notifications)
          {:ok, new_state}

        {:ok, new_state} ->
          # SSE mode or other transports
          {:ok, new_state}

        error ->
          error
      end
    end
  end

  defp start_receiver_task(parent, transport_mod, transport_state) do
    task = Task.async(fn -> __MODULE__.receive_loop(parent, transport_mod, transport_state) end)
    {:ok, task}
  end
end
