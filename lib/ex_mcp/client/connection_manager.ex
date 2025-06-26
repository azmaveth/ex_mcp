defmodule ExMCP.Client.ConnectionManager do
  @moduledoc """
  Connection lifecycle management for ExMCP client.

  This module handles all aspects of connection establishment, transport management,
  health checks, and message receiving for MCP clients.
  """

  require Logger
  # alias ExMCP.TransportManager  # Not using full manager for now
  alias ExMCP.Internal.Protocol
  alias ExMCP.Reliability.Retry
  alias ExMCP.Transport.{HTTP, Local, SSE, Stdio, Test, ReliabilityWrapper}

  @doc """
  Establishes connection using the provided options and updates client state.

  Takes the current client state and connection options, establishes the connection,
  and returns the updated state with connection information.

  Supports retry policies for connection establishment through the :retry_policy option.
  """
  def establish_connection(state, opts) do
    retry_policy = Keyword.get(opts, :retry_policy, [])

    if retry_policy != [] do
      establish_connection_with_retry(state, opts, retry_policy)
    else
      do_establish_connection(state, opts)
    end
  end

  @doc """
  Establishes connection with retry logic applied.
  """
  def establish_connection_with_retry(state, opts, retry_policy) do
    connection_operation = fn ->
      do_establish_connection(state, opts)
    end

    retry_opts = Retry.mcp_defaults(retry_policy)
    Retry.with_retry(connection_operation, retry_opts)
  end

  defp do_establish_connection(state, opts) do
    with {:ok, transport_manager_opts} <- prepare_transport_config(opts),
         {:ok, {transport_mod, transport_state}} <- connect_transport(transport_manager_opts),
         {:ok, result, state_after_handshake} <-
           do_handshake(transport_mod, transport_state, opts),
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
        |> Map.put(:protocol_version, result["protocolVersion"])
        |> Map.put(:server_info, result["serverInfo"])

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
    reliability_opts = Keyword.get(transport_manager_opts, :reliability, [])

    # For now, just connect to the first transport directly
    case Keyword.get(transport_manager_opts, :transports) do
      [{transport_mod, transport_opts} | _] ->
        connect_with_reliability(transport_mod, transport_opts, reliability_opts)

      [] ->
        {:error, "No transports configured"}

      nil ->
        # Single transport specified
        case Keyword.get(transport_manager_opts, :transports) do
          [{transport_mod, transport_opts}] ->
            connect_with_reliability(transport_mod, transport_opts, reliability_opts)

          _ ->
            {:error, "No transport specified"}
        end
    end
  end

  defp connect_with_reliability(transport_mod, transport_opts, reliability_opts) do
    case transport_mod.connect(transport_opts) do
      {:ok, transport_state} ->
        if reliability_opts != [] do
          # Wrap with reliability features
          {:ok, wrapped_state} =
            ReliabilityWrapper.wrap(transport_mod, transport_state, reliability_opts)

          {:ok, {ReliabilityWrapper, wrapped_state}}
        else
          # No reliability features requested
          {:ok, {transport_mod, transport_state}}
        end

      error ->
        error
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
            :retry_interval,
            :reliability
          ])

        normalized_transports =
          Enum.map(transport_manager_opts[:transports], &normalize_transport_spec(&1, opts))

        # Check for any errors in normalization
        case Enum.find(normalized_transports, &match?({:error, _}, &1)) do
          {:error, reason} -> {:error, reason}
          nil -> {:ok, Keyword.put(transport_manager_opts, :transports, normalized_transports)}
        end

      Keyword.has_key?(opts, :transport) ->
        # Single transport specified
        transport_spec = Keyword.get(opts, :transport)

        case normalize_transport_spec(transport_spec, opts) do
          {:error, reason} ->
            {:error, reason}

          normalized_spec ->
            result = [transports: [normalized_spec]]

            result =
              if Keyword.has_key?(opts, :reliability),
                do: Keyword.put(result, :reliability, opts[:reliability]),
                else: result

            {:ok, result}
        end

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
        :mock -> {Test, []}
        mod when is_atom(mod) -> {mod, []}
      end

    {transport_mod, Keyword.merge(mode_opts, opts)}
  end

  defp normalize_transport_spec({transport, transport_opts}, _opts) do
    normalize_transport_spec(transport, transport_opts)
  end

  defp normalize_transport_spec(transport_spec, opts) when is_list(transport_spec) do
    # Handle keyword list format: [type: :mock, server_pid: pid, ...]
    case Keyword.get(transport_spec, :type) do
      nil ->
        # If no :type key, try to infer from the presence of known keys
        cond do
          Keyword.has_key?(transport_spec, :server_pid) ->
            # Convert :server_pid to :server for Test transport
            server_pid = Keyword.get(transport_spec, :server_pid)

            test_opts =
              transport_spec |> Keyword.delete(:server_pid) |> Keyword.put(:server, server_pid)

            {Test, test_opts}

          Keyword.has_key?(transport_spec, :command) ->
            {Stdio, transport_spec}

          Keyword.has_key?(transport_spec, :url) ->
            {HTTP, transport_spec}

          true ->
            {:error, "Cannot determine transport type from #{inspect(transport_spec)}"}
        end

      transport_type ->
        # Use the :type key to determine the transport module
        transport_spec_without_type = Keyword.delete(transport_spec, :type)

        # Convert :server_pid to :server for Test transport
        transport_spec_normalized =
          if (transport_type == :mock or transport_type == :test) and
               Keyword.has_key?(transport_spec_without_type, :server_pid) do
            server_pid = Keyword.get(transport_spec_without_type, :server_pid)

            transport_spec_without_type
            |> Keyword.delete(:server_pid)
            |> Keyword.put(:server, server_pid)
          else
            transport_spec_without_type
          end

        normalize_transport_spec(transport_type, Keyword.merge(transport_spec_normalized, opts))
    end
  end

  # Handle invalid transport types gracefully
  defp normalize_transport_spec(invalid_transport, _opts) do
    {:error, "Invalid transport specification: #{inspect(invalid_transport)}"}
  end

  defp do_handshake(transport_mod, transport_state, opts) do
    raw_terms_enabled = check_transport_capabilities(transport_mod, transport_state)
    protocol_version = Keyword.get(opts, :protocol_version)

    case send_initialize_request(
           transport_mod,
           transport_state,
           raw_terms_enabled,
           protocol_version
         ) do
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

  defp send_initialize_request(
         transport_mod,
         transport_state,
         raw_terms_enabled,
         protocol_version
       ) do
    client_info = %{
      "name" => "ExMCP",
      "version" => "0.8.0"
    }

    capabilities = %{"experimental" => %{"rawTerms" => raw_terms_enabled}}
    request = Protocol.encode_initialize(client_info, capabilities, protocol_version)

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
