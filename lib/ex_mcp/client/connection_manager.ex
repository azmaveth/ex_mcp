defmodule ExMCP.Client.ConnectionManager do
  @moduledoc """
  Connection lifecycle management for ExMCP client.

  This module handles all aspects of connection establishment, transport management,
  health checks, and message receiving for MCP clients.
  """

  require Logger
  # alias ExMCP.TransportManager  # Not using full manager for now
  alias ExMCP.Client.{EraCache, EraProbe}
  alias ExMCP.Internal.{Protocol, VersionInfo, VersionRegistry}
  alias ExMCP.Reliability.Retry
  alias ExMCP.Transport.{HTTP, Local, ReliabilityWrapper, Stdio, Test}

  @default_handshake_timeout 10_000

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
         era_identity = EraCache.identity(transport_mod, transport_state, opts),
         :ok <- maybe_reset_era_cache(era_identity, opts),
         {:ok, result, state_after_protocol} <-
           establish_protocol(transport_mod, transport_state, opts, era_identity),
         state_after_protocol = settle_transport_era(transport_mod, state_after_protocol, result),
         {:ok, receiver_result} <-
           start_receiver_task(self(), transport_mod, state_after_protocol) do
      # Push mode returns {:push, updated_transport_state} — extract it
      {receiver_task, final_transport_state} =
        case receiver_result do
          {:push, new_ts} -> {:push, new_ts}
          task -> {task, state_after_protocol}
        end

      new_state =
        state
        |> Map.put(:transport_mod, transport_mod)
        |> Map.put(:transport_state, final_transport_state)
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

  defp establish_protocol(transport_mod, transport_state, opts, era_identity) do
    mode = Keyword.get(opts, :protocol_mode) || VersionRegistry.protocol_mode()

    case mode do
      :legacy_only ->
        establish_legacy_protocol(transport_mod, transport_state, opts, era_identity)

      :modern_only ->
        establish_modern_protocol(transport_mod, transport_state, opts, era_identity, false)

      :prefer_modern ->
        establish_prefer_modern(transport_mod, transport_state, opts, era_identity)

      :prefer_legacy ->
        establish_prefer_legacy(transport_mod, transport_state, opts, era_identity)

      invalid ->
        {:error, {:invalid_protocol_mode, invalid}}
    end
  end

  defp establish_legacy_protocol(transport_mod, transport_state, opts, era_identity) do
    with {:ok, result, state_after_handshake} <-
           do_handshake(transport_mod, transport_state, opts),
         {:ok, state_after_initialized} <-
           send_initialized(transport_mod, state_after_handshake, result) do
      emit_settled_era(:legacy, result["protocolVersion"])
      observe_era(era_identity, :legacy, result["protocolVersion"], opts)
      {:ok, result, state_after_initialized}
    end
  end

  defp establish_modern_protocol(transport_mod, transport_state, opts, era_identity, pinned?) do
    case EraProbe.probe(transport_mod, transport_state, opts) do
      {:ok, discovery, updated_state} ->
        emit_settled_era(:modern, discovery.protocol_version)
        observe_era(era_identity, :modern, discovery.protocol_version, opts)
        {:ok, discovery_as_connection_result(discovery), updated_state}

      {:error, reason, _updated_state} ->
        if pinned? do
          {:error,
           {:pinned_modern_era_probe_failed,
            %{probe: reason, action: :clear_era_observation_or_change_configuration}}}
        else
          {:error, {:era_probe_failed, reason}}
        end
    end
  end

  defp establish_prefer_modern(transport_mod, transport_state, opts, era_identity) do
    case cached_era(era_identity) do
      :modern ->
        establish_modern_protocol(transport_mod, transport_state, opts, era_identity, true)

      :legacy ->
        establish_legacy_protocol(transport_mod, transport_state, opts, era_identity)

      :miss ->
        probe_then_maybe_legacy(transport_mod, transport_state, opts, era_identity)
    end
  end

  defp probe_then_maybe_legacy(transport_mod, transport_state, opts, era_identity) do
    case EraProbe.probe(transport_mod, transport_state, opts) do
      {:ok, discovery, updated_state} ->
        emit_settled_era(:modern, discovery.protocol_version)
        observe_era(era_identity, :modern, discovery.protocol_version, opts)
        {:ok, discovery_as_connection_result(discovery), updated_state}

      {:error, probe_error, updated_state} ->
        if legacy_fallback_evidence?(probe_error) and
             transport_alive?(transport_mod, updated_state) do
          case establish_legacy_protocol(
                 transport_mod,
                 updated_state,
                 opts,
                 era_identity
               ) do
            {:ok, _result, _state} = success ->
              :telemetry.execute(
                [:ex_mcp, :client, :era, :fallback],
                %{},
                %{from: :modern, to: :legacy, probe_error: probe_error}
              )

              success

            {:error, initialize_error} ->
              {:error,
               {:era_probe_and_initialize_failed,
                %{probe: probe_error, initialize: initialize_error}}}
          end
        else
          {:error, {:era_probe_failed, probe_error}}
        end
    end
  end

  defp establish_prefer_legacy(transport_mod, transport_state, opts, era_identity) do
    case cached_era(era_identity) do
      :modern ->
        establish_modern_protocol(transport_mod, transport_state, opts, era_identity, true)

      :legacy ->
        establish_legacy_protocol(transport_mod, transport_state, opts, era_identity)

      :miss ->
        initialize_then_maybe_modern(transport_mod, transport_state, opts, era_identity)
    end
  end

  defp initialize_then_maybe_modern(transport_mod, transport_state, opts, era_identity) do
    case establish_legacy_protocol(transport_mod, transport_state, opts, era_identity) do
      {:ok, _result, _state} = success ->
        success

      {:error, initialize_error} ->
        if legacy_protocol_failure?(initialize_error) and
             transport_alive?(transport_mod, transport_state) do
          case EraProbe.probe(transport_mod, transport_state, opts) do
            {:ok, discovery, updated_state} ->
              emit_settled_era(:modern, discovery.protocol_version)
              observe_era(era_identity, :modern, discovery.protocol_version, opts)
              {:ok, discovery_as_connection_result(discovery), updated_state}

            {:error, probe_error, _updated_state} ->
              {:error,
               {:initialize_and_era_probe_failed,
                %{initialize: initialize_error, probe: probe_error}}}
          end
        else
          {:error, initialize_error}
        end
    end
  end

  defp discovery_as_connection_result(discovery) do
    %{
      "protocolVersion" => discovery.protocol_version,
      "capabilities" => discovery.server_capabilities,
      "serverInfo" => discovery.server_info
    }
  end

  defp legacy_fallback_evidence?({:json_rpc_error, error}) do
    not modern_specific_error?(error)
  end

  defp legacy_fallback_evidence?({:probe_timeout, _reason}), do: true
  defp legacy_fallback_evidence?({:http_probe_rejected, _response}), do: true
  defp legacy_fallback_evidence?(_reason), do: false

  defp modern_specific_error?(%{"code" => -32022, "data" => data}) when is_map(data) do
    is_list(data["supported"]) and is_binary(data["requested"])
  end

  defp modern_specific_error?(_error), do: false

  defp legacy_protocol_failure?(:invalid_request), do: true
  defp legacy_protocol_failure?({:method_not_found, _message}), do: true
  defp legacy_protocol_failure?({:initialize_rejected, _error}), do: true
  defp legacy_protocol_failure?(_reason), do: false

  defp transport_alive?(transport_mod, transport_state) do
    if function_exported?(transport_mod, :connected?, 1) do
      transport_mod.connected?(transport_state)
    else
      true
    end
  rescue
    _error -> false
  end

  defp emit_settled_era(era, version) do
    :telemetry.execute(
      [:ex_mcp, :client, :era, :settled],
      %{},
      %{era: era, protocol_version: version}
    )
  end

  defp cached_era(identity) do
    case EraCache.lookup(identity) do
      {:ok, %{era: era, protocol_version: version}} ->
        :telemetry.execute(
          [:ex_mcp, :client, :era, :cache_hit],
          %{},
          %{era: era, protocol_version: version}
        )

        era

      :miss ->
        :miss
    end
  end

  defp maybe_reset_era_cache(identity, opts) do
    if Keyword.get(opts, :reset_era_cache, false), do: EraCache.clear(identity), else: :ok
  end

  defp observe_era(identity, era, version, opts) when is_binary(version) do
    EraCache.observe(identity, era, version, opts)
  end

  defp observe_era(_identity, _era, _version, _opts), do: :ok

  defp settle_transport_era(HTTP, transport_state, result) do
    version = result["protocolVersion"]
    HTTP.settle_protocol_era(transport_state, VersionRegistry.era_for(version), version)
  end

  defp settle_transport_era(ReliabilityWrapper, transport_state, result) do
    case ReliabilityWrapper.unwrap(transport_state) do
      {HTTP, http_state} ->
        version = result["protocolVersion"]
        settled = HTTP.settle_protocol_era(http_state, VersionRegistry.era_for(version), version)
        %{transport_state | wrapped_state: settled}

      _other ->
        transport_state
    end
  end

  defp settle_transport_era(_transport_mod, transport_state, _result), do: transport_state

  @doc """
  The message receiving loop.

  This function is intended to be run in a separate process (e.g., a Task).
  It continuously receives messages from the transport and forwards them to the parent process.
  """
  def receive_loop(parent, transport_mod, transport_state) do
    case transport_mod.receive_message(transport_state) do
      {:ok, message, new_state} ->
        :telemetry.execute(
          [:ex_mcp, :client, :receiver, :message],
          %{},
          %{}
        )

        send(parent, {:transport_message, message})
        receive_loop(parent, transport_mod, new_state)

      {:error, :closed} ->
        send(parent, {:transport_closed, :normal})
        :ok

      {:error, :waiting_for_session} ->
        # SSE not started yet — retry (SSE will start when server provides session ID)
        receive_loop(parent, transport_mod, transport_state)

      {:error, :not_supported_in_sync_mode} ->
        # Non-SSE HTTP mode — responses come from send_message directly.
        # Keep the loop alive but sleep to avoid busy-waiting.
        Process.sleep(100)
        receive_loop(parent, transport_mod, transport_state)

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

      _missing ->
        {:error, "No transport specified"}
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
    case transport do
      :native ->
        {:error, "Unsupported transport :native. Use :beam for local BEAM MCP transport."}

      :sse ->
        {:error, "Unsupported transport :sse. Use :http with use_sse: true."}

      :beam ->
        {Local, opts}

      :stdio ->
        {Stdio, opts}

      :http ->
        {HTTP, opts}

      :test ->
        {Test, opts}

      :mock ->
        {Test, opts}

      mod when is_atom(mod) ->
        {mod, opts}
    end
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
    protocol_version = Keyword.get(opts, :protocol_version)
    handshake_timeout = Keyword.get(opts, :handshake_timeout, @default_handshake_timeout)

    case send_initialize_request(
           transport_mod,
           transport_state,
           protocol_version
         ) do
      {:ok, state_after_send, response_data} ->
        # Non-SSE HTTP mode - response came back immediately
        parse_handshake_response(response_data, state_after_send)

      {:ok, state_after_send} ->
        # SSE mode or other transports - need to receive separately
        with {:ok, response_data, state_after_receive} <-
               receive_handshake_message(transport_mod, state_after_send, handshake_timeout) do
          parse_handshake_response(response_data, state_after_receive)
        end

      error ->
        error
    end
  end

  defp send_initialize_request(
         transport_mod,
         transport_state,
         protocol_version
       ) do
    client_info = VersionInfo.client_info()

    request = Protocol.encode_initialize(client_info, %{}, protocol_version)

    with {:ok, outbound_request} <- encode_for_transport(transport_mod, request) do
      case transport_mod.send_message(outbound_request, transport_state) do
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

  defp encode_for_transport(Local, message), do: {:ok, message}
  defp encode_for_transport(_transport_mod, message), do: Protocol.encode_to_string(message)

  # Receives the initialize response with a bounded wait so a silent server
  # cannot hang client start_link forever. Transports that export
  # receive_message/2 (e.g. the Test transport, which reads the caller's
  # mailbox) are called in-process with the timeout; transports with only the
  # blocking receive_message/1 are wrapped in a task that is shut down on
  # expiry. On timeout the connection attempt fails with :handshake_timeout.
  defp receive_handshake_message(transport_mod, transport_state, timeout) do
    result =
      if function_exported?(transport_mod, :receive_message, 2) do
        transport_mod.receive_message(transport_state, timeout)
      else
        receive_handshake_via_task(transport_mod, transport_state, timeout)
      end

    case result do
      {:ok, message, new_state} ->
        {:ok, message, new_state}

      {:error, :handshake_timeout} ->
        {:error, :handshake_timeout}

      {:error, {:timeout_error, _reason}} ->
        {:error, :handshake_timeout}

      {:error, reason} ->
        {:error, "Failed to receive handshake response: #{inspect(reason)}"}
    end
  end

  defp receive_handshake_via_task(transport_mod, transport_state, timeout) do
    task = Task.async(fn -> transport_mod.receive_message(transport_state) end)

    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      {:exit, reason} -> {:error, {:handshake_receive_failed, reason}}
      nil -> {:error, :handshake_timeout}
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
          _ -> {:error, {:initialize_rejected, error_details}}
        end

      {:error, :invalid_message} ->
        {:error, "Failed to parse handshake response: invalid message format"}

      other ->
        {:error, "Unexpected handshake response: #{inspect(other)}"}
    end
  end

  defp send_initialized(transport_mod, transport_state, _result) do
    notification = Protocol.encode_initialized()

    with {:ok, outbound_notification} <- encode_for_transport(transport_mod, notification) do
      case transport_mod.send_message(outbound_notification, transport_state) do
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
    cond do
      # HTTP non-SSE: no receiver needed (responses come from send_message)
      transport_mod == ExMCP.Transport.HTTP and not transport_state.use_sse ->
        {:ok, nil}

      # Push mode: subscribe instead of polling
      ExMCP.Transport.supports_push?(transport_mod) ->
        case transport_mod.subscribe(parent, transport_state) do
          {:ok, new_state} ->
            :telemetry.execute(
              [:ex_mcp, :client, :receiver, :started],
              %{},
              %{mode: :push}
            )

            # Return :push atom as receiver_task to signal push mode is active.
            # The updated transport_state with subscriber must be stored by caller.
            {:ok, {:push, new_state}}

          {:error, _reason} ->
            # Fall back to polling
            :telemetry.execute(
              [:ex_mcp, :client, :receiver, :started],
              %{},
              %{mode: :pull}
            )

            task =
              Task.async(fn ->
                __MODULE__.receive_loop(parent, transport_mod, transport_state)
              end)

            {:ok, task}
        end

      # Legacy polling mode
      true ->
        :telemetry.execute(
          [:ex_mcp, :client, :receiver, :started],
          %{},
          %{mode: :pull}
        )

        task =
          Task.async(fn ->
            __MODULE__.receive_loop(parent, transport_mod, transport_state)
          end)

        {:ok, task}
    end
  end
end
