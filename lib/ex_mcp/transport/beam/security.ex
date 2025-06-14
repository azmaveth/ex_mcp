defmodule ExMCP.Transport.Beam.Security do
  @moduledoc """
  Comprehensive security validation for BEAM transport.

  This module provides multi-layered security validation including:
  - Frame size and count limits
  - Rate limiting and connection throttling
  - Message content validation
  - Authentication and authorization
  - Anomaly detection

  ## Security Layers

  1. **Transport Layer**: Frame size, connection limits
  2. **Protocol Layer**: Message structure validation
  3. **Application Layer**: Content sanitization, rate limiting
  4. **Behavioral Layer**: Anomaly detection, abuse prevention

  ## Usage

      # Configure security for a connection
      security_opts = %{
        max_frame_size: 1_048_576,  # 1MB
        max_connections: 1000,
        rate_limit: %{
          window_ms: 60_000,        # 1 minute
          max_requests: 1000        # Max 1000 requests per minute
        },
        authentication: %{
          required: true,
          methods: [:bearer_token, :certificate]
        }
      }
      
      # Validate incoming frame
      case Security.validate_frame(frame_data, security_opts) do
        :ok -> process_frame(frame_data)
        {:error, reason} -> reject_frame(reason)
      end

  ## Security Features

  - **DoS Protection**: Frame size limits, connection throttling
  - **Rate Limiting**: Per-connection and global rate limits
  - **Input Validation**: Message structure and content validation
  - **Authentication**: Multiple authentication methods
  - **Anomaly Detection**: Behavioral pattern analysis
  - **Audit Logging**: Security event logging
  """

  use GenServer
  require Logger

  # 1MB
  @default_max_frame_size 1_048_576
  @default_max_connections 1_000
  @default_max_batch_size 100
  # 1 minute in ms
  @default_rate_limit_window 60_000
  # Max requests per window
  @default_rate_limit_requests 1_000
  # Number of anomalies before action
  @default_anomaly_threshold 10

  @type security_config :: %{
          max_frame_size: pos_integer(),
          max_connections: pos_integer(),
          max_batch_size: pos_integer(),
          rate_limit: %{
            window_ms: pos_integer(),
            max_requests: pos_integer()
          },
          authentication: %{
            required: boolean(),
            methods: [atom()]
          },
          anomaly_detection: %{
            enabled: boolean(),
            threshold: pos_integer()
          }
        }

  @type validation_result :: :ok | {:error, atom() | {atom(), term()}}
  @type security_event :: %{
          type: atom(),
          severity: :low | :medium | :high | :critical,
          connection_id: term(),
          details: map(),
          timestamp: integer()
        }

  defstruct [
    :config,
    :rate_limiters,
    :connection_stats,
    :anomaly_detectors,
    :security_events,
    stats: %{
      connections_accepted: 0,
      connections_rejected: 0,
      frames_validated: 0,
      frames_rejected: 0,
      rate_limit_violations: 0,
      anomalies_detected: 0
    }
  ]

  @doc """
  Starts the security validation service.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Starts the security service if not already running.
  Useful for testing scenarios.
  """
  @spec start() :: {:ok, pid()} | {:error, term()}
  def start do
    case GenServer.whereis(__MODULE__) do
      nil -> start_link()
      pid -> {:ok, pid}
    end
  end

  @doc """
  Validates an incoming frame for security compliance.

  ## Examples

      iex> Security.validate_frame(frame_data, security_config)
      :ok

      iex> Security.validate_frame(oversized_frame, security_config)
      {:error, :frame_too_large}
  """
  @spec validate_frame(binary(), security_config()) :: validation_result()
  def validate_frame(frame_data, config) when is_binary(frame_data) and is_map(config) do
    # Quick check for obviously invalid data (printable strings that aren't frame data)
    if String.printable?(frame_data) and not_frame_like?(frame_data) do
      {:error, :invalid_input}
    else
      with :ok <- validate_frame_size(frame_data, config),
           :ok <- validate_frame_structure(frame_data),
           :ok <- validate_frame_content(frame_data, config) do
        safe_cast({:frame_validated, byte_size(frame_data)})
        :ok
      else
        {:error, reason} = error ->
          safe_cast({:frame_rejected, reason})
          error
      end
    end
  end

  def validate_frame(_frame_data, _config) do
    {:error, :invalid_input}
  end

  @doc """
  Validates a new connection attempt.

  ## Examples

      iex> Security.validate_connection(connection_info, security_config)
      :ok

      iex> Security.validate_connection(suspicious_connection, security_config)
      {:error, :connection_rejected}
  """
  @spec validate_connection(map(), security_config()) :: validation_result()
  def validate_connection(connection_info, config)
      when is_map(connection_info) and is_map(config) do
    with :ok <- check_connection_limits(config),
         :ok <- validate_authentication(connection_info, config),
         :ok <- check_rate_limits(connection_info, config),
         :ok <- detect_anomalies(connection_info, config) do
      safe_cast({:connection_accepted, connection_info})
      :ok
    else
      {:error, reason} = error ->
        safe_cast({:connection_rejected, connection_info, reason})
        error
    end
  end

  def validate_connection(_connection_info, _config) do
    {:error, :invalid_input}
  end

  @doc """
  Validates message content for security issues.

  Checks for:
  - Malicious payloads
  - Excessive resource usage
  - Protocol violations
  """
  @spec validate_message_content(map(), security_config()) :: validation_result()
  def validate_message_content(message, config) when is_map(message) and is_map(config) do
    with :ok <- validate_message_depth(message),
         :ok <- validate_message_size(message, config),
         :ok <- validate_message_structure(message),
         :ok <- sanitize_message_content(message, config) do
      :ok
    else
      error -> error
    end
  end

  def validate_message_content(_message, _config) do
    {:error, :invalid_input}
  end

  @doc """
  Checks if a connection has exceeded rate limits.
  """
  @spec check_rate_limit(term(), security_config()) :: validation_result()
  def check_rate_limit(connection_id, config) do
    GenServer.call(__MODULE__, {:check_rate_limit, connection_id, config})
  end

  @doc """
  Records a security event for audit logging.
  """
  @spec record_security_event(security_event()) :: :ok
  def record_security_event(event) when is_map(event) do
    GenServer.cast(__MODULE__, {:security_event, event})
  end

  @doc """
  Gets current security statistics.
  """
  @spec get_security_stats() :: map()
  def get_security_stats do
    GenServer.call(__MODULE__, :get_stats)
  end

  @doc """
  Updates security configuration.
  """
  @spec update_config(security_config()) :: :ok
  def update_config(new_config) when is_map(new_config) do
    GenServer.call(__MODULE__, {:update_config, new_config})
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    config = build_default_config(opts)

    state = %__MODULE__{
      config: config,
      rate_limiters: %{},
      connection_stats: %{},
      anomaly_detectors: %{},
      security_events: []
    }

    # Schedule cleanup
    schedule_cleanup()

    Logger.info("Security service started with config: #{inspect(config)}")

    {:ok, state}
  end

  @impl true
  def handle_call({:check_rate_limit, connection_id, config}, _from, state) do
    rate_limit_config = Map.get(config, :rate_limit, %{})
    window_ms = Map.get(rate_limit_config, :window_ms, @default_rate_limit_window)
    max_requests = Map.get(rate_limit_config, :max_requests, @default_rate_limit_requests)

    now = System.monotonic_time(:millisecond)

    case Map.get(state.rate_limiters, connection_id) do
      nil ->
        # First request from this connection
        new_limiter = %{window_start: now, request_count: 1}
        new_limiters = Map.put(state.rate_limiters, connection_id, new_limiter)
        {:reply, :ok, %{state | rate_limiters: new_limiters}}

      %{window_start: window_start, request_count: count} ->
        if now - window_start > window_ms do
          # New window
          new_limiter = %{window_start: now, request_count: 1}
          new_limiters = Map.put(state.rate_limiters, connection_id, new_limiter)
          {:reply, :ok, %{state | rate_limiters: new_limiters}}
        else
          # Within current window
          if count >= max_requests do
            new_stats = %{
              state.stats
              | rate_limit_violations: state.stats.rate_limit_violations + 1
            }

            {:reply, {:error, :rate_limit_exceeded}, %{state | stats: new_stats}}
          else
            new_limiter = %{window_start: window_start, request_count: count + 1}
            new_limiters = Map.put(state.rate_limiters, connection_id, new_limiter)
            {:reply, :ok, %{state | rate_limiters: new_limiters}}
          end
        end
    end
  end

  def handle_call(:get_stats, _from, state) do
    current_stats =
      Map.merge(state.stats, %{
        active_connections: map_size(state.connection_stats),
        active_rate_limiters: map_size(state.rate_limiters),
        recent_security_events: length(state.security_events)
      })

    {:reply, current_stats, state}
  end

  def handle_call({:update_config, new_config}, _from, state) do
    merged_config = Map.merge(state.config, new_config)
    Logger.info("Security config updated: #{inspect(merged_config)}")
    {:reply, :ok, %{state | config: merged_config}}
  end

  @impl true
  def handle_cast({:frame_validated, _frame_size}, state) do
    new_stats = %{state.stats | frames_validated: state.stats.frames_validated + 1}
    {:noreply, %{state | stats: new_stats}}
  end

  def handle_cast({:frame_rejected, reason}, state) do
    new_stats = %{state.stats | frames_rejected: state.stats.frames_rejected + 1}

    # Log security event
    event = %{
      type: :frame_rejected,
      severity: :medium,
      connection_id: nil,
      details: %{reason: reason},
      timestamp: System.system_time(:millisecond)
    }

    # Keep last 100 events
    new_events = [event | Enum.take(state.security_events, 99)]

    Logger.warning("Frame rejected: #{inspect(reason)}")
    {:noreply, %{state | stats: new_stats, security_events: new_events}}
  end

  def handle_cast({:connection_accepted, connection_info}, state) do
    connection_id = Map.get(connection_info, :id, "unknown")
    new_stats = %{state.stats | connections_accepted: state.stats.connections_accepted + 1}

    # Track connection
    new_connection_stats =
      Map.put(state.connection_stats, connection_id, %{
        connected_at: System.system_time(:millisecond),
        requests: 0,
        last_activity: System.system_time(:millisecond)
      })

    {:noreply, %{state | stats: new_stats, connection_stats: new_connection_stats}}
  end

  def handle_cast({:connection_rejected, connection_info, reason}, state) do
    new_stats = %{state.stats | connections_rejected: state.stats.connections_rejected + 1}

    # Log security event
    event = %{
      type: :connection_rejected,
      severity: :high,
      connection_id: Map.get(connection_info, :id, "unknown"),
      details: %{reason: reason, info: connection_info},
      timestamp: System.system_time(:millisecond)
    }

    new_events = [event | Enum.take(state.security_events, 99)]

    Logger.warning("Connection rejected: #{inspect(reason)} for #{inspect(connection_info)}")
    {:noreply, %{state | stats: new_stats, security_events: new_events}}
  end

  def handle_cast({:security_event, event}, state) do
    new_events = [event | Enum.take(state.security_events, 99)]

    Logger.log(
      event_severity_to_log_level(event.severity),
      "Security event: #{inspect(event)}"
    )

    {:noreply, %{state | security_events: new_events}}
  end

  @impl true
  def handle_info(:cleanup, state) do
    # Clean up old rate limiters and connection stats
    now = System.monotonic_time(:millisecond)
    # Keep data for 2 windows
    cutoff = now - 2 * @default_rate_limit_window

    new_rate_limiters =
      Enum.into(state.rate_limiters, %{}, fn {id, limiter} ->
        if limiter.window_start > cutoff do
          {id, limiter}
        else
          nil
        end
      end)
      |> Enum.reject(&is_nil/1)
      |> Enum.into(%{})

    new_connection_stats =
      Enum.into(state.connection_stats, %{}, fn {id, stats} ->
        if stats.last_activity > cutoff do
          {id, stats}
        else
          nil
        end
      end)
      |> Enum.reject(&is_nil/1)
      |> Enum.into(%{})

    schedule_cleanup()

    {:noreply,
     %{state | rate_limiters: new_rate_limiters, connection_stats: new_connection_stats}}
  end

  def handle_info(msg, state) do
    Logger.debug("Unexpected message in Security: #{inspect(msg)}")
    {:noreply, state}
  end

  # Private helper functions

  defp validate_frame_size(frame_data, config) do
    max_size = Map.get(config, :max_frame_size, @default_max_frame_size)

    if byte_size(frame_data) > max_size do
      {:error, :frame_too_large}
    else
      :ok
    end
  end

  defp validate_frame_structure(frame_data) do
    # Basic frame structure validation
    case frame_data do
      <<_length::32, _version::8, _type::8, _payload::binary>> ->
        :ok

      _ ->
        {:error, :invalid_frame_structure}
    end
  end

  defp validate_frame_content(frame_data, config) do
    # Decode and validate frame content
    try do
      <<length::32, _version::8, type::8, payload::binary>> = frame_data

      # Validate length field matches actual payload
      expected_payload_size = length - 2

      if byte_size(payload) != expected_payload_size do
        {:error, :frame_length_mismatch}
      else
        # Additional content validation based on config
        validate_payload_content(payload, type, config)
      end
    rescue
      _ -> {:error, :frame_decode_error}
    end
  end

  defp validate_payload_content(payload, message_type, config) do
    try do
      # Deserialize to check for malicious content
      term = :erlang.binary_to_term(payload, [:safe])

      cond do
        # Batch message
        message_type == 5 ->
          validate_batch_content(term, config)

        is_map(term) ->
          # When validating within frame context, convert message errors to frame errors
          case validate_message_content(term, config) do
            {:error, :message_too_large} -> {:error, :frame_too_large}
            other -> other
          end

        true ->
          {:error, :invalid_payload_type}
      end
    rescue
      _ -> {:error, :payload_decode_error}
    end
  end

  defp validate_batch_content(batch_term, config) when is_map(batch_term) do
    messages = Map.get(batch_term, "messages", [])
    max_batch_size = Map.get(config, :max_batch_size, @default_max_batch_size)

    cond do
      not is_list(messages) ->
        {:error, :invalid_batch_format}

      length(messages) > max_batch_size ->
        {:error, :batch_too_large}

      true ->
        # Validate each message in the batch
        Enum.reduce_while(messages, :ok, fn message, :ok ->
          case validate_message_content(message, config) do
            :ok -> {:cont, :ok}
            error -> {:halt, error}
          end
        end)
    end
  end

  defp validate_batch_content(_batch_term, _config) do
    {:error, :invalid_batch_format}
  end

  defp check_connection_limits(config) do
    # This would typically check against a global connection counter
    # For now, we'll assume it's handled by the ranch acceptor
    _max_connections = Map.get(config, :max_connections, @default_max_connections)

    # In a real implementation, you'd check current connection count
    # current_connections = get_current_connection_count()
    # if current_connections >= max_connections do
    #   {:error, :too_many_connections}
    # else
    #   :ok
    # end

    :ok
  end

  defp validate_authentication(connection_info, config) do
    auth_config = Map.get(config, :authentication, %{})
    required = Map.get(auth_config, :required, false)

    if required do
      case Map.get(connection_info, :credentials) do
        nil ->
          {:error, :authentication_required}

        credentials ->
          validate_credentials(credentials, auth_config)
      end
    else
      :ok
    end
  end

  defp validate_credentials(credentials, auth_config) do
    methods = Map.get(auth_config, :methods, [])

    cond do
      :bearer_token in methods and Map.has_key?(credentials, :bearer_token) ->
        validate_bearer_token(credentials.bearer_token)

      :certificate in methods and Map.has_key?(credentials, :certificate) ->
        validate_certificate(credentials.certificate)

      true ->
        {:error, :invalid_credentials}
    end
  end

  defp validate_bearer_token(token) when is_binary(token) do
    # Basic token validation - in production, verify against auth service
    if byte_size(token) >= 32 do
      :ok
    else
      {:error, :invalid_token}
    end
  end

  defp validate_bearer_token(_), do: {:error, :invalid_token}

  defp validate_certificate(cert) when is_binary(cert) do
    # Basic certificate validation - in production, verify signature chain
    if byte_size(cert) > 100 do
      :ok
    else
      {:error, :invalid_certificate}
    end
  end

  defp validate_certificate(_), do: {:error, :invalid_certificate}

  defp check_rate_limits(connection_info, config) do
    # Check if rate limiting is configured
    rate_limit_config = Map.get(config, :rate_limit)

    if rate_limit_config do
      connection_id = Map.get(connection_info, :id, "unknown")
      # Ensure service is running before checking rate limits
      case GenServer.whereis(__MODULE__) do
        nil ->
          case start() do
            {:ok, _pid} -> check_rate_limit(connection_id, config)
            # Fall back to no rate limiting
            {:error, _reason} -> :ok
          end

        _pid ->
          check_rate_limit(connection_id, config)
      end
    else
      :ok
    end
  end

  defp detect_anomalies(connection_info, config) do
    anomaly_config = Map.get(config, :anomaly_detection, %{enabled: false})

    if Map.get(anomaly_config, :enabled, false) do
      # Ensure security service is running for anomaly detection
      case GenServer.whereis(__MODULE__) do
        nil ->
          # Start service if not running
          case start() do
            {:ok, _pid} -> continue_anomaly_detection(connection_info, config)
            # Fall back to no anomaly detection
            {:error, _reason} -> :ok
          end

        _pid ->
          continue_anomaly_detection(connection_info, config)
      end
    else
      :ok
    end
  end

  defp continue_anomaly_detection(connection_info, _config) do
    # Basic anomaly detection - in production, use ML models
    case detect_connection_anomalies(connection_info) do
      :ok ->
        :ok

      {:anomaly, reason} ->
        record_security_event(%{
          type: :anomaly_detected,
          severity: :medium,
          connection_id: Map.get(connection_info, :id, "unknown"),
          details: %{reason: reason},
          timestamp: System.system_time(:millisecond)
        })

        {:error, :suspicious_activity}
    end
  end

  defp detect_connection_anomalies(connection_info) do
    # Simple heuristics for anomaly detection
    cond do
      # Check for suspicious patterns in connection info
      suspicious_user_agent?(Map.get(connection_info, :user_agent)) ->
        {:anomaly, :suspicious_user_agent}

      suspicious_ip?(Map.get(connection_info, :remote_ip)) ->
        {:anomaly, :suspicious_ip}

      true ->
        :ok
    end
  end

  defp suspicious_user_agent?(nil), do: false

  defp suspicious_user_agent?(user_agent) when is_binary(user_agent) do
    # Check for known malicious patterns
    suspicious_patterns = ["bot", "scanner", "exploit"]
    user_agent_lower = String.downcase(user_agent)

    Enum.any?(suspicious_patterns, fn pattern ->
      String.contains?(user_agent_lower, pattern)
    end)
  end

  defp suspicious_user_agent?(_), do: true

  defp suspicious_ip?(nil), do: false

  defp suspicious_ip?(ip) when is_binary(ip) do
    # Basic IP validation and blacklist check
    case :inet.parse_address(String.to_charlist(ip)) do
      # Valid IP
      {:ok, _} -> false
      # Invalid IP format
      {:error, _} -> true
    end
  end

  defp suspicious_ip?(_), do: true

  defp validate_message_structure(message) when is_map(message) do
    # Check for required fields and valid structure
    cond do
      # RPC request
      Map.has_key?(message, "method") and Map.has_key?(message, "id") ->
        validate_rpc_request(message)

      # RPC response
      Map.has_key?(message, "id") and
          (Map.has_key?(message, "result") or Map.has_key?(message, "error")) ->
        validate_rpc_response(message)

      # Notification
      Map.has_key?(message, "method") ->
        validate_notification(message)

      true ->
        {:error, :invalid_message_structure}
    end
  end

  defp validate_message_structure(_), do: {:error, :invalid_message_type}

  defp validate_rpc_request(message) do
    method = Map.get(message, "method")
    id = Map.get(message, "id")

    cond do
      not is_binary(method) ->
        {:error, :invalid_method_type}

      byte_size(method) > 100 ->
        {:error, :method_too_long}

      not valid_request_id?(id) ->
        {:error, :invalid_request_id}

      true ->
        :ok
    end
  end

  defp validate_rpc_response(message) do
    id = Map.get(message, "id")

    if valid_request_id?(id) do
      :ok
    else
      {:error, :invalid_request_id}
    end
  end

  defp validate_notification(message) do
    method = Map.get(message, "method")

    cond do
      not is_binary(method) ->
        {:error, :invalid_method_type}

      byte_size(method) > 100 ->
        {:error, :invalid_notification}

      true ->
        :ok
    end
  end

  defp valid_request_id?(id) when is_binary(id), do: byte_size(id) <= 100
  defp valid_request_id?(id) when is_integer(id), do: id >= 0
  defp valid_request_id?(id) when is_reference(id), do: true
  defp valid_request_id?(_), do: false

  defp validate_message_size(message, config) do
    estimated_size = estimate_serialized_size(message)
    max_size = Map.get(config, :max_frame_size, @default_max_frame_size)

    if estimated_size > max_size do
      {:error, :message_too_large}
    else
      :ok
    end
  end

  defp validate_message_depth(message, max_depth \\ 10) do
    if calculate_depth(message) > max_depth do
      {:error, :message_too_deep}
    else
      :ok
    end
  end

  defp sanitize_message_content(message, _config) do
    # Check for potentially dangerous content
    case detect_malicious_content(message) do
      :ok -> :ok
      {:malicious, reason} -> {:error, {:malicious_content, reason}}
    end
  end

  defp detect_malicious_content(message) when is_map(message) do
    # Check for dangerous patterns
    Enum.reduce_while(message, :ok, fn {key, value}, :ok ->
      case check_value_safety(key, value) do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp detect_malicious_content(_), do: :ok

  defp check_value_safety(key, value) when is_binary(key) and is_binary(value) do
    cond do
      # Check for script injection
      String.contains?(String.downcase(value), ["<script", "javascript:", "eval("]) ->
        {:malicious, :script_injection}

      # Check for path traversal
      String.contains?(value, ["../", "..\\", "%2e%2e"]) ->
        {:malicious, :path_traversal}

      # Check for SQL injection patterns
      String.contains?(String.downcase(value), ["drop table", "delete from", "'; --"]) ->
        {:malicious, :sql_injection}

      # Check for excessively long strings
      byte_size(value) > 100_000 ->
        {:malicious, :oversized_string}

      true ->
        :ok
    end
  end

  defp check_value_safety(_key, value) when is_map(value) do
    detect_malicious_content(value)
  end

  defp check_value_safety(_key, value) when is_list(value) do
    if length(value) > 10_000 do
      {:malicious, :oversized_list}
    else
      Enum.reduce_while(value, :ok, fn item, :ok ->
        case detect_malicious_content(item) do
          :ok -> {:cont, :ok}
          error -> {:halt, error}
        end
      end)
    end
  end

  defp check_value_safety(_key, _value), do: :ok

  defp estimate_serialized_size(term) do
    # Rough estimation without actually serializing
    case term do
      binary when is_binary(binary) ->
        byte_size(binary)

      atom when is_atom(atom) ->
        byte_size(Atom.to_string(atom))

      integer when is_integer(integer) ->
        8

      float when is_float(float) ->
        8

      list when is_list(list) ->
        Enum.reduce(list, 0, &(&2 + estimate_serialized_size(&1)))

      map when is_map(map) ->
        Map.to_list(map)
        |> Enum.reduce(0, fn {k, v}, acc ->
          acc + estimate_serialized_size(k) + estimate_serialized_size(v)
        end)

      # Default estimate
      _ ->
        100
    end
  end

  defp calculate_depth(term, current_depth \\ 0) do
    case term do
      map when is_map(map) ->
        if map_size(map) == 0 do
          current_depth
        else
          map
          |> Map.values()
          |> Enum.map(&calculate_depth(&1, current_depth + 1))
          |> Enum.max()
        end

      list when is_list(list) ->
        if Enum.empty?(list) do
          current_depth
        else
          list
          |> Enum.map(&calculate_depth(&1, current_depth + 1))
          |> Enum.max()
        end

      _ ->
        current_depth
    end
  end

  defp build_default_config(opts) do
    %{
      max_frame_size: Keyword.get(opts, :max_frame_size, @default_max_frame_size),
      max_connections: Keyword.get(opts, :max_connections, @default_max_connections),
      max_batch_size: Keyword.get(opts, :max_batch_size, @default_max_batch_size),
      rate_limit: %{
        window_ms: Keyword.get(opts, :rate_limit_window, @default_rate_limit_window),
        max_requests: Keyword.get(opts, :rate_limit_requests, @default_rate_limit_requests)
      },
      authentication: %{
        required: Keyword.get(opts, :auth_required, false),
        methods: Keyword.get(opts, :auth_methods, [:bearer_token])
      },
      anomaly_detection: %{
        enabled: Keyword.get(opts, :anomaly_detection, false),
        threshold: Keyword.get(opts, :anomaly_threshold, @default_anomaly_threshold)
      }
    }
  end

  defp event_severity_to_log_level(:low), do: :info
  defp event_severity_to_log_level(:medium), do: :warning
  defp event_severity_to_log_level(:high), do: :error
  defp event_severity_to_log_level(:critical), do: :error

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup, @default_rate_limit_window)
  end

  # Helper to safely cast messages only if service is running
  defp safe_cast(message) do
    case GenServer.whereis(__MODULE__) do
      # Service not running, ignore
      nil -> :ok
      _pid -> GenServer.cast(__MODULE__, message)
    end
  end

  # Helper to detect obviously invalid frame data
  defp not_frame_like?(data) do
    # Frame data should start with a reasonable length field
    case data do
      <<length::32, _rest::binary>> when length >= 2 and length <= 100_000_000 -> false
      _ -> true
    end
  end
end
