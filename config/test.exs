import Config

# Log at :info level so capture_log has useful output on test failures.
# ExUnit's capture_log: true suppresses logs for passing tests, so this
# doesn't add noise — it only shows when a test fails.
# Override with LOG_LEVEL env var: LOG_LEVEL=debug mix test
config :logger, level: :info

# Keep the same metadata configuration as in config.exs for consistency
config :logger, :console,
  metadata: [
    :request_id,
    :request_id_hash,
    :progress_id,
    :session_id_hash,
    :event_id_hash,
    :endpoint_hash,
    :issuer_hash,
    :resource_hash,
    :registration_endpoint_hash,
    :duration_ms,
    :status,
    :server_hash,
    :metadata_keys,
    :uptime_seconds,
    :batch_id_hash,
    :method_hash,
    :reason_shape,
    :error_shape,
    :message_shape,
    :line_shape,
    :reply_shape,
    :detail_shape,
    :error_class,
    :handler,
    :handler_kind,
    :return_shape,
    :size,
    :limit,
    :tag,
    :audit,
    :client_id,
    :reason,
    :registration_type,
    :service_id,
    :event_type,
    :data_size,
    :use_sse,
    :storage_backend,
    :max_sessions,
    :max_request_ids,
    :session_ttl_seconds,
    :max_events_per_session,
    :max_event_bytes,
    :max_replay_bytes_per_session,
    :cleanup_interval_ms,
    :method,
    :module,
    :function,
    :error,
    :transport,
    :file,
    :line
  ]
