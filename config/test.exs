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
    :tag,
    :audit,
    :client_id,
    :reason,
    :registration_type,
    :service_id,
    :method,
    :module,
    :function,
    :error,
    :url,
    :transport,
    :user_id,
    :token,
    :file,
    :line
  ]
