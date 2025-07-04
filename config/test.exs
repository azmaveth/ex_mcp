import Config

# Reduce log verbosity in tests to make output cleaner
config :logger, level: :warning

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
