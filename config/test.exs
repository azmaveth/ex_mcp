import Config

# Reduce log verbosity in tests to make output cleaner
config :logger, level: :warning

# Compile time purging - remove debug calls at compile time for better performance
config :logger,
  compile_time_purge_matching: [
    [level_lower_than: :warning]
  ]

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
