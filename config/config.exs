import Config

# ExMCP Configuration
config :ex_mcp,
  # Protocol version to use when initiating connections
  # Options: "2024-11-05", "2025-03-26", "2025-06-18"
  # Default: "2025-03-26" (latest stable)
  protocol_version: "2025-03-26"

# Logger Configuration
# Configure metadata fields to avoid warnings
# Note: :file and :line are performance-intensive and should only be used in dev/test
config :logger, :console,
  metadata: [:request_id, :tag, :audit, :client_id, :reason, :registration_type]

# Environment-specific configuration
if Mix.env() in [:dev, :test] do
  # Note: This configuration overwrites the default metadata for dev/test environments,
  # adding :file and :line for detailed logging (performance-intensive operations).
  config :logger, :console,
    metadata: [:request_id, :tag, :audit, :client_id, :reason, :registration_type, :file, :line]
end

if Mix.env() == :dev do
  config :git_hooks,
    auto_install: true,
    verbose: true,
    hooks: [
      pre_commit: [
        tasks: [
          {:cmd, "mix format --check-formatted"},
          {:cmd, "mix compile --warnings-as-errors"},
          {:cmd, "mix credo --strict"},
          {:cmd, "mix dialyzer"}
          # To enable unit tests in pre-commit, uncomment the following line:
          # {:cmd, "mix test --exclude oauth_integration --exclude streaming --max-cases 4 --seed 0"}
        ]
      ],
      pre_push: [
        tasks: []
      ]
    ]
end
