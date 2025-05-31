import Config

# ExMCP Configuration
config :ex_mcp,
  # Protocol version to use when initiating connections
  # Options: "2024-11-05", "2025-03-26", "draft"
  # Default: "2025-03-26" (latest stable)
  protocol_version: "2025-03-26"

# Logger Configuration for security metadata
# Note: Logger metadata warnings are acceptable for development
# In production, configure these keys in your logger backend

if Mix.env() == :dev do
  config :git_hooks,
    auto_install: true,
    verbose: true,
    hooks: [
      pre_commit: [
        tasks: [
          {:cmd, "mix format --check-formatted"},
          {:cmd, "mix compile --warnings-as-errors"}
        ]
      ],
      pre_push: [
        tasks: [
          {:cmd, "mix format --check-formatted"},
          {:cmd, "mix credo --strict"},
          {:cmd, "mix dialyzer"},
          {:cmd, "mix test"},
          {:cmd, "mix sobelow --skip"}
        ]
      ]
    ]
end
