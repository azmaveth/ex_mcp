import Config

# ExMCP Configuration
config :ex_mcp,
  # Protocol version to use when initiating connections
  # Options: "2024-11-05", "2025-03-26", "2025-06-18", "2025-11-25"
  # Default: "2025-11-25" (latest)
  protocol_version: "2025-11-25",
  # Feature flags for phased rollout of new MCP features.
  # These flags allow for enabling new functionality in a controlled manner.
  #
  # When `protocol_version_required` is true, the server will require the
  # `mcp-protocol-version` header on incoming requests.
  protocol_version_required: false,
  # When `structured_output_enabled` is true, tools can return structured
  # content alongside the standard content blocks.
  structured_output_enabled: false,
  # When `oauth2_enabled` is true, the server will enforce OAuth 2.1
  # for authorization.
  oauth2_enabled: false

# OAuth 2.1 Server (Resource Server) Configuration
# This configuration is used when the server needs to validate OAuth 2.1 tokens.
# There is intentionally no default: the values below are illustrative
# placeholders, not endpoints anyone should point at. Configure the real
# authorization server in YOUR application's config. The key actually read at
# runtime is `ExMCP.Authorization.ServerConfig`:
#
#     config :ex_mcp, ExMCP.Authorization.ServerConfig,
#       default_server: :auth_server,
#       servers: %{
#         auth_server: %{
#           # (Required) URL of the token introspection endpoint (RFC 7662).
#           # Must be HTTPS unless it is localhost.
#           introspection_endpoint: "https://auth.example.com/introspect",
#           # (Optional) OAuth realm name for WWW-Authenticate headers.
#           realm: "mcp-service",
#           client_id: "mcp-server-id",
#           # Load secrets from the environment in config/runtime.exs, never
#           # from a checked-in config file.
#           client_secret: "server-secret"
#         }
#       }

# OAuth 2.1 Authorization Server Metadata (RFC 8414)
# Defines the metadata returned by /.well-known/oauth-authorization-server.
# Also intentionally left unset; example:
#
#     config :ex_mcp, :oauth2_authorization_server_metadata,
#       # (Required) The authorization server issuer identifier.
#       issuer: "https://auth.example.com",
#       # (Required) URL of the authorization endpoint.
#       authorization_endpoint: "https://auth.example.com/authorize",
#       # (Required) URL of the token endpoint.
#       token_endpoint: "https://auth.example.com/token",
#       # (Optional) URL of the JWK Set document.
#       jwks_uri: "https://auth.example.com/.well-known/jwks.json",
#       # (Optional) Supported scopes / response types / grant types / PKCE methods.
#       scopes_supported: ["mcp:read", "mcp:write"],
#       response_types_supported: ["code"],
#       grant_types_supported: ["authorization_code", "client_credentials", "refresh_token"],
#       code_challenge_methods_supported: ["S256"],
#       # (Optional) Introspection / revocation endpoints.
#       introspection_endpoint: "https://auth.example.com/introspect",
#       revocation_endpoint: "https://auth.example.com/revoke"

# Security Configuration
#
# These are ExMCP's own defaults, restated here for visibility. A library's
# config is not loaded by dependent applications: to change any of this, copy
# the setting into YOUR application's config.
#
# The defaults are fail-closed. `ExMCP.Transport.SecurityGuard` classifies
# every outbound URL against :trusted_origins; anything else has its
# credential headers stripped and must be approved by :consent_handler, which
# denies by default. A client pointed at a non-localhost MCP server therefore
# needs that server's origin declared:
#
#     config :ex_mcp, :security,
#       trusted_origins: ["https://mcp.example.com"]
#
# A trusted origin is exempt from both stripping and consent. See
# docs/SECURITY.md.
config :ex_mcp, :security,
  # Origins treated as the same security domain. "*.example.com" matches
  # subdomains. Add the MCP servers this application connects to.
  trusted_origins: ["localhost", "127.0.0.1", "::1"],
  additional_sensitive_headers: [],

  # Consent management. Asked to approve access to origins that are NOT
  # trusted. ExMCP.ConsentHandler.CLI prompts interactively;
  # ExMCP.ConsentHandler.Web defers to an out-of-band web flow.
  consent_handler: ExMCP.ConsentHandler.Deny,
  # Milliseconds. Handlers receive this as :consent_ttl in seconds.
  consent_ttl: :timer.hours(24),
  consent_cache_cleanup_interval: :timer.minutes(5),

  # Security logging
  log_security_actions: true,
  audit_log_level: :info,

  # Enforcement switches, read by ExMCP.Transport.SecurityGuard. Setting either
  # to false disables that control for every transport; prefer declaring
  # :trusted_origins instead.
  enable_token_passthrough_prevention: true,
  enable_user_consent_validation: true

# Logger Configuration
# Configure metadata fields to avoid warnings
# Note: :file and :line are performance-intensive and should only be used in dev/test
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
    :token
  ]

# Environment-specific configuration
if Mix.env() in [:dev, :test] do
  # Note: This configuration overwrites the default metadata for dev/test environments,
  # adding :file and :line for detailed logging (performance-intensive operations).
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
          {:cmd, "mix credo"},
          {:cmd, "mix dialyzer"},
          {:cmd, "./scripts/check_skip_tags.sh staged"}
          # To enable unit tests in pre-commit, uncomment the following line:
          # {:cmd, "mix test --exclude oauth_integration --exclude streaming --max-cases 4 --seed 0"}
        ]
      ],
      pre_push: [
        tasks: []
      ]
    ]
end
