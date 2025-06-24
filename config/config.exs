import Config

# ExMCP Configuration
config :ex_mcp,
  # Protocol version to use when initiating connections
  # Options: "2024-11-05", "2025-03-26", "2025-06-18"
  # Default: "2025-06-18" (latest)
  protocol_version: "2025-06-18",
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
# It is required when `oauth2_enabled` is set to `true`.
config :ex_mcp, :oauth2_server_config,
  # (Required) URL of the token introspection endpoint (RFC 7662).
  introspection_endpoint: "https://auth.example.com/introspect",
  # (Optional) Base URL of the authorization server for metadata discovery.
  authorization_server: "https://auth.example.com",
  # (Optional) Default scopes required to access protected resources.
  required_scopes: ["mcp:read"],
  # (Optional) Cache TTL for token validation results.
  token_cache_ttl: :timer.minutes(5),
  # (Optional) OAuth realm name for WWW-Authenticate headers.
  realm: "mcp-service"

# OAuth 2.1 Authorization Server Metadata (RFC 8414)
# This configuration defines the authorization server metadata returned by
# the /.well-known/oauth-authorization-server endpoint.
config :ex_mcp, :oauth2_authorization_server_metadata,
  # (Required) The authorization server issuer identifier.
  issuer: "https://auth.example.com",
  # (Required) URL of the authorization server's authorization endpoint.
  authorization_endpoint: "https://auth.example.com/authorize",
  # (Required) URL of the authorization server's token endpoint.
  token_endpoint: "https://auth.example.com/token",
  # (Optional) URL of the authorization server's JWK Set document.
  jwks_uri: "https://auth.example.com/.well-known/jwks.json",
  # (Optional) JSON array containing a list of the OAuth 2.0 scope values.
  scopes_supported: ["mcp:read", "mcp:write", "mcp:admin", "offline_access"],
  # (Optional) JSON array containing a list of the OAuth 2.0 response_type values.
  response_types_supported: ["code"],
  # (Optional) JSON array containing a list of the OAuth 2.0 grant type values.
  grant_types_supported: ["authorization_code", "client_credentials", "refresh_token"],
  # (Optional) JSON array containing a list of PKCE code challenge methods.
  code_challenge_methods_supported: ["S256"],
  # (Optional) URL of the authorization server's token introspection endpoint.
  introspection_endpoint: "https://auth.example.com/introspect",
  # (Optional) URL of the authorization server's token revocation endpoint.
  revocation_endpoint: "https://auth.example.com/revoke"

# Security Configuration
config :ex_mcp, :security,
  # Token passthrough prevention
  trusted_origins: ["localhost", "127.0.0.1", "::1"],
  additional_sensitive_headers: [],

  # Consent management
  consent_handler: ExMCP.ConsentHandler.Deny,
  consent_ttl: :timer.hours(24),
  consent_cache_cleanup_interval: :timer.minutes(5),

  # Security logging
  log_security_actions: true,
  audit_log_level: :info,

  # Feature flags
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
