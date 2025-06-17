defmodule ExMCP.Security do
  @moduledoc """
  **Deprecated**: This module has been moved to `ExMCP.Internal.Security`.

  Internal modules are not part of the public API and may change without notice.
  If you were using this module directly, please consider using the public APIs
  in `ExMCP.Client` and `ExMCP.Server` instead.
  """

  @deprecated "Use ExMCP.Internal.Security instead. Note that internal modules are not part of the public API."

  # Delegate all function calls to the new location
  defdelegate build_auth_headers(config), to: ExMCP.Internal.Security
  defdelegate build_security_headers(config), to: ExMCP.Internal.Security
  defdelegate validate_localhost_binding(config), to: ExMCP.Internal.Security
  defdelegate validate_origin(origin, allowed_origins), to: ExMCP.Internal.Security
  defdelegate build_cors_headers(cors_config, origin \\ nil), to: ExMCP.Internal.Security
  defdelegate build_standard_security_headers(), to: ExMCP.Internal.Security
  defdelegate validate_request(headers, config \\ %{}), to: ExMCP.Internal.Security
  defdelegate validate_tls_config(config), to: ExMCP.Internal.Security
  defdelegate validate_cipher_suites(config), to: ExMCP.Internal.Security
  defdelegate validate_mtls_config(config), to: ExMCP.Internal.Security
  defdelegate validate_certificate_pinning_config(config), to: ExMCP.Internal.Security
  defdelegate preferred_tls_version(versions), to: ExMCP.Internal.Security
  defdelegate recommended_cipher_suites(), to: ExMCP.Internal.Security
  defdelegate build_mtls_options(config), to: ExMCP.Internal.Security
  defdelegate verify_hostname(cert, validation_result, hostname), to: ExMCP.Internal.Security
  defdelegate validate_transport_security(config), to: ExMCP.Internal.Security
  defdelegate validate_config(config), to: ExMCP.Internal.Security
  defdelegate apply_security(transport_opts, security_config), to: ExMCP.Internal.Security
  defdelegate validate_security_requirements(config), to: ExMCP.Internal.Security
  defdelegate enforce_https_requirement(url), to: ExMCP.Internal.Security
  defdelegate validate_request_origin(origin, config), to: ExMCP.Internal.Security
  defdelegate secure_defaults(url), to: ExMCP.Internal.Security
end
