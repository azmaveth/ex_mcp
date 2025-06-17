defmodule ExMCP.VersionRegistry do
  @moduledoc """
  **Deprecated**: This module has been moved to `ExMCP.Internal.VersionRegistry`.

  Internal modules are not part of the public API and may change without notice.
  If you were using this module directly, please consider using the public APIs
  in `ExMCP.Client` and `ExMCP.Server` instead.
  """

  @deprecated "Use ExMCP.Internal.VersionRegistry instead. Note that internal modules are not part of the public API."

  # Delegate all function calls to the new location
  defdelegate supported_versions(), to: ExMCP.Internal.VersionRegistry
  defdelegate latest_version(), to: ExMCP.Internal.VersionRegistry
  defdelegate preferred_version(), to: ExMCP.Internal.VersionRegistry
  defdelegate supported?(version), to: ExMCP.Internal.VersionRegistry
  defdelegate capabilities_for_version(version), to: ExMCP.Internal.VersionRegistry
  defdelegate feature_available?(version, feature), to: ExMCP.Internal.VersionRegistry
  defdelegate message_format(version), to: ExMCP.Internal.VersionRegistry

  defdelegate negotiate_version(client_versions, server_versions),
    to: ExMCP.Internal.VersionRegistry

  defdelegate types_module(version), to: ExMCP.Internal.VersionRegistry
end
