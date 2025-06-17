defmodule ExMCP.Discovery do
  @moduledoc """
  **Deprecated**: This module has been moved to `ExMCP.Internal.Discovery`.

  Internal modules are not part of the public API and may change without notice.
  If you were using this module directly, please consider using the public APIs
  in `ExMCP.Client` and `ExMCP.Server` instead.
  """

  @deprecated "Use ExMCP.Internal.Discovery instead. Note that internal modules are not part of the public API."

  # Delegate all function calls to the new location
  defdelegate discover_servers(options \\ []), to: ExMCP.Internal.Discovery
  defdelegate discover_from_env(servers \\ []), to: ExMCP.Internal.Discovery
  defdelegate discover_npm_packages(), to: ExMCP.Internal.Discovery
  defdelegate discover_from_config(config \\ %{}), to: ExMCP.Internal.Discovery
  defdelegate discover_from_well_known(host \\ "localhost"), to: ExMCP.Internal.Discovery
  defdelegate test_server(server_config), to: ExMCP.Internal.Discovery
  defdelegate get_server_metadata(server_url), to: ExMCP.Internal.Discovery
  defdelegate register_server(server_info), to: ExMCP.Internal.Discovery
end
