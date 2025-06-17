defmodule ExMCP.ServerManager do
  @moduledoc """
  **Deprecated**: This module has been moved to `ExMCP.Internal.ServerManager`.

  Internal modules are not part of the public API and may change without notice.
  If you were using this module directly, please consider using the public APIs
  in `ExMCP.Client` and `ExMCP.Server` instead.
  """

  @deprecated "Use ExMCP.Internal.ServerManager instead. Note that internal modules are not part of the public API."

  # Suppress Dialyzer warnings for wrapper delegations
  @dialyzer {:nowarn_function,
             [
               start_link: 0,
               start_link: 1,
               start_server: 1,
               start_server: 2,
               stop_server: 1,
               stop_server: 2,
               list_servers: 0,
               list_servers: 1,
               get_server: 1,
               get_server: 2,
               route_request: 2,
               route_request: 3,
               discover_and_start: 0,
               discover_and_start: 1
             ]}

  # Delegate all function calls to the new location
  defdelegate start_link(opts \\ []), to: ExMCP.Internal.ServerManager
  defdelegate start_server(server_id, opts \\ []), to: ExMCP.Internal.ServerManager
  defdelegate stop_server(server_id, reason \\ :normal), to: ExMCP.Internal.ServerManager
  defdelegate list_servers(manager \\ __MODULE__), to: ExMCP.Internal.ServerManager
  defdelegate get_server(manager \\ __MODULE__, server_id), to: ExMCP.Internal.ServerManager

  defdelegate route_request(manager \\ __MODULE__, server_id, request),
    to: ExMCP.Internal.ServerManager

  defdelegate discover_and_start(opts \\ []), to: ExMCP.Internal.ServerManager
end
