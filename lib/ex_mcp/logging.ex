defmodule ExMCP.Logging do
  @moduledoc """
  **Deprecated**: This module has been moved to `ExMCP.Internal.Logging`.

  Internal modules are not part of the public API and may change without notice.
  If you were using this module directly, please consider using the public APIs
  in `ExMCP.Client` and `ExMCP.Server` instead.
  """

  @deprecated "Use ExMCP.Internal.Logging instead. Note that internal modules are not part of the public API."

  # Suppress Dialyzer warnings for wrapper delegations
  @dialyzer {:nowarn_function,
             [
               set_global_level: 1,
               get_global_level: 0,
               level_enabled?: 1,
               log: 2,
               log: 3,
               log: 4,
               debug: 1,
               debug: 2,
               debug: 3,
               info: 1,
               info: 2,
               info: 3,
               warning: 1,
               warning: 2,
               warning: 3,
               error: 1,
               error: 2,
               error: 3,
               critical: 1,
               critical: 2,
               critical: 3,
               valid_level?: 1,
               valid_levels: 0
             ]}

  # Delegate all function calls to the new location
  defdelegate set_global_level(level), to: ExMCP.Internal.Logging
  defdelegate get_global_level(), to: ExMCP.Internal.Logging
  defdelegate level_enabled?(level), to: ExMCP.Internal.Logging
  defdelegate log(level, message, data \\ nil, opts \\ []), to: ExMCP.Internal.Logging
  defdelegate debug(message, data \\ nil, opts \\ []), to: ExMCP.Internal.Logging
  defdelegate info(message, data \\ nil, opts \\ []), to: ExMCP.Internal.Logging
  defdelegate warning(message, data \\ nil, opts \\ []), to: ExMCP.Internal.Logging
  defdelegate error(message, data \\ nil, opts \\ []), to: ExMCP.Internal.Logging
  defdelegate critical(message, data \\ nil, opts \\ []), to: ExMCP.Internal.Logging
  defdelegate valid_level?(level), to: ExMCP.Internal.Logging
  defdelegate valid_levels(), to: ExMCP.Internal.Logging
end
