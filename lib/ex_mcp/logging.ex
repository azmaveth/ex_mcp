defmodule ExMCP.Logging do
  @moduledoc """
  **Deprecated**: This module has been moved to `ExMCP.Internal.Logging`.

  Internal modules are not part of the public API and may change without notice.
  If you were using this module directly, please consider using the public APIs
  in `ExMCP.Client` and `ExMCP.Server` instead.
  """

  @deprecated "Use ExMCP.Internal.Logging instead. Note that internal modules are not part of the public API."

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
