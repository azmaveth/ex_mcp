defmodule ExMCP.Resilience do
  @moduledoc """
  **Deprecated**: This module has been moved to `ExMCP.Internal.Resilience`.

  Internal modules are not part of the public API and may change without notice.
  If you were using this module directly, please consider using the public APIs
  in `ExMCP.Client` and `ExMCP.Server` instead.
  """

  @deprecated "Use ExMCP.Internal.Resilience instead. Note that internal modules are not part of the public API."

  # Delegate all function calls to the new location
  defdelegate call_with_retry(module, function, args, opts \\ []), to: ExMCP.Internal.Resilience

  defdelegate call_with_fallback(primary_fun, fallback_fun, args, opts),
    to: ExMCP.Internal.Resilience

  defdelegate call_with_breaker(module, function, args, opts \\ []), to: ExMCP.Internal.Resilience
end
