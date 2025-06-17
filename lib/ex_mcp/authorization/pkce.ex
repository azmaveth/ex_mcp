defmodule ExMCP.Authorization.PKCE do
  @moduledoc """
  **Deprecated**: This module has been moved to `ExMCP.Internal.Authorization.PKCE`.

  Internal modules are not part of the public API and may change without notice.
  If you were using this module directly, please consider using the public APIs
  in `ExMCP.Client` and `ExMCP.Server` instead.
  """

  @deprecated "Use ExMCP.Internal.Authorization.PKCE instead. Note that internal modules are not part of the public API."

  # Delegate all function calls to the new location
  defdelegate generate_challenge(), to: ExMCP.Internal.Authorization.PKCE

  defdelegate verify_challenge(code_verifier, expected_challenge),
    to: ExMCP.Internal.Authorization.PKCE

  defdelegate validate_code_verifier(code_verifier), to: ExMCP.Internal.Authorization.PKCE
end
