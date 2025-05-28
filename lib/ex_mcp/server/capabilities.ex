defmodule ExMCP.Server.Capabilities do
  @moduledoc """
  Helper module to build server capabilities.

  For now, this returns a standard set of capabilities.
  In the future, this could be enhanced to dynamically detect
  which callbacks are truly implemented vs using defaults.
  """

  @doc """
  Builds server capabilities object.

  Currently returns a standard capability set. Handlers should
  override this in their handle_initialize/2 if they want to
  advertise specific capabilities.

  Returns a map with:
  - tools: empty object (details provided by tools/list)
  - experimental: always included for future extensions
  """
  @spec build_capabilities(module()) :: map()
  def build_capabilities(_handler_module) do
    # For now, just return standard capabilities
    # Handlers can customize this in their handle_initialize
    %{
      "tools" => %{},
      "experimental" => %{}
    }
  end
end
