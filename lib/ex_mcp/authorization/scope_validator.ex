defmodule ExMCP.Authorization.ScopeValidator do
  @moduledoc """
  Maps MCP operations to required OAuth scopes and validates them.

  This module provides a centralized way to manage scope requirements for
  different MCP methods. It defines a standard set of scopes following the
  `mcp:<domain>:<action>` convention and offers functions to retrieve required
  scopes for a method and validate them against the scopes granted in a token.

  ## Standard Scopes

  The following standard scopes are defined. More specific scopes can be satisfied
  by granting a less specific, "wildcard" scope. For example, a token with the
  `mcp:tools:execute` scope can access any tool, satisfying the more specific
  `mcp:tools:execute:<tool_name>` requirement.

  ### Tools
  - `mcp:tools:list`: Allows listing available tools (`tools/list`).
  - `mcp:tools:get`: Allows retrieving the definition of a specific tool (`tools/get`).
  - `mcp:tools:execute`: Allows executing any tool. This is a wildcard scope.
  - `mcp:tools:execute:<tool_name>`: Allows executing a specific tool (`tools/execute`).

  ### Resources
  - `mcp:resources:list`: Allows listing available resources (`resources/list`).
  - `mcp:resources:get`: Allows retrieving any specific resource (`resources/get`).
  - `mcp:resources:create`: Allows creating any new resource (`resources/create`).
  - `mcp:resources:update`: Allows updating any existing resource (`resources/update`).
  - `mcp:resources:delete`: Allows deleting any resource (`resources/delete`).
  - More granular resource scopes (e.g., `mcp:resources:get:<resource_id>`) can be
    implemented with a custom mapper.

  ### Prompts
  - `mcp:prompts:execute`: Allows executing a prompt (`prompts/execute`).

  ## Usage

  To get the required scopes for an MCP request:

      request = %{"method" => "tools/list"}
      ExMCP.Authorization.ScopeValidator.get_required_scopes(request)
      #=> ["mcp:tools:list"]

      execute_request = %{
        "method" => "tools/execute",
        "params" => %{"tool_name" => "my_calculator"}
      }
      ExMCP.Authorization.ScopeValidator.get_required_scopes(execute_request)
      #=> ["mcp:tools:execute:my_calculator"]

  To validate scopes from a token:

      token_scopes = ["mcp:tools:execute"]
      required_scopes = ["mcp:tools:execute:my_calculator"]

      ExMCP.Authorization.ScopeValidator.validate(token_scopes, required_scopes)
      #=> :ok

      token_scopes = ["mcp:tools:list", "mcp:tools:get"]
      required_scopes = ["mcp:tools:execute:my_calculator"]
      ExMCP.Authorization.ScopeValidator.validate(token_scopes, required_scopes)
      #=> {:error, :insufficient_scope}

  ## Extensibility

  The scope mapping can be extended by passing a custom mapping function
  to `get_required_scopes/2`. This function receives the request map and should
  return a list of required scopes. If it returns `nil`, the standard mapping
  is used as a fallback.

      my_mapper = fn
        %{"method" => "custom/op"} -> ["my:custom:scope"]
        %{"method" => "resources/get", "params" => %{"id" => resource_id}} ->
          ["mcp:resources:get:" <> resource_id]
        _ ->
          nil # Fallback to default for other methods
      end

      ExMCP.Authorization.ScopeValidator.get_required_scopes(request, my_mapper)
  """

  @type token_scopes :: [String.t()]
  @type required_scopes :: [String.t()]
  @type request :: map()
  @type custom_mapper :: (request() -> required_scopes() | nil)

  @doc """
  Retrieves the list of required OAuth scopes for a given MCP request.

  This function inspects the `method` and `params` of the request to determine
  the necessary scopes. It can be extended with a custom mapping function.
  """
  @spec get_required_scopes(request(), custom_mapper()) :: required_scopes()
  def get_required_scopes(request, custom_mapper \\ fn _ -> nil end)

  def get_required_scopes(request, custom_mapper) do
    case custom_mapper.(request) do
      nil -> default_scope_mapping(request)
      scopes when is_list(scopes) -> scopes
    end
  end

  @doc """
  Validates that a set of token scopes satisfies a set of required scopes.

  This function checks if for every required scope, either the scope itself or a
  more general "wildcard" scope exists in the token scopes.

  For example, if `required_scopes` is `["mcp:tools:execute:calculator"]`, this
  function will return `:ok` if `token_scopes` contains either
  `"mcp:tools:execute:calculator"` or `"mcp:tools:execute"`.
  """
  @spec validate(token_scopes(), required_scopes()) :: :ok | {:error, :insufficient_scope}
  def validate(token_scopes, required_scopes) do
    token_scope_set = MapSet.new(token_scopes)

    all_required_present? =
      Enum.all?(required_scopes, &scope_satisfied?(&1, token_scope_set))

    if all_required_present? do
      :ok
    else
      {:error, :insufficient_scope}
    end
  end

  @doc """
  Returns a list of all standard, statically-defined MCP scopes.

  This is useful for advertising supported scopes, for example in a
  `.well-known/oauth-protected-resource` endpoint. Note that this list does not
  include dynamically generated scopes like `mcp:tools:execute:<tool_name>`.
  """
  @spec get_all_static_scopes() :: [String.t()]
  def get_all_static_scopes do
    [
      "mcp:tools:list",
      "mcp:tools:get",
      "mcp:tools:execute",
      "mcp:resources:list",
      "mcp:resources:get",
      "mcp:resources:create",
      "mcp:resources:update",
      "mcp:resources:delete",
      "mcp:prompts:execute"
    ]
  end

  #
  # Private Functions
  #

  defp default_scope_mapping(%{"method" => "tools/list"}), do: ["mcp:tools:list"]
  defp default_scope_mapping(%{"method" => "tools/get"}), do: ["mcp:tools:get"]

  defp default_scope_mapping(%{
         "method" => "tools/execute",
         "params" => %{"tool_name" => tool_name}
       })
       when is_binary(tool_name) and tool_name != "" do
    ["mcp:tools:execute:#{tool_name}"]
  end

  defp default_scope_mapping(%{"method" => "tools/execute"}), do: ["mcp:tools:execute"]

  defp default_scope_mapping(%{"method" => "resources/list"}), do: ["mcp:resources:list"]
  defp default_scope_mapping(%{"method" => "resources/get"}), do: ["mcp:resources:get"]
  defp default_scope_mapping(%{"method" => "resources/create"}), do: ["mcp:resources:create"]
  defp default_scope_mapping(%{"method" => "resources/update"}), do: ["mcp:resources:update"]
  defp default_scope_mapping(%{"method" => "resources/delete"}), do: ["mcp:resources:delete"]

  defp default_scope_mapping(%{"method" => "prompts/execute"}), do: ["mcp:prompts:execute"]

  defp default_scope_mapping(_request), do: []

  defp scope_satisfied?(required_scope, token_scope_set) do
    # A required scope is satisfied if it is present in the token scopes,
    # or if a less specific, wildcard scope is present.
    # e.g., required "mcp:tools:execute:foo" is satisfied by "mcp:tools:execute" in token.

    # 1. Check for exact match
    if MapSet.member?(token_scope_set, required_scope) do
      true
    else
      # 2. Check for parent wildcard scopes
      required_parts = String.split(required_scope, ":")

      parent_scopes =
        Stream.iterate(1, &(&1 + 1))
        |> Stream.take(length(required_parts) - 1)
        |> Stream.map(&Enum.take(required_parts, &1))
        |> Stream.map(&Enum.join(&1, ":"))

      Enum.any?(parent_scopes, &MapSet.member?(token_scope_set, &1))
    end
  end
end
