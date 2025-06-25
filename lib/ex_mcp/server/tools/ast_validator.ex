defmodule ExMCP.Server.Tools.ASTValidator do
  @moduledoc """
  Validates AST nodes to ensure they are safe for evaluation.

  This module provides security validation for AST nodes that will be
  evaluated using Code.eval_quoted, preventing code injection attacks.
  """

  @allowed_nodes [
    # Literals
    :binary,
    :number,
    :atom,
    :list,
    # Map construction
    :%,
    :%{},
    # Basic operators
    :-,
    :+,
    :*,
    :/,
    # Aliases for module names (e.g., DateTime)
    :__aliases__,
    # Block expressions
    :__block__,
    # Tuple construction
    :{}
    # String interpolation (safe when contents are validated)
    # :<<>> # Commented out for now, binary construction syntax
  ]

  @doc """
  Validates that an AST node contains only safe constructs.

  Returns {:ok, ast} if the AST is safe, or {:error, reason} if unsafe
  constructs are detected.

  ## Examples

      iex> ASTValidator.validate_schema_ast({:%{}, [], [type: "string"]})
      {:ok, {:%{}, [], [type: "string"]}}
      
      iex> ASTValidator.validate_schema_ast({:eval, [], ["dangerous code"]})
      {:error, "Unsafe AST node: eval"}
  """
  @spec validate_schema_ast(any()) :: {:ok, any()} | {:error, String.t()}
  def validate_schema_ast(ast) do
    case validate_node(ast) do
      :ok -> {:ok, ast}
      {:error, reason} -> {:error, reason}
    end
  end

  # Validate individual nodes recursively
  defp validate_node({node, _meta, args}) when node in @allowed_nodes do
    validate_args(args)
  end

  # Allow literals
  defp validate_node(literal) when is_binary(literal), do: :ok
  defp validate_node(literal) when is_number(literal), do: :ok
  defp validate_node(literal) when is_atom(literal), do: :ok
  defp validate_node(literal) when is_boolean(literal), do: :ok
  defp validate_node(nil), do: :ok

  # Validate lists
  defp validate_node(list) when is_list(list) do
    validate_args(list)
  end

  # Reject function calls and other unsafe constructs
  defp validate_node({node, _, _}) do
    {:error, "Unsafe AST node: #{node}"}
  end

  defp validate_node(_), do: :ok

  # Validate arguments/children of AST nodes
  defp validate_args(nil), do: :ok

  defp validate_args(args) when is_list(args) do
    Enum.reduce_while(args, :ok, fn arg, :ok ->
      case validate_node(arg) do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp validate_args(_), do: :ok
end
