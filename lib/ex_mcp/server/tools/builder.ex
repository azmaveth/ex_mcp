defmodule ExMCP.Server.Tools.Builder do
  @moduledoc """
  Simplified tool builder that reduces metaprogramming complexity.

  Instead of heavy AST manipulation, this module uses a cleaner builder pattern
  with explicit data structures and runtime registration.
  """

  alias ExMCP.Server.Tools.ASTValidator

  defmodule Tool do
    @moduledoc """
    Runtime representation of a tool definition.
    """

    defstruct [
      :name,
      :title,
      :description,
      :input_schema,
      :output_schema,
      :handler,
      :annotations,
      params: []
    ]

    @type t :: %__MODULE__{
            name: String.t(),
            title: String.t() | nil,
            description: String.t(),
            input_schema: map() | nil,
            output_schema: map() | nil,
            handler: function(),
            annotations: map(),
            params: [param()]
          }

    @type param :: %{
            name: atom(),
            type: atom() | {atom(), any()},
            required: boolean(),
            default: any(),
            schema: map() | nil
          }
  end

  @doc """
  Creates a new tool builder.

  ## Example

      ExMCP.Server.Tools.Builder.new("echo")
      |> description("Echo back the input")
      |> param(:message, :string, required: true)
      |> handler(fn %{message: msg}, state -> {:ok, %{text: msg}, state} end)
      |> build()
  """
  @spec new(String.t()) :: __MODULE__.Tool.t()
  def new(name) when is_binary(name) do
    %Tool{
      name: name,
      description: "No description provided",
      annotations: %{}
    }
  end

  @doc """
  Sets the tool's title (optional).
  """
  @spec title(Tool.t(), String.t()) :: Tool.t()
  def title(%Tool{} = tool, title) when is_binary(title) do
    %{tool | title: title}
  end

  @doc """
  Sets the tool's description.
  """
  @spec description(Tool.t(), String.t()) :: Tool.t()
  def description(%Tool{} = tool, desc) when is_binary(desc) do
    %{tool | description: desc}
  end

  @doc """
  Adds a parameter to the tool.
  """
  @spec param(Tool.t(), atom(), atom() | {atom(), any()}, keyword()) :: Tool.t()
  def param(%Tool{params: params} = tool, name, type, opts \\ [])
      when is_atom(name) do
    param_def = %{
      name: name,
      type: type,
      required: Keyword.get(opts, :required, false),
      default: Keyword.get(opts, :default),
      schema: Keyword.get(opts, :schema)
    }

    %{tool | params: params ++ [param_def]}
  end

  @doc """
  Sets the input schema explicitly.
  """
  @spec input_schema(Tool.t(), map() | any()) :: Tool.t()
  def input_schema(%Tool{} = tool, schema) do
    # Handle both runtime maps and compile-time AST
    evaluated_schema =
      case schema do
        {:%{}, _, _} = ast ->
          # This is an AST node, validate and evaluate it
          case ASTValidator.validate_schema_ast(ast) do
            {:ok, safe_ast} ->
              {schema_map, _} = Code.eval_quoted(safe_ast)
              schema_map

            {:error, reason} ->
              raise ArgumentError, "Invalid schema AST: #{reason}"
          end

        %{} = map ->
          # This is already a map
          map

        _ ->
          # Fallback for other cases
          schema
      end

    %{tool | input_schema: evaluated_schema}
  end

  @doc """
  Sets the output schema for validation.
  """
  @spec output_schema(Tool.t(), map() | any()) :: Tool.t()
  def output_schema(%Tool{} = tool, schema) do
    # Handle both runtime maps and compile-time AST
    evaluated_schema =
      case schema do
        {:%{}, _, _} = ast ->
          # This is an AST node, validate and evaluate it
          case ASTValidator.validate_schema_ast(ast) do
            {:ok, safe_ast} ->
              {schema_map, _} = Code.eval_quoted(safe_ast)
              schema_map

            {:error, reason} ->
              raise ArgumentError, "Invalid schema AST: #{reason}"
          end

        %{} = map ->
          # This is already a map
          map

        _ ->
          # Fallback for other cases
          schema
      end

    %{tool | output_schema: evaluated_schema}
  end

  @doc """
  Sets annotations for the tool.
  """
  @spec annotations(Tool.t(), map() | any()) :: Tool.t()
  def annotations(%Tool{} = tool, anns) do
    # Handle both runtime maps and compile-time AST
    evaluated_anns =
      case anns do
        {:%{}, _, _} = ast ->
          # This is an AST node, validate and evaluate it
          case ASTValidator.validate_schema_ast(ast) do
            {:ok, safe_ast} ->
              {anns_map, _} = Code.eval_quoted(safe_ast)
              anns_map

            {:error, reason} ->
              raise ArgumentError, "Invalid annotations AST: #{reason}"
          end

        %{} = map ->
          # This is already a map
          map

        _ ->
          # Fallback for other cases
          anns
      end

    %{tool | annotations: Map.merge(tool.annotations, evaluated_anns)}
  end

  @doc """
  Sets the handler function.
  """
  @spec handler(Tool.t(), function()) :: Tool.t()
  def handler(%Tool{} = tool, handler_fn) when is_function(handler_fn, 2) do
    %{tool | handler: handler_fn}
  end

  @doc """
  Builds the final tool definition.

  Generates input schema from params if not explicitly set.
  """
  @spec build(Tool.t()) :: {:ok, map()} | {:error, String.t()}
  def build(%Tool{handler: nil}) do
    {:error, "Tool must have a handler function"}
  end

  def build(%Tool{} = tool) do
    # Generate input schema from params if not set
    final_input_schema = tool.input_schema || generate_schema_from_params(tool.params)

    # Build the MCP tool definition
    definition = %{
      name: tool.name,
      description: tool.description,
      inputSchema: final_input_schema
    }

    # Add optional fields
    definition =
      definition
      |> maybe_add_field(:title, tool.title)
      |> maybe_add_field(:outputSchema, tool.output_schema)
      |> Map.merge(tool.annotations)

    {:ok, {definition, tool.handler}}
  end

  # Private helpers

  defp generate_schema_from_params(params) do
    properties =
      params
      |> Enum.map(fn param ->
        schema = param.schema || type_to_schema(param.type)
        schema = if param.default, do: Map.put(schema, :default, param.default), else: schema
        {safe_atom_to_string(param.name), schema}
      end)
      |> Map.new()

    required =
      params
      |> Enum.filter(& &1.required)
      |> Enum.map(&safe_atom_to_string(&1.name))

    base = %{
      type: "object",
      properties: properties
    }

    if required != [], do: Map.put(base, :required, required), else: base
  end

  defp type_to_schema(:string), do: %{type: "string"}
  defp type_to_schema(:integer), do: %{type: "integer"}
  defp type_to_schema(:number), do: %{type: "number"}
  defp type_to_schema(:boolean), do: %{type: "boolean"}
  defp type_to_schema(:object), do: %{type: "object"}
  defp type_to_schema({:array, item_type}), do: %{type: "array", items: type_to_schema(item_type)}
  defp type_to_schema(_), do: %{type: "string"}

  defp safe_atom_to_string(value) when is_atom(value), do: Atom.to_string(value)
  defp safe_atom_to_string(value) when is_binary(value), do: value
  defp safe_atom_to_string(value), do: inspect(value)

  defp maybe_add_field(map, _key, nil), do: map
  defp maybe_add_field(map, key, value), do: Map.put(map, key, value)
end
