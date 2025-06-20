defmodule ExMCP.Server.Tools do
  @moduledoc """
  DSL for defining MCP tools in server handlers.

  This module provides a declarative way to define tools with automatic
  schema generation and validation. It supports both simple and advanced APIs.

  ## Simple API

  For common cases, use the simple API with automatic schema generation:

      defmodule MyServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools

        tool "echo", "Echo back the input" do
          param :message, :string, required: true

          handle fn %{message: message}, _state ->
            {:ok, text: message}
          end
        end
      end

  ## Advanced API

  For full control over schemas and metadata:

      tool "calculate" do
        description "Perform mathematical calculations"

        input_schema %{
          type: "object",
          properties: %{
            expression: %{type: "string", pattern: "^[0-9+\\-*/().\\s]+$"}
          }
        }

        annotations %{
          readOnlyHint: true
        }

        handle fn %{expression: expr}, state ->
          result = evaluate_expression(expr)
          {:ok, %{
            content: [{type: "text", text: "Result: #\{result}"}],
            structuredContent: %{result: result, expression: expr}
          }, state}
        end
      end
  """

  defmacro __using__(_opts) do
    quote do
      import ExMCP.Server.Tools
      Module.register_attribute(__MODULE__, :tools, accumulate: true)
      Module.register_attribute(__MODULE__, :tool_handlers, accumulate: true)
      @before_compile ExMCP.Server.Tools
    end
  end

  defmacro __before_compile__(env) do
    tools = Module.get_attribute(env.module, :tools, [])

    # Generate handle_list_tools/2
    tools_without_handler =
      tools
      |> Enum.reverse()
      |> Enum.map(fn tool -> Map.delete(tool, :__handler_ast__) end)

    # Generate the handler functions
    handler_funcs =
      tools
      |> Enum.reverse()
      |> Enum.with_index()
      |> Enum.map(fn {tool, index} ->
        handler_name = :"__tool_handler_#{index}__"
        handler_ast = tool[:__handler_ast__]

        quote do
          def unquote(handler_name)(args, state) do
            unquote(handler_ast).(args, state)
          end
        end
      end)

    # Build tool name to handler mapping
    tool_mapping =
      tools
      |> Enum.reverse()
      |> Enum.with_index()
      |> Enum.map(fn {tool, index} ->
        {tool.name, :"__tool_handler_#{index}__"}
      end)

    quote do
      alias ExMCP.Server.Tools
      unquote_splicing(handler_funcs)

      @impl ExMCP.Server.Handler
      def handle_list_tools(_params, state) do
        {:ok, unquote(Macro.escape(tools_without_handler)), state}
      end

      @impl ExMCP.Server.Handler
      def handle_call_tool(params, state) do
        tool_mapping = unquote(tool_mapping)

        case List.keyfind(tool_mapping, params.name, 0) do
          {_, handler_func} ->
            result = apply(__MODULE__, handler_func, [params.arguments, state])
            Tools.__normalize_response__(result, state)

          nil ->
            {:ok,
             %{
               content: [%{type: "text", text: "Unknown tool: #{params.name}"}],
               isError: true
             }, state}
        end
      end
    end
  end

  @doc false
  def __normalize_response__({:ok, response}, state) when is_binary(response) do
    {:ok, %{content: [%{type: "text", text: response}]}, state}
  end

  def __normalize_response__({:ok, %{text: text}}, state) do
    {:ok, %{content: [%{type: "text", text: text}]}, state}
  end

  def __normalize_response__({:ok, [text: text]}, state) do
    # Handle keyword list response
    {:ok, %{content: [%{type: "text", text: text}]}, state}
  end

  def __normalize_response__({:ok, response}, state) when is_map(response) do
    {:ok, response, state}
  end

  def __normalize_response__({:ok, response, new_state}, _state) when is_binary(response) do
    {:ok, %{content: [%{type: "text", text: response}]}, new_state}
  end

  def __normalize_response__({:ok, %{text: text}, new_state}, _state) do
    {:ok, %{content: [%{type: "text", text: text}]}, new_state}
  end

  def __normalize_response__({:ok, response, new_state}, _state) do
    {:ok, response, new_state}
  end

  def __normalize_response__({:error, reason}, state) when is_binary(reason) do
    {:ok, %{content: [%{type: "text", text: reason}], isError: true}, state}
  end

  def __normalize_response__({:error, reason}, state) do
    {:ok, %{content: [%{type: "text", text: inspect(reason)}], isError: true}, state}
  end

  @doc """
  Define a tool using the DSL.

  ## Examples

      tool "echo", "Echo back the input" do
        param :message, :string, required: true
        handle fn %{message: message}, _state ->
          {:ok, text: message}
        end
      end
  """
  defmacro tool(name, description \\ nil, do: block) do
    quote do
      alias ExMCP.Server.Tools

      Tools.__tool__(
        __MODULE__,
        unquote(name),
        unquote(description),
        unquote(Macro.escape(block))
      )
    end
  end

  @doc false
  def __tool__(module, name, description, block) do
    {params, annotations, input_schema, output_schema, handler, final_description} =
      extract_tool_info(block, description)

    # Build the tool definition
    tool_def =
      build_tool_definition(
        name,
        final_description,
        params,
        annotations,
        input_schema,
        output_schema
      )

    # Store the handler AST for later compilation
    tool_with_handler = Map.put(tool_def, :__handler_ast__, handler)

    # Register the tool
    Module.put_attribute(module, :tools, tool_with_handler)
  end

  defp extract_tool_info(block, default_description) do
    # Parse the DSL block - order matters!
    statements =
      case block do
        {:__block__, _, stmts} -> stmts
        single -> [single]
      end

    # Extract in correct order
    {description, statements} = extract_description(statements, default_description)
    {params, statements} = extract_params_from_statements(statements, [])
    {input_schema, statements} = extract_input_schema(statements)
    {output_schema, statements} = extract_output_schema(statements)
    {annotations, statements} = extract_annotations(statements)
    handler = extract_handler(statements)

    {params, annotations, input_schema, output_schema, handler, description}
  end

  defp extract_params_from_statements([], params) do
    {Enum.reverse(params), []}
  end

  defp extract_params_from_statements([{:param, _, args} | rest], params) do
    param = parse_param(args)
    extract_params_from_statements(rest, [param | params])
  end

  defp extract_params_from_statements(statements, params) do
    {Enum.reverse(params), statements}
  end

  defp parse_param([name, type]) do
    %{name: name, type: type, required: false}
  end

  defp parse_param([name, type, opts]) do
    %{
      name: name,
      type: type,
      required: Keyword.get(opts, :required, false),
      default: Keyword.get(opts, :default),
      schema: opts[:schema] |> evaluate_ast_value()
    }
  end

  defp extract_annotations([{:annotations, _, [annotations]} | rest]) do
    # Evaluate the annotations if they're AST
    anns =
      case annotations do
        {:%{}, _, kvs} ->
          # It's a map literal in AST form
          Enum.into(kvs, %{})

        map when is_map(map) ->
          # Already a map
          map

        _ ->
          %{}
      end

    {anns, rest}
  end

  defp extract_annotations(statements) do
    {%{}, statements}
  end

  defp extract_input_schema([{:input_schema, _, [schema]} | rest]) do
    # Evaluate the schema if it's AST
    evaluated_schema = evaluate_ast_map(schema)
    {evaluated_schema, rest}
  end

  defp extract_input_schema(statements) do
    {nil, statements}
  end

  defp extract_output_schema([{:output_schema, _, [schema]} | rest]) do
    # Evaluate the schema if it's AST
    evaluated_schema = evaluate_ast_map(schema)
    {evaluated_schema, rest}
  end

  defp extract_output_schema(statements) do
    {nil, statements}
  end

  defp extract_description([{:description, _, [desc]} | rest], _default) do
    {desc, rest}
  end

  defp extract_description(statements, default) do
    {default || "No description provided", statements}
  end

  defp extract_handler([{:handle, _, [handler]} | _rest]) do
    handler
  end

  defp extract_handler([_ | rest]) do
    extract_handler(rest)
  end

  defp extract_handler([]) do
    raise "Tool must have a handle function"
  end

  defp build_tool_definition(name, description, params, annotations, input_schema, output_schema) do
    # Build base tool
    tool = %{
      name: name,
      description: description
    }

    # Add input schema
    tool =
      if input_schema do
        Map.put(tool, :inputSchema, input_schema)
      else
        # Generate schema from params
        schema = generate_schema_from_params(params)
        Map.put(tool, :inputSchema, schema)
      end

    # Add output schema if provided
    tool =
      if output_schema do
        Map.put(tool, :outputSchema, output_schema)
      else
        tool
      end

    # Add annotations
    Enum.reduce(annotations, tool, fn {key, value}, acc ->
      Map.put(acc, key, value)
    end)
  end

  defp generate_schema_from_params(params) do
    properties =
      Enum.reduce(params, %{}, fn param, acc ->
        schema = build_param_schema(param)
        Map.put(acc, param.name, schema)
      end)

    required =
      params
      |> Enum.filter(& &1.required)
      |> Enum.map(&to_string(&1.name))

    schema = %{
      type: "object",
      properties: properties
    }

    if required != [] do
      Map.put(schema, :required, required)
    else
      schema
    end
  end

  defp build_param_schema(param) do
    if param[:schema] do
      param.schema
    else
      base_schema = type_to_schema(param.type)

      if param[:default] do
        Map.put(base_schema, :default, param.default)
      else
        base_schema
      end
    end
  end

  defp type_to_schema(:string), do: %{type: "string"}
  defp type_to_schema(:integer), do: %{type: "integer"}
  defp type_to_schema(:number), do: %{type: "number"}
  defp type_to_schema(:boolean), do: %{type: "boolean"}
  defp type_to_schema(:object), do: %{type: "object"}

  defp type_to_schema({:array, item_type}) do
    %{type: "array", items: type_to_schema(item_type)}
  end

  defp type_to_schema(_), do: %{type: "string"}

  defp evaluate_ast_map({:%{}, _, kvs}) when is_list(kvs) do
    # Convert AST map to actual map
    Enum.reduce(kvs, %{}, fn
      {key, value}, acc when is_atom(key) ->
        Map.put(acc, key, evaluate_ast_value(value))

      {key, value}, acc ->
        Map.put(acc, evaluate_ast_value(key), evaluate_ast_value(value))
    end)
  end

  defp evaluate_ast_map(map) when is_map(map), do: map
  defp evaluate_ast_map(_), do: %{}

  defp evaluate_ast_value({:%{}, _, _} = ast), do: evaluate_ast_map(ast)

  defp evaluate_ast_value({:%, _, [{:__aliases__, _, _}, {:%{}, _, _}]} = ast),
    do: evaluate_ast_map(ast)

  defp evaluate_ast_value(list) when is_list(list), do: Enum.map(list, &evaluate_ast_value/1)
  defp evaluate_ast_value(value), do: value

  @doc """
  Define a parameter for a tool.

  This macro is used within a tool definition to specify parameters.

  ## Examples

      param :name, :string, required: true
      param :age, :integer, default: 0
      param :tags, {:array, :string}
  """
  defmacro param(name, type, opts \\ []) do
    # This is handled by the tool macro
    quote do
      {:param, [], [unquote(name), unquote(type), unquote(opts)]}
    end
  end

  @doc """
  Define the handler function for a tool.

  The handler receives the arguments and state, and should return
  {:ok, response} or {:ok, response, new_state}.
  """
  defmacro handle(func) do
    quote do
      {:handle, [], [unquote(func)]}
    end
  end

  @doc """
  Set the description for a tool.
  """
  defmacro description(desc) do
    quote do
      {:description, [], [unquote(desc)]}
    end
  end

  @doc """
  Set the input schema for a tool.
  """
  defmacro input_schema(schema) do
    quote do
      {:input_schema, [], [unquote(schema)]}
    end
  end

  @doc """
  Set the output schema for a tool.
  """
  defmacro output_schema(schema) do
    quote do
      {:output_schema, [], [unquote(schema)]}
    end
  end

  @doc """
  Set annotations for a tool.
  """
  defmacro annotations(anns) do
    quote do
      {:annotations, [], [unquote(anns)]}
    end
  end
end
