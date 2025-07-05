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
          def unquote(handler_name)(_args, state) do
            unquote(handler_ast).(_args, state)
          end
        end
      end)

    # Build tool name to handler mapping with PRE-COMPILED output schemas
    # Convert to map for O(1) lookup performance instead of O(n) list search
    tool_mapping =
      tools
      |> Enum.reverse()
      |> Enum.with_index()
      |> Enum.map(fn {tool, index} ->
        # Pre-compile the output schema at compile time.
        # This moves the expensive schema resolution from runtime to compile time.
        resolved_schema = __MODULE__.compile_schema(tool[:outputSchema])
        {tool.name, {:"__tool_handler_#{index}__", resolved_schema}}
      end)
      |> Map.new()
      |> Macro.escape()

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

        case Map.get(tool_mapping, params.name) do
          {handler_func, output_schema} ->
            result = apply(__MODULE__, handler_func, [params.arguments, state])

            # Validate output if schema is defined
            validated_result =
              if output_schema do
                Tools.__validate_and_normalize_response__(result, output_schema, state)
              else
                Tools.__normalize_response__(result, state)
              end

            validated_result

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
    # Ensure response has proper structure for MCP 2025-06-18 spec
    normalized_response = normalize_response_structure(response)
    {:ok, normalized_response, state}
  end

  def __normalize_response__({:ok, response, new_state}, _state) when is_binary(response) do
    {:ok, %{content: [%{type: "text", text: response}]}, new_state}
  end

  def __normalize_response__({:ok, %{text: text}, new_state}, _state) do
    {:ok, %{content: [%{type: "text", text: text}]}, new_state}
  end

  def __normalize_response__({:ok, response, new_state}, _state) do
    # Ensure response has proper structure for MCP 2025-06-18 spec
    normalized_response = normalize_response_structure(response)
    {:ok, normalized_response, new_state}
  end

  def __normalize_response__({:error, reason}, state) when is_binary(reason) do
    {:ok, %{content: [%{type: "text", text: reason}], isError: true}, state}
  end

  def __normalize_response__({:error, reason}, state) do
    {:ok, %{content: [%{type: "text", text: inspect(reason)}], isError: true}, state}
  end

  # Helper function to normalize response structure for MCP 2025-06-18 spec compliance
  defp normalize_response_structure(response) when is_map(response) do
    response
    |> ensure_content_field()
    |> preserve_structured_output()
    |> preserve_other_fields()
  end

  defp ensure_content_field(%{content: _} = response), do: response

  defp ensure_content_field(%{structuredOutput: _} = response) do
    # If only structuredOutput is present, add empty content array
    Map.put_new(response, :content, [])
  end

  defp ensure_content_field(response) do
    # If neither content nor structuredOutput, add empty content array
    Map.put_new(response, :content, [])
  end

  defp preserve_structured_output(%{structuredOutput: _} = response), do: response

  defp preserve_structured_output(%{structuredContent: structured_content} = response) do
    # Support legacy structuredContent field by mapping to structuredOutput
    response
    |> Map.put(:structuredOutput, structured_content)
    |> Map.delete(:structuredContent)
  end

  defp preserve_structured_output(response), do: response

  defp preserve_other_fields(response) do
    # Preserve all other fields like isError, resourceLinks, metadata, etc.
    response
    |> ensure_resource_links_format()
  end

  defp ensure_resource_links_format(%{resourceLinks: links} = response) when is_list(links) do
    # Ensure resource links are properly formatted for 2025-06-18 spec
    response
  end

  defp ensure_resource_links_format(response) do
    # No resource links or not a list, leave as is
    response
  end

  @doc false
  def compile_schema(nil), do: nil

  def compile_schema(schema) do
    if Code.ensure_loaded?(ExJsonSchema) do
      try do
        string_schema = atomize_keys_to_strings(schema)
        ExJsonSchema.Schema.resolve(string_schema)
      rescue
        e ->
          # credo:disable-for-next-line Credo.Check.Warning.RaiseInsideRescue
          raise CompileError,
            description:
              "Invalid output schema provided to output_schema/1. Details: #{inspect(e)}",
            file: __ENV__.file,
            line: __ENV__.line
      end
    else
      # If ExJsonSchema is not available, emit a compile-time warning
      IO.warn(
        "ExJsonSchema is not available at compile time. " <>
          "Output schema validation will be limited to a non-nil check at runtime. " <>
          "Please ensure {:ex_json_schema, \"~> 0.9\"} is in your mix.exs deps.",
        []
      )

      schema
    end
  end

  @doc false
  def __validate_and_normalize_response__(result, output_schema, state) do
    {:ok, response, new_state} = __normalize_response__(result, state)
    validate_structured_output(response, output_schema, new_state)
  end

  defp validate_structured_output(response, output_schema, state) do
    case response do
      %{structuredOutput: structured_output} ->
        # Validate the structured output against the schema
        # Convert structured output to string keys for validation
        string_data = atomize_keys_to_strings(structured_output)

        case validate_with_schema(string_data, output_schema) do
          :ok ->
            {:ok, response, state}

          {:error, validation_errors} ->
            # Return error response with validation details
            error_message = format_validation_errors(validation_errors)

            {:ok,
             %{
               content: [%{type: "text", text: "Output validation failed: #{error_message}"}],
               isError: true
             }, state}
        end

      _ ->
        # No structured output to validate
        {:ok, response, state}
    end
  end

  @doc false
  def validate_with_schema(data, resolved_schema) do
    # Use ExJsonSchema if available, otherwise basic validation
    if Code.ensure_loaded?(ExJsonSchema) and resolved_schema do
      # The schema is now expected to be pre-resolved at compile time.
      try do
        case ExJsonSchema.Validator.validate(resolved_schema, data) do
          :ok -> :ok
          {:error, errors} -> {:error, errors}
        end
      rescue
        e ->
          # Log the full error for debugging
          require Logger

          Logger.error("Unexpected error during output schema validation: #{inspect(e)}")

          # Return sanitized error to prevent information leakage
          {:error, ["Output validation error. Please check server logs for details."]}
      end
    else
      # Fallback: just check that data is not nil
      if data != nil do
        :ok
      else
        {:error, ["ExJsonSchema not available for validation"]}
      end
    end
  end

  defp atomize_keys_to_strings(map) when is_map(map) do
    Map.new(map, fn
      {:is_error, v} -> {"isError", atomize_keys_to_strings(v)}
      {k, v} when is_atom(k) -> {Atom.to_string(k), atomize_keys_to_strings(v)}
      {k, v} -> {k, atomize_keys_to_strings(v)}
    end)
  end

  defp atomize_keys_to_strings(list) when is_list(list) do
    Enum.map(list, &atomize_keys_to_strings/1)
  end

  defp atomize_keys_to_strings(value), do: value

  defp format_validation_errors(errors) when is_list(errors) do
    Enum.map_join(errors, ", ", &format_single_error/1)
  end

  defp format_single_error({error_msg, path}) when is_binary(error_msg) do
    "#{path}: #{error_msg}"
  end

  defp format_single_error(error) do
    inspect(error)
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
    {params, annotations, input_schema, output_schema, handler, final_description, title} =
      extract_tool_info(block, description)

    # Build the tool definition
    tool_def =
      build_tool_definition(
        name,
        final_description,
        params,
        annotations,
        input_schema,
        output_schema,
        title
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
    {title, statements} = extract_title(statements)
    {description, statements} = extract_description(statements, default_description)
    {params, statements} = extract_params_from_statements(statements, [])
    {input_schema, statements} = extract_input_schema(statements)
    {output_schema, statements} = extract_output_schema(statements)
    {annotations, statements} = extract_annotations(statements)
    handler = extract_handler(statements)

    {params, annotations, input_schema, output_schema, handler, description, title}
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

  defp extract_title([{:title, _, [title]} | rest]) do
    {title, rest}
  end

  defp extract_title(statements) do
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

  defp build_tool_definition(
         name,
         description,
         params,
         annotations,
         input_schema,
         output_schema,
         title
       ) do
    # Build base tool
    tool = %{
      name: name,
      description: description
    }

    # Add title if provided (2025-06-18 feature)
    tool =
      if title do
        Map.put(tool, :title, title)
      else
        tool
      end

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
  Set the title for a tool (2025-06-18 feature).
  """
  defmacro title(title_text) do
    quote do
      {:title, [], [unquote(title_text)]}
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
