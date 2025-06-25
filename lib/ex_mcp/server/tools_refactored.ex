defmodule ExMCP.Server.ToolsRefactored do
  @moduledoc """
  Refactored DSL for defining MCP tools with reduced metaprogramming complexity.

  This module provides the same developer experience as the original Server.Tools
  but uses the new simplified architecture:

  - `ExMCP.Server.Tools.Builder` - Builder pattern for tool creation
  - `ExMCP.Server.Tools.Registry` - Runtime tool registry
  - `ExMCP.Server.Tools.Simplified` - Clean DSL without heavy metaprogramming
  - `ExMCP.Server.Tools.ResponseNormalizer` - Response formatting

  ## Migration Guide

  The API is 100% compatible with the original. Simply change:

      use ExMCP.Server.Tools

  to:

      use ExMCP.Server.ToolsRefactored

  ## Simple API

      defmodule MyServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.ToolsRefactored
        
        tool "echo", "Echo back the input" do
          param :message, :string, required: true
          
          handle fn %{message: message}, _state ->
            {:ok, text: message}
          end
        end
      end

  ## Advanced API

      # tool "calculate" do
      #   description "Perform mathematical calculations"
      #   
      #   input_schema %{
      #     type: "object",
      #     properties: %{
      #       expression: %{type: "string", pattern: "^[0-9+\\-*/().\\s]+$"}
      #     }
      #   }
      #   
      #   annotations %{
      #     readOnlyHint: true
      #   }
      #   
      #   handle fn %{expression: expr}, state ->
      #     result = evaluate_expression(expr)
      #     {:ok, %{
      #       content: [{type: "text", text: "Result: " <> to_string(result)}],
      #       structuredOutput: %{result: result, expression: expr}
      #     }, state}
      #   end
      # end
  """

  alias ExMCP.Server.Tools.{Builder, Registry, ResponseNormalizer}

  defmacro __using__(opts) do
    quote do
      import ExMCP.Server.ToolsRefactored

      # Store tools at compile time for the module
      Module.register_attribute(__MODULE__, :refactored_tools, accumulate: true)
      @before_compile ExMCP.Server.ToolsRefactored

      # Configuration
      @tool_registry_name Module.concat(__MODULE__, ToolRegistry)
      @tool_options unquote(opts)

      # Start a registry for this module
      def __tool_registry__ do
        case Process.whereis(@tool_registry_name) do
          nil ->
            {:ok, pid} = Registry.start_link(name: @tool_registry_name)
            pid

          pid ->
            pid
        end
      end
    end
  end

  defmacro __before_compile__(env) do
    tools = Module.get_attribute(env.module, :refactored_tools, [])

    init_code = generate_init_code(tools)
    handler_implementations = generate_handler_implementations()

    quote do
      unquote(init_code)
      unquote(handler_implementations)
    end
  end

  # Generate initialization code based on whether there are tools
  defp generate_init_code([]), do: generate_empty_init_code()
  defp generate_init_code(tools), do: generate_tools_init_code(tools)

  defp generate_empty_init_code do
    quote do
      def __init_tools__, do: :ok
      defp ensure_tools_initialized, do: :ok
    end
  end

  defp generate_tools_init_code(tools) do
    handler_defs = generate_handler_definitions(tools)
    init_function = generate_init_function(tools)
    ensure_function = generate_ensure_function()

    quote do
      # Define all handler functions
      unquote_splicing(handler_defs)
      unquote(init_function)
      unquote(ensure_function)
    end
  end

  defp generate_handler_definitions(tools) do
    tools
    |> Enum.with_index()
    |> Enum.map(fn {{_tool_def, handler_ast}, index} ->
      handler_name = :"__tool_handler_#{index}__"

      quote do
        def unquote(handler_name)(args, state) do
          # Inject the handler AST here
          handler_fun = unquote(handler_ast)
          handler_fun.(args, state)
        end
      end
    end)
  end

  defp generate_init_function(tools) do
    registrations =
      tools
      |> Enum.with_index()
      |> Enum.map(fn {{tool_def, _handler_ast}, index} ->
        handler_name = :"__tool_handler_#{index}__"

        quote do
          Registry.register_tool(
            registry,
            unquote(Macro.escape(tool_def)),
            &(__MODULE__.unquote(handler_name) / 2)
          )
        end
      end)

    quote do
      def __init_tools__ do
        registry = __tool_registry__()
        unquote_splicing(registrations)
        :ok
      end
    end
  end

  defp generate_ensure_function do
    quote do
      defp ensure_tools_initialized do
        if Process.get(:tools_initialized) != true do
          case __init_tools__() do
            :ok ->
              Process.put(:tools_initialized, true)
              :ok

            error ->
              error
          end
        else
          :ok
        end
      end
    end
  end

  defp generate_handler_implementations do
    quote do
      @impl ExMCP.Server.Handler
      def handle_list_tools(_params, state) do
        case ensure_tools_initialized() do
          :ok ->
            tools = Registry.list_tools(__tool_registry__())
            {:ok, tools, state}

          error ->
            {:error, error, state}
        end
      end

      @impl ExMCP.Server.Handler
      def handle_call_tool(tool_name, args, state) do
        case ensure_tools_initialized() do
          :ok ->
            handle_tool_execution(tool_name, args, state)

          error ->
            {:error, error, state}
        end
      end

      defp handle_tool_execution(tool_name, args, state) do
        case Registry.call_tool(__tool_registry__(), tool_name, args, state) do
          {:ok, result, new_state} ->
            normalized = ResponseNormalizer.normalize(result)
            {:ok, normalized, new_state}

          {:ok, result} ->
            # Handler didn't return state, use original state
            normalized = ResponseNormalizer.normalize(result)
            {:ok, normalized, state}

          {:error, :tool_not_found} ->
            error_response = ResponseNormalizer.normalize_error("Tool '#{tool_name}' not found")
            {:ok, error_response, state}

          {:error, reason} ->
            error_response = ResponseNormalizer.normalize_error("Tool error: #{inspect(reason)}")
            {:ok, error_response, state}

          error ->
            error_response =
              ResponseNormalizer.normalize_error("Unexpected error: #{inspect(error)}")

            {:ok, error_response, state}
        end
      end
    end
  end

  @doc """
  Define a tool with a cleaner syntax that reduces metaprogramming complexity.

  This macro builds a tool at compile time using the Builder pattern and
  registers it at runtime, avoiding heavy AST manipulation.

  ## Examples

      # Simple tool with description
      tool "echo", "Echo back the input" do
        param :message, :string, required: true
        
        handle fn %{message: msg}, state ->
          {:ok, %{text: msg}, state}
        end
      end
      
      # Advanced tool with full schema control
      tool "calculator" do
        description "Perform basic calculations"
        title "Calculator Tool"
        
        input_schema %{
          type: "object",
          properties: %{
            operation: %{type: "string", enum: ["add", "subtract"]},
            a: %{type: "number"},
            b: %{type: "number"}
          },
          required: ["operation", "a", "b"]
        }
        
        output_schema %{
          type: "object",
          properties: %{
            result: %{type: "number"}
          }
        }
        
        annotations %{
          readOnlyHint: true,
          category: "math"
        }
        
        handle &Calculator.execute/2
      end
  """
  defmacro tool(name, description \\ nil, do: block) do
    # Process the block to extract instructions
    instructions =
      case block do
        {:__block__, _, stmts} -> stmts
        single -> [single]
      end

    # Find handler AST
    handler_ast =
      Enum.find_value(instructions, fn
        {:handle, _, [handler]} -> handler
        _ -> nil
      end) || raise "No handler defined in tool block"

    # Build the tool definition at compile time
    tool_def = Builder.new(name)

    # Add description if provided
    tool_def =
      if description do
        Builder.description(tool_def, description)
      else
        tool_def
      end

    # Process all non-handler instructions
    tool_def =
      instructions
      |> Enum.filter(fn
        {:handle, _, _} -> false
        _ -> true
      end)
      |> Enum.reduce(tool_def, fn
        {:param, _, [param_name, type, opts]}, acc ->
          Builder.param(acc, param_name, type, opts)

        {:param, _, [param_name, type]}, acc ->
          Builder.param(acc, param_name, type, [])

        {:description, _, [text]}, acc ->
          Builder.description(acc, text)

        {:title, _, [text]}, acc ->
          Builder.title(acc, text)

        {:input_schema, _, [schema]}, acc ->
          Builder.input_schema(acc, schema)

        {:output_schema, _, [schema]}, acc ->
          Builder.output_schema(acc, schema)

        {:annotations, _, [anns]}, acc ->
          Builder.annotations(acc, anns)

        _, acc ->
          acc
      end)

    # Add a placeholder handler for building
    tool_def = Builder.handler(tool_def, fn _, _ -> {:ok, %{}} end)

    # Build the final tool
    case Builder.build(tool_def) do
      {:ok, {tool_definition, _placeholder_handler}} ->
        # Store tool definition and handler AST for later use in a quote block
        quote do
          @refactored_tools {unquote(Macro.escape(tool_definition)),
                             unquote(Macro.escape(handler_ast))}
        end

      {:error, reason} ->
        raise CompileError,
          description: "Failed to build tool '#{name}': #{reason}",
          file: __ENV__.file,
          line: __ENV__.line
    end
  end

  # DSL macros for use within tool blocks

  @doc """
  Define a parameter for a tool with type and options.

  ## Options

  - `:required` - Whether the parameter is required (default: false)
  - `:default` - Default value for the parameter
  - `:description` - Description of the parameter
  - `:enum` - List of allowed values for the parameter
  - `:pattern` - Regex pattern for string validation
  - `:minimum` - Minimum value for numbers
  - `:maximum` - Maximum value for numbers

  ## Examples

      param :message, :string, required: true, description: "The message to echo"
      param :count, :integer, default: 1, minimum: 1, maximum: 100
      param :format, :string, enum: ["json", "xml", "yaml"]
  """
  defmacro param(name, type, opts \\ []) do
    {:param, [], [name, type, opts]}
  end

  @doc """
  Set the description for a tool.
  """
  defmacro description(text) do
    {:description, [], [text]}
  end

  @doc """
  Set the title for a tool (2025-06-18 feature).
  """
  defmacro title(text) do
    {:title, [], [text]}
  end

  @doc """
  Set the input schema for a tool using JSON Schema format.

  When provided, this overrides any schema generated from param/3 calls.
  """
  defmacro input_schema(schema) do
    {:input_schema, [], [schema]}
  end

  @doc """
  Set the output schema for a tool using JSON Schema format.
  """
  defmacro output_schema(schema) do
    {:output_schema, [], [schema]}
  end

  @doc """
  Set annotations for a tool.

  Annotations provide additional metadata about the tool.
  Common annotations include:

  - `readOnlyHint` - Tool only reads data, doesn't modify state
  - `category` - Category for organizing tools
  - `experimental` - Tool is experimental/unstable
  """
  defmacro annotations(anns) do
    {:annotations, [], [anns]}
  end

  @doc """
  Define the handler function for a tool.

  The handler receives the arguments and state, and should return:
  - `{:ok, response}` - Success with response, state unchanged
  - `{:ok, response, new_state}` - Success with response and new state
  - `{:error, reason}` - Error with reason

  The response will be automatically normalized to MCP format.
  """
  defmacro handle(func) do
    {:handle, [], [func]}
  end
end
