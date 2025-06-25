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

    init_code =
      if length(tools) > 0 do
        quote do
          def __init_tools__ do
            registry = __tool_registry__()

            tools = unquote(Macro.escape(tools))

            Enum.each(tools, fn {tool_def, handler} ->
              Registry.register_tool(registry, tool_def, handler)
            end)

            :ok
          end

          # Auto-initialize on first use with proper error handling
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
      else
        quote do
          def __init_tools__, do: :ok
          defp ensure_tools_initialized, do: :ok
        end
      end

    quote do
      unquote(init_code)

      @impl ExMCP.Server.Handler
      def handle_list_tools(_params, state) do
        with :ok <- ensure_tools_initialized() do
          tools = Registry.list_tools(__tool_registry__())
          {:ok, tools, state}
        else
          error -> {:error, error, state}
        end
      end

      @impl ExMCP.Server.Handler
      def handle_call_tool(%{name: tool_name, arguments: args}, state) do
        with :ok <- ensure_tools_initialized(),
             {:ok, result, new_state} <-
               Registry.call_tool(__tool_registry__(), tool_name, args, state) do
          # Normalize response for MCP spec
          normalized = ResponseNormalizer.normalize(result)
          {:ok, normalized, new_state}
        else
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
    tool_builder = build_tool_from_block(name, description, block)

    quote do
      # Build the tool at compile time
      tool_result = unquote(tool_builder)

      case tool_result do
        {:ok, {tool_def, handler}} ->
          # Store for registration at runtime
          @refactored_tools {tool_def, handler}

        {:error, reason} ->
          raise CompileError,
            description: "Failed to build tool '#{unquote(name)}': #{reason}",
            file: __ENV__.file,
            line: __ENV__.line
      end
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
    quote do
      {:param, unquote(name), unquote(type), unquote(opts)}
    end
  end

  @doc """
  Set the description for a tool.
  """
  defmacro description(text) do
    quote do
      {:description, unquote(text)}
    end
  end

  @doc """
  Set the title for a tool (2025-06-18 feature).
  """
  defmacro title(text) do
    quote do
      {:title, unquote(text)}
    end
  end

  @doc """
  Set the input schema for a tool using JSON Schema format.

  When provided, this overrides any schema generated from param/3 calls.
  """
  defmacro input_schema(schema) do
    quote do
      {:input_schema, unquote(Macro.escape(schema))}
    end
  end

  @doc """
  Set the output schema for a tool using JSON Schema format.
  """
  defmacro output_schema(schema) do
    quote do
      {:output_schema, unquote(Macro.escape(schema))}
    end
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
    quote do
      {:annotations, unquote(Macro.escape(anns))}
    end
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
    quote do
      {:handle, unquote(func)}
    end
  end

  # Private helper for building tools from block content

  defp build_tool_from_block(name, default_desc, block) do
    # Convert block to list of instructions
    instructions =
      case block do
        {:__block__, _, stmts} -> stmts
        single -> [single]
      end

    # Build the tool using the Builder pattern
    quote do
      # Start with a new tool builder
      tool = Builder.new(unquote(name))

      # Set default description if provided
      tool =
        if unquote(default_desc) do
          Builder.description(tool, unquote(default_desc))
        else
          tool
        end

      # Process each instruction in the block
      final_tool =
        Enum.reduce(unquote(instructions), tool, fn instruction, acc ->
          case instruction do
            {:param, name, type, opts} ->
              Builder.param(acc, name, type, opts)

            {:description, text} ->
              Builder.description(acc, text)

            {:title, text} ->
              Builder.title(acc, text)

            {:input_schema, schema} ->
              Builder.input_schema(acc, schema)

            {:output_schema, schema} ->
              Builder.output_schema(acc, schema)

            {:annotations, anns} ->
              Builder.annotations(acc, anns)

            {:handle, handler} ->
              Builder.handler(acc, handler)

            _ ->
              # Ignore unknown instructions for forward compatibility
              acc
          end
        end)

      # Build the final tool
      Builder.build(final_tool)
    end
  end
end
