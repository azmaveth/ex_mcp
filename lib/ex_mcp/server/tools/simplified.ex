defmodule ExMCP.Server.Tools.Simplified do
  @moduledoc """
  Simplified tool definition DSL that reduces metaprogramming complexity.

  This module provides a cleaner API that avoids heavy AST manipulation
  while maintaining a similar developer experience to the original DSL.

  ## Example Usage

      defmodule MyServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.Tools.Simplified
        
        deftool "echo", "Echo back the input" do
          param :message, :string, required: true
          param :uppercase, :boolean, default: false
          
          run fn %{message: msg, uppercase: up}, state ->
            result = if up, do: String.upcase(msg), else: msg
            {:ok, %{text: result}, state}
          end
        end
        
        deftool "calculator" do
          description "Perform basic calculations"
          
          input_schema %{
            type: "object",
            properties: %{
              operation: %{type: "string", enum: ["add", "subtract", "multiply", "divide"]},
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
          
          run &Calculator.execute/2
        end
      end
  """

  alias ExMCP.Server.Tools.{Builder, Registry, ResponseNormalizer}

  defmacro __using__(_opts) do
    quote do
      import ExMCP.Server.Tools.Simplified

      # Store tools at compile time for the module
      Module.register_attribute(__MODULE__, :simple_tools, accumulate: true)
      @before_compile ExMCP.Server.Tools.Simplified

      # Start a registry for this module if not using global one
      def __tool_registry__ do
        case Process.whereis(__MODULE__.ToolRegistry) do
          nil ->
            {:ok, pid} = Registry.start_link(name: __MODULE__.ToolRegistry)
            pid

          pid ->
            pid
        end
      end
    end
  end

  defmacro __before_compile__(env) do
    tools = Module.get_attribute(env.module, :simple_tools, [])

    init_code = generate_init_code(tools)
    handler_implementations = generate_handler_implementations()

    quote do
      unquote(init_code)
      unquote(handler_implementations)
    end
  end

  # Generate initialization code based on tools
  defp generate_init_code([]), do: generate_empty_init()
  defp generate_init_code(tools), do: generate_tools_init(tools)

  defp generate_empty_init do
    quote do
      def __init_tools__, do: :ok
      defp ensure_tools_initialized, do: :ok
    end
  end

  defp generate_tools_init(tools) do
    quote do
      def __init_tools__ do
        registry = __tool_registry__()
        tools = unquote(Macro.escape(tools))

        Enum.each(tools, fn {tool_def, handler} ->
          Registry.register_tool(registry, tool_def, handler)
        end)

        :ok
      end

      # Auto-initialize on first use
      defp ensure_tools_initialized do
        if Process.get(:tools_initialized) != true do
          __init_tools__()
          Process.put(:tools_initialized, true)
        end

        :ok
      end
    end
  end

  defp generate_handler_implementations do
    quote do
      @impl ExMCP.Server.Handler
      def handle_list_tools(_params, state) do
        ensure_tools_initialized()
        tools = Registry.list_tools(__tool_registry__())
        {:ok, tools, state}
      end

      @impl ExMCP.Server.Handler
      def handle_call_tool(%{name: tool_name, arguments: args}, state) do
        ensure_tools_initialized()

        case Registry.call_tool(__tool_registry__(), tool_name, args, state) do
          {:ok, result, new_state} ->
            # Normalize response for MCP spec
            normalized = ResponseNormalizer.normalize(result)
            {:ok, normalized, new_state}

          {:ok, result} ->
            normalized = ResponseNormalizer.normalize(result)
            {:ok, normalized, state}

          {:error, reason} ->
            {:ok,
             %{
               content: [%{type: "text", text: "Error: #{inspect(reason)}"}],
               isError: true
             }, state}
        end
      end
    end
  end

  @doc """
  Define a tool with a cleaner syntax.

  Instead of heavy metaprogramming, this builds a tool at compile time
  and registers it at runtime.
  """
  defmacro deftool(name, description \\ nil, do: block) do
    tool_builder = build_tool_from_block(name, description, block)

    quote do
      # Build the tool at compile time
      tool_result = unquote(tool_builder)

      case tool_result do
        {:ok, {tool_def, handler}} ->
          # Store for registration at runtime
          @simple_tools {tool_def, handler}

        {:error, reason} ->
          raise CompileError,
            description: "Failed to build tool '#{unquote(name)}': #{reason}"
      end
    end
  end

  # Macros for the DSL within deftool

  defmacro param(name, type, opts \\ []) do
    quote do
      {:param, unquote(name), unquote(type), unquote(opts)}
    end
  end

  defmacro description(text) do
    quote do
      {:description, unquote(text)}
    end
  end

  defmacro title(text) do
    quote do
      {:title, unquote(text)}
    end
  end

  defmacro input_schema(schema) do
    quote do
      {:input_schema, unquote(Macro.escape(schema))}
    end
  end

  defmacro output_schema(schema) do
    quote do
      {:output_schema, unquote(Macro.escape(schema))}
    end
  end

  defmacro annotations(anns) do
    quote do
      {:annotations, unquote(Macro.escape(anns))}
    end
  end

  defmacro run(handler) do
    quote do
      {:run, unquote(handler)}
    end
  end

  # Private helpers

  defp build_tool_from_block(name, default_desc, block) do
    # Convert block to list of instructions
    instructions = extract_instructions(block)

    # Build the tool using the Builder pattern
    quote do
      tool = Builder.new(unquote(name))

      # Set default description if provided
      tool = apply_default_description(tool, unquote(default_desc))

      # Process each instruction
      final_tool = process_instructions(tool, unquote(instructions))

      # Build the final tool
      Builder.build(final_tool)
    end
  end

  defp extract_instructions(block) do
    case block do
      {:__block__, _, stmts} -> stmts
      single -> [single]
    end
  end

  # Helper function that will be called at runtime in the generated code
  def apply_default_description(tool, nil), do: tool

  def apply_default_description(tool, desc) do
    Builder.description(tool, desc)
  end

  # Helper function that will be called at runtime to process instructions
  def process_instructions(tool, instructions) do
    Enum.reduce(instructions, tool, &process_single_instruction/2)
  end

  defp process_single_instruction(instruction, acc) do
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

      {:run, handler} ->
        Builder.handler(acc, handler)

      _ ->
        acc
    end
  end
end
