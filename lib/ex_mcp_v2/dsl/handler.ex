defmodule ExMCP.DSL.Handler do
  @moduledoc """
  DSL for defining MCP handlers with reduced boilerplate.

  The `defhandler` macro provides a convenient way to define handler functions
  for tools, resources, and prompts without needing to remember the exact
  callback names or add `@impl true` annotations.

  ## Examples

      # Tool handler
      defhandler :tool, "say_hello", %{"name" => name}, state do
        response = text("Hello, \#{name}!")
        {:ok, [response], state}
      end
      
      # Resource handler with pattern matching
      defhandler :resource, "file://" <> path, _uri, state do
        case File.read(path) do
          {:ok, content} -> {:ok, [text(content)], state}
          {:error, reason} -> {:error, reason, state}
        end
      end
      
      # Prompt handler with guards
      defhandler :prompt, "greeting", %{"style" => style}, state when style in ["formal", "casual"] do
        template = if style == "formal", do: "Good day", else: "Hey"
        {:ok, %{template: template}, state}
      end
  """

  @doc """
  Defines a handler function for tools, resources, or prompts.

  ## Arguments

  The macro expects a function head followed by a do block:

  - For tools: `defhandler :tool, name_pattern, args_pattern, state_pattern do ... end`
  - For resources: `defhandler :resource, uri_pattern, full_uri_pattern, state_pattern do ... end`
  - For prompts: `defhandler :prompt, name_pattern, args_pattern, state_pattern do ... end`

  ## Features

  - Supports pattern matching on all arguments
  - Supports guard clauses with `when`
  - Supports multiple function clauses
  - Automatically adds `@impl true`
  - Generates the appropriate callback function

  ## Examples

      # Simple tool handler
      defhandler :tool, "echo", args, state do
        msg = args["message"] || "No message"
        {:ok, [text(msg)], state}
      end
      
      # Multiple clauses with pattern matching
      defhandler :tool, "divide", %{"a" => a, "b" => b}, state when b != 0 do
        {:ok, [text("Result: \#{a / b}")], state}
      end
      
      defhandler :tool, "divide", %{"a" => _a, "b" => 0}, state do
        {:error, "Division by zero", state}
      end
  """
  defmacro defhandler(type, arg1, arg2, arg3, do: block) do
    # Determine the callback function name
    callback_name =
      case type do
        :tool ->
          :handle_tool_call

        :resource ->
          :handle_resource_read

        :prompt ->
          :handle_prompt_get

        _ ->
          raise CompileError,
            description:
              "Invalid handler type #{inspect(type)}. Must be :tool, :resource, or :prompt."
      end

    quote do
      @impl true
      def unquote(callback_name)(unquote(arg1), unquote(arg2), unquote(arg3)) do
        unquote(block)
      end
    end
  end
end
