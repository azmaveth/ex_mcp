defmodule ExMCP.DSL.Prompt do
  @moduledoc """
  DSL for defining MCP prompts with simpler argument syntax.

  Provides the `defprompt` macro for defining prompt templates
  that can be used by MCP clients.
  """

  @doc """
  Defines a prompt template with its arguments and metadata.

  ## Examples

      defprompt "code_review" do
        name "Code Review Assistant"
        description "Reviews code with specific focus areas"
        
        arguments do
          arg :code, required: true, description: "Code to review"
          arg :language, required: false, description: "Programming language"
          arg :focus, required: false, description: "Review focus areas"
        end
      end
      
      # Simple prompt without arguments
      defprompt "greeting" do
        name "Greeting Template"
        description "A simple greeting prompt"
      end
  """
  defmacro defprompt(prompt_name, do: body) do
    quote do
      @__prompt_name__ unquote(prompt_name)

      unquote(body)

      # Register the prompt in the module's metadata
      @__prompts__ Map.put(
                     Module.get_attribute(__MODULE__, :__prompts__) || %{},
                     unquote(prompt_name),
                     %{
                       name: unquote(prompt_name),
                       display_name:
                         Module.get_attribute(__MODULE__, :__prompt_display_name__) ||
                           unquote(prompt_name),
                       description: Module.get_attribute(__MODULE__, :__prompt_description__),
                       arguments: Module.get_attribute(__MODULE__, :__prompt_arguments__) || []
                     }
                   )

      # Clean up temporary attributes
      Module.delete_attribute(__MODULE__, :__prompt_name__)
      Module.delete_attribute(__MODULE__, :__prompt_display_name__)
      Module.delete_attribute(__MODULE__, :__prompt_description__)
      Module.delete_attribute(__MODULE__, :__prompt_arguments__)
    end
  end

  @doc """
  Sets the display name for the current prompt.
  """
  defmacro prompt_name(display_name) do
    quote do
      @__prompt_display_name__ unquote(display_name)
    end
  end

  @doc """
  Sets the description for the current prompt.
  """
  defmacro prompt_description(desc) do
    quote do
      @__prompt_description__ unquote(desc)
    end
  end

  @doc """
  Begins an arguments block for defining prompt arguments.
  """
  defmacro arguments(do: body) do
    quote do
      @__prompt_arguments__ []
      unquote(body)
    end
  end

  @doc """
  Defines an argument within an arguments block.

  ## Options

  - `:required` - Whether the argument is required (default: false)
  - `:description` - Human-readable description

  ## Examples

      arg :code, required: true, description: "Code to review"
      arg :language, description: "Programming language"
      arg :focus, required: false, description: "Areas to focus on"
  """
  defmacro arg(name, opts \\ []) do
    quote do
      arg_def = %{
        name: to_string(unquote(name)),
        description: Keyword.get(unquote(opts), :description),
        required: Keyword.get(unquote(opts), :required, false)
      }

      @__prompt_arguments__ [
        arg_def | Module.get_attribute(__MODULE__, :__prompt_arguments__) || []
      ]
    end
  end

  @doc """
  Validates prompt arguments against their definitions.

  ## Examples

      arguments = [
        %{name: "code", required: true, description: "Code to review"},
        %{name: "language", required: false, description: "Programming language"}
      ]
      
      # Valid arguments
      ExMCP.DSL.Prompt.validate_arguments(%{"code" => "def hello, do: :world"}, arguments)
      # => :ok
      
      # Missing required argument
      ExMCP.DSL.Prompt.validate_arguments(%{"language" => "elixir"}, arguments)
      # => {:error, "Missing required argument: code"}
  """
  def validate_arguments(args, argument_definitions)
      when is_map(args) and is_list(argument_definitions) do
    required_args =
      argument_definitions
      |> Enum.filter(& &1.required)
      |> Enum.map(& &1.name)

    provided_args = Map.keys(args)

    missing_required = required_args -- provided_args

    case missing_required do
      [] -> :ok
      [missing | _] -> {:error, "Missing required argument: #{missing}"}
    end
  end

  @doc """
  Converts argument definitions to the MCP protocol format.

  The MCP spec uses a simpler format for prompt arguments compared to tools.
  """
  def arguments_to_mcp_format(argument_definitions) when is_list(argument_definitions) do
    Enum.map(argument_definitions, fn arg ->
      base = %{
        "name" => arg.name,
        "required" => arg.required
      }

      case arg.description do
        nil -> base
        desc -> Map.put(base, "description", desc)
      end
    end)
  end
end
