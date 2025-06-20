defmodule ExMCP.DSL.Prompt do
  @moduledoc """
  DSL for defining MCP prompts with simpler argument syntax.

  Provides the `defprompt` macro for defining prompt templates
  that can be used by MCP clients.
  """

  @doc """
  Defines a prompt template with its arguments and metadata.

  ## Examples

      # Meta block syntax (recommended)
      defprompt "code_review" do
        meta do
          name "Code Review Assistant"
          description "Reviews code with specific focus areas"
          version "2.0.0"
        end

        arguments do
          arg :code, required: true, description: "Code to review"
          arg :language, required: false, description: "Programming language"
          arg :focus, required: false, description: "Review focus areas"
        end
      end

      # Simple prompt without arguments
      defprompt "greeting" do
        meta do
          name "Greeting Template"
          description "A simple greeting prompt"
        end
      end

      # Legacy syntax (deprecated but supported)
      defprompt "legacy_prompt" do
        name "Legacy Prompt"  # Deprecated - use meta block
        description "Legacy description"  # Deprecated - use meta block

        arguments do
          arg :data, description: "Some data"
        end
      end
  """
  defmacro defprompt(prompt_name, do: body) do
    quote do
      # Import meta DSL functions
      import ExMCP.DSL.Meta, only: [meta: 1]

      # Clear any previous meta attributes
      ExMCP.DSL.Meta.clear_meta(__MODULE__)

      @__prompt_name__ unquote(prompt_name)

      unquote(body)

      # Get accumulated meta and validate
      prompt_meta = ExMCP.DSL.Meta.get_meta(__MODULE__)

      # Get legacy name/description for backward compatibility
      legacy_name = Module.get_attribute(__MODULE__, :__prompt_display_name__)
      legacy_description = Module.get_attribute(__MODULE__, :__prompt_description__)

      # Validate the prompt definition before registering
      ExMCP.DSL.Prompt.__validate_prompt_definition__(
        unquote(prompt_name),
        prompt_meta,
        legacy_name,
        legacy_description
      )

      # Register the prompt in the module's metadata
      final_name = prompt_meta[:name] || legacy_name || unquote(prompt_name)
      final_description = prompt_meta[:description] || legacy_description

      @__prompts__ Map.put(
                     Module.get_attribute(__MODULE__, :__prompts__) || %{},
                     unquote(prompt_name),
                     %{
                       name: unquote(prompt_name),
                       display_name: final_name,
                       description: final_description,
                       arguments: Module.get_attribute(__MODULE__, :__prompt_arguments__) || [],
                       meta: prompt_meta
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
  Sets the display name for the current prompt (deprecated syntax).
  """
  defmacro prompt_name(display_name) do
    caller = __CALLER__
    file = Path.relative_to_cwd(caller.file)
    line = caller.line

    quote do
      require Logger

      Logger.warning(
        "prompt_name/1 is deprecated. Use name/1 instead.",
        file: unquote(file),
        line: unquote(line)
      )

      @__prompt_display_name__ unquote(display_name)
    end
  end

  @doc """
  Sets the description for the current prompt (deprecated syntax).
  """
  defmacro prompt_description(desc) do
    caller = __CALLER__
    file = Path.relative_to_cwd(caller.file)
    line = caller.line

    quote do
      require Logger

      Logger.warning(
        "prompt_description/1 is deprecated. Use description/1 instead.",
        file: unquote(file),
        line: unquote(line)
      )

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
  Validates a prompt definition at compile time.

  This function is called during the defprompt macro expansion to ensure
  the prompt definition is complete and valid.
  """
  def __validate_prompt_definition__(prompt_name, meta, legacy_name, legacy_description) do
    # Check for name in meta block or legacy location
    name = meta[:name] || legacy_name
    description = meta[:description] || legacy_description

    unless name do
      raise CompileError,
        description:
          "Prompt #{inspect(prompt_name)} is missing a name. Use meta do name \"...\" end to provide one."
    end

    unless description do
      raise CompileError,
        description:
          "Prompt #{inspect(prompt_name)} is missing a description. Use meta do description \"...\" end to provide one."
    end

    :ok
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
