defmodule ExMCP.DSL.Meta do
  @moduledoc """
  Shared metadata DSL for Tools, Resources, and Prompts.

  Provides a consistent `meta` block syntax across all DSL types:

      deftool "say_hello" do
        meta do
          name "Hello Tool"
          description "Says hello to someone"
          version "1.0.0"
        end
        
        input_schema %{...}
      end
      
      defresource "config://app" do
        meta do
          name "App Config"
          description "Application configuration"
        end
        
        mime_type "application/json"
      end
      
      defprompt "greeting" do
        meta do
          name "Greeting Template"
          description "A greeting template"
        end
        
        arguments do
          arg :style, description: "Greeting style"
        end
      end
  """

  @doc """
  Defines a metadata block for DSL elements.

  The meta block accumulates metadata using module attributes
  and validates required fields.
  """
  defmacro meta(do: block) do
    quote do
      # Import meta field functions into this scope
      import ExMCP.DSL.Meta, only: [name: 1, description: 1, version: 1, author: 1, tags: 1]

      # Execute the meta block to accumulate fields
      unquote(block)
    end
  end

  @doc """
  Sets the name for the current DSL element.
  """
  defmacro name(value) do
    quote do
      Module.put_attribute(__MODULE__, :__meta_name__, unquote(value))
    end
  end

  @doc """
  Sets the description for the current DSL element.
  """
  defmacro description(value) do
    quote do
      Module.put_attribute(__MODULE__, :__meta_description__, unquote(value))
    end
  end

  @doc """
  Sets the version for the current DSL element.
  """
  defmacro version(value) do
    quote do
      Module.put_attribute(__MODULE__, :__meta_version__, unquote(value))
    end
  end

  @doc """
  Sets the author for the current DSL element.
  """
  defmacro author(value) do
    quote do
      Module.put_attribute(__MODULE__, :__meta_author__, unquote(value))
    end
  end

  @doc """
  Sets tags for the current DSL element.
  """
  defmacro tags(value) when is_list(value) do
    quote do
      Module.put_attribute(__MODULE__, :__meta_tags__, unquote(value))
    end
  end

  @doc """
  Retrieves all accumulated metadata for the current DSL element.

  Returns a map with all the metadata fields that have been set.
  """
  def get_meta(module) do
    %{
      name: Module.get_attribute(module, :__meta_name__),
      description: Module.get_attribute(module, :__meta_description__),
      version: Module.get_attribute(module, :__meta_version__),
      author: Module.get_attribute(module, :__meta_author__),
      tags: Module.get_attribute(module, :__meta_tags__)
    }
    |> Enum.reject(fn {_k, v} -> is_nil(v) end)
    |> Map.new()
  end

  @doc """
  Clears all metadata attributes for the current DSL element.

  Should be called before processing each new DSL element.
  """
  def clear_meta(module) do
    Module.delete_attribute(module, :__meta_name__)
    Module.delete_attribute(module, :__meta_description__)
    Module.delete_attribute(module, :__meta_version__)
    Module.delete_attribute(module, :__meta_author__)
    Module.delete_attribute(module, :__meta_tags__)
  end

  @doc """
  Validates that required metadata fields are present.

  ## Options

  * `:require_name` - Requires name field (default: true)
  * `:require_description` - Requires description field (default: true)
  """
  def validate_meta!(element_type, element_id, meta, opts \\ []) do
    require_name = Keyword.get(opts, :require_name, true)
    require_description = Keyword.get(opts, :require_description, true)

    if require_name && !meta[:name] do
      raise CompileError,
        description:
          "#{element_type} #{inspect(element_id)} is missing a name. Use meta do name \"...\" end to provide one."
    end

    if require_description && !meta[:description] do
      raise CompileError,
        description:
          "#{element_type} #{inspect(element_id)} is missing a description. Use meta do description \"...\" end to provide one."
    end

    meta
  end
end
