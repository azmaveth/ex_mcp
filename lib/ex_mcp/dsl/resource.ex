defmodule ExMCP.DSL.Resource do
  @moduledoc """
  Simplified DSL for defining MCP resources.

  Provides the `defresource` macro for defining resources with metadata and MIME types.
  """

  require Logger
  alias ExMCP.DSL.Meta

  @doc """
  Defines a resource with its URI and metadata.

  ## Examples

      defresource "config://app/settings" do
        meta do
          name "Application Settings"
          description "Current application configuration"
        end

        mime_type "application/json"
        annotations %{
          audience: ["admin"],
          priority: 0.8
        }
      end

      defresource "file://logs/*.log" do
        meta do
          name "Log Files"
          description "Application log files"
        end

        mime_type "text/plain"
        list_pattern true
        subscribable true
      end
  """
  defmacro defresource(uri, do: body) do
    quote do
      # Import meta DSL functions
      import Meta, only: [meta: 1]

      # Clear any previous meta attributes
      Meta.clear_meta(__MODULE__)

      @__resource_uri__ unquote(uri)

      unquote(body)

      # Get accumulated meta and validate
      resource_meta = Meta.get_meta(__MODULE__)

      # Validate the resource definition before registering
      # credo:disable-for-next-line Credo.Check.Design.AliasUsage
      ExMCP.DSL.Resource.__validate_resource_definition__(
        unquote(uri),
        resource_meta
      )

      # Register the resource in the module's metadata
      @__resources__ Map.put(
                       Module.get_attribute(__MODULE__, :__resources__) || %{},
                       unquote(uri),
                       %{
                         uri: unquote(uri),
                         name: resource_meta[:name],
                         description: resource_meta[:description],
                         mime_type: Module.get_attribute(__MODULE__, :__resource_mime_type__),
                         annotations:
                           Module.get_attribute(__MODULE__, :__resource_annotations__) || %{},
                         list_pattern:
                           Module.get_attribute(__MODULE__, :__resource_list_pattern__) || false,
                         subscribable:
                           Module.get_attribute(__MODULE__, :__resource_subscribable__) || false,
                         size: Module.get_attribute(__MODULE__, :__resource_size__),
                         meta: resource_meta
                       }
                     )

      # Clean up temporary attributes
      Module.delete_attribute(__MODULE__, :__resource_uri__)
      Module.delete_attribute(__MODULE__, :__resource_mime_type__)
      Module.delete_attribute(__MODULE__, :__resource_annotations__)
      Module.delete_attribute(__MODULE__, :__resource_list_pattern__)
      Module.delete_attribute(__MODULE__, :__resource_subscribable__)
      Module.delete_attribute(__MODULE__, :__resource_size__)
    end
  end

  @doc """
  Sets annotations for the current resource.
  """
  defmacro annotations(annotations) do
    quote do
      @__resource_annotations__ unquote(annotations)
    end
  end

  @doc """
  Sets the MIME type for the current resource.
  """
  defmacro mime_type(type) do
    quote do
      @__resource_mime_type__ unquote(type)
    end
  end

  @doc """
  Marks the resource as a list pattern (contains wildcards like *).
  """
  defmacro list_pattern(enabled) do
    quote do
      @__resource_list_pattern__ unquote(enabled)
    end
  end

  @doc """
  Marks the resource as subscribable for change notifications.
  """
  defmacro subscribable(enabled) do
    quote do
      @__resource_subscribable__ unquote(enabled)
    end
  end

  @doc """
  Sets the expected size of the resource content in bytes.
  """
  defmacro size(bytes) do
    quote do
      @__resource_size__ unquote(bytes)
    end
  end

  @doc """
  Validates a resource definition at compile time.

  This function is called during the defresource macro expansion to ensure
  the resource definition is complete and valid.
  """
  def __validate_resource_definition__(uri, meta) do
    # Check for name and description in meta block
    unless meta[:name] do
      raise CompileError,
        description:
          "Resource #{inspect(uri)} is missing a name. Use meta do name \"...\" end to provide one."
    end

    unless meta[:description] do
      raise CompileError,
        description:
          "Resource #{inspect(uri)} is missing a description. Use meta do description \"...\" end to provide one."
    end

    :ok
  end

  @doc """
  Checks if a URI matches a resource pattern.

  Supports glob-style patterns with * wildcard.

  ## Examples

      iex> ExMCP.DSL.Resource.uri_matches?("file://logs/app.log", "file://logs/*.log")
      true

      iex> ExMCP.DSL.Resource.uri_matches?("file://data/config.json", "file://logs/*.log")
      false
  """
  def uri_matches?(uri, pattern) do
    regex_pattern =
      pattern
      |> Regex.escape()
      |> String.replace("\\*", ".*")
      |> then(&("^" <> &1 <> "$"))

    case Regex.compile(regex_pattern) do
      {:ok, regex} -> Regex.match?(regex, uri)
      {:error, _} -> false
    end
  end

  @doc """
  Extracts variables from a URI based on a template pattern.

  Templates use {variable_name} syntax to indicate variable segments.
  Returns a map of variable names to their values from the URI.

  ## Examples

      iex> ExMCP.DSL.Resource.extract_variables("repos/octocat/hello/issues/42", "repos/{owner}/{repo}/issues/{id}")
      %{"owner" => "octocat", "repo" => "hello", "id" => "42"}

      iex> ExMCP.DSL.Resource.extract_variables("api/v2/users/123", "api/v{version}/users/{id}")
      %{"version" => "2", "id" => "123"}

      iex> ExMCP.DSL.Resource.extract_variables("static/path", "static/path")
      %{}
  """
  def extract_variables(uri, template) do
    # Extract variable names from template
    variable_names =
      Regex.scan(~r/\{([^}]+)\}/, template)
      |> Enum.map(fn [_, name] -> name end)

    # Convert template to regex pattern
    regex_pattern =
      template
      |> Regex.escape()
      |> String.replace("\\{", "{")
      |> String.replace("\\}", "}")
      |> String.replace(~r/\{[^}]+\}/, "([^/]+)")
      |> then(&("^" <> &1 <> "$"))

    case Regex.compile(regex_pattern) do
      {:ok, regex} ->
        case Regex.run(regex, uri) do
          nil ->
            %{}

          [_ | values] ->
            # Zip variable names with captured values
            variable_names
            |> Enum.zip(values)
            |> Enum.into(%{})
        end

      {:error, _} ->
        # Return empty map for invalid patterns
        %{}
    end
  end
end
