defmodule ExMCP.DSL.Resource do
  @moduledoc """
  DSL for defining MCP resources with URI patterns and metadata.

  Provides the `defresource` and `defresource_template` macros for
  defining resources that can be read by MCP clients.
  """

  @doc """
  Defines a resource with its URI and metadata.

  ## Examples

      defresource "config://app/settings" do
        name "Application Settings"
        description "Current application configuration"
        mime_type "application/json"
        
        annotations %{
          audience: ["admin"],
          priority: 0.8
        }
      end
      
      # Pattern-based resource with subscription support
      defresource "file://logs/*.log" do
        name "Log Files"
        description "Application log files"
        mime_type "text/plain"
        list_pattern true
        subscribable true
      end
  """
  defmacro defresource(uri, do: body) do
    quote do
      @__resource_uri__ unquote(uri)
      @__resource_opts__ []

      unquote(body)

      # Register the resource in the module's metadata
      @__resources__ Map.put(
                       Module.get_attribute(__MODULE__, :__resources__) || %{},
                       unquote(uri),
                       %{
                         uri: unquote(uri),
                         name: Module.get_attribute(__MODULE__, :__resource_name__),
                         description: Module.get_attribute(__MODULE__, :__resource_description__),
                         mime_type: Module.get_attribute(__MODULE__, :__resource_mime_type__),
                         annotations:
                           Module.get_attribute(__MODULE__, :__resource_annotations__) || %{},
                         list_pattern:
                           Module.get_attribute(__MODULE__, :__resource_list_pattern__) || false,
                         subscribable:
                           Module.get_attribute(__MODULE__, :__resource_subscribable__) || false,
                         size: Module.get_attribute(__MODULE__, :__resource_size__)
                       }
                     )

      # Clean up temporary attributes
      Module.delete_attribute(__MODULE__, :__resource_uri__)
      Module.delete_attribute(__MODULE__, :__resource_name__)
      Module.delete_attribute(__MODULE__, :__resource_description__)
      Module.delete_attribute(__MODULE__, :__resource_mime_type__)
      Module.delete_attribute(__MODULE__, :__resource_annotations__)
      Module.delete_attribute(__MODULE__, :__resource_list_pattern__)
      Module.delete_attribute(__MODULE__, :__resource_subscribable__)
      Module.delete_attribute(__MODULE__, :__resource_size__)
    end
  end

  @doc """
  Defines a resource template with URI pattern.

  ## Examples

      defresource_template "github://repos/{owner}/{repo}/issues/{id}" do
        name "GitHub Issues"
        description "Access GitHub issue data"
        mime_type "application/json"
      end
  """
  defmacro defresource_template(uri_template, do: body) do
    quote do
      @__resource_template_uri__ unquote(uri_template)

      unquote(body)

      # Register the resource template in the module's metadata
      @__resource_templates__ Map.put(
                                Module.get_attribute(__MODULE__, :__resource_templates__) || %{},
                                unquote(uri_template),
                                %{
                                  uri_template: unquote(uri_template),
                                  name: Module.get_attribute(__MODULE__, :__resource_name__),
                                  description:
                                    Module.get_attribute(__MODULE__, :__resource_description__),
                                  mime_type:
                                    Module.get_attribute(__MODULE__, :__resource_mime_type__),
                                  annotations:
                                    Module.get_attribute(__MODULE__, :__resource_annotations__) ||
                                      %{}
                                }
                              )

      # Clean up temporary attributes
      Module.delete_attribute(__MODULE__, :__resource_template_uri__)
      Module.delete_attribute(__MODULE__, :__resource_name__)
      Module.delete_attribute(__MODULE__, :__resource_description__)
      Module.delete_attribute(__MODULE__, :__resource_mime_type__)
      Module.delete_attribute(__MODULE__, :__resource_annotations__)
    end
  end

  @doc """
  Sets the human-readable name for the current resource.
  """
  defmacro resource_name(resource_name) do
    quote do
      @__resource_name__ unquote(resource_name)
    end
  end

  @doc """
  Sets the description for the current resource.
  """
  defmacro resource_description(desc) do
    quote do
      @__resource_description__ unquote(desc)
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
  Sets annotations for the current resource.
  """
  defmacro resource_annotations(annotations) do
    quote do
      @__resource_annotations__ unquote(annotations)
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
  Extracts variables from a URI template.

  ## Examples

      iex> ExMCP.DSL.Resource.extract_variables("repos/owner/repo/issues/123", "repos/{owner}/{repo}/issues/{id}")
      %{"owner" => "owner", "repo" => "repo", "id" => "123"}
  """
  def extract_variables(uri, template) do
    # Convert template to regex with named groups
    regex_pattern =
      template
      |> String.replace(~r/\{([^}]+)\}/, "(?<\\1>[^/]+)")
      |> then(&("^" <> &1 <> "$"))

    case Regex.compile(regex_pattern) do
      {:ok, regex} ->
        case Regex.named_captures(regex, uri) do
          nil -> %{}
          captures -> captures
        end

      {:error, _} ->
        %{}
    end
  end
end
