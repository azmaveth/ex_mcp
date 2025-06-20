defmodule ExMCP.Response do
  @moduledoc """
  Structured response types for MCP operations.

  This module provides structured response handling for MCP tool calls,
  resource reads, and other operations. It represents a key improvement in v2,
  providing type-safe responses instead of raw maps.

  ## Response Types

  - `:text` - Simple text responses
  - `:json` - Structured JSON data
  - `:error` - Error responses with detailed information
  - `:tools` - Tool listing responses
  - `:resources` - Resource listing responses
  - `:prompts` - Prompt listing responses
  - `:server_info` - Server information responses
  - `:mixed` - Responses with multiple content types

  ## Usage

      # Create a text response
      response = ExMCP.Response.text("Hello, world!", "greeting_tool")

      # Create a JSON response
      response = ExMCP.Response.json(%{result: 42}, "calculator")

      # Create an error response
      response = ExMCP.Response.error("Invalid input", "validation_tool")

      # Extract content
      text = ExMCP.Response.text_content(response)
      json = ExMCP.Response.json_content(response)

  ## Protocol Data Handling

  Protocol data from MCP servers is kept as string-keyed maps to maintain
  compatibility with the MCP JSON-RPC protocol and avoid atom exhaustion issues.

      # Tools, resources, and prompts use string keys
      response.tools
      #=> [%{"name" => "hello", "description" => "Says hello", "inputSchema" => %{...}}]

      # Use accessor functions for convenience
      tool = hd(response.tools)
      ExMCP.Response.tool_name(tool)        #=> "hello"
      ExMCP.Response.tool_description(tool)  #=> "Says hello"
      ExMCP.Response.tool_input_schema(tool) #=> %{"type" => "object", ...}

  ## Design Rationale

  The structured response type provides:
  - Type safety and consistency across all MCP operations
  - Clear extraction functions for different content types
  - Metadata support for tracing and debugging
  - Unified error handling
  - Protocol fidelity by keeping MCP data as strings
  - Protection against atom exhaustion attacks
  """

  defstruct [
    :content,
    :meta,
    :tool_name,
    :request_id,
    :server_info,
    :is_error,
    # 2025-06-18 features
    :structuredOutput,
    :resourceLinks,
    # List response fields
    :tools,
    :resources,
    :prompts,
    :messages,
    # Resource read response
    :contents,
    # Prompt get response
    :description
  ]

  @type t :: %__MODULE__{
          content: [content_item()],
          meta: map() | nil,
          tool_name: String.t() | nil,
          request_id: String.t() | nil,
          server_info: map() | nil,
          is_error: boolean(),
          # 2025-06-18 features
          structuredOutput: any() | nil,
          resourceLinks: [map()] | nil,
          # List response fields
          tools: [map()] | nil,
          resources: [map()] | nil,
          prompts: [map()] | nil,
          messages: [map()] | nil,
          # Resource read response
          contents: [map()] | nil,
          # Prompt get response
          description: String.t() | nil
        }

  @type content_item :: %{
          type: String.t(),
          text: String.t() | nil,
          data: any() | nil,
          annotations: map() | nil
        }

  @doc """
  Creates a response from a raw MCP response.

  ## Examples

      iex> raw = %{"content" => [%{"type" => "text", "text" => "Hello"}]}
      iex> ExMCP.Response.from_raw_response(raw)
      %ExMCP.Response{
        content: [%{type: "text", text: "Hello", data: nil, annotations: nil}],
        meta: nil,
        tool_name: nil,
        request_id: nil,
        server_info: nil,
        is_error: false
      }
  """
  @spec from_raw_response(map(), keyword()) :: t()
  def from_raw_response(raw_response, opts \\ []) when is_map(raw_response) do
    # Handle tool content
    content = normalize_content(Map.get(raw_response, "content", []))

    %__MODULE__{
      content: content,
      meta: Map.get(raw_response, "meta"),
      tool_name: Keyword.get(opts, :tool_name),
      request_id: Keyword.get(opts, :request_id),
      server_info: Keyword.get(opts, :server_info),
      is_error: Map.get(raw_response, "is_error", Map.get(raw_response, "isError", false)),
      # 2025-06-18 features
      structuredOutput:
        Map.get(raw_response, "structuredOutput") || Map.get(raw_response, "structuredContent") ||
          if(Map.has_key?(raw_response, "completion"), do: raw_response, else: nil),
      resourceLinks: Map.get(raw_response, "resourceLinks"),
      # List response fields - kept as strings to match MCP protocol
      tools: Map.get(raw_response, "tools"),
      resources: Map.get(raw_response, "resources"),
      prompts: Map.get(raw_response, "prompts"),
      messages: Map.get(raw_response, "messages"),
      # Resource read response
      contents: Map.get(raw_response, "contents"),
      # Prompt get response
      description: Map.get(raw_response, "description")
    }
  end

  @doc """
  Creates an error response.

  ## Examples

      iex> ExMCP.Response.error("Tool execution failed", "calculate_sum")
      %ExMCP.Response{
        content: [%{type: "text", text: "Error: Tool execution failed", data: nil, annotations: nil}],
        meta: nil,
        tool_name: "calculate_sum",
        request_id: nil,
        server_info: nil,
        is_error: true
      }
  """
  @spec error(String.t(), String.t() | nil, keyword()) :: t()
  def error(message, tool_name \\ nil, opts \\ []) do
    content = [
      %{
        type: "text",
        text: "Error: #{message}",
        data: nil,
        annotations: nil
      }
    ]

    %__MODULE__{
      content: content,
      meta: Keyword.get(opts, :meta),
      tool_name: tool_name,
      request_id: Keyword.get(opts, :request_id),
      server_info: Keyword.get(opts, :server_info),
      is_error: true
    }
  end

  @doc """
  Creates a success response with text content.

  ## Examples

      iex> ExMCP.Response.text("Hello, World!", "say_hello")
      %ExMCP.Response{
        content: [%{type: "text", text: "Hello, World!", data: nil, annotations: nil}],
        meta: nil,
        tool_name: "say_hello",
        request_id: nil,
        server_info: nil,
        is_error: false
      }
  """
  @spec text(String.t(), String.t() | nil, keyword()) :: t()
  def text(text_content, tool_name \\ nil, opts \\ []) do
    content = [
      %{
        type: "text",
        text: text_content,
        data: nil,
        annotations: Keyword.get(opts, :annotations)
      }
    ]

    %__MODULE__{
      content: content,
      meta: Keyword.get(opts, :meta),
      tool_name: tool_name,
      request_id: Keyword.get(opts, :request_id),
      server_info: Keyword.get(opts, :server_info),
      is_error: false
    }
  end

  @doc """
  Creates a response with JSON data content.

  ## Examples

      iex> data = %{"result" => 42}
      iex> ExMCP.Response.json(data, "calculate")
      %ExMCP.Response{
        content: [%{type: "text", text: nil, data: %{"result" => 42}, annotations: nil}],
        meta: nil,
        tool_name: "calculate",
        request_id: nil,
        server_info: nil,
        is_error: false
      }
  """
  @spec json(any(), String.t() | nil, keyword()) :: t()
  def json(data, tool_name \\ nil, opts \\ []) do
    content = [
      %{
        type: "text",
        text: nil,
        data: data,
        annotations: Keyword.get(opts, :annotations)
      }
    ]

    %__MODULE__{
      content: content,
      meta: Keyword.get(opts, :meta),
      tool_name: tool_name,
      request_id: Keyword.get(opts, :request_id),
      server_info: Keyword.get(opts, :server_info),
      is_error: false
    }
  end

  @doc """
  Checks if the response represents an error.
  """
  @spec error?(t()) :: boolean()
  def error?(%__MODULE__{is_error: is_error}), do: is_error

  @doc """
  Gets the text content from the response.

  Returns the first text content item, or nil if none exists.
  """
  @spec text_content(t()) :: String.t() | nil
  def text_content(%__MODULE__{content: content}) do
    content
    |> Enum.find(&(&1.type == "text" && &1.text))
    |> case do
      %{text: text} -> text
      _ -> nil
    end
  end

  @doc """
  Gets all text content from the response as a concatenated string.
  """
  @spec all_text_content(t()) :: String.t()
  def all_text_content(%__MODULE__{content: content}) do
    content
    |> Enum.filter(&(&1.type == "text" && &1.text))
    |> Enum.map(& &1.text)
    |> Enum.join("\n")
  end

  @doc """
  Gets the data content from the response.

  Returns the first data content item, or nil if none exists.
  """
  @spec data_content(t()) :: any()
  def data_content(%__MODULE__{content: content}) do
    content
    |> Enum.find(& &1.data)
    |> case do
      %{data: data} -> data
      _ -> nil
    end
  end

  @doc """
  Gets the resource content from the response.

  Returns the text from the first resource content item, or nil if none exists.
  This is used for resource read responses that have a `contents` field.
  """
  @spec resource_content(t()) :: String.t() | nil
  def resource_content(%__MODULE__{contents: nil}), do: nil

  def resource_content(%__MODULE__{contents: contents}) do
    contents
    |> Enum.find(&(Map.get(&1, :text) || Map.get(&1, "text")))
    |> case do
      %{text: text} -> text
      %{"text" => text} -> text
      _ -> nil
    end
  end

  @doc """
  Converts the response back to raw MCP format.
  """
  @spec to_raw(t()) :: map()
  def to_raw(%__MODULE__{} = response) do
    content = Enum.map(response.content, &content_item_to_raw/1)

    base = %{"content" => content}

    base
    |> maybe_put("meta", response.meta)
    |> maybe_put("isError", response.is_error)
  end

  # Private helper functions

  defp normalize_content(content) when is_list(content) do
    Enum.map(content, &normalize_content_item/1)
  end

  defp normalize_content(_), do: []

  defp normalize_content_item(%{"type" => type} = item) do
    %{
      type: type,
      text: Map.get(item, "text"),
      data: Map.get(item, "data"),
      annotations: Map.get(item, "annotations")
    }
  end

  defp normalize_content_item(item) when is_map(item) do
    # Handle legacy content format
    %{
      type: "text",
      text: Map.get(item, "text"),
      data: Map.get(item, "data"),
      annotations: Map.get(item, "annotations")
    }
  end

  defp normalize_content_item(_), do: nil

  defp content_item_to_raw(%{type: type} = item) do
    base = %{"type" => type}

    base
    |> maybe_put("text", item.text)
    |> maybe_put("data", item.data)
    |> maybe_put("annotations", item.annotations)
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, _key, false), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  # Note: Protocol data is kept as strings to maintain compatibility with MCP
  # and avoid atom exhaustion issues. Use accessor functions for convenience.

  @doc """
  Gets the tool name from a tool definition.

  ## Examples

      iex> tool = %{"name" => "hello", "description" => "Says hello"}
      iex> ExMCP.Response.tool_name(tool)
      "hello"
  """
  def tool_name(%{"name" => name}), do: name
  def tool_name(_), do: nil

  @doc """
  Gets the tool description from a tool definition.

  ## Examples

      iex> tool = %{"name" => "hello", "description" => "Says hello"}
      iex> ExMCP.Response.tool_description(tool)
      "Says hello"
  """
  def tool_description(%{"description" => desc}), do: desc
  def tool_description(_), do: nil

  @doc """
  Gets the input schema from a tool definition.

  ## Examples

      iex> tool = %{"name" => "hello", "inputSchema" => %{"type" => "object"}}
      iex> ExMCP.Response.tool_input_schema(tool)
      %{"type" => "object"}
  """
  def tool_input_schema(%{"inputSchema" => schema}), do: schema
  def tool_input_schema(_), do: nil

  @doc """
  Gets a property from a schema properties map.

  ## Examples

      iex> schema = %{"properties" => %{"name" => %{"type" => "string"}}}
      iex> ExMCP.Response.schema_property(schema, "name")
      %{"type" => "string"}
  """
  def schema_property(%{"properties" => props}, key) when is_map(props) do
    Map.get(props, key)
  end

  def schema_property(_, _), do: nil
end
