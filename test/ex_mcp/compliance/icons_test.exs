defmodule ExMCP.Compliance.IconsTest do
  @moduledoc """
  Compliance tests for icons support in MCP 2025-11-25.

  Verifies that tools, resources, and prompts defined with the `icons` macro
  in the DSL correctly include icon metadata, and that definitions without
  icons do not include the icons key.

  Note: Because `use ExMCP.Server` imports ExMCP.DSL.Tool, ExMCP.DSL.Resource,
  and ExMCP.DSL.Prompt -- all of which define an `icons/1` macro -- calls to
  `icons` must be fully qualified to avoid ambiguity.
  """

  # credo:disable-for-this-file Credo.Check.Design.AliasUsage
  use ExUnit.Case, async: true

  @moduletag :compliance

  # --- Test Modules ---

  defmodule ToolWithIcons do
    use ExMCP.Server

    deftool "wrench_tool" do
      meta do
        description("A tool with an emoji icon")
      end

      ExMCP.DSL.Tool.icons([%{type: "emoji", uri: "\u{1F527}"}])

      input_schema(%{
        type: "object",
        properties: %{input: %{type: "string"}},
        required: ["input"]
      })
    end

    @impl true
    def handle_tool_call("wrench_tool", %{"input" => input}, state) do
      {:ok, %{content: [text("Processed: #{input}")]}, state}
    end
  end

  defmodule ToolWithMultipleIcons do
    use ExMCP.Server

    deftool "multi_icon_tool" do
      meta do
        description("A tool with multiple icon types")
      end

      ExMCP.DSL.Tool.icons([
        %{type: "emoji", uri: "\u{1F4E6}"},
        %{type: "icon", uri: "https://example.com/tool-icon.svg", mediaType: "image/svg+xml"}
      ])

      input_schema(%{
        type: "object",
        properties: %{data: %{type: "string"}},
        required: ["data"]
      })
    end

    @impl true
    def handle_tool_call("multi_icon_tool", _params, state) do
      {:ok, %{content: [text("done")]}, state}
    end
  end

  defmodule ToolWithoutIcons do
    use ExMCP.Server

    deftool "plain_tool" do
      meta do
        description("A tool with no icons defined")
      end

      input_schema(%{
        type: "object",
        properties: %{value: %{type: "string"}},
        required: ["value"]
      })
    end

    @impl true
    def handle_tool_call("plain_tool", _params, state) do
      {:ok, %{content: [text("ok")]}, state}
    end
  end

  defmodule ResourceWithIcons do
    use ExMCP.Server

    defresource "config://app/database" do
      meta do
        name("Database Config")
        description("Database configuration resource")
      end

      mime_type("application/json")

      ExMCP.DSL.Resource.icons([%{type: "emoji", uri: "\u{1F5C4}\u{FE0F}"}])
    end

    @impl true
    def handle_resource_read(_uri, _full_uri, state) do
      {:ok, [json(%{host: "localhost"})], state}
    end
  end

  defmodule ResourceWithSvgIcon do
    use ExMCP.Server

    defresource "file:///data/report.csv" do
      meta do
        name("Report Data")
        description("CSV report data resource")
      end

      mime_type("text/csv")

      ExMCP.DSL.Resource.icons([
        %{type: "icon", uri: "https://example.com/csv-icon.svg", mediaType: "image/svg+xml"}
      ])
    end

    @impl true
    def handle_resource_read(_uri, _full_uri, state) do
      {:ok, [text("col1,col2\nval1,val2")], state}
    end
  end

  defmodule ResourceWithoutIcons do
    use ExMCP.Server

    defresource "config://app/settings" do
      meta do
        name("App Settings")
        description("Application settings without icons")
      end

      mime_type("application/json")
    end

    @impl true
    def handle_resource_read(_uri, _full_uri, state) do
      {:ok, [json(%{debug: false})], state}
    end
  end

  defmodule PromptWithIcons do
    use ExMCP.Server

    defprompt "code_review" do
      meta do
        name("Code Review")
        description("Review code for best practices")
      end

      ExMCP.DSL.Prompt.icons([%{type: "emoji", uri: "\u{1F50D}"}])

      arguments do
        arg(:code, required: true, description: "Code to review")
      end
    end

    @impl true
    def handle_prompt_get("code_review", args, state) do
      {:ok, %{messages: [user("Review: #{args["code"]}")]}, state}
    end
  end

  defmodule PromptWithMultipleIcons do
    use ExMCP.Server

    defprompt "summarize" do
      meta do
        name("Summarizer")
        description("Summarize text content")
      end

      ExMCP.DSL.Prompt.icons([
        %{type: "emoji", uri: "\u{1F4DD}"},
        %{type: "icon", uri: "https://example.com/summarize.png", mediaType: "image/png"}
      ])

      arguments do
        arg(:text, required: true, description: "Text to summarize")
        arg(:max_length, description: "Maximum summary length")
      end
    end

    @impl true
    def handle_prompt_get("summarize", _args, state) do
      {:ok, %{messages: [user("Summarize this")]}, state}
    end
  end

  defmodule PromptWithoutIcons do
    use ExMCP.Server

    defprompt "greeting" do
      meta do
        name("Greeting")
        description("A simple greeting prompt without icons")
      end
    end

    @impl true
    def handle_prompt_get("greeting", _args, state) do
      {:ok, %{messages: [user("Hello!")]}, state}
    end
  end

  defmodule ServerWithMixedIcons do
    use ExMCP.Server

    deftool "icon_tool" do
      meta do
        description("Tool with icon")
      end

      ExMCP.DSL.Tool.icons([%{type: "emoji", uri: "\u{2699}\u{FE0F}"}])

      input_schema(%{
        type: "object",
        properties: %{x: %{type: "string"}}
      })
    end

    deftool "no_icon_tool" do
      meta do
        description("Tool without icon")
      end

      input_schema(%{
        type: "object",
        properties: %{y: %{type: "string"}}
      })
    end

    defresource "res://with-icon" do
      meta do
        name("With Icon")
        description("Resource with icon")
      end

      ExMCP.DSL.Resource.icons([%{type: "emoji", uri: "\u{1F4C1}"}])
    end

    defresource "res://without-icon" do
      meta do
        name("Without Icon")
        description("Resource without icon")
      end
    end

    defprompt "icon_prompt" do
      meta do
        name("Icon Prompt")
        description("Prompt with icon")
      end

      ExMCP.DSL.Prompt.icons([%{type: "emoji", uri: "\u{1F4AC}"}])
    end

    defprompt "no_icon_prompt" do
      meta do
        name("No Icon Prompt")
        description("Prompt without icon")
      end
    end

    @impl true
    def handle_tool_call(_name, _params, state) do
      {:ok, %{content: [text("ok")]}, state}
    end

    @impl true
    def handle_resource_read(_uri, _full_uri, state) do
      {:ok, [text("data")], state}
    end

    @impl true
    def handle_prompt_get(_name, _args, state) do
      {:ok, %{messages: [user("msg")]}, state}
    end
  end

  # --- Tests ---

  describe "tool icons compliance" do
    test "tool defined with icons macro has icons in metadata" do
      tools = ToolWithIcons.get_tools()
      tool = tools["wrench_tool"]

      assert Map.has_key?(tool, :icons)
      assert is_list(tool.icons)
      assert length(tool.icons) == 1

      [icon] = tool.icons
      assert icon.type == "emoji"
      assert icon.uri == "\u{1F527}"
    end

    test "tool with multiple icon types has all icons in metadata" do
      tools = ToolWithMultipleIcons.get_tools()
      tool = tools["multi_icon_tool"]

      assert Map.has_key?(tool, :icons)
      assert is_list(tool.icons)
      assert length(tool.icons) == 2

      emoji_icon = Enum.find(tool.icons, &(&1.type == "emoji"))
      assert emoji_icon != nil
      assert emoji_icon.uri == "\u{1F4E6}"

      svg_icon = Enum.find(tool.icons, &(&1.type == "icon"))
      assert svg_icon != nil
      assert svg_icon.uri == "https://example.com/tool-icon.svg"
      assert svg_icon.mediaType == "image/svg+xml"
    end

    test "tool without icons macro does not have icons key" do
      tools = ToolWithoutIcons.get_tools()
      tool = tools["plain_tool"]

      refute Map.has_key?(tool, :icons)
    end

    test "tool with icons still has all other required fields" do
      tools = ToolWithIcons.get_tools()
      tool = tools["wrench_tool"]

      assert tool.name == "wrench_tool"
      assert tool.description == "A tool with an emoji icon"
      assert is_map(tool.input_schema)
      assert tool.input_schema["type"] == "object"
    end
  end

  describe "resource icons compliance" do
    test "resource defined with icons macro has icons in metadata" do
      resources = ResourceWithIcons.get_resources()
      resource = resources["config://app/database"]

      assert Map.has_key?(resource, :icons)
      assert is_list(resource.icons)
      assert length(resource.icons) == 1

      [icon] = resource.icons
      assert icon.type == "emoji"
      assert icon.uri == "\u{1F5C4}\u{FE0F}"
    end

    test "resource with SVG icon has correct icon metadata" do
      resources = ResourceWithSvgIcon.get_resources()
      resource = resources["file:///data/report.csv"]

      assert Map.has_key?(resource, :icons)
      assert is_list(resource.icons)
      assert length(resource.icons) == 1

      [icon] = resource.icons
      assert icon.type == "icon"
      assert icon.uri == "https://example.com/csv-icon.svg"
      assert icon.mediaType == "image/svg+xml"
    end

    test "resource without icons macro does not have icons key" do
      resources = ResourceWithoutIcons.get_resources()
      resource = resources["config://app/settings"]

      refute Map.has_key?(resource, :icons)
    end

    test "resource with icons still has all other required fields" do
      resources = ResourceWithIcons.get_resources()
      resource = resources["config://app/database"]

      assert resource.name == "Database Config"
      assert resource.description == "Database configuration resource"
      assert resource.uri == "config://app/database"
      assert resource.mime_type == "application/json"
    end
  end

  describe "prompt icons compliance" do
    test "prompt defined with icons macro has icons in metadata" do
      prompts = PromptWithIcons.get_prompts()
      prompt = prompts["code_review"]

      assert Map.has_key?(prompt, :icons)
      assert is_list(prompt.icons)
      assert length(prompt.icons) == 1

      [icon] = prompt.icons
      assert icon.type == "emoji"
      assert icon.uri == "\u{1F50D}"
    end

    test "prompt with multiple icon types has all icons in metadata" do
      prompts = PromptWithMultipleIcons.get_prompts()
      prompt = prompts["summarize"]

      assert Map.has_key?(prompt, :icons)
      assert is_list(prompt.icons)
      assert length(prompt.icons) == 2

      emoji_icon = Enum.find(prompt.icons, &(&1.type == "emoji"))
      assert emoji_icon != nil
      assert emoji_icon.uri == "\u{1F4DD}"

      png_icon = Enum.find(prompt.icons, &(&1.type == "icon"))
      assert png_icon != nil
      assert png_icon.uri == "https://example.com/summarize.png"
      assert png_icon.mediaType == "image/png"
    end

    test "prompt without icons macro does not have icons key" do
      prompts = PromptWithoutIcons.get_prompts()
      prompt = prompts["greeting"]

      refute Map.has_key?(prompt, :icons)
    end

    test "prompt with icons still has all other required fields" do
      prompts = PromptWithIcons.get_prompts()
      prompt = prompts["code_review"]

      assert prompt.name == "code_review"
      assert prompt.display_name == "Code Review"
      assert prompt.description == "Review code for best practices"
      assert is_list(prompt.arguments)
      assert length(prompt.arguments) == 1
    end
  end

  describe "mixed icons in a single server" do
    test "tools with and without icons coexist correctly" do
      tools = ServerWithMixedIcons.get_tools()

      icon_tool = tools["icon_tool"]
      assert Map.has_key?(icon_tool, :icons)
      assert length(icon_tool.icons) == 1
      assert hd(icon_tool.icons).type == "emoji"

      no_icon_tool = tools["no_icon_tool"]
      refute Map.has_key?(no_icon_tool, :icons)
    end

    test "resources with and without icons coexist correctly" do
      resources = ServerWithMixedIcons.get_resources()

      icon_resource = resources["res://with-icon"]
      assert Map.has_key?(icon_resource, :icons)
      assert length(icon_resource.icons) == 1
      assert hd(icon_resource.icons).uri == "\u{1F4C1}"

      no_icon_resource = resources["res://without-icon"]
      refute Map.has_key?(no_icon_resource, :icons)
    end

    test "prompts with and without icons coexist correctly" do
      prompts = ServerWithMixedIcons.get_prompts()

      icon_prompt = prompts["icon_prompt"]
      assert Map.has_key?(icon_prompt, :icons)
      assert length(icon_prompt.icons) == 1
      assert hd(icon_prompt.icons).uri == "\u{1F4AC}"

      no_icon_prompt = prompts["no_icon_prompt"]
      refute Map.has_key?(no_icon_prompt, :icons)
    end
  end

  describe "icon structure validation" do
    test "emoji icon has required type and uri fields" do
      tools = ToolWithIcons.get_tools()
      [icon] = tools["wrench_tool"].icons

      assert Map.has_key?(icon, :type)
      assert Map.has_key?(icon, :uri)
      assert icon.type == "emoji"
    end

    test "URI-based icon has type, uri, and mediaType fields" do
      tools = ToolWithMultipleIcons.get_tools()
      svg_icon = Enum.find(tools["multi_icon_tool"].icons, &(&1.type == "icon"))

      assert Map.has_key?(svg_icon, :type)
      assert Map.has_key?(svg_icon, :uri)
      assert Map.has_key?(svg_icon, :mediaType)
      assert svg_icon.type == "icon"
      assert String.starts_with?(svg_icon.uri, "https://")
      assert svg_icon.mediaType == "image/svg+xml"
    end

    test "icons list is always a list when present" do
      tools = ToolWithIcons.get_tools()
      assert is_list(tools["wrench_tool"].icons)

      resources = ResourceWithIcons.get_resources()
      assert is_list(resources["config://app/database"].icons)

      prompts = PromptWithIcons.get_prompts()
      assert is_list(prompts["code_review"].icons)
    end
  end
end
