defmodule ExMCP.ClientUnifiedAPITest do
  use ExUnit.Case, async: true

  alias ExMCP.Client

  describe "URL parsing" do
    test "parses HTTP URLs" do
      opts = Client.parse_connection_spec("http://localhost:8080/mcp")
      assert opts[:transport] == :http
      assert opts[:url] == "http://localhost:8080/mcp"
    end

    test "parses HTTPS URLs" do
      opts = Client.parse_connection_spec("https://example.com/mcp")
      assert opts[:transport] == :http
      assert opts[:url] == "https://example.com/mcp"
    end

    test "parses stdio URLs" do
      opts = Client.parse_connection_spec("stdio://my-server")
      assert opts[:transport] == :stdio
      assert opts[:command] == "my-server"
    end

    test "parses file URLs as stdio" do
      opts = Client.parse_connection_spec("file:///usr/bin/mcp-server")
      assert opts[:transport] == :stdio
      assert opts[:command] == "/usr/bin/mcp-server"
    end

    test "handles transport specs" do
      opts = Client.parse_connection_spec({:http, url: "http://localhost:8080"})
      assert is_list(opts)
      assert opts[:transport] == :http
    end

    test "handles multiple transports" do
      opts =
        Client.parse_connection_spec([
          "http://primary.com/mcp",
          "stdio://fallback-server"
        ])

      assert opts[:transports]
      assert length(opts[:transports]) == 2

      [{mod1, opts1}, {mod2, opts2}] = opts[:transports]
      assert mod1 == ExMCP.Transport.HTTP
      assert opts1[:url] == "http://primary.com/mcp"
      assert mod2 == ExMCP.Transport.Stdio
      assert opts2[:command] == "fallback-server"
    end
  end

  describe "API compatibility" do
    test "exports all expected functions" do
      # Connection functions
      assert function_exported?(Client, :start_link, 1)
      assert function_exported?(Client, :connect, 1)
      assert function_exported?(Client, :connect, 2)
      assert function_exported?(Client, :stop, 1)
      assert function_exported?(Client, :stop, 2)

      # Tool functions
      assert function_exported?(Client, :list_tools, 1)
      assert function_exported?(Client, :list_tools, 2)
      assert function_exported?(Client, :tools, 1)
      assert function_exported?(Client, :tools, 2)
      assert function_exported?(Client, :call_tool, 3)
      assert function_exported?(Client, :call_tool, 4)
      assert function_exported?(Client, :call, 2)
      assert function_exported?(Client, :call, 3)
      assert function_exported?(Client, :call, 4)
      assert function_exported?(Client, :find_tool, 2)
      assert function_exported?(Client, :find_tool, 3)

      # Resource functions
      assert function_exported?(Client, :list_resources, 1)
      assert function_exported?(Client, :list_resources, 2)
      assert function_exported?(Client, :read_resource, 2)
      assert function_exported?(Client, :read_resource, 3)

      # Prompt functions
      assert function_exported?(Client, :list_prompts, 1)
      assert function_exported?(Client, :list_prompts, 2)
      assert function_exported?(Client, :get_prompt, 2)
      assert function_exported?(Client, :get_prompt, 3)
      assert function_exported?(Client, :get_prompt, 4)

      # Status functions
      assert function_exported?(Client, :get_status, 1)
      assert function_exported?(Client, :server_info, 1)
      assert function_exported?(Client, :ping, 1)
      assert function_exported?(Client, :ping, 2)
    end
  end

  describe "transport configuration" do
    test "prepares single transport config" do
      result = Client.prepare_transport_config(transport: :http, url: "http://example.com")
      assert {:single, ExMCP.Transport.HTTP, opts} = result
      assert opts[:url] == "http://example.com"
    end

    test "prepares multiple transport config" do
      result =
        Client.prepare_transport_config(
          transports: [
            {ExMCP.Transport.HTTP, url: "http://example.com"},
            {ExMCP.Transport.Stdio, command: "server"}
          ]
        )

      assert {:multiple, opts} = result
      assert opts[:transports]
    end

    test "defaults to stdio transport" do
      result = Client.prepare_transport_config([])
      assert {:single, ExMCP.Transport.Stdio, _opts} = result
    end
  end

  describe "helper functions" do
    test "find_matching_tool exact match" do
      tools = [
        %{"name" => "tool1"},
        %{"name" => "tool2"},
        %{"name" => "tool3"}
      ]

      assert {:ok, %{"name" => "tool2"}} = Client.find_matching_tool(tools, "tool2", [])
      assert {:error, :not_found} = Client.find_matching_tool(tools, "tool4", [])
    end

    test "find_matching_tool fuzzy match" do
      tools = [
        %{"name" => "calculator"},
        %{"name" => "weather_tool"},
        %{"name" => "translator"}
      ]

      assert {:ok, %{"name" => "calculator"}} =
               Client.find_matching_tool(tools, "calc", fuzzy: true)

      assert {:ok, %{"name" => "weather_tool"}} =
               Client.find_matching_tool(tools, "weather", fuzzy: true)
    end

    test "find_matching_tool returns first when name is nil" do
      tools = [%{"name" => "first"}, %{"name" => "second"}]
      assert {:ok, %{"name" => "first"}} = Client.find_matching_tool(tools, nil, [])
    end
  end
end
