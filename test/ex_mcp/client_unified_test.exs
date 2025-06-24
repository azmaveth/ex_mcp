defmodule ExMCP.ClientUnifiedTest do
  use ExUnit.Case, async: false

  alias ExMCP.ClientUnified, as: Client

  describe "connection" do
    test "connects with URL string" do
      server = start_test_server()

      assert {:ok, client} = Client.connect("http://localhost:#{server.port}/mcp")
      assert {:ok, status} = Client.get_status(client)
      assert status.connection_status == :connected
    end

    test "connects with transport spec" do
      server = start_test_server()

      assert {:ok, client} = Client.connect({:http, url: "http://localhost:#{server.port}/mcp"})
      assert {:ok, status} = Client.get_status(client)
      assert status.connection_status == :connected
    end

    test "connects with multiple transports for fallback" do
      server = start_test_server()

      # First URL is invalid, second should work
      assert {:ok, client} =
               Client.connect([
                 "http://localhost:99999/invalid",
                 "http://localhost:#{server.port}/mcp"
               ])

      assert {:ok, status} = Client.get_status(client)
      assert status.connection_status == :connected
    end
  end

  describe "tools API" do
    setup do
      server = start_test_server()
      {:ok, client} = Client.connect("http://localhost:#{server.port}/mcp")
      {:ok, client: client}
    end

    test "list_tools returns tools", %{client: client} do
      assert {:ok, %{tools: tools}} = Client.list_tools(client)
      assert is_list(tools)
      assert length(tools) > 0
    end

    test "tools alias works", %{client: client} do
      assert {:ok, %{tools: tools}} = Client.tools(client)
      assert is_list(tools)
    end

    test "call_tool executes tool", %{client: client} do
      assert {:ok, result} = Client.call_tool(client, "echo", %{"message" => "hello"})
      assert result["output"] == "hello"
    end

    test "call alias works", %{client: client} do
      assert {:ok, result} = Client.call(client, "echo", %{"message" => "hello"})
      assert result["output"] == "hello"
    end

    test "find_tool locates tool by name", %{client: client} do
      assert {:ok, tool} = Client.find_tool(client, "echo")
      assert tool["name"] == "echo"
    end

    test "find_tool with fuzzy matching", %{client: client} do
      assert {:ok, tool} = Client.find_tool(client, "ech", fuzzy: true)
      assert tool["name"] == "echo"
    end
  end

  describe "backward compatibility" do
    setup do
      server = start_test_server()
      {:ok, server: server}
    end

    test "supports old Client.start_link syntax", %{server: server} do
      assert {:ok, client} =
               Client.start_link(
                 transport: :http,
                 url: "http://localhost:#{server.port}/mcp"
               )

      assert {:ok, %{tools: _tools}} = Client.list_tools(client)
    end

    test "supports timeout as positional argument", %{server: server} do
      {:ok, client} = Client.connect("http://localhost:#{server.port}/mcp")

      # Old syntax with timeout as last argument
      assert {:ok, _result} = Client.call_tool(client, "echo", %{"message" => "hi"}, 5000)
    end

    test "supports struct format option", %{server: server} do
      {:ok, client} = Client.connect("http://localhost:#{server.port}/mcp")

      # Request struct format
      assert {:ok, %ExMCP.Response{}} = Client.list_tools(client, format: :struct)
    end
  end

  describe "error handling" do
    test "returns error for invalid URL" do
      assert {:error, _reason} = Client.connect("http://invalid-host-12345:99999")
    end

    test "returns error for unknown tool" do
      server = start_test_server()
      {:ok, client} = Client.connect("http://localhost:#{server.port}/mcp")

      assert {:error, _reason} = Client.call_tool(client, "unknown_tool", %{})
    end

    test "returns not_found for missing tool" do
      server = start_test_server()
      {:ok, client} = Client.connect("http://localhost:#{server.port}/mcp")

      assert {:error, :not_found} = Client.find_tool(client, "nonexistent")
    end
  end

  # Test helpers

  defp start_test_server do
    port = Enum.random(10000..20000)
    {:ok, server} = TestServer.start_link(port: port)
    TestServer.set_handler(server, &handle_request/1)
    %{pid: server, port: port}
  end

  defp handle_request(%{"method" => "initialize"}) do
    %{
      "protocolVersion" => "0.1.0",
      "serverInfo" => %{
        "name" => "test-server",
        "version" => "1.0.0"
      },
      "capabilities" => %{
        "tools" => %{},
        "resources" => %{},
        "prompts" => %{}
      }
    }
  end

  defp handle_request(%{"method" => "tools/list"}) do
    %{
      "tools" => [
        %{
          "name" => "echo",
          "description" => "Echoes back the input",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "message" => %{"type" => "string"}
            },
            "required" => ["message"]
          }
        },
        %{
          "name" => "calculator",
          "description" => "Performs calculations",
          "inputSchema" => %{
            "type" => "object",
            "properties" => %{
              "expression" => %{"type" => "string"}
            },
            "required" => ["expression"]
          }
        }
      ]
    }
  end

  defp handle_request(%{
         "method" => "tools/call",
         "params" => %{"name" => "echo", "arguments" => args}
       }) do
    %{
      "output" => args["message"] || ""
    }
  end

  defp handle_request(%{"method" => "tools/call", "params" => %{"name" => name}}) do
    {:error,
     %{
       "code" => -32001,
       "message" => "Unknown tool: #{name}"
     }}
  end

  defp handle_request(%{"method" => "ping"}) do
    %{}
  end
end
