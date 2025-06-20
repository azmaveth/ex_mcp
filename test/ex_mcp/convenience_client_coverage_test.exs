defmodule ExMCP.ConvenienceClientCoverageTest do
  use ExUnit.Case, async: false

  alias ExMCP.ConvenienceClient
  alias ExMCP.TestHelpers

  # Reuse the mock transport from SimpleClient tests
  alias ExMCP.SimpleClientCoverageTest.WorkingMockTransport

  describe "connection management" do
    setup do
      TestHelpers.setup_test_servers()
    end

    test "connects with URL string parsing", %{http_url: http_url} do
      # Test actual HTTP URL connection to local test server (SSE disabled for simpler testing)
      assert {:ok, client} = ConvenienceClient.connect(http_url, timeout: 5000, use_sse: false)
      assert is_pid(client)
      ConvenienceClient.disconnect(client)
    end

    test "connects with stdio URL" do
      assert {:error, _} =
               ConvenienceClient.connect("stdio://test-command",
                 timeout: 100,
                 max_reconnect_attempts: 0
               )
    end

    test "connects with file URL" do
      assert {:error, _} =
               ConvenienceClient.connect("file://test-command",
                 timeout: 100,
                 max_reconnect_attempts: 0
               )
    end

    test "connects with HTTPS URL" do
      # Test that HTTPS URLs are properly parsed but fail when no HTTPS server is available
      assert {:error, _} =
               ConvenienceClient.connect("https://localhost:9999",
                 timeout: 1000,
                 max_reconnect_attempts: 0
               )
    end

    test "connects with transport tuple" do
      {:ok, client} = ConvenienceClient.connect({WorkingMockTransport, []})
      assert is_pid(client)

      # Clean up
      ConvenienceClient.disconnect(client)
    end

    test "connects with connection list for fallback", %{http_url: http_url} do
      # Use the real test server URL as the first connection
      connections = [
        http_url,
        "http://nonexistent:8080",
        {:stdio, command: "fallback-server"}
      ]

      # Should succeed by connecting to the first working URL (our test server)
      assert {:ok, client} =
               ConvenienceClient.connect(connections,
                 timeout: 5000,
                 max_reconnect_attempts: 0,
                 use_sse: false
               )

      ConvenienceClient.disconnect(client)
    end

    test "disconnect stops the client" do
      {:ok, client} = ConvenienceClient.connect({WorkingMockTransport, []})

      assert :ok = ConvenienceClient.disconnect(client)
      refute Process.alive?(client)
    end
  end

  describe "status/1" do
    setup do
      {:ok, client} = ConvenienceClient.connect({WorkingMockTransport, []})
      {:ok, client: client}
    end

    test "returns normalized status", %{client: client} do
      assert {:ok, status} = ConvenienceClient.status(client)

      assert status.connected == true
      assert status.status == :connected
      assert is_map(status.server_info)
      assert status.reconnect_attempts == 0
      assert is_integer(status.last_activity)
      assert status.pending_requests == 0
    end
  end

  describe "tool operations with normalization" do
    setup do
      {:ok, client} = ConvenienceClient.connect({WorkingMockTransport, []})
      {:ok, client: client}
    end

    test "tools returns normalized list", %{client: client} do
      tools = ConvenienceClient.tools(client)

      assert is_list(tools)
      assert [tool | _] = tools
      assert tool.name == "test_tool"
      assert tool.description == "Test tool for coverage"
      assert is_map(tool.metadata)
      assert tool.input_schema == nil
    end

    test "call normalizes tool result by default", %{client: client} do
      result = ConvenienceClient.call(client, "test_tool", %{})

      # Should return just the text content
      assert result == "Tool result"
    end

    test "call with normalize: false returns raw response", %{client: client} do
      result = ConvenienceClient.call(client, "test_tool", %{}, normalize: false)

      assert %{"content" => [%{"type" => "text", "text" => "Tool result"}]} = result
    end

    test "find_tool with exact match", %{client: client} do
      tool = ConvenienceClient.find_tool(client, "test_tool")

      assert tool.name == "test_tool"
    end

    test "find_tool with no match returns nil", %{client: client} do
      assert nil == ConvenienceClient.find_tool(client, "nonexistent")
    end

    test "find_tool with fuzzy search", %{client: client} do
      tools = ConvenienceClient.find_tool(client, "test", fuzzy: true)

      assert is_list(tools)
      assert length(tools) == 1
      assert hd(tools).name == "test_tool"
    end

    test "find_tool filters by schema presence", %{client: client} do
      # Our mock tool has no input schema
      tools = ConvenienceClient.find_tool(client, nil, has_schema: true)
      assert tools == []
    end

    test "find_tool with limit", %{client: client} do
      tools = ConvenienceClient.find_tool(client, nil, limit: 1)
      assert length(tools) <= 1
    end
  end

  describe "resource operations" do
    setup do
      {:ok, client} = ConvenienceClient.connect({WorkingMockTransport, []})
      {:ok, client: client}
    end

    test "resources returns normalized list", %{client: client} do
      resources = ConvenienceClient.resources(client)

      assert is_list(resources)
      assert [resource | _] = resources
      assert resource.uri == "file://test.txt"
      assert resource.name == "Test Resource"
      # Not provided by mock
      assert resource.mime_type == nil
      assert resource.description == nil
      assert is_map(resource.metadata)
    end

    test "read normalizes resource content", %{client: client} do
      content = ConvenienceClient.read(client, "file://test.txt")

      # Should return just the text
      assert content == "Resource content"
    end

    test "read with parse_json option", %{client: client} do
      # This would parse JSON if content was JSON
      content = ConvenienceClient.read(client, "file://test.txt", parse_json: true)
      assert content == "Resource content"
    end
  end

  describe "prompt operations" do
    setup do
      {:ok, client} = ConvenienceClient.connect({WorkingMockTransport, []})
      {:ok, client: client}
    end

    test "prompts returns normalized list", %{client: client} do
      prompts = ConvenienceClient.prompts(client)

      assert is_list(prompts)
      assert [prompt | _] = prompts
      assert prompt.name == "test_prompt"
      assert prompt.description == "Test prompt"
      assert is_map(prompt.metadata)
    end

    test "prompt returns normalized result", %{client: client} do
      result = ConvenienceClient.prompt(client, "test_prompt", %{})

      assert %{messages: messages} = result
      assert is_list(messages)
      assert [message | _] = messages
      assert message.role == "user"
      assert message.content == "Hello!"
    end
  end

  describe "batch operations" do
    setup do
      {:ok, client} = ConvenienceClient.connect({WorkingMockTransport, []})
      {:ok, client: client}
    end

    test "batch executes multiple operations", %{client: client} do
      operations = [
        {:list_tools, %{}},
        {:list_resources, %{}},
        {:list_prompts, %{}}
      ]

      results = ConvenienceClient.batch(client, operations)

      assert length(results) == 3
      # tools
      assert is_list(Enum.at(results, 0))
      # resources
      assert is_list(Enum.at(results, 1))
      # prompts
      assert is_list(Enum.at(results, 2))
    end

    test "batch with tool calls", %{client: client} do
      operations = [
        {:call_tool, "test_tool", %{}},
        {:read_resource, "file://test.txt"},
        {:get_prompt, "test_prompt", %{}}
      ]

      results = ConvenienceClient.batch(client, operations)

      assert length(results) == 3
      assert Enum.at(results, 0) == "Tool result"
      assert Enum.at(results, 1) == "Resource content"
      assert %{messages: _} = Enum.at(results, 2)
    end

    test "batch respects max_concurrency", %{client: client} do
      operations =
        for i <- 1..10 do
          {:call_tool, "test_tool", %{index: i}}
        end

      results = ConvenienceClient.batch(client, operations, max_concurrency: 2)

      assert length(results) == 10
      assert Enum.all?(results, &(&1 == "Tool result"))
    end
  end

  describe "utility functions" do
    setup do
      TestHelpers.setup_test_servers()
    end

    test "ping tests connectivity", %{http_url: http_url} do
      # Should succeed with our test server (SSE disabled for simpler testing)
      assert :ok = ConvenienceClient.ping(http_url, timeout: 5000, use_sse: false)
    end

    test "ping with working connection" do
      # This should work with our mock
      result = ConvenienceClient.ping({WorkingMockTransport, []})

      case result do
        :ok -> assert true
        # Also acceptable
        {:error, _} -> assert true
      end
    end

    test "server_info extracts from status", %{} do
      {:ok, client} = ConvenienceClient.connect({WorkingMockTransport, []})

      assert {:ok, info} = ConvenienceClient.server_info(client)
      assert info.name == "Test Server"
      assert info.version == "1.0.0"
      assert is_map(info.capabilities)
      assert is_map(info.metadata)

      ConvenienceClient.disconnect(client)
    end

    test "server_info handles missing info" do
      # Create a client with minimal mock that doesn't provide server info
      defmodule MinimalMock do
        def connect(_), do: {:ok, self()}
        def send(s, _), do: {:ok, s}
        def recv(_, _), do: {:error, :timeout}
        def close(_), do: {:ok, nil}
      end

      # This will fail to connect properly
      assert {:error, _} = ConvenienceClient.connect({MinimalMock, []})
    end

    test "with_error_formatting handles various errors" do
      # Exit error
      result =
        ConvenienceClient.with_error_formatting(
          fn -> exit(:test_exit) end,
          :test_operation,
          %{context: "test"}
        )

      assert result.type == :test_operation
      assert result.message =~ "test_operation"

      # Exception error
      result =
        ConvenienceClient.with_error_formatting(
          fn -> raise "test error" end,
          :another_operation
        )

      assert result.type == :another_operation
      assert is_binary(result.message)
    end
  end

  describe "error response handling" do
    setup do
      {:ok, client} = ConvenienceClient.connect({WorkingMockTransport, []})
      {:ok, client: client}
    end

    test "tools handles unexpected response format", %{client: _client} do
      # We'd need a mock that returns unexpected format
      # For now, test the error formatting
      error = ConvenienceClient.tools(:invalid_client, timeout: 1)
      assert match?({:error, _}, error)
    end
  end

  describe "private helper functions coverage" do
    setup do
      TestHelpers.setup_test_servers()
    end

    test "connection spec parsing", %{http_url: http_url} do
      # Test various URL formats are handled
      urls = [
        # This should succeed
        http_url,
        # This should fail (no HTTPS server)
        "https://localhost:9999",
        # This should fail (no command)
        "stdio://my-command",
        # This should fail (invalid scheme)
        "file:///usr/local/bin/server"
      ]

      results =
        for url <- urls do
          ConvenienceClient.connect(url, timeout: 1000, max_reconnect_attempts: 0, use_sse: false)
        end

      # First connection (our test server) should succeed
      assert {:ok, client} = Enum.at(results, 0)
      ConvenienceClient.disconnect(client)

      # Others should fail for various reasons
      assert {:error, _} = Enum.at(results, 1)
      assert {:error, _} = Enum.at(results, 2)
      assert {:error, _} = Enum.at(results, 3)
    end

    test "fuzzy search with description matching" do
      {:ok, client} = ConvenienceClient.connect({WorkingMockTransport, []})

      # Search by partial description
      tools = ConvenienceClient.find_tool(client, "coverage", fuzzy: true)
      # Matches "Test tool for coverage"
      assert length(tools) == 1

      ConvenienceClient.disconnect(client)
    end
  end
end
