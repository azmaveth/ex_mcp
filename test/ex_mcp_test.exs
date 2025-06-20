defmodule ExMCPTest do
  # Can't be async since we're using real servers
  use ExUnit.Case, async: false

  alias ExMCP.{Response, Error}
  import ExMCP.TestHelpers

  describe "version and metadata functions" do
    test "protocol_version/0 returns current protocol version" do
      assert ExMCP.protocol_version() == "2025-03-26"
    end

    test "version/0 returns library version" do
      version = ExMCP.version()
      assert is_binary(version)
      assert String.match?(version, ~r/\d+\.\d+\.\d+/)
    end

    test "supported_versions/0 returns list of supported versions" do
      versions = ExMCP.supported_versions()
      assert is_list(versions)
      assert "2025-06-18" in versions
      assert "2025-03-26" in versions
    end

    test "info/0 returns library information" do
      info = ExMCP.info()
      assert %{version: _, protocol_versions: _, transports: _, features: _} = info
      assert is_list(info.transports)
      assert :http in info.transports
      assert :stdio in info.transports
      assert is_list(info.features)
      assert :structured_responses in info.features
    end
  end

  describe "v1 compatibility functions" do
    test "start_client/1 delegates to ExMCP.Client" do
      assert function_exported?(ExMCP, :start_client, 1)
    end

    test "start_server/1 delegates to ExMCP.Server" do
      assert function_exported?(ExMCP, :start_server, 1)
    end
  end

  describe "v2 convenience functions with real servers" do
    setup do
      setup_test_servers()
    end

    test "connect/2 with HTTP URL", %{http_url: http_url} do
      assert {:ok, client} = ExMCP.connect(http_url, client_type: :simple)
      assert is_pid(client)
      ExMCP.disconnect(client)
    end

    test "connect/2 with HTTP URL and v2 client", %{http_url: http_url} do
      assert {:ok, client} = ExMCP.connect(http_url, client_type: :v2)
      assert is_pid(client)
      ExMCP.disconnect(client)
    end

    test "tools/2 returns actual tool list", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple)

      tools = ExMCP.tools(client)
      assert is_list(tools)

      # Check for our test tools
      tool_names = Enum.map(tools, & &1["name"])
      assert "echo" in tool_names
      assert "add" in tool_names
      assert "greet" in tool_names

      ExMCP.disconnect(client)
    end

    test "call/4 executes tool and normalizes response", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple)

      result = ExMCP.call(client, "echo", %{"message" => "Hello World"})
      assert result == "Echo: Hello World"

      ExMCP.disconnect(client)
    end

    test "call/4 with add tool", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple)

      result = ExMCP.call(client, "add", %{"a" => 5, "b" => 3})
      assert result == "5 + 3 = 8"

      ExMCP.disconnect(client)
    end

    test "call/4 with normalize: false returns raw response", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple)

      result = ExMCP.call(client, "echo", %{"message" => "test"}, normalize: false)
      assert %Response{} = result
      assert Response.text_content(result) == "Echo: test"

      ExMCP.disconnect(client)
    end

    test "resources/2 returns actual resource list", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple)

      resources = ExMCP.resources(client)
      assert is_list(resources)

      # Check for our test resources
      resource_uris = Enum.map(resources, & &1["uri"])
      assert "test://config" in resource_uris
      assert "test://data.txt" in resource_uris

      ExMCP.disconnect(client)
    end

    test "read/3 reads resource content", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple)

      content = ExMCP.read(client, "test://data.txt")
      assert is_binary(content)
      assert String.contains?(content, "test data")

      ExMCP.disconnect(client)
    end

    test "read/3 with parse_json: true parses JSON content", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple)

      # Read the JSON config resource
      content = ExMCP.read(client, "test://config", parse_json: true)
      assert is_map(content)
      assert content["environment"] == "test"
      assert content["debug"] == true

      ExMCP.disconnect(client)
    end

    test "status/1 returns connection status", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple)

      {:ok, status} = ExMCP.status(client)
      assert is_map(status)
      assert Map.has_key?(status, :connection_status)

      ExMCP.disconnect(client)
    end

    test "disconnect/1 stops the client", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple)

      assert :ok = ExMCP.disconnect(client)
      # Give it a moment to stop
      Process.sleep(100)
      refute Process.alive?(client)
    end
  end

  describe "connection specification normalization" do
    setup do
      setup_test_servers()
    end

    test "handles HTTP URLs", %{http_url: http_url} do
      assert {:ok, client1} = ExMCP.connect(http_url, client_type: :simple)

      assert {:ok, client2} =
               ExMCP.connect(String.replace(http_url, "http://", "https://"),
                 client_type: :simple
               )

      ExMCP.disconnect(client1)
      ExMCP.disconnect(client2)
    end

    test "handles transport tuples", %{http_url: http_url} do
      assert {:ok, client} = ExMCP.connect({:http, url: http_url}, client_type: :simple)
      ExMCP.disconnect(client)
    end

    test "client_type option selects appropriate client", %{http_url: http_url} do
      assert {:ok, client1} = ExMCP.connect(http_url, client_type: :simple)
      assert {:ok, client2} = ExMCP.connect(http_url, client_type: :v2)
      # Note: convenience client with fallback may not work with our simple test setup

      ExMCP.disconnect(client1)
      ExMCP.disconnect(client2)
    end
  end

  describe "error handling" do
    test "handles connection errors gracefully" do
      # Try to connect to a non-existent server
      result = ExMCP.connect("http://localhost:99999", client_type: :simple)
      assert {:error, _reason} = result
    end

    test "handles invalid tool calls gracefully" do
      {:ok, %{http_url: http_url}} = setup_test_servers()
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple)

      # Try to call a non-existent tool
      result = ExMCP.call(client, "nonexistent_tool", %{})
      assert {:error, _error} = result

      ExMCP.disconnect(client)
    end

    test "handles invalid resource reads gracefully" do
      {:ok, %{http_url: http_url}} = setup_test_servers()
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple)

      # Try to read a non-existent resource
      result = ExMCP.read(client, "test://nonexistent")
      assert {:error, _error} = result

      ExMCP.disconnect(client)
    end
  end

  describe "use ExMCP.Server macro" do
    test "allows using ExMCP.Server as alias for ExMCP.Server" do
      # Create a temporary module to test the macro
      code = """
      defmodule TestServerAlias do
        use ExMCP.Server
        
        deftool "test" do
          description "Test tool"
          input_schema %{type: "object"}
        end
        
        @impl true
        def handle_tool_call("test", _args, state) do
          {:ok, %{content: []}, state}
        end
      end
      """

      # Compile and check it works
      [{module, _}] = Code.compile_string(code)

      # Verify the module has the expected behavior
      assert function_exported?(module, :get_tools, 0)
      tools = module.get_tools()
      assert Map.has_key?(tools, "test")
    end
  end
end
