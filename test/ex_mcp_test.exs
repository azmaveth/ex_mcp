defmodule ExMCPTest do
  # Can't be async since we're using real servers
  use ExUnit.Case, async: false

  alias ExMCP.Response
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

  describe "v2 convenience functions with real servers" do
    setup context do
      start_test_servers_for_api(context)
    end

    test "connect/2 with HTTP URL", %{http_url: http_url} do
      assert {:ok, client} = ExMCP.connect(http_url, client_type: :simple, use_sse: false)
      assert is_pid(client)
      ExMCP.disconnect(client)
    end

    test "connect/2 with HTTP URL and v2 client", %{http_url: http_url} do
      assert {:ok, client} = ExMCP.connect(http_url, client_type: :v2, use_sse: false)
      assert is_pid(client)
      ExMCP.disconnect(client)
    end

    test "tools/2 returns actual tool list", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple, use_sse: false)

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
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple, use_sse: false)

      result = ExMCP.call(client, "echo", %{"message" => "Hello World"})
      assert result == "Echo: Hello World"

      ExMCP.disconnect(client)
    end

    test "call/4 with add tool", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple, use_sse: false)

      result = ExMCP.call(client, "add", %{"a" => 5, "b" => 3})
      assert result == "5 + 3 = 8"

      ExMCP.disconnect(client)
    end

    test "call/4 with normalize: false returns raw response", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple, use_sse: false)

      result = ExMCP.call(client, "echo", %{"message" => "test"}, normalize: false)
      assert %Response{} = result
      assert Response.text_content(result) == "Echo: test"

      ExMCP.disconnect(client)
    end

    test "status/1 returns connection status", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple, use_sse: false)

      {:ok, status} = ExMCP.status(client)
      assert is_map(status)
      assert Map.has_key?(status, :connection_status)

      ExMCP.disconnect(client)
    end

    test "disconnect/1 stops the client", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple, use_sse: false)

      assert :ok = ExMCP.disconnect(client)
      # Give it a moment to stop
      Process.sleep(100)
      refute Process.alive?(client)
    end
  end

  describe "connection specification normalization" do
    setup context do
      start_test_servers_for_api(context)
    end

    test "handles HTTP URLs", %{http_url: http_url} do
      assert {:ok, client1} = ExMCP.connect(http_url, client_type: :simple, use_sse: false)
      ExMCP.disconnect(client1)

      # Test with explicit http:// URL as well
      assert {:ok, client2} = ExMCP.connect(http_url, client_type: :simple, use_sse: false)
      ExMCP.disconnect(client2)
    end

    test "handles transport tuples", %{http_url: http_url} do
      assert {:ok, client} =
               ExMCP.connect({:http, url: http_url}, client_type: :simple, use_sse: false)

      ExMCP.disconnect(client)
    end

    test "client_type option selects appropriate client", %{http_url: http_url} do
      assert {:ok, client1} = ExMCP.connect(http_url, client_type: :simple, use_sse: false)
      assert {:ok, client2} = ExMCP.connect(http_url, client_type: :v2, use_sse: false)
      # Note: convenience client with fallback may not work with our simple test setup

      ExMCP.disconnect(client1)
      ExMCP.disconnect(client2)
    end
  end

  describe "error handling" do
    setup context do
      start_test_servers_for_api(context)
    end

    @tag timeout: 10_000
    test "handles connection errors gracefully" do
      # Try to connect to a non-existent server
      # Note: We need to use a short request_timeout to avoid test timeout
      # Also trap exits to handle the client process dying
      Process.flag(:trap_exit, true)

      result =
        ExMCP.connect("http://localhost:99999",
          client_type: :simple,
          timeout: 1_000,
          request_timeout: 1_000,
          use_sse: false
        )

      # Handle potential EXIT messages
      receive do
        {:EXIT, _pid, _reason} -> :ok
      after
        100 -> :ok
      end

      assert {:error, _reason} = result
    end

    test "handles invalid tool calls gracefully", %{http_url: http_url} do
      {:ok, client} = ExMCP.connect(http_url, client_type: :simple, use_sse: false)

      # Try to call a non-existent tool
      result = ExMCP.call(client, "nonexistent_tool", %{})
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
          meta do
            description "Test tool"
          end
          input_schema %{type: "object"}
        end

        @impl true
        def handle_tool_call("test", _args, state) do
          {:ok, %{content: []}, state}
        end

        @impl true
        def handle_resource_read(_uri, _original_uri, state) do
          {:error, "Not implemented", state}
        end

        @impl true
        def handle_prompt_get(_name, _args, state) do
          {:error, "Not implemented", state}
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
