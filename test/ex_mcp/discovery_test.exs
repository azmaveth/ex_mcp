defmodule ExMCP.DiscoveryTest do
  use ExUnit.Case
  doctest ExMCP.Internal.Discovery

  describe "discover_servers/1" do
    test "discovers servers using default methods" do
      servers = ExMCP.Internal.Discovery.discover_servers()
      assert is_list(servers)

      # Each server should have required fields
      Enum.each(servers, fn server ->
        assert Map.has_key?(server, :name)
        assert Map.has_key?(server, :transport)
      end)
    end

    test "discovers servers using specific methods" do
      servers = ExMCP.Internal.Discovery.discover_servers(methods: [:env])
      assert is_list(servers)
    end
  end

  describe "discover_from_env/1" do
    setup do
      # Save original env
      original_env = System.get_env("MCP_SERVERS")

      on_exit(fn ->
        if original_env do
          System.put_env("MCP_SERVERS", original_env)
        else
          System.delete_env("MCP_SERVERS")
        end
      end)
    end

    test "discovers servers from MCP_SERVERS JSON" do
      json = ~s([{"name": "test-server", "command": ["node", "server.js"]}])
      System.put_env("MCP_SERVERS", json)

      servers = ExMCP.Discovery.discover_from_env()
      assert length(servers) >= 1
      assert Enum.any?(servers, fn s -> s.name == "test-server" end)
    end

    test "discovers servers from pattern-based env vars" do
      System.put_env("MYAPP_MCP_SERVER", "node myapp.js --port 3000")
      System.put_env("API_SERVER_URL", "http://localhost:8080")

      servers = ExMCP.Discovery.discover_from_env()

      assert Enum.any?(servers, fn s -> s["name"] == "myapp-env" end)
      assert Enum.any?(servers, fn s -> s["name"] == "api-env" end)

      # Cleanup
      System.delete_env("MYAPP_MCP_SERVER")
      System.delete_env("API_SERVER_URL")
    end
  end

  describe "discover_npm_packages/0" do
    @tag :skip
    test "discovers npm MCP packages" do
      # This test requires npm to be installed
      servers = ExMCP.Discovery.discover_npm_packages()
      assert is_list(servers)
    end
  end

  describe "test_server/1" do
    test "validates stdio server config" do
      # Test with a command that should exist on most systems
      result = ExMCP.Discovery.test_server(%{command: ["echo"]})
      assert is_boolean(result)
    end

    test "validates SSE server URL" do
      result = ExMCP.Discovery.test_server(%{url: "http://localhost:8080"})
      # Valid URL format
      assert result == true

      result = ExMCP.Discovery.test_server(%{url: "invalid-url"})
      assert result == false
    end
  end
end
