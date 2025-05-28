defmodule ExMCP.VersionNegotiationTest do
  use ExUnit.Case, async: true

  alias ExMCP.{Client, Protocol, Server}

  defmodule TestVersionHandler do
    use ExMCP.Server.Handler

    @supported_versions ["2025-03-26", "2024-11-05"]

    @impl true
    def handle_initialize(params, state) do
      client_version = params["protocolVersion"]
      client_capabilities = params["capabilities"] || %{}
      client_info = params["clientInfo"] || %{}

      # Store client info for verification
      new_state =
        Map.put(state, :client_info, %{
          version: client_version,
          capabilities: client_capabilities,
          info: client_info
        })

      # Negotiate version
      negotiated_version =
        case client_version do
          v when v in @supported_versions -> v
          # Propose latest for unknown versions
          "unknown-version" -> "2025-03-26"
          # Simulate incompatible version
          "incompatible" -> nil
          # Default to latest
          _ -> "2025-03-26"
        end

      if negotiated_version do
        result = %{
          protocolVersion: negotiated_version,
          serverInfo: %{
            name: "test-version-server",
            version: "1.0.0"
          },
          capabilities: %{
            tools: %{},
            resources: %{}
          }
        }

        {:ok, result, new_state}
      else
        {:error, "Incompatible protocol version: #{client_version}", new_state}
      end
    end

    @impl true
    def handle_list_tools(_cursor, state) do
      # Simple tool for testing
      tools = [%{name: "test", description: "Test tool"}]
      {:ok, tools, nil, state}
    end

    # Required callbacks
    @impl true
    def handle_call_tool(_name, _args, state), do: {:error, "Not implemented", state}

    @impl true
    def handle_list_resources(_cursor, state), do: {:ok, [], nil, state}

    @impl true
    def handle_read_resource(_uri, state), do: {:error, "Not found", state}

    @impl true
    def handle_list_prompts(_cursor, state), do: {:ok, [], nil, state}

    @impl true
    def handle_get_prompt(_name, _args, state), do: {:error, "Not found", state}

    @impl true
    def handle_complete(_ref, _arg, state), do: {:ok, %{completion: []}, state}

    @impl true
    def handle_list_resource_templates(_cursor, state), do: {:ok, [], nil, state}

    @impl true
    def handle_subscribe_resource(_uri, state), do: {:ok, %{}, state}

    @impl true
    def handle_unsubscribe_resource(_uri, state), do: {:ok, %{}, state}

    @impl true
    def handle_create_message(_params, state), do: {:error, "Not supported", state}

    @impl true
    def handle_list_roots(state), do: {:ok, [], state}
  end

  describe "version negotiation" do
    test "client and server negotiate supported version" do
      {:ok, server} =
        Server.start_link(
          handler: TestVersionHandler,
          transport: :beam,
          name: :test_version_server
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :test_version_server,
          client_info: %{name: "test-client", version: "1.0.0"}
        )

      # Wait for initialization
      Process.sleep(100)

      # Verify the negotiated version
      {:ok, server_info} = Client.server_info(client)
      assert server_info["name"] == "test-version-server"

      # Both should be using 2025-03-26 (latest)
      # We can't directly access the negotiated version from public API,
      # but we can verify the connection works
      {:ok, %{tools: tools}} = Client.list_tools(client)
      assert length(tools) == 1
      assert hd(tools).name == "test"

      GenServer.stop(client)
      GenServer.stop(server)
    end

    test "server can propose alternative version for unknown client version" do
      # This test simulates what happens when version negotiation occurs
      # by directly testing the protocol encoding/parsing

      # Simulate a client with unknown version requesting initialization
      client_info = %{name: "test-client", version: "1.0.0"}
      capabilities = %{}

      # Create initialize message with unknown version
      init_params = %{
        "protocolVersion" => "unknown-version",
        "capabilities" => capabilities,
        "clientInfo" => client_info
      }

      # Test the handler directly
      handler = TestVersionHandler
      {:ok, handler_state} = handler.init([])
      {:ok, result, _new_state} = handler.handle_initialize(init_params, handler_state)

      # Server should propose 2025-03-26 for unknown version
      assert result.protocolVersion == "2025-03-26"
      assert result.serverInfo.name == "test-version-server"
    end

    test "server rejects incompatible version" do
      client_info = %{name: "test-client", version: "1.0.0"}
      capabilities = %{}

      init_params = %{
        "protocolVersion" => "incompatible",
        "capabilities" => capabilities,
        "clientInfo" => client_info
      }

      handler = TestVersionHandler
      {:ok, handler_state} = handler.init([])
      {:error, reason, _state} = handler.handle_initialize(init_params, handler_state)

      assert reason =~ "Incompatible protocol version"
    end

    test "server accepts multiple supported versions" do
      handler = TestVersionHandler
      {:ok, handler_state} = handler.init([])

      client_info = %{name: "test-client", version: "1.0.0"}
      capabilities = %{}

      # Test 2025-03-26
      init_params_latest = %{
        "protocolVersion" => "2025-03-26",
        "capabilities" => capabilities,
        "clientInfo" => client_info
      }

      {:ok, result_latest, _} = handler.handle_initialize(init_params_latest, handler_state)
      assert result_latest.protocolVersion == "2025-03-26"

      # Test 2024-11-05
      init_params_older = %{
        "protocolVersion" => "2024-11-05",
        "capabilities" => capabilities,
        "clientInfo" => client_info
      }

      {:ok, result_older, _} = handler.handle_initialize(init_params_older, handler_state)
      assert result_older.protocolVersion == "2024-11-05"
    end
  end

  describe "protocol negotiation" do
    test "initialize request includes all required fields" do
      client_info = %{name: "test-client", version: "1.0.0"}
      capabilities = %{"roots" => %{}, "sampling" => %{}}

      msg = Protocol.encode_initialize(client_info, capabilities)

      assert msg["jsonrpc"] == "2.0"
      assert msg["method"] == "initialize"
      assert msg["params"]["protocolVersion"] == "2025-03-26"
      assert msg["params"]["clientInfo"] == client_info
      assert msg["params"]["capabilities"] == capabilities
      assert is_integer(msg["id"])
    end

    test "client can specify custom capabilities" do
      client_info = %{name: "custom-client", version: "2.0.0"}

      custom_capabilities = %{
        "roots" => %{"listChanged" => true},
        "sampling" => %{},
        "experimental" => %{"feature1" => true}
      }

      msg = Protocol.encode_initialize(client_info, custom_capabilities)

      assert msg["params"]["capabilities"] == custom_capabilities
      assert msg["params"]["clientInfo"] == client_info
    end
  end
end
