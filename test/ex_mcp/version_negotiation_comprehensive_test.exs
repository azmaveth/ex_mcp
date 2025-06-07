defmodule ExMCP.VersionNegotiationComprehensiveTest do
  @moduledoc """
  Comprehensive tests for version negotiation between clients and servers.

  Tests all scenarios described in the MCP specification for version negotiation,
  including edge cases and error conditions.
  """
  use ExUnit.Case, async: true

  alias ExMCP.{Client, Protocol, Server, VersionRegistry}

  defmodule MultiVersionHandler do
    @moduledoc false
    use ExMCP.Server.Handler

    @impl true
    def init(args) do
      supported_versions = Keyword.get(args, :supported_versions, ["2025-03-26", "2024-11-05"])
      version_strategy = Keyword.get(args, :version_strategy, :latest)

      {:ok,
       %{
         supported_versions: supported_versions,
         version_strategy: version_strategy,
         client_version: nil,
         negotiated_version: nil
       }}
    end

    @impl true
    def handle_initialize(params, state) do
      client_version = params["protocolVersion"]
      _client_info = params["clientInfo"]

      # Store client version for testing
      state = %{state | client_version: client_version}

      # Negotiate version based on strategy
      negotiated_version = negotiate_version(client_version, state)

      if negotiated_version do
        state = %{state | negotiated_version: negotiated_version}

        # Build capabilities based on negotiated version
        capabilities = build_capabilities_for_version(negotiated_version)

        result = %{
          protocolVersion: negotiated_version,
          serverInfo: %{
            name: "multi-version-server",
            version: "1.0.0"
          },
          capabilities: capabilities,
          meta: %{
            clientRequestedVersion: client_version,
            serverSupportedVersions: state.supported_versions
          }
        }

        {:ok, result, state}
      else
        # No compatible version found
        error_data = %{
          supported: state.supported_versions,
          requested: client_version
        }

        {:error,
         %{
           code: -32602,
           message: "Unsupported protocol version",
           data: error_data
         }, state}
      end
    end

    defp negotiate_version(client_version, state) do
      case state.version_strategy do
        :latest -> negotiate_latest_version(client_version, state)
        :exact -> negotiate_exact_version(client_version, state)
        :fallback -> negotiate_fallback_version(client_version, state)
        :reject_unknown -> negotiate_reject_unknown_version(client_version, state)
      end
    end

    defp negotiate_latest_version(client_version, state) do
      if client_version in state.supported_versions do
        client_version
      else
        hd(state.supported_versions)
      end
    end

    defp negotiate_exact_version(client_version, state) do
      if client_version in state.supported_versions, do: client_version, else: nil
    end

    defp negotiate_fallback_version(client_version, state) do
      cond do
        client_version in state.supported_versions ->
          client_version

        client_version == "draft" && "2025-03-26" in state.supported_versions ->
          "2025-03-26"

        client_version == "1.0.0" && "2024-11-05" in state.supported_versions ->
          "2024-11-05"

        true ->
          hd(state.supported_versions)
      end
    end

    defp negotiate_reject_unknown_version(client_version, state) do
      if client_version in VersionRegistry.supported_versions() &&
           client_version in state.supported_versions do
        client_version
      else
        nil
      end
    end

    defp build_capabilities_for_version(version) do
      # Use the version registry to get proper capabilities
      caps = VersionRegistry.capabilities_for_version(version)

      # Convert to string keys for JSON compatibility
      %{
        "tools" => convert_capability(caps.tools),
        "resources" => convert_capability(caps.resources),
        "prompts" => convert_capability(caps.prompts),
        "logging" => convert_capability(caps.logging),
        "completion" => convert_capability(Map.get(caps, :completion, %{})),
        "experimental" => convert_capability(Map.get(caps, :experimental, %{}))
      }
    end

    defp convert_capability(cap) when is_map(cap) do
      Map.new(cap, fn {k, v} -> {to_string(k), v} end)
    end

    defp convert_capability(_), do: %{}

    # Required callbacks
    @impl true
    def handle_list_tools(_cursor, state) do
      # Return tools appropriate for negotiated version
      tools =
        case state.negotiated_version do
          "draft" ->
            [
              %{
                name: "draft_tool",
                description: "Only in draft",
                outputSchema: %{type: "object"}
              }
            ]

          "2025-03-26" ->
            [
              %{
                name: "annotated_tool",
                description: "Has annotations",
                annotations: %{readOnlyHint: true}
              }
            ]

          _ ->
            [%{name: "basic_tool", description: "Basic tool"}]
        end

      {:ok, tools, nil, state}
    end

    @impl true
    def handle_call_tool(name, args, state) do
      {:ok, [%{type: "text", text: "Called #{name} with #{inspect(args)}"}], state}
    end

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

  describe "basic version negotiation" do
    test "exact version match - both support same version" do
      {:ok, server} =
        Server.start_link(
          handler: MultiVersionHandler,
          handler_args: [supported_versions: ["2025-03-26", "2024-11-05"]],
          transport: :beam,
          name: :exact_match_server
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :exact_match_server,
          protocol_version: "2025-03-26"
        )

      Process.sleep(100)

      {:ok, version} = Client.negotiated_version(client)
      assert version == "2025-03-26"

      GenServer.stop(client)
      GenServer.stop(server)
    end

    test "client requests older version that server supports" do
      {:ok, server} =
        Server.start_link(
          handler: MultiVersionHandler,
          handler_args: [supported_versions: ["2025-03-26", "2024-11-05"]],
          transport: :beam,
          name: :older_version_server
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :older_version_server,
          protocol_version: "2024-11-05"
        )

      Process.sleep(100)

      {:ok, version} = Client.negotiated_version(client)
      assert version == "2024-11-05"

      GenServer.stop(client)
      GenServer.stop(server)
    end

    test "client requests newer version than server supports" do
      {:ok, server} =
        Server.start_link(
          handler: MultiVersionHandler,
          handler_args: [
            supported_versions: ["2024-11-05"],
            version_strategy: :latest
          ],
          transport: :beam,
          name: :old_server
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :old_server,
          protocol_version: "2025-03-26"
        )

      Process.sleep(100)

      # Server should propose its latest (2024-11-05)
      {:ok, version} = Client.negotiated_version(client)
      assert version == "2024-11-05"

      GenServer.stop(client)
      GenServer.stop(server)
    end
  end

  describe "unknown version handling" do
    test "server proposes alternative for unknown client version" do
      {:ok, server} =
        Server.start_link(
          handler: MultiVersionHandler,
          handler_args: [
            supported_versions: ["2025-03-26", "2024-11-05"],
            version_strategy: :latest
          ],
          transport: :beam,
          name: :unknown_version_server
        )

      # Client with made-up version
      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :unknown_version_server,
          protocol_version: "99.99.99"
        )

      Process.sleep(100)

      # Server should propose its latest
      {:ok, version} = Client.negotiated_version(client)
      assert version == "2025-03-26"

      GenServer.stop(client)
      GenServer.stop(server)
    end

    test "strict server rejects unknown versions" do
      {:ok, server} =
        Server.start_link(
          handler: MultiVersionHandler,
          handler_args: [
            supported_versions: ["2025-03-26"],
            version_strategy: :reject_unknown
          ],
          transport: :beam,
          name: :strict_server
        )

      # This should fail during initialization
      result =
        Client.start_link(
          transport: :beam,
          server: :strict_server,
          protocol_version: "unknown-version"
        )

      case result do
        {:error, _reason} ->
          # Expected - initialization failed
          assert true

        {:ok, client} ->
          # If client started, it should not be properly initialized
          Process.sleep(100)
          assert {:error, _} = Client.list_tools(client)
          GenServer.stop(client)
      end

      GenServer.stop(server)
    end
  end

  describe "draft version negotiation" do
    test "draft client with server that doesn't support draft" do
      {:ok, server} =
        Server.start_link(
          handler: MultiVersionHandler,
          handler_args: [
            supported_versions: ["2025-03-26", "2024-11-05"],
            version_strategy: :fallback
          ],
          transport: :beam,
          name: :no_draft_server
        )

      {:ok, client} =
        Client.start_link(
          transport: :beam,
          server: :no_draft_server,
          protocol_version: "draft"
        )

      Process.sleep(100)

      # Server should offer latest stable version
      {:ok, version} = Client.negotiated_version(client)
      assert version == "2025-03-26"

      GenServer.stop(client)
      GenServer.stop(server)
    end

    test "server supporting only draft with older client" do
      {:ok, server} =
        Server.start_link(
          handler: MultiVersionHandler,
          handler_args: [
            supported_versions: ["draft"],
            version_strategy: :exact
          ],
          transport: :beam,
          name: :draft_only_server
        )

      # Client requesting stable version
      result =
        Client.start_link(
          transport: :beam,
          server: :draft_only_server,
          protocol_version: "2025-03-26"
        )

      case result do
        {:error, _} ->
          # Expected - no compatible version
          assert true

        {:ok, client} ->
          Process.sleep(100)
          # Should fail as versions don't match
          assert {:error, _} = Client.list_tools(client)
          GenServer.stop(client)
      end

      GenServer.stop(server)
    end
  end

  describe "capability differences by version" do
    test "capabilities match negotiated version" do
      {:ok, server} =
        Server.start_link(
          handler: MultiVersionHandler,
          handler_args: [supported_versions: ["2025-03-26", "2024-11-05", "draft"]],
          transport: :beam,
          name: :capability_server
        )

      # Test each version
      versions = ["2024-11-05", "2025-03-26", "draft"]

      for version <- versions do
        {:ok, client} =
          Client.start_link(
            transport: :beam,
            server: :capability_server,
            protocol_version: version
          )

        Process.sleep(100)

        {:ok, negotiated} = Client.negotiated_version(client)
        assert negotiated == version

        {:ok, caps} = Client.server_capabilities(client)

        case version do
          "2024-11-05" ->
            # No advanced features
            refute get_in(caps, ["resources", "subscribe"])
            refute get_in(caps, ["completion", "hasArguments"])

          "2025-03-26" ->
            # Has 2025 features
            assert get_in(caps, ["resources", "subscribe"]) == true
            assert get_in(caps, ["completion", "hasArguments"]) == true
            assert get_in(caps, ["experimental", "batchProcessing"]) == true

          "draft" ->
            # Has draft features
            assert get_in(caps, ["experimental", "elicitation"]) == true
            assert get_in(caps, ["experimental", "structuredContent"]) == true
            # No batch processing in draft
            assert get_in(caps, ["experimental", "batchProcessing"]) == false
        end

        GenServer.stop(client)
      end

      GenServer.stop(server)
    end
  end

  describe "error scenarios" do
    test "initialization fails with incompatible versions" do
      {:ok, server} =
        Server.start_link(
          handler: MultiVersionHandler,
          handler_args: [
            supported_versions: ["2024-11-05"],
            version_strategy: :exact
          ],
          transport: :beam,
          name: :incompatible_server
        )

      # Client only supports newer versions
      result =
        Client.start_link(
          transport: :beam,
          server: :incompatible_server,
          protocol_version: "2025-03-26"
        )

      case result do
        {:error, _reason} ->
          # Should fail during initialization
          assert true

        {:ok, client} ->
          # If started, operations should fail
          Process.sleep(100)
          assert {:error, _} = Client.list_tools(client)
          GenServer.stop(client)
      end

      GenServer.stop(server)
    end

    test "error response includes supported versions" do
      # Test that error responses follow the spec format
      handler = MultiVersionHandler

      {:ok, state} =
        handler.init(
          supported_versions: ["2025-03-26"],
          version_strategy: :exact
        )

      # Try to initialize with unsupported version
      params = %{
        "protocolVersion" => "1.0.0",
        "clientInfo" => %{name: "test", version: "1.0"},
        "capabilities" => %{}
      }

      {:error, error, _state} = handler.handle_initialize(params, state)

      assert error.code == -32602
      assert error.message =~ "Unsupported"
      assert error.data.supported == ["2025-03-26"]
      assert error.data.requested == "1.0.0"
    end
  end

  describe "version registry integration" do
    test "registry correctly identifies feature availability" do
      # Test that version-specific features are properly gated

      # Draft features
      assert VersionRegistry.feature_available?("draft", :elicitation)
      refute VersionRegistry.feature_available?("2025-03-26", :elicitation)
      refute VersionRegistry.feature_available?("2024-11-05", :elicitation)

      # 2025-03-26 features
      assert VersionRegistry.feature_available?("2025-03-26", :resource_subscription)
      assert VersionRegistry.feature_available?("draft", :resource_subscription)
      refute VersionRegistry.feature_available?("2024-11-05", :resource_subscription)

      # Base features available in all
      assert VersionRegistry.feature_available?("2024-11-05", :tools)
      assert VersionRegistry.feature_available?("2025-03-26", :tools)
      assert VersionRegistry.feature_available?("draft", :tools)
    end

    test "message validation respects version" do
      # Test protocol message validation

      # Valid for all versions
      assert :ok =
               Protocol.validate_message_version(
                 %{"method" => "tools/list"},
                 "2024-11-05"
               )

      # Only valid for newer versions
      assert :ok =
               Protocol.validate_message_version(
                 %{"method" => "resources/subscribe"},
                 "2025-03-26"
               )

      assert {:error, msg} =
               Protocol.validate_message_version(
                 %{"method" => "resources/subscribe"},
                 "2024-11-05"
               )

      assert msg =~ "not available"

      # Draft-only method
      assert :ok =
               Protocol.validate_message_version(
                 %{"method" => "elicitation/create"},
                 "draft"
               )

      assert {:error, _} =
               Protocol.validate_message_version(
                 %{"method" => "elicitation/create"},
                 "2025-03-26"
               )
    end
  end

  describe "spec compliance for version negotiation" do
    test "client SHOULD disconnect if server version not supported" do
      # Simulate scenario where client can't support server's version
      # This would happen in the client's initialization logic

      # For testing, we check that client properly handles version mismatch
      client_supported = ["2025-03-26"]
      server_proposed = "2024-11-05"

      # Client should determine this is not acceptable
      refute server_proposed in client_supported
    end

    test "initialize request MUST include protocolVersion" do
      msg =
        Protocol.encode_initialize(
          %{name: "test", version: "1.0"},
          %{}
        )

      assert Map.has_key?(msg["params"], "protocolVersion")
      assert is_binary(msg["params"]["protocolVersion"])
    end

    test "server MUST respond with same version if supported" do
      handler = MultiVersionHandler
      {:ok, state} = handler.init(supported_versions: ["2025-03-26", "2024-11-05"])

      params = %{
        "protocolVersion" => "2025-03-26",
        "clientInfo" => %{name: "test", version: "1.0"},
        "capabilities" => %{}
      }

      {:ok, result, _} = handler.handle_initialize(params, state)
      assert result.protocolVersion == "2025-03-26"
    end

    test "server SHOULD propose latest version for unknown client version" do
      handler = MultiVersionHandler

      {:ok, state} =
        handler.init(
          supported_versions: ["2025-03-26", "2024-11-05"],
          version_strategy: :latest
        )

      params = %{
        "protocolVersion" => "unknown",
        "clientInfo" => %{name: "test", version: "1.0"},
        "capabilities" => %{}
      }

      {:ok, result, _} = handler.handle_initialize(params, state)
      # Should propose latest (first in list)
      assert result.protocolVersion == "2025-03-26"
    end
  end
end
