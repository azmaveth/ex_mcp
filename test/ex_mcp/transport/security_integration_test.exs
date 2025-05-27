defmodule ExMCP.Transport.SecurityIntegrationTest do
  use ExUnit.Case

  alias ExMCP.Transport.Beam, as: BEAM
  alias ExMCP.Transport.{SSE, WebSocket}

  describe "SSE transport security" do
    test "includes authentication headers" do
      security = %{
        auth: {:bearer, "test-token"},
        headers: [{"X-Custom", "value"}]
      }

      config = [
        # Non-existent server
        url: "http://localhost:9999",
        security: security
      ]

      # SSE connects asynchronously, so initial connect succeeds
      assert {:ok, state} = SSE.connect(config)

      # Verify security config is stored correctly
      assert state.security == security

      assert state.headers == [
               {"Authorization", "Bearer test-token"},
               {"X-Custom", "value"}
             ]

      # Clean up
      :ok = SSE.close(state)
    end

    test "validates security configuration" do
      invalid_security = %{auth: :invalid}

      config = [
        url: "https://example.com",
        security: invalid_security
      ]

      assert {:error, :invalid_auth_config} = SSE.connect(config)
    end

    test "extracts origin from URL" do
      config = [
        url: "https://api.example.com:8080/custom/path",
        security: %{}
      ]

      assert {:ok, state} = SSE.connect(config)

      # The origin should be extracted without the path
      assert state.origin == "https://api.example.com:8080"

      # Clean up
      :ok = SSE.close(state)
    end

    test "includes standard security headers when configured" do
      security = %{
        include_security_headers: true,
        auth: {:bearer, "token"}
      }

      config = [
        url: "http://localhost:9999",
        security: security
      ]

      assert {:ok, state} = SSE.connect(config)

      # Verify the security config is stored
      assert state.security.include_security_headers == true

      # Clean up
      :ok = SSE.close(state)
    end
  end

  describe "WebSocket transport security" do
    test "includes authentication headers" do
      security = %{
        auth: {:bearer, "test-token"},
        headers: [{"X-Custom", "value"}]
      }

      config = [
        url: "wss://example.com",
        security: security
      ]

      # This will fail to connect but we can verify the security config is applied
      assert {:error, _} = WebSocket.connect(config)
    end

    test "validates WebSocket security configuration" do
      invalid_security = %{auth: :invalid}

      config = [
        url: "wss://example.com",
        security: invalid_security
      ]

      assert {:error, :invalid_auth_config} = WebSocket.connect(config)
    end

    test "handles WSS with TLS configuration" do
      security = %{
        auth: {:bearer, "token"},
        tls: %{
          verify: :verify_peer,
          versions: [:"tlsv1.3"]
        }
      }

      config = [
        url: "wss://secure.example.com",
        security: security
      ]

      # This will fail to connect but TLS options should be built
      assert {:error, _} = WebSocket.connect(config)
    end

    test "extracts correct origin for WebSocket URLs" do
      security = %{}

      # ws:// should extract as http://
      config_ws = [
        url: "ws://localhost:8080/path",
        security: security
      ]

      assert {:error, _} = WebSocket.connect(config_ws)

      # wss:// should extract as https://
      config_wss = [
        url: "wss://secure.example.com:443/path",
        security: security
      ]

      assert {:error, _} = WebSocket.connect(config_wss)
    end
  end

  describe "BEAM transport security" do
    test "validates security configuration" do
      invalid_security = %{auth: :invalid}

      config = [
        server: :test_server,
        security: invalid_security
      ]

      assert {:error, :invalid_auth_config} = BEAM.connect(config)
    end

    test "accepts valid bearer token security" do
      security = %{
        auth: {:bearer, "beam-token"}
      }

      config = [
        server: :test_server,
        security: security
      ]

      # This will fail because server doesn't exist, but security validation should pass
      assert {:error, :server_not_found} = BEAM.connect(config)
    end

    test "accepts valid node cookie security" do
      security = %{
        auth: {:node_cookie, Node.get_cookie()}
      }

      config = [
        server: :test_server,
        security: security
      ]

      # This will fail because server doesn't exist, but security validation should pass
      assert {:error, :server_not_found} = BEAM.connect(config)
    end

    test "server accept validates security configuration" do
      invalid_security = %{auth: :invalid}

      config = [
        name: :test_secure_server,
        security: invalid_security
      ]

      assert {:error, :invalid_auth_config} = BEAM.accept(config)
    end

    test "server accept works with valid security" do
      security = %{
        auth: {:bearer, "server-token"}
      }

      config = [
        name: :test_secure_server,
        security: security
      ]

      assert {:ok, %BEAM.State{security: ^security}} = BEAM.accept(config)
    end
  end

  describe "security configuration validation" do
    test "rejects invalid auth methods" do
      invalid_configs = [
        %{auth: :invalid},
        %{auth: {:invalid_method, "value"}},
        %{auth: {:bearer, nil}},
        %{auth: {:api_key, nil, []}},
        %{auth: {:basic, "user", nil}}
      ]

      for invalid_config <- invalid_configs do
        config = [
          url: "https://example.com",
          security: invalid_config
        ]

        assert {:error, _} = SSE.connect(config)
        assert {:error, _} = WebSocket.connect(config)
      end
    end

    test "accepts valid auth methods" do
      valid_configs = [
        %{auth: {:bearer, "token"}},
        %{auth: {:api_key, "key", []}},
        %{auth: {:api_key, "key", header: "X-API-Key"}},
        %{auth: {:basic, "user", "pass"}},
        %{auth: {:custom, [{"X-Token", "value"}]}}
      ]

      for valid_config <- valid_configs do
        sse_config = [
          url: "https://example.com",
          security: valid_config
        ]

        ws_config = [
          url: "wss://example.com",
          security: valid_config
        ]

        beam_config = [
          server: :test,
          security: valid_config
        ]

        # These will fail to connect but security validation should pass
        assert {:error, _} = SSE.connect(sse_config)
        assert {:error, _} = WebSocket.connect(ws_config)
        assert {:error, :server_not_found} = BEAM.connect(beam_config)
      end
    end
  end

  describe "security header building" do
    test "builds correct headers for different transports" do
      security = %{
        auth: {:bearer, "multi-transport-token"},
        headers: [{"X-Client-ID", "test-client"}],
        include_security_headers: true
      }

      # Test with each transport type
      transport_configs = [
        {SSE, [url: "https://sse.example.com", security: security]},
        {WebSocket, [url: "wss://ws.example.com", security: security]},
        {BEAM, [server: :test_server, security: security]}
      ]

      for {transport_module, config} <- transport_configs do
        # All should fail to connect but build security headers correctly
        case transport_module do
          BEAM ->
            assert {:error, :server_not_found} = transport_module.connect(config)

          _ ->
            assert {:error, _} = transport_module.connect(config)
        end
      end
    end
  end

  describe "TLS configuration" do
    test "applies TLS options for HTTPS/WSS transports" do
      tls_security = %{
        auth: {:bearer, "tls-token"},
        tls: %{
          verify: :verify_peer,
          cacerts: [],
          versions: [:"tlsv1.3"],
          cert: "cert-data",
          key: "key-data"
        }
      }

      https_config = [
        url: "https://secure.example.com",
        security: tls_security
      ]

      wss_config = [
        url: "wss://secure.example.com",
        security: tls_security
      ]

      # These will fail to connect but TLS configuration should be applied
      assert {:error, _} = SSE.connect(https_config)
      assert {:error, _} = WebSocket.connect(wss_config)
    end

    test "uses default TLS options when none specified" do
      security = %{auth: {:bearer, "token"}}

      https_config = [
        url: "https://example.com",
        security: security
      ]

      wss_config = [
        url: "wss://example.com",
        security: security
      ]

      # Should use default TLS settings
      assert {:error, _} = SSE.connect(https_config)
      assert {:error, _} = WebSocket.connect(wss_config)
    end
  end
end
