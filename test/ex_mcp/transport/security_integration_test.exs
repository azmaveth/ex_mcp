defmodule ExMCP.Transport.SecurityIntegrationTest do
  use ExUnit.Case

  @moduletag :transport

  alias ExMCP.Test.HTTPServer
  alias ExMCP.Transport.HTTP

  describe "HTTP transport security" do
    setup do
      # Start test HTTP server
      {:ok, server} = HTTPServer.start_link()
      url = HTTPServer.get_url(server)

      on_exit(fn ->
        HTTPServer.stop(server)
      end)

      {:ok, server: server, url: url}
    end

    test "includes authentication headers", %{server: server, url: url} do
      security = %{
        auth: {:bearer, "test-token"},
        headers: [{"X-Custom", "value"}]
      }

      config = [
        url: url,
        endpoint: "/mcp/v1",
        security: security
      ]

      # Connect to the test server
      assert {:ok, state} = HTTP.connect(config)

      # Wait a bit for the connection to establish
      Process.sleep(100)

      # Send a test message to verify headers are included
      test_message = %{
        "jsonrpc" => "2.0",
        "method" => "test",
        "params" => %{},
        "id" => 1
      }

      assert {:ok, _state} = HTTP.send_message(test_message, state)

      # Give the request time to complete
      Process.sleep(50)

      # Verify the request included our security headers
      request = HTTPServer.get_last_request(server)
      assert request != nil
      assert request.auth == "Bearer test-token"
      assert request.headers["x-custom"] == "value"

      # Clean up
      :ok = HTTP.close(state)
    end

    test "validates security configuration" do
      invalid_security = %{auth: :invalid}

      config = [
        url: "https://example.com",
        security: invalid_security
      ]

      assert {:error, :invalid_auth_config} = HTTP.connect(config)
    end

    test "extracts origin from URL", %{url: base_url} do
      # Use a custom path to test origin extraction
      config = [
        url: "#{base_url}/custom/path",
        endpoint: "/mcp/v1",
        security: %{}
      ]

      assert {:ok, state} = HTTP.connect(config)

      # The origin should be extracted without the path
      # Parse to get just the origin
      uri = URI.parse(base_url)
      expected_origin = "#{uri.scheme}://#{uri.host}:#{uri.port}"
      assert state.origin == expected_origin

      # Clean up
      :ok = HTTP.close(state)
    end

    test "includes standard security headers when configured", %{url: url} do
      security = %{
        include_security_headers: true,
        auth: {:bearer, "token"}
      }

      config = [
        url: url,
        endpoint: "/mcp/v1",
        security: security
      ]

      assert {:ok, state} = HTTP.connect(config)

      # Verify the security config is stored
      assert state.security.include_security_headers == true

      # TODO: Verify actual security headers are sent when HTTP connects
      # This would require checking the HTTP connection headers

      # Clean up
      :ok = HTTP.close(state)
    end
  end

  describe "security configuration validation" do
    @tag :skip
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

        assert {:error, _} = HTTP.connect(config)
      end
    end

    @tag :skip
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

        # These will fail to connect but security validation should pass
        assert {:error, _} = HTTP.connect(sse_config)
      end
    end
  end

  describe "security header building" do
    @tag :skip
    test "builds correct headers for different transports" do
      security = %{
        auth: {:bearer, "multi-transport-token"},
        headers: [{"X-Client-ID", "test-client"}],
        include_security_headers: true
      }

      # Test with HTTP transport
      http_config = [url: "https://sse.example.com", security: security]

      # Should fail to connect but build security headers correctly
      assert {:error, _} = HTTP.connect(http_config)
    end
  end

  describe "TLS configuration" do
    @tag :skip
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

      # These will fail to connect but TLS configuration should be applied
      assert {:error, _} = HTTP.connect(https_config)
    end

    @tag :skip
    test "uses default TLS options when none specified" do
      security = %{auth: {:bearer, "token"}}

      https_config = [
        url: "https://example.com",
        security: security
      ]

      # Should use default TLS settings
      assert {:error, _} = HTTP.connect(https_config)
    end
  end
end
