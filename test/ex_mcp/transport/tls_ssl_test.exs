defmodule ExMCP.Transport.TLSSSLTest do
  @moduledoc """
  Comprehensive tests for TLS/SSL configuration across all transports.

  Tests TLS/SSL requirements from MCP specification:
  - HTTPS enforcement for non-localhost
  - Certificate validation options
  - Mutual TLS support  
  - TLS version configuration
  - Cipher suite configuration
  - Custom CA certificates
  - Client certificate authentication
  """
  use ExUnit.Case, async: true

  @moduletag :transport

  alias ExMCP.{Security, Transport}

  describe "TLS configuration validation" do
    test "validates TLS config structure" do
      valid_config = %{
        verify: :verify_peer,
        cacerts: [:public_key.cacerts_get()],
        versions: [:"tlsv1.2", :"tlsv1.3"]
      }

      assert :ok = Security.validate_tls_config(valid_config)
    end

    test "rejects invalid verification modes" do
      invalid_config = %{
        verify: :invalid_mode
      }

      assert {:error, :invalid_verify_mode} = Security.validate_tls_config(invalid_config)
    end

    test "validates TLS versions" do
      # Valid versions
      valid_config = %{
        versions: [:"tlsv1.2", :"tlsv1.3"]
      }

      assert :ok = Security.validate_tls_config(valid_config)

      # Invalid versions should be rejected
      invalid_config = %{
        # Insecure versions
        versions: [:"tlsv1.0", :sslv3]
      }

      assert {:error, :insecure_tls_versions} = Security.validate_tls_config(invalid_config)
    end

    test "validates certificate formats" do
      # PEM encoded certificate
      pem_cert = """
      -----BEGIN CERTIFICATE-----
      MIIDXTCCAkWgAwIBAgIJAKoK/heBjcOuMA0GCSqGSIb3DQEBBQUAMEUxCzAJBgNV
      BAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRlcm5ldCBX
      aWRnaXRzIFB0eSBMdGQwHhcNMTExMjMxMDMyNTUxWhcNMTIxMjMwMDMyNTUxWjBF
      MQswCQYDVQQGEwJBVTETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UECgwYSW50
      ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIB
      CgKCAQEAzUCFozgNb1h1M0jzNRSCjhOBnR+uVbVpaWfXYIR+AhWDdEe5ryY+Cgav
      Og8bfLybyzFdehlYdDRgkedEB/GjG8aJw06l0qF4jDOAw0kEygWDQY9X/zp2yAp3
      m8uI0u6qXnuCIf02SdUBDNSp9ELH0xTdz9WBrUV1bGFO0Q8U+a9VsEvO0fqQE+9P
      uP8Q8v60sX+WAbVcyKyA3XbYk5Zv8CCvqg9MNxvJm+XLa7qMjWOSKrEgAjK5r+0s
      RQY7fWTgk5lJmZf+hVP5kB/j+Hl3n9L3rN+yVJ3jBd+ZOZNa5JwJQzx3yOYCHQJG
      KwIDAQABo1AwTjAdBgNVHQ4EFgQUqKq6tzFdz3xg4VHQL+w+8/+j7QwwHwYDVR0j
      BBgwFoAUqKq6tzFdz3xg4VHQL+w+8/+j7QwwDAYDVR0TBAUwAwEB/zANBgkqhkiG
      9w0BAQUFAAOCAQEAjU3xKgC7VZ4I6VxMJhGpQF+2x2dULUgmTYJBZWXSV7Bfxha+
      4Oz7JVlnQGfuWJXxq6Ee8cJ2o8QJNzG2h2O3V4sQ1v4Q0L1qG5i8dF3FYyDu8vJB
      9n3p2v9vwF8Qx+h9W6zQ7H9+v7M5M5j7w8m8C9m3V4m0Q1K3z8j2Q6mQ9x4V4vVq
      Y9Q6x7v8M5M5j7w8m8C9m3V4m0Q1K3z8j2Q6mQ9x4V4vVqY9Q6x7v8M5M5j7w8m8
      C9m3V4m0Q1K3z8j2Q6mQ9x4V4vVqY9Q6x7v8M5M5j7w8m8C9m3V4m0Q1K3z8j2Q6
      mQ9x4V4vVqY9Q6x7v8M5M5j7w8m8C9m3V4m0Q1K3z8j2Q6mQ9x4V4vVqY9Q6x7
      -----END CERTIFICATE-----
      """

      pem_key = """
      -----BEGIN PRIVATE KEY-----
      MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDNQIWjOA1vWHUz
      SPM1FIKOE4GdH65VtWlpZ9dghH4CFYN0R7mvJj4KBq86Dxt8vJvLMV16GVh0NGCRw
      -----END PRIVATE KEY-----
      """

      config = %{
        cert: pem_cert,
        key: pem_key
      }

      assert :ok = Security.validate_tls_config(config)
    end

    test "validates cipher suites" do
      config = %{
        ciphers: [
          "ECDHE-RSA-AES256-GCM-SHA384",
          "ECDHE-RSA-AES128-GCM-SHA256",
          "ECDHE-RSA-AES256-SHA384",
          "ECDHE-RSA-AES128-SHA256"
        ]
      }

      assert :ok = Security.validate_tls_config(config)
    end
  end

  describe "HTTPS enforcement" do
    test "requires HTTPS for non-localhost URLs" do
      # Non-localhost URLs must use HTTPS
      assert {:error, :https_required} = Security.enforce_https_requirement("http://example.com")

      assert {:error, :https_required} =
               Security.enforce_https_requirement("http://192.168.1.100")

      # HTTPS should be allowed
      assert :ok = Security.enforce_https_requirement("https://example.com")
      assert :ok = Security.enforce_https_requirement("https://api.example.com")
    end

    test "allows HTTP for localhost" do
      # Localhost URLs can use HTTP
      assert :ok = Security.enforce_https_requirement("http://localhost")
      assert :ok = Security.enforce_https_requirement("http://127.0.0.1")
      assert :ok = Security.enforce_https_requirement("http://[::1]")

      # But HTTPS is still preferred
      assert :ok = Security.enforce_https_requirement("https://localhost")
      assert :ok = Security.enforce_https_requirement("https://127.0.0.1")
    end

    test "handles ports correctly" do
      assert :ok = Security.enforce_https_requirement("http://localhost:8080")
      assert :ok = Security.enforce_https_requirement("https://localhost:8443")

      assert {:error, :https_required} =
               Security.enforce_https_requirement("http://example.com:8080")

      assert :ok = Security.enforce_https_requirement("https://example.com:8443")
    end
  end

  describe "HTTP transport TLS configuration" do
    test "builds SSL options correctly" do
      tls_config = %{
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        versions: [:"tlsv1.2", :"tlsv1.3"],
        ciphers: ["ECDHE-RSA-AES256-GCM-SHA384"]
      }

      ssl_opts = Transport.HTTP.build_ssl_options(tls_config)

      assert ssl_opts[:ssl][:verify] == :verify_peer
      assert ssl_opts[:ssl][:versions] == [:"tlsv1.2", :"tlsv1.3"]
      assert is_list(ssl_opts[:ssl][:cacerts])
    end

    test "includes client certificate when provided" do
      cert_pem = "-----BEGIN CERTIFICATE-----\n...\n-----END CERTIFICATE-----"
      key_pem = "-----BEGIN PRIVATE KEY-----\n...\n-----END PRIVATE KEY-----"

      tls_config = %{
        cert: cert_pem,
        key: key_pem
      }

      ssl_opts = Transport.HTTP.build_ssl_options(tls_config)

      assert ssl_opts[:ssl][:cert] == cert_pem
      assert ssl_opts[:ssl][:key] == key_pem
    end

    test "supports verify_none for development" do
      tls_config = %{
        verify: :verify_none
      }

      ssl_opts = Transport.HTTP.build_ssl_options(tls_config)

      assert ssl_opts[:ssl][:verify] == :verify_none
    end

    test "defaults to secure settings" do
      ssl_opts = Transport.HTTP.build_ssl_options(%{})

      assert ssl_opts[:ssl][:verify] == :verify_peer
      assert :"tlsv1.2" in ssl_opts[:ssl][:versions]
      assert :"tlsv1.3" in ssl_opts[:ssl][:versions]
      # Should not include old versions
      refute :"tlsv1.1" in ssl_opts[:ssl][:versions]
    end
  end

  describe "WebSocket transport TLS configuration" do
    test "applies TLS configuration to WebSocket connections" do
      tls_config = %{
        verify: :verify_peer,
        versions: [:"tlsv1.3"]
      }

      ws_opts = Transport.WebSocket.build_ssl_options(tls_config)

      assert ws_opts[:ssl][:verify] == :verify_peer
      assert ws_opts[:ssl][:versions] == [:"tlsv1.3"]
    end

    test "supports WebSocket over TLS (WSS)" do
      config = [
        transport: :websocket,
        url: "wss://api.example.com/mcp",
        security: %{
          tls: %{
            verify: :verify_peer,
            versions: [:"tlsv1.2", :"tlsv1.3"]
          }
        }
      ]

      # Should not raise an error during configuration validation
      assert {:ok, _validated_config} = Transport.WebSocket.validate_config(config)
    end
  end

  describe "BEAM transport native security" do
    test "supports distributed Erlang with node cookies" do
      security_config = %{
        node_cookie: :secret_production_cookie,
        hidden: true,
        max_connections: 10
      }

      dist_opts = Transport.Beam.build_distribution_options(security_config)

      assert dist_opts[:cookie] == :secret_production_cookie
      assert dist_opts[:hidden] == true
      assert dist_opts[:max_connections] == 10
    end

    test "supports node cookie authentication" do
      config = %{
        node_cookie: :secret_cookie
      }

      assert {:ok, opts} = Transport.Beam.prepare_secure_connection(config)
      assert opts[:cookie] == :secret_cookie
    end

    test "rejects invalid node cookies" do
      config = %{
        # Invalid - this is the default insecure cookie
        node_cookie: :nocookie
      }

      assert {:error, :invalid_node_cookie} = Transport.Beam.prepare_secure_connection(config)
    end

    test "supports VM-level TLS distribution configuration" do
      # This is for advanced deployments where TLS over distribution is needed
      tls_config = %{
        verify: :verify_peer,
        cert: "path/to/cert.pem",
        key: "path/to/key.pem",
        cacerts: "path/to/ca.pem"
      }

      assert {:ok, ssl_opts} = Transport.Beam.configure_distribution_tls(tls_config)

      assert ssl_opts[:verify] == :verify_peer
      assert ssl_opts[:certfile] == "path/to/cert.pem"
      assert ssl_opts[:keyfile] == "path/to/key.pem"
      assert ssl_opts[:cacertfile] == "path/to/ca.pem"
    end
  end

  describe "mutual TLS (mTLS)" do
    test "configures client certificate authentication" do
      mtls_config = %{
        verify: :verify_peer,
        cert: "client-cert.pem",
        key: "client-key.pem",
        cacerts: "ca-bundle.pem",
        fail_if_no_peer_cert: true
      }

      ssl_opts = Security.build_mtls_options(mtls_config)

      assert ssl_opts[:verify] == :verify_peer
      assert ssl_opts[:fail_if_no_peer_cert] == true
      assert ssl_opts[:cert] == "client-cert.pem"
      assert ssl_opts[:key] == "client-key.pem"
    end

    test "validates certificate chain" do
      # Should validate that cert, key, and CA are all provided for mTLS
      incomplete_config = %{
        verify: :verify_peer,
        cert: "client.pem"
        # Missing key and cacerts
      }

      assert {:error, :incomplete_mtls_config} = Security.validate_mtls_config(incomplete_config)
    end

    test "supports certificate revocation checking" do
      config = %{
        verify: :verify_peer,
        cert: "client.pem",
        key: "client.key",
        cacerts: "ca.pem",
        crl_check: true,
        crl_cache: {:ssl_crl_cache, {:internal, [{:http, 5000}]}}
      }

      assert :ok = Security.validate_mtls_config(config)
    end
  end

  describe "certificate validation" do
    test "validates hostname verification" do
      config = %{
        verify: :verify_peer,
        verify_fun: {&Security.verify_hostname/3, "api.example.com"}
      }

      assert :ok = Security.validate_tls_config(config)
    end

    test "supports custom certificate verification" do
      custom_verify_fun = fn _cert, event, _state ->
        # Custom verification logic
        case event do
          :valid_peer -> :valid_peer
          {:bad_cert, reason} -> {:fail, reason}
          {:extension, _} -> :unknown
        end
      end

      config = %{
        verify: :verify_peer,
        verify_fun: {custom_verify_fun, []}
      }

      assert :ok = Security.validate_tls_config(config)
    end

    test "supports certificate pinning" do
      expected_cert_hash = "sha256:abc123def456..."

      config = %{
        verify: :verify_peer,
        certificate_pinning: [expected_cert_hash]
      }

      assert :ok = Security.validate_certificate_pinning_config(config)
    end
  end

  describe "TLS version negotiation" do
    test "negotiates highest available TLS version" do
      config = %{
        versions: [:"tlsv1.2", :"tlsv1.3"]
      }

      # Should prefer TLS 1.3 if available
      assert :"tlsv1.3" = Security.preferred_tls_version(config[:versions])
    end

    test "rejects insecure TLS versions" do
      insecure_config = %{
        versions: [:"tlsv1.0", :"tlsv1.1"]
      }

      assert {:error, :insecure_tls_versions} = Security.validate_tls_config(insecure_config)
    end

    test "allows TLS 1.2 and 1.3 only" do
      secure_config = %{
        versions: [:"tlsv1.2", :"tlsv1.3"]
      }

      assert :ok = Security.validate_tls_config(secure_config)
    end
  end

  describe "integration with MCP security requirements" do
    test "validates transport security configuration" do
      # MCP requires proper TLS for non-localhost
      config = %{
        url: "https://api.example.com",
        security: %{
          tls: %{
            verify: :verify_peer,
            versions: [:"tlsv1.2", :"tlsv1.3"]
          }
        }
      }

      assert :ok = Security.validate_transport_security(config)
    end

    test "allows insecure config for localhost development" do
      localhost_config = %{
        url: "http://localhost:8080",
        security: %{
          tls: %{
            verify: :verify_none
          }
        }
      }

      assert :ok = Security.validate_transport_security(localhost_config)
    end

    test "enforces TLS for production deployments" do
      production_config = %{
        # HTTP for production
        url: "http://api.example.com",
        security: %{
          enforce_https: true
        }
      }

      assert {:error, :https_required} = Security.validate_transport_security(production_config)
    end
  end

  describe "cipher suite configuration" do
    test "supports modern cipher suites" do
      modern_ciphers = [
        "TLS_AES_256_GCM_SHA384",
        "TLS_AES_128_GCM_SHA256",
        "TLS_CHACHA20_POLY1305_SHA256",
        "ECDHE-RSA-AES256-GCM-SHA384",
        "ECDHE-RSA-AES128-GCM-SHA256"
      ]

      config = %{
        ciphers: modern_ciphers
      }

      assert :ok = Security.validate_cipher_suites(config)
    end

    test "rejects weak cipher suites" do
      weak_ciphers = [
        # 3DES
        "DES-CBC3-SHA",
        # RC4
        "RC4-SHA",
        # NULL encryption
        "NULL-SHA"
      ]

      config = %{
        ciphers: weak_ciphers
      }

      assert {:error, :weak_cipher_suites} = Security.validate_cipher_suites(config)
    end

    test "prefers AEAD cipher suites" do
      preferred_order = Security.recommended_cipher_suites()

      # First cipher should be AEAD
      assert String.contains?(hd(preferred_order), "GCM") or
               String.contains?(hd(preferred_order), "POLY1305")
    end
  end
end
