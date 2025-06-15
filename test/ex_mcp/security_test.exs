defmodule ExMCP.SecurityTest do
  use ExUnit.Case
  doctest ExMCP.Security

  @moduletag :security

  alias ExMCP.Security

  describe "build_auth_headers/1" do
    test "builds bearer token headers" do
      config = %{auth: {:bearer, "secret-token"}}
      headers = Security.build_auth_headers(config)

      assert headers == [{"Authorization", "Bearer secret-token"}]
    end

    test "builds API key headers with default header name" do
      config = %{auth: {:api_key, "api-key-123", []}}
      headers = Security.build_auth_headers(config)

      assert headers == [{"X-API-Key", "api-key-123"}]
    end

    test "builds API key headers with custom header name" do
      config = %{auth: {:api_key, "api-key-123", header: "X-Custom-Key"}}
      headers = Security.build_auth_headers(config)

      assert headers == [{"X-Custom-Key", "api-key-123"}]
    end

    test "builds basic auth headers" do
      config = %{auth: {:basic, "user", "pass"}}
      headers = Security.build_auth_headers(config)

      assert headers == [{"Authorization", "Basic dXNlcjpwYXNz"}]
    end

    test "builds custom headers" do
      config = %{auth: {:custom, [{"X-Token", "value"}, {"X-Secret", "secret"}]}}
      headers = Security.build_auth_headers(config)

      assert headers == [{"X-Token", "value"}, {"X-Secret", "secret"}]
    end

    test "returns empty list when no auth config" do
      config = %{}
      headers = Security.build_auth_headers(config)

      assert headers == []
    end
  end

  describe "build_security_headers/1" do
    test "combines auth and custom headers" do
      config = %{
        auth: {:bearer, "token"},
        headers: [{"X-Custom", "value"}]
      }

      headers = Security.build_security_headers(config)

      assert {"Authorization", "Bearer token"} in headers
      assert {"X-Custom", "value"} in headers
    end

    test "deduplicates headers by name" do
      config = %{
        auth: {:custom, [{"X-Token", "auth-value"}]},
        headers: [{"X-Token", "custom-value"}]
      }

      headers = Security.build_security_headers(config)

      # Should only have one X-Token header (auth takes precedence)
      token_headers = Enum.filter(headers, fn {name, _} -> name == "X-Token" end)
      assert length(token_headers) == 1
      assert {"X-Token", "auth-value"} in headers
    end
  end

  describe "validate_origin/2" do
    test "allows any origin when configured as :any" do
      assert Security.validate_origin("https://evil.com", :any) == :ok
    end

    test "rejects nil origin when not :any" do
      assert Security.validate_origin(nil, ["https://allowed.com"]) ==
               {:error, :origin_not_allowed}
    end

    test "allows origin in allowed list" do
      allowed = ["https://app.example.com", "https://trusted.com"]
      assert Security.validate_origin("https://app.example.com", allowed) == :ok
    end

    test "rejects origin not in allowed list" do
      allowed = ["https://app.example.com"]

      assert Security.validate_origin("https://evil.com", allowed) ==
               {:error, :origin_not_allowed}
    end
  end

  describe "build_cors_headers/2" do
    test "builds basic CORS headers" do
      cors_config = %{
        allowed_origins: ["https://app.example.com"],
        allowed_methods: ["GET", "POST"],
        allowed_headers: ["Content-Type", "Authorization"]
      }

      headers = Security.build_cors_headers(cors_config, "https://app.example.com")

      assert {"Access-Control-Allow-Origin", "https://app.example.com"} in headers
      assert {"Access-Control-Allow-Methods", "GET, POST"} in headers
      assert {"Access-Control-Allow-Headers", "Content-Type, Authorization"} in headers
    end

    test "allows any origin with wildcard" do
      cors_config = %{allowed_origins: :any}
      headers = Security.build_cors_headers(cors_config)

      assert {"Access-Control-Allow-Origin", "*"} in headers
    end

    test "includes credentials header when configured" do
      cors_config = %{allow_credentials: true}
      headers = Security.build_cors_headers(cors_config)

      assert {"Access-Control-Allow-Credentials", "true"} in headers
    end

    test "includes max age when configured" do
      cors_config = %{max_age: 3600}
      headers = Security.build_cors_headers(cors_config)

      assert {"Access-Control-Max-Age", "3600"} in headers
    end

    test "includes expose headers when configured" do
      cors_config = %{expose_headers: ["X-Total-Count", "X-Rate-Limit"]}
      headers = Security.build_cors_headers(cors_config)

      assert {"Access-Control-Expose-Headers", "X-Total-Count, X-Rate-Limit"} in headers
    end

    test "does not include origin header for non-matching origins" do
      cors_config = %{allowed_origins: ["https://allowed.com"]}
      headers = Security.build_cors_headers(cors_config, "https://not-allowed.com")

      refute Enum.any?(headers, fn {name, _} -> name == "Access-Control-Allow-Origin" end)
    end
  end

  describe "build_standard_security_headers/0" do
    test "includes all standard security headers" do
      headers = Security.build_standard_security_headers()

      assert {"X-Content-Type-Options", "nosniff"} in headers
      assert {"X-Frame-Options", "DENY"} in headers
      assert {"X-XSS-Protection", "1; mode=block"} in headers
      assert {"Strict-Transport-Security", "max-age=31536000; includeSubDomains"} in headers
    end
  end

  describe "validate_config/1" do
    test "validates valid bearer auth config" do
      config = %{auth: {:bearer, "token"}}
      assert Security.validate_config(config) == :ok
    end

    test "validates valid API key config" do
      config = %{auth: {:api_key, "key", []}}
      assert Security.validate_config(config) == :ok
    end

    test "validates valid basic auth config" do
      config = %{auth: {:basic, "user", "pass"}}
      assert Security.validate_config(config) == :ok
    end

    test "validates valid custom auth config" do
      config = %{auth: {:custom, [{"X-Token", "value"}]}}
      assert Security.validate_config(config) == :ok
    end

    test "rejects invalid auth config" do
      config = %{auth: :invalid}
      assert Security.validate_config(config) == {:error, :invalid_auth_config}
    end

    test "validates empty config" do
      config = %{}
      assert Security.validate_config(config) == :ok
    end

    test "validates config with CORS" do
      config = %{
        auth: {:bearer, "token"},
        cors: %{allowed_origins: ["https://app.com"]}
      }

      assert Security.validate_config(config) == :ok
    end

    test "validates config with TLS" do
      config = %{
        auth: {:bearer, "token"},
        tls: %{verify: :verify_peer}
      }

      assert Security.validate_config(config) == :ok
    end
  end

  describe "apply_security/2" do
    test "applies security headers to transport options" do
      transport_opts = [url: "https://example.com"]

      security_config = %{
        auth: {:bearer, "token"},
        headers: [{"X-Custom", "value"}]
      }

      result = Security.apply_security(transport_opts, security_config)

      assert Keyword.has_key?(result, :headers)
      headers = Keyword.get(result, :headers)
      assert {"Authorization", "Bearer token"} in headers
      assert {"X-Custom", "value"} in headers
    end

    test "merges with existing headers" do
      transport_opts = [headers: [{"Existing", "header"}]]
      security_config = %{auth: {:bearer, "token"}}

      result = Security.apply_security(transport_opts, security_config)

      headers = Keyword.get(result, :headers)
      assert {"Existing", "header"} in headers
      assert {"Authorization", "Bearer token"} in headers
    end

    test "adds TLS options when configured" do
      transport_opts = [url: "https://example.com"]

      security_config = %{
        tls: %{
          verify: :verify_peer,
          cacerts: ["cert1"],
          versions: [:"tlsv1.3"]
        }
      }

      result = Security.apply_security(transport_opts, security_config)

      assert Keyword.has_key?(result, :ssl_options)
      ssl_opts = Keyword.get(result, :ssl_options)
      assert ssl_opts[:verify] == :verify_peer
      assert ssl_opts[:cacerts] == ["cert1"]
      assert ssl_opts[:versions] == [:"tlsv1.3"]
    end

    test "filters out nil TLS values" do
      transport_opts = []

      security_config = %{
        tls: %{
          verify: :verify_peer,
          cacerts: nil,
          cert: "cert-data"
        }
      }

      result = Security.apply_security(transport_opts, security_config)

      ssl_opts = Keyword.get(result, :ssl_options)
      assert ssl_opts[:verify] == :verify_peer
      assert ssl_opts[:cert] == "cert-data"
      refute Keyword.has_key?(ssl_opts, :cacerts)
    end
  end
end
