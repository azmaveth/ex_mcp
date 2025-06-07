defmodule ExMCP.SecurityOriginValidationTest do
  @moduledoc """
  Tests for Origin header validation and DNS rebinding protection.

  These tests ensure that ExMCP properly validates Origin headers to prevent
  DNS rebinding attacks as required by security best practices.
  """
  use ExUnit.Case, async: true

  alias ExMCP.Security

  describe "origin validation" do
    test "validates allowed origins correctly" do
      allowed = ["https://example.com", "https://app.example.com"]

      assert :ok = Security.validate_origin("https://example.com", allowed)
      assert :ok = Security.validate_origin("https://app.example.com", allowed)
      assert {:error, :origin_not_allowed} = Security.validate_origin("https://evil.com", allowed)

      assert {:error, :origin_not_allowed} =
               Security.validate_origin("http://example.com", allowed)
    end

    test "allows any origin when configured" do
      assert :ok = Security.validate_origin("https://anywhere.com", :any)
      assert :ok = Security.validate_origin("http://localhost", :any)
    end

    test "rejects nil origin when not allowed" do
      allowed = ["https://example.com"]
      assert {:error, :origin_not_allowed} = Security.validate_origin(nil, allowed)
    end

    test "validates origin with port numbers" do
      allowed = ["https://example.com:8080", "http://localhost:3000"]

      assert :ok = Security.validate_origin("https://example.com:8080", allowed)
      assert :ok = Security.validate_origin("http://localhost:3000", allowed)

      assert {:error, :origin_not_allowed} =
               Security.validate_origin("https://example.com:9000", allowed)
    end
  end

  describe "request validation" do
    test "validates origin header when required" do
      headers = [{"origin", "https://example.com"}, {"host", "api.example.com"}]
      config = %{validate_origin: true, allowed_origins: ["https://example.com"]}

      assert :ok = Security.validate_request(headers, config)
    end

    test "fails when origin header missing and validation enabled" do
      headers = [{"host", "api.example.com"}]
      config = %{validate_origin: true, allowed_origins: ["https://example.com"]}

      assert {:error, :origin_header_required} = Security.validate_request(headers, config)
    end

    test "fails when origin not in allowed list" do
      headers = [{"origin", "https://evil.com"}, {"host", "api.example.com"}]
      config = %{validate_origin: true, allowed_origins: ["https://example.com"]}

      assert {:error, :origin_not_allowed} = Security.validate_request(headers, config)
    end

    test "skips origin validation when disabled" do
      headers = [{"origin", "https://evil.com"}, {"host", "api.example.com"}]
      config = %{validate_origin: false}

      assert :ok = Security.validate_request(headers, config)
    end

    test "validates host header presence" do
      headers = [{"origin", "https://example.com"}]
      config = %{validate_origin: true, allowed_origins: ["https://example.com"]}

      assert {:error, :host_header_required} = Security.validate_request(headers, config)
    end

    test "allows localhost hosts" do
      for host <- ["localhost", "127.0.0.1", "[::1]"] do
        headers = [{"origin", "http://#{host}"}, {"host", host}]
        config = %{validate_origin: true, allowed_origins: ["http://#{host}"]}

        assert :ok = Security.validate_request(headers, config)
      end
    end

    test "validates non-localhost hosts against allowed list" do
      headers = [{"origin", "https://example.com"}, {"host", "api.example.com"}]

      config = %{
        validate_origin: true,
        allowed_origins: ["https://example.com"],
        allowed_hosts: ["api.example.com"]
      }

      assert :ok = Security.validate_request(headers, config)
    end

    test "rejects non-localhost hosts not in allowed list" do
      headers = [{"origin", "https://example.com"}, {"host", "malicious.com"}]

      config = %{
        validate_origin: true,
        allowed_origins: ["https://example.com"],
        allowed_hosts: ["api.example.com"]
      }

      assert {:error, :host_not_allowed} = Security.validate_request(headers, config)
    end
  end

  describe "HTTPS enforcement" do
    test "requires HTTPS for non-localhost when enforced" do
      headers = [
        {"origin", "http://example.com"},
        {"host", "example.com"},
        {"x-forwarded-proto", "http"}
      ]

      config = %{enforce_https: true}

      assert {:error, :https_required} = Security.validate_request(headers, config)
    end

    test "allows HTTPS when enforced" do
      headers = [
        {"origin", "https://example.com"},
        {"host", "example.com"},
        {"x-forwarded-proto", "https"}
      ]

      config = %{enforce_https: true}

      assert :ok = Security.validate_request(headers, config)
    end

    test "allows HTTP for localhost when HTTPS enforced" do
      headers = [
        {"origin", "http://localhost"},
        {"host", "localhost"}
      ]

      config = %{enforce_https: true}

      assert :ok = Security.validate_request(headers, config)
    end

    test "skips HTTPS check when not enforced" do
      headers = [
        {"origin", "http://example.com"},
        {"host", "example.com"},
        {"x-forwarded-proto", "http"}
      ]

      config = %{enforce_https: false}

      assert :ok = Security.validate_request(headers, config)
    end
  end

  describe "CORS headers" do
    test "builds basic CORS headers" do
      cors_config = %{
        allowed_origins: ["https://example.com"],
        allowed_methods: ["GET", "POST"],
        allowed_headers: ["Content-Type", "Authorization"],
        max_age: 3600,
        allow_credentials: true
      }

      headers = Security.build_cors_headers(cors_config, "https://example.com")

      assert {"Access-Control-Allow-Origin", "https://example.com"} in headers
      assert {"Access-Control-Allow-Methods", "GET, POST"} in headers
      assert {"Access-Control-Allow-Headers", "Content-Type, Authorization"} in headers
      assert {"Access-Control-Max-Age", "3600"} in headers
      assert {"Access-Control-Allow-Credentials", "true"} in headers
    end

    test "builds wildcard CORS headers" do
      cors_config = %{allowed_origins: :any}

      headers = Security.build_cors_headers(cors_config)

      assert {"Access-Control-Allow-Origin", "*"} in headers
    end

    test "filters origin not in allowed list" do
      cors_config = %{allowed_origins: ["https://example.com"]}

      headers = Security.build_cors_headers(cors_config, "https://evil.com")

      refute Enum.any?(headers, fn {name, _} -> name == "Access-Control-Allow-Origin" end)
    end

    test "handles minimal CORS config" do
      cors_config = %{}

      headers = Security.build_cors_headers(cors_config)

      # Should at least have wildcard origin
      assert {"Access-Control-Allow-Origin", "*"} in headers
    end
  end

  describe "security headers" do
    test "builds comprehensive security headers" do
      headers = Security.build_standard_security_headers()

      expected_headers = [
        "X-Content-Type-Options",
        "X-Frame-Options",
        "X-XSS-Protection",
        "Strict-Transport-Security",
        "Referrer-Policy",
        "X-Permitted-Cross-Domain-Policies"
      ]

      header_names = Enum.map(headers, fn {name, _} -> name end)

      for expected <- expected_headers do
        assert expected in header_names, "Missing security header: #{expected}"
      end
    end

    test "security headers have correct values" do
      headers = Security.build_standard_security_headers()

      assert {"X-Content-Type-Options", "nosniff"} in headers
      assert {"X-Frame-Options", "DENY"} in headers
      assert {"X-XSS-Protection", "1; mode=block"} in headers
      assert {"Strict-Transport-Security", "max-age=31536000; includeSubDomains"} in headers
      assert {"Referrer-Policy", "strict-origin-when-cross-origin"} in headers
      assert {"X-Permitted-Cross-Domain-Policies", "none"} in headers
    end
  end

  describe "DNS rebinding attack scenarios" do
    test "prevents DNS rebinding with external domain to localhost" do
      # Attacker sets up evil.com to resolve to 127.0.0.1
      headers = [
        {"origin", "https://evil.com"},
        {"host", "127.0.0.1:8080"}
      ]

      config = %{
        validate_origin: true,
        allowed_origins: ["https://trusted.com"]
      }

      assert {:error, :origin_not_allowed} = Security.validate_request(headers, config)
    end

    test "prevents DNS rebinding with subdomain attack" do
      # Attacker uses subdomain like 127.0.0.1.evil.com
      headers = [
        {"origin", "https://127.0.0.1.evil.com"},
        {"host", "localhost"}
      ]

      config = %{
        validate_origin: true,
        allowed_origins: ["https://trusted.com"]
      }

      assert {:error, :origin_not_allowed} = Security.validate_request(headers, config)
    end

    test "allows legitimate localhost usage" do
      headers = [
        {"origin", "http://localhost:3000"},
        {"host", "localhost:8080"}
      ]

      config = %{
        validate_origin: true,
        allowed_origins: ["http://localhost:3000"]
      }

      assert :ok = Security.validate_request(headers, config)
    end
  end

  describe "case insensitive header matching" do
    test "finds headers regardless of case" do
      headers = [
        {"Origin", "https://example.com"},
        {"HOST", "example.com"},
        {"X-Forwarded-Proto", "https"}
      ]

      config = %{
        validate_origin: true,
        allowed_origins: ["https://example.com"],
        enforce_https: true
      }

      assert :ok = Security.validate_request(headers, config)
    end

    test "handles mixed case header names" do
      headers = [
        {"oRiGiN", "https://example.com"},
        {"hOsT", "example.com"}
      ]

      config = %{
        validate_origin: true,
        allowed_origins: ["https://example.com"]
      }

      assert :ok = Security.validate_request(headers, config)
    end
  end
end
