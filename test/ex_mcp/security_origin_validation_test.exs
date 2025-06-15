defmodule ExMCP.SecurityOriginValidationTest do
  @moduledoc """
  Tests for Origin header validation and DNS rebinding protection.

  These tests ensure that ExMCP properly validates Origin headers to prevent
  DNS rebinding attacks as required by security best practices.
  """
  use ExUnit.Case, async: true

  @moduletag :security
  @moduletag :unit

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

    test "validates non-localhost hosts against allowed list" do
      headers = [{"origin", "https://example.com"}, {"host", "api.example.com"}]

      config = %{
        validate_origin: true,
        allowed_origins: ["https://example.com"],
        allowed_hosts: ["api.example.com"]
      }

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
  end
end
