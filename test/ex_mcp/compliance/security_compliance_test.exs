defmodule ExMCP.Compliance.SecurityComplianceTest do
  @moduledoc """
  Tests for MCP security requirements compliance.

  These tests validate security features required by the MCP specification,
  including DNS rebinding protection, HTTPS enforcement, and origin validation.
  """
  use ExUnit.Case, async: true

  @moduletag :compliance

  alias ExMCP.Security

  describe "MCP Security Requirements - DNS Rebinding Protection" do
    test "prevents DNS rebinding with external domain to localhost" do
      # MCP spec requires protection against DNS rebinding attacks
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
      # MCP spec: prevent subdomain-based attacks like 127.0.0.1.evil.com
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
      # MCP spec: localhost should work for local development
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

  describe "MCP Security Requirements - HTTPS Enforcement" do
    test "requires HTTPS for non-localhost when enforced" do
      # MCP spec: HTTPS should be enforced for production use
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
      # MCP spec: localhost exception for development
      headers = [
        {"origin", "http://localhost"},
        {"host", "localhost"}
      ]

      config = %{enforce_https: true}

      assert :ok = Security.validate_request(headers, config)
    end
  end

  describe "MCP Security Requirements - Origin Validation" do
    test "validates origin header when required" do
      # MCP spec: origin validation prevents unauthorized access
      headers = [{"origin", "https://example.com"}, {"host", "api.example.com"}]
      config = %{validate_origin: true, allowed_origins: ["https://example.com"]}

      assert :ok = Security.validate_request(headers, config)
    end

    test "fails when origin header missing and validation enabled" do
      # MCP spec: missing origin should be rejected when validation enabled
      headers = [{"host", "api.example.com"}]
      config = %{validate_origin: true, allowed_origins: ["https://example.com"]}

      assert {:error, :origin_header_required} = Security.validate_request(headers, config)
    end

    test "fails when origin not in allowed list" do
      # MCP spec: only whitelisted origins should be allowed
      headers = [{"origin", "https://evil.com"}, {"host", "api.example.com"}]
      config = %{validate_origin: true, allowed_origins: ["https://example.com"]}

      assert {:error, :origin_not_allowed} = Security.validate_request(headers, config)
    end

    test "validates origin with port numbers" do
      # MCP spec: port numbers must match exactly
      allowed = ["https://example.com:8080", "http://localhost:3000"]

      assert :ok = Security.validate_origin("https://example.com:8080", allowed)
      assert :ok = Security.validate_origin("http://localhost:3000", allowed)

      assert {:error, :origin_not_allowed} =
               Security.validate_origin("https://example.com:9000", allowed)
    end
  end

  describe "MCP Security Requirements - Host Header Validation" do
    test "validates host header presence" do
      # MCP spec: host header required for security
      headers = [{"origin", "https://example.com"}]
      config = %{validate_origin: true, allowed_origins: ["https://example.com"]}

      assert {:error, :host_header_required} = Security.validate_request(headers, config)
    end

    test "allows localhost hosts" do
      # MCP spec: localhost variants should be recognized
      for host <- ["localhost", "127.0.0.1", "[::1]"] do
        headers = [{"origin", "http://#{host}"}, {"host", host}]
        config = %{validate_origin: true, allowed_origins: ["http://#{host}"]}

        assert :ok = Security.validate_request(headers, config)
      end
    end

    test "rejects non-localhost hosts not in allowed list" do
      # MCP spec: non-localhost hosts must be explicitly allowed
      headers = [{"origin", "https://example.com"}, {"host", "malicious.com"}]

      config = %{
        validate_origin: true,
        allowed_origins: ["https://example.com"],
        allowed_hosts: ["api.example.com"]
      }

      assert {:error, :host_not_allowed} = Security.validate_request(headers, config)
    end
  end

  describe "MCP Security Headers" do
    test "security headers have correct values" do
      # MCP spec recommends standard security headers
      headers = Security.build_standard_security_headers()

      assert {"X-Content-Type-Options", "nosniff"} in headers
      assert {"X-Frame-Options", "DENY"} in headers
      assert {"X-XSS-Protection", "1; mode=block"} in headers
      assert {"Strict-Transport-Security", "max-age=31536000; includeSubDomains"} in headers
      assert {"Referrer-Policy", "strict-origin-when-cross-origin"} in headers
      assert {"X-Permitted-Cross-Domain-Policies", "none"} in headers
    end
  end

  describe "MCP Security - Confused Deputy Prevention" do
    @tag :skip
    test "prevents token passthrough to external resources" do
      # MCP spec: tokens should never be passed to external URLs
      # Currently not implemented - marked as missing in coverage matrix

      # Expected behavior:
      # 1. Server has API token for external service
      # 2. Client requests resource from external URL
      # 3. Server MUST NOT include its own auth tokens in request
    end

    @tag :skip
    test "validates user consent before accessing external resources" do
      # MCP spec: user consent required for external resource access
      # Currently not implemented - marked as missing in coverage matrix

      # Expected behavior:
      # 1. Client requests external resource
      # 2. Server checks if user has consented to access that domain
      # 3. Reject if no consent given
    end
  end

  describe "MCP Security - Case Insensitive Headers" do
    test "finds headers regardless of case" do
      # HTTP headers are case-insensitive per RFC
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
  end
end
