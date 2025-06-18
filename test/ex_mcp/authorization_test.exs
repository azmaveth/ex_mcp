defmodule ExMCP.AuthorizationTest do
  use ExUnit.Case, async: true

  alias ExMCP.Authorization
  alias ExMCP.Authorization.PKCE

  describe "PKCE" do
    test "generates valid code verifier and challenge" do
      {:ok, verifier, challenge} = PKCE.generate_challenge()

      assert is_binary(verifier)
      assert is_binary(challenge)
      assert String.length(verifier) >= 43
      assert String.length(verifier) <= 128
      # Base64url encoded SHA256 hash
      assert String.length(challenge) == 43
    end

    test "verifies challenge correctly" do
      {:ok, verifier, challenge} = PKCE.generate_challenge()

      assert :ok = PKCE.verify_challenge(verifier, challenge)
      assert {:error, :invalid_challenge} = PKCE.verify_challenge(verifier, "invalid")
      assert {:error, :invalid_challenge} = PKCE.verify_challenge("invalid", challenge)
    end

    test "validates code verifier format" do
      {:ok, verifier, _} = PKCE.generate_challenge()
      assert :ok = PKCE.validate_code_verifier(verifier)

      # Too short
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier("short")

      # Invalid characters
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier("invalid chars!")

      # Not a string
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier(123)
    end
  end

  describe "authorization flow" do
    test "validates HTTPS endpoints" do
      config = %{
        client_id: "test-client",
        # HTTP not allowed
        authorization_endpoint: "http://example.com/auth",
        redirect_uri: "https://localhost:8080/callback",
        scopes: ["read"]
      }

      assert {:error, :https_required} = Authorization.start_authorization_flow(config)
    end

    test "validates redirect URIs" do
      config = %{
        client_id: "test-client",
        authorization_endpoint: "https://auth.example.com/authorize",
        # Invalid redirect
        redirect_uri: "http://evil.com/callback",
        scopes: ["read"]
      }

      assert {:error, :invalid_redirect_uri} = Authorization.start_authorization_flow(config)
    end

    test "allows localhost for development" do
      config = %{
        client_id: "test-client",
        authorization_endpoint: "http://localhost:3000/auth",
        redirect_uri: "http://localhost:8080/callback",
        scopes: ["read"]
      }

      assert {:ok, auth_url, state} = Authorization.start_authorization_flow(config)

      assert String.starts_with?(auth_url, "http://localhost:3000/auth?")
      assert String.contains?(auth_url, "client_id=test-client")
      assert String.contains?(auth_url, "response_type=code")
      assert String.contains?(auth_url, "code_challenge_method=S256")
      assert is_binary(state.code_verifier)
      assert is_binary(state.state_param)
    end

    test "builds correct authorization URL" do
      config = %{
        client_id: "test-client",
        authorization_endpoint: "https://auth.example.com/authorize",
        redirect_uri: "https://localhost:8080/callback",
        scopes: ["mcp:read", "mcp:write"],
        additional_params: %{
          "audience" => "api.example.com"
        }
      }

      assert {:ok, auth_url, _state} = Authorization.start_authorization_flow(config)

      assert String.starts_with?(auth_url, "https://auth.example.com/authorize?")
      assert String.contains?(auth_url, "client_id=test-client")
      assert String.contains?(auth_url, "redirect_uri=https%3A%2F%2Flocalhost%3A8080%2Fcallback")
      assert String.contains?(auth_url, "scope=mcp%3Aread+mcp%3Awrite")
      assert String.contains?(auth_url, "audience=api.example.com")
      assert String.contains?(auth_url, "code_challenge=")
      assert String.contains?(auth_url, "code_challenge_method=S256")
      assert String.contains?(auth_url, "state=")
    end
  end

  describe "server metadata discovery" do
    test "validates HTTPS for metadata endpoint" do
      assert {:error, :https_required} =
               Authorization.discover_server_metadata("http://example.com")
    end

    test "allows localhost for development" do
      # This would make an actual HTTP request, so we just test the URL validation
      # In a real test, you'd mock the HTTP client
      issuer = "http://localhost:3000"

      # The function should not fail on HTTPS validation for localhost
      # (actual HTTP request would fail in test environment)
      case Authorization.discover_server_metadata(issuer) do
        # Expected in test environment
        {:error, {:request_failed, _}} -> :ok
        {:error, :https_required} -> flunk("Should allow localhost")
        _ -> :ok
      end
    end
  end

  describe "client credentials flow" do
    test "validates HTTPS for token endpoint" do
      config = %{
        client_id: "client",
        client_secret: "secret",
        token_endpoint: "http://example.com/token"
      }

      assert {:error, :https_required} = Authorization.client_credentials_flow(config)
    end

    test "allows localhost for development" do
      config = %{
        client_id: "client",
        client_secret: "secret",
        token_endpoint: "http://localhost:3000/token",
        scopes: ["mcp:admin"]
      }

      # Would make actual request - expect connection failure in test
      case Authorization.client_credentials_flow(config) do
        # Expected in test environment
        {:error, {:request_failed, _}} -> :ok
        {:error, :https_required} -> flunk("Should allow localhost")
        _ -> :ok
      end
    end
  end

  describe "code exchange" do
    test "validates HTTPS for token endpoint" do
      params = %{
        code: "auth_code",
        code_verifier: "code_verifier",
        client_id: "client",
        redirect_uri: "https://localhost:8080/callback",
        token_endpoint: "http://example.com/token"
      }

      assert {:error, :https_required} = Authorization.exchange_code_for_token(params)
    end

    test "allows localhost for development" do
      params = %{
        code: "auth_code",
        code_verifier: "code_verifier",
        client_id: "client",
        redirect_uri: "https://localhost:8080/callback",
        token_endpoint: "http://localhost:3000/token"
      }

      # Would make actual request - expect connection failure in test
      case Authorization.exchange_code_for_token(params) do
        # Expected in test environment
        {:error, {:request_failed, _}} -> :ok
        {:error, :https_required} -> flunk("Should allow localhost")
        _ -> :ok
      end
    end
  end

  describe "token validation" do
    test "validates HTTPS for introspection endpoint" do
      assert {:error, :https_required} =
               Authorization.validate_token(
                 "token",
                 "http://example.com/introspect"
               )
    end

    test "allows localhost for development" do
      # Would make actual request - expect connection failure in test
      case Authorization.validate_token("token", "http://localhost:3000/introspect") do
        # Expected in test environment
        {:error, {:request_failed, _}} -> :ok
        {:error, :https_required} -> flunk("Should allow localhost")
        _ -> :ok
      end
    end
  end
end
