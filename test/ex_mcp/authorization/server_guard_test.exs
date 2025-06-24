defmodule ExMCP.Authorization.ServerGuardTest do
  use ExUnit.Case, async: true

  alias ExMCP.Authorization.ServerGuard
  alias ExMCP.FeatureFlags

  setup do
    # Set feature flag to true for most tests
    Application.put_env(:ex_mcp, :oauth2_enabled, true)

    on_exit(fn ->
      # Reset feature flag
      Application.delete_env(:ex_mcp, :oauth2_enabled)
    end)
  end

  @config %{
    introspection_endpoint: "https://auth.example.com/introspect",
    realm: "test-realm"
  }

  describe "authorize/3" do
    test "returns :ok when :oauth2_auth feature flag is disabled" do
      Application.put_env(:ex_mcp, :oauth2_enabled, false)
      assert ServerGuard.authorize([], [], @config) == :ok
    end

    test "returns error for missing token" do
      headers = []
      {:error, {status, www_auth, body}} = ServerGuard.authorize(headers, [], @config)

      assert status == 401

      assert www_auth ==
               ~s(Bearer realm="test-realm", error="invalid_request", error_description="Authorization header is missing or malformed.")

      assert Jason.decode!(body) == %{
               "error" => "invalid_request",
               "error_description" => "Authorization header is missing or malformed."
             }
    end

    test "returns error for malformed Authorization header" do
      headers = [{"authorization", "Basic some-token"}]
      {:error, {401, _, _}} = ServerGuard.authorize(headers, [], @config)
    end

    @tag :skip
    test "successfully authorizes with valid token and sufficient scopes" do
      # This test requires an actual OAuth introspection endpoint
      # Skip for unit testing - would be covered in integration tests
    end

    @tag :skip
    test "returns error for invalid token" do
      # This test requires an actual OAuth introspection endpoint
      # Skip for unit testing - would be covered in integration tests
    end

    @tag :skip
    test "returns error for insufficient scope" do
      # This test requires an actual OAuth introspection endpoint
      # Skip for unit testing - would be covered in integration tests
    end

    @tag :skip
    test "handles token with no scope when scopes are required" do
      # This test requires an actual OAuth introspection endpoint
      # Skip for unit testing - would be covered in integration tests
    end

    @tag :skip
    test "handles token with nil scope when scopes are required" do
      # This test requires an actual OAuth introspection endpoint
      # Skip for unit testing - would be covered in integration tests
    end

    @tag :skip
    test "succeeds when no scopes are required" do
      # This test requires an actual OAuth introspection endpoint
      # Skip for unit testing - would be covered in integration tests
    end

    test "returns error for invalid config (non-https introspection endpoint)" do
      bad_config = %{introspection_endpoint: "http://insecure.com/introspect"}
      headers = [{"authorization", "Bearer some-token"}]

      {:error, {status, _, _}} = ServerGuard.authorize(headers, [], bad_config)
      assert status == 500
    end

    @tag :skip
    test "allows http for localhost introspection endpoint" do
      # This test requires an actual OAuth introspection endpoint
      # Skip for unit testing - would be covered in integration tests
    end
  end

  describe "extract_bearer_token/1" do
    test "extracts token from valid header (list of tuples)" do
      headers = [{"authorization", "Bearer my-secret-token"}]
      assert ServerGuard.extract_bearer_token(headers) == {:ok, "my-secret-token"}
    end

    test "extracts token from valid header (map)" do
      headers = %{"authorization" => "Bearer my-secret-token"}
      assert ServerGuard.extract_bearer_token(headers) == {:ok, "my-secret-token"}
    end

    test "is case-insensitive to header key" do
      headers = [{"Authorization", "Bearer my-secret-token"}]
      assert ServerGuard.extract_bearer_token(headers) == {:ok, "my-secret-token"}
    end

    test "returns error for missing header" do
      headers = []
      assert ServerGuard.extract_bearer_token(headers) == {:error, :missing_token}
    end

    test "returns error for wrong scheme" do
      headers = [{"authorization", "Basic my-secret-token"}]
      assert ServerGuard.extract_bearer_token(headers) == {:error, :missing_token}
    end

    test "returns error for missing token value" do
      headers = [{"authorization", "Bearer "}]
      assert ServerGuard.extract_bearer_token(headers) == {:error, :missing_token}
    end
  end
end
