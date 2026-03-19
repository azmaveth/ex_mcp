defmodule ExMCP.Authorization.ServerGuardTest do
  use ExUnit.Case, async: false

  alias ExMCP.Authorization.ServerGuard

  setup do
    # Set feature flag to true for most tests
    Application.put_env(:ex_mcp, :oauth2_enabled, true)

    on_exit(fn ->
      # Reset feature flag
      Application.delete_env(:ex_mcp, :oauth2_enabled)
    end)
  end

  @base_config %{
    realm: "test-realm"
  }

  describe "authorize/3" do
    test "returns :ok when :oauth2_auth feature flag is disabled" do
      Application.put_env(:ex_mcp, :oauth2_enabled, false)

      config =
        Map.put(@base_config, :introspection_endpoint, "https://auth.example.com/introspect")

      assert ServerGuard.authorize([], [], config) == :ok
    end

    test "returns error for missing token" do
      headers = []

      config =
        Map.put(@base_config, :introspection_endpoint, "https://auth.example.com/introspect")

      {:error, {status, www_auth, body}} = ServerGuard.authorize(headers, [], config)

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

      config =
        Map.put(@base_config, :introspection_endpoint, "https://auth.example.com/introspect")

      {:error, {401, _, _}} = ServerGuard.authorize(headers, [], config)
    end
  end

  describe "authorize/3 with mock introspection endpoint" do
    @describetag :requires_bypass
    setup do
      bypass = Bypass.open()

      # Most tests can use a localhost http endpoint
      config =
        @base_config
        |> Map.put(:introspection_endpoint, "http://localhost:#{bypass.port}/introspect")

      {:ok, bypass: bypass, config: config}
    end

    test "successfully authorizes with valid token and sufficient scopes", %{
      bypass: bypass,
      config: config
    } do
      Bypass.stub(bypass, "POST", "/introspect", fn conn ->
        {:ok, body, conn} = Plug.Conn.read_body(conn)
        assert body =~ "token=valid-token"
        Plug.Conn.resp(conn, 200, Jason.encode!(%{active: true, scope: "read write"}))
      end)

      headers = [{"authorization", "Bearer valid-token"}]
      required_scopes = ["read"]

      assert {:ok, token_info} = ServerGuard.authorize(headers, required_scopes, config)
      assert token_info.active == true
      assert token_info.scope == "read write"
    end

    test "returns error for invalid token", %{bypass: bypass, config: config} do
      Bypass.stub(bypass, "POST", "/introspect", fn conn ->
        Plug.Conn.resp(conn, 200, Jason.encode!(%{active: false}))
      end)

      headers = [{"authorization", "Bearer invalid-token"}]
      {:error, {status, www_auth, body}} = ServerGuard.authorize(headers, [], config)

      assert status == 401

      assert www_auth ==
               ~s(Bearer realm="test-realm", error="invalid_token", error_description="The access token is expired, revoked, or malformed.")

      assert Jason.decode!(body) == %{
               "error" => "invalid_token",
               "error_description" => "The access token is expired, revoked, or malformed."
             }
    end

    test "returns error for insufficient scope", %{bypass: bypass, config: config} do
      Bypass.stub(bypass, "POST", "/introspect", fn conn ->
        Plug.Conn.resp(conn, 200, Jason.encode!(%{active: true, scope: "read"}))
      end)

      headers = [{"authorization", "Bearer valid-token"}]
      required_scopes = ["write"]

      {:error, {status, www_auth, body}} =
        ServerGuard.authorize(headers, required_scopes, config)

      assert status == 403

      assert www_auth ==
               ~s(Bearer realm="test-realm", error="insufficient_scope", error_description="The request requires higher privileges.", scope="write")

      assert Jason.decode!(body) == %{
               "error" => "insufficient_scope",
               "error_description" => "The request requires higher privileges."
             }
    end

    test "handles token with no scope when scopes are required", %{
      bypass: bypass,
      config: config
    } do
      Bypass.stub(bypass, "POST", "/introspect", fn conn ->
        Plug.Conn.resp(conn, 200, Jason.encode!(%{active: true}))
      end)

      headers = [{"authorization", "Bearer valid-token"}]
      required_scopes = ["read"]
      {:error, {status, _, _}} = ServerGuard.authorize(headers, required_scopes, config)
      assert status == 403
    end

    test "handles token with nil scope when scopes are required", %{
      bypass: bypass,
      config: config
    } do
      Bypass.stub(bypass, "POST", "/introspect", fn conn ->
        Plug.Conn.resp(conn, 200, Jason.encode!(%{active: true, scope: nil}))
      end)

      headers = [{"authorization", "Bearer valid-token"}]
      required_scopes = ["read"]
      {:error, {status, _, _}} = ServerGuard.authorize(headers, required_scopes, config)
      assert status == 403
    end

    test "succeeds when no scopes are required", %{bypass: bypass, config: config} do
      Bypass.stub(bypass, "POST", "/introspect", fn conn ->
        Plug.Conn.resp(conn, 200, Jason.encode!(%{active: true, scope: "read"}))
      end)

      headers = [{"authorization", "Bearer valid-token"}]
      assert {:ok, token_info} = ServerGuard.authorize(headers, [], config)
      assert token_info.active == true
    end

    test "allows http for localhost introspection endpoint", %{bypass: bypass, config: config} do
      Bypass.stub(bypass, "POST", "/introspect", fn conn ->
        Plug.Conn.resp(conn, 200, Jason.encode!(%{active: true}))
      end)

      headers = [{"authorization", "Bearer valid-token"}]
      # The config from setup already uses http://localhost
      assert {:ok, token_info} = ServerGuard.authorize(headers, [], config)
      assert token_info.active == true
    end
  end

  describe "configuration validation" do
    test "returns error for invalid config (non-https introspection endpoint)" do
      bad_config =
        @base_config
        |> Map.put(:introspection_endpoint, "http://insecure.com/introspect")

      headers = [{"authorization", "Bearer some-token"}]

      {:error, {status, _, _}} = ServerGuard.authorize(headers, [], bad_config)
      assert status == 500
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
