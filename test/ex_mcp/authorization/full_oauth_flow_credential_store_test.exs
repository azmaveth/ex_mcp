defmodule ExMCP.Authorization.FullOAuthFlowCredentialStoreTest do
  use ExUnit.Case, async: true

  alias ExMCP.Authorization.CredentialStore
  alias ExMCP.Authorization.FullOAuthFlow

  defmodule StoreAdapter do
    @behaviour CredentialStore

    @impl true
    def fetch_registration(context, issuer, agent) do
      Agent.get(agent, fn state ->
        case state.index[{context, issuer}] do
          nil -> :not_found
          key -> {:ok, key, state.registrations[key]}
        end
      end)
    end

    @impl true
    def put_registration(context, key, registration, agent) do
      Agent.update(agent, fn state ->
        issuer = elem(key, 3)

        state
        |> put_in([:index, {context, issuer}], key)
        |> put_in([:registrations, key], registration)
      end)
    end

    @impl true
    def fetch_token(key, agent) do
      Agent.get(agent, fn state ->
        case state.tokens[key] do
          nil -> :not_found
          token -> {:ok, token}
        end
      end)
    end

    @impl true
    def put_token(key, token, agent) do
      Agent.update(agent, &put_in(&1, [:tokens, key], token))
    end
  end

  test "reuses DCR credentials only for the same issuer and re-registers after an AS change" do
    {:ok, store_agent} =
      Agent.start_link(fn -> %{index: %{}, registrations: %{}, tokens: %{}} end)

    first_as = oauth_server("client-at-first-as")
    store = {StoreAdapter, store_agent}

    config = flow_config(first_as, store)

    assert {:ok, %{access_token: "token-for-client-at-first-as"}} =
             FullOAuthFlow.execute(config)

    assert {:ok, %{access_token: "token-for-client-at-first-as"}} =
             FullOAuthFlow.execute(config)

    assert Agent.get(first_as.counter, & &1.registrations) == 1

    second_as = oauth_server("client-at-second-as")

    assert {:ok, %{access_token: "token-for-client-at-second-as"}} =
             second_as
             |> flow_config(store)
             |> FullOAuthFlow.execute()

    assert Agent.get(second_as.counter, & &1.registrations) == 1

    stored_issuers =
      Agent.get(store_agent, fn state ->
        state.registrations
        |> Map.values()
        |> Enum.map(& &1.issuer)
        |> Enum.sort()
      end)

    assert stored_issuers == Enum.sort([first_as.issuer, second_as.issuer])
  end

  test "rejects AS metadata whose issuer differs from the discovered identifier" do
    {:ok, store_agent} =
      Agent.start_link(fn -> %{index: %{}, registrations: %{}, tokens: %{}} end)

    server = oauth_server("unused-client", "https://different.example")

    assert {:error, {:issuer_mismatch, expected: expected, actual: "https://different.example"}} =
             server
             |> flow_config({StoreAdapter, store_agent})
             |> FullOAuthFlow.execute()

    assert expected == server.issuer
    assert Agent.get(server.counter, & &1.registrations) == 0
  end

  test "preserves native and web DCR redirect rejections without a weakening retry" do
    {:ok, store_agent} =
      Agent.start_link(fn -> %{index: %{}, registrations: %{}, tokens: %{}} end)

    response = %{
      "error" => "invalid_redirect_uri",
      "error_description" => "redirect URI is not registered by policy"
    }

    for application_type <- [:native, :web] do
      server = oauth_server("unused-client", nil, registration_error: response)

      config =
        server
        |> flow_config({StoreAdapter, store_agent})
        |> Map.put(:application_type, application_type)

      assert {:error,
              {:redirect_uri_rejected,
               application_type: ^application_type,
               redirect_uri: "http://127.0.0.1:45321/callback",
               status: 400,
               response: ^response}} = FullOAuthFlow.execute(config)

      state = Agent.get(server.counter, & &1)
      assert state.registrations == 1
      assert state.tokens == 0
      assert [request] = state.registration_requests
      assert request["application_type"] == Atom.to_string(application_type)
      assert request["redirect_uris"] == ["http://127.0.0.1:45321/callback"]
    end
  end

  defp oauth_server(client_id, metadata_issuer \\ nil, opts \\ []) do
    bypass = Bypass.open()
    unique = System.unique_integer([:positive])
    issuer_host = "auth-#{unique}.example"
    resource_host = "resource-#{unique}.example"
    issuer = "https://#{issuer_host}"
    resource_origin = "https://#{resource_host}"
    actual_origin = "http://localhost:#{bypass.port}"
    metadata_issuer = metadata_issuer || issuer

    {:ok, counter} =
      Agent.start_link(fn -> %{registrations: 0, registration_requests: [], tokens: 0} end)

    metadata_client = fn uri, _address, _opts ->
      body =
        case {uri.host, uri.path} do
          {^resource_host, "/prm"} ->
            %{"authorization_servers" => [issuer]}

          {^issuer_host, "/.well-known/openid-configuration"} ->
            %{
              "issuer" => metadata_issuer,
              "authorization_endpoint" => actual_origin <> "/authorize",
              "token_endpoint" => actual_origin <> "/token",
              "registration_endpoint" => actual_origin <> "/register",
              "grant_types_supported" => ["client_credentials"],
              "token_endpoint_auth_methods_supported" => ["client_secret_post"]
            }

          _other ->
            nil
        end

      if body,
        do: {:ok, %{status: 200, headers: [], body: Jason.encode!(body)}},
        else: {:ok, %{status: 404, headers: [], body: ""}}
    end

    Bypass.stub(bypass, "POST", "/register", fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn)
      request = Jason.decode!(body)

      Agent.update(counter, fn state ->
        state
        |> Map.update!(:registrations, fn count -> count + 1 end)
        |> Map.update!(:registration_requests, &[request | &1])
      end)

      case opts[:registration_error] do
        nil ->
          json(conn, 201, %{
            "client_id" => client_id,
            "client_secret" => "secret-for-#{client_id}"
          })

        error ->
          json(conn, 400, error)
      end
    end)

    Bypass.stub(bypass, "POST", "/token", fn conn ->
      Agent.update(counter, &Map.update!(&1, :tokens, fn count -> count + 1 end))

      json(conn, 200, %{
        "access_token" => "token-for-#{client_id}",
        "token_type" => "Bearer",
        "expires_in" => 3_600,
        "scope" => "tools:read"
      })
    end)

    %{
      bypass: bypass,
      issuer: issuer,
      resource_origin: resource_origin,
      metadata_client: metadata_client,
      counter: counter
    }
  end

  defp flow_config(server, store) do
    %{
      resource_url: server.resource_origin <> "/mcp",
      www_authenticate: ~s(Bearer resource_metadata="#{server.resource_origin}/prm"),
      client_registration: :auto,
      application_type: :native,
      redirect_port: 45_321,
      credential_store: store,
      credential_context: "test-installation",
      scopes: ["tools:read"],
      metadata_fetch: [
        http_client: server.metadata_client,
        dns_resolver: fn _host, _timeout -> {:ok, [{93, 184, 216, 34}]} end
      ]
    }
  end

  defp json(conn, status, body) do
    conn
    |> Plug.Conn.put_resp_content_type("application/json")
    |> Plug.Conn.resp(status, Jason.encode!(body))
  end
end
