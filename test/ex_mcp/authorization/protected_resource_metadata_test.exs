defmodule ExMCP.Authorization.ProtectedResourceMetadataTest do
  use ExUnit.Case, async: true

  alias ExMCP.Authorization.ProtectedResourceMetadata

  @public_address {93, 184, 216, 34}

  test "discovers string and structured authorization-server entries through the safe fetcher" do
    body =
      Jason.encode!(%{
        "authorization_servers" => [
          "https://auth.example",
          %{
            "issuer" => "https://other.example",
            "metadata_endpoint" => "https://other.example/metadata",
            "scopes_supported" => ["tools:read"]
          }
        ]
      })

    parent = self()

    client = fn uri, address, opts ->
      send(parent, {:request, uri, address, opts[:request_headers]})
      {:ok, %{status: 200, headers: [], body: body}}
    end

    assert {:ok, %{authorization_servers: [first, second]}} =
             ProtectedResourceMetadata.discover("https://mcp.example/api",
               dns_resolver: public_dns(),
               http_client: client
             )

    assert first == %{
             issuer: "https://auth.example",
             metadata_endpoint: nil,
             scopes_supported: nil,
             audience: nil
           }

    assert second.issuer == "https://other.example"
    assert second.metadata_endpoint == "https://other.example/metadata"
    assert second.scopes_supported == ["tools:read"]

    assert_received {:request, uri, @public_address, headers}
    assert uri.path == "/.well-known/oauth-protected-resource/api"
    refute Enum.any?(headers, fn {name, _value} -> name in ["authorization", "cookie"] end)
  end

  test "falls back from path-based to root protected-resource metadata" do
    parent = self()

    client = fn uri, _address, _opts ->
      send(parent, {:request_path, uri.path})

      case uri.path do
        "/.well-known/oauth-protected-resource/api" ->
          {:ok, %{status: 404, headers: [], body: ""}}

        "/.well-known/oauth-protected-resource" ->
          {:ok,
           %{
             status: 200,
             headers: [],
             body: Jason.encode!(%{"authorization_servers" => ["https://auth.example"]})
           }}
      end
    end

    assert {:ok, %{authorization_servers: [%{issuer: "https://auth.example"}]}} =
             ProtectedResourceMetadata.discover("https://mcp.example/api",
               dns_resolver: public_dns(),
               http_client: client
             )

    assert_received {:request_path, "/.well-known/oauth-protected-resource/api"}
    assert_received {:request_path, "/.well-known/oauth-protected-resource"}
  end

  test "rejects HTTP resources and private DNS before requesting" do
    assert {:error, :https_required} =
             ProtectedResourceMetadata.discover("http://mcp.example/api")

    dns = fn _host, _timeout -> {:ok, [{127, 0, 0, 1}]} end
    client = fn _uri, _address, _opts -> flunk("request must not be made") end

    assert {:error, {:metadata_fetch_error, :non_public_address}} =
             ProtectedResourceMetadata.discover("https://mcp.example/api",
               dns_resolver: dns,
               http_client: client
             )
  end

  test "returns an error instead of raising for malformed authorization-server entries" do
    client = fn _uri, _address, _opts ->
      {:ok,
       %{
         status: 200,
         headers: [],
         body: Jason.encode!(%{"authorization_servers" => [%{"not_issuer" => true}]})
       }}
    end

    assert {:error, {:invalid_metadata, "Invalid authorization server"}} =
             ProtectedResourceMetadata.discover("https://mcp.example/api",
               dns_resolver: public_dns(),
               http_client: client
             )
  end

  defp public_dns do
    fn _host, _timeout -> {:ok, [@public_address]} end
  end
end
