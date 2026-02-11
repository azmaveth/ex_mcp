defmodule ExMCP.Authorization.ClientIdMetadataTest do
  use ExUnit.Case, async: true

  alias ExMCP.Authorization.ClientIdMetadata

  @moduletag :oauth

  @valid_client_id "https://myapp.example.com/client"
  @valid_metadata %{
    "client_id" => "https://myapp.example.com/client",
    "client_name" => "My Test App",
    "redirect_uris" => ["https://myapp.example.com/callback"]
  }

  describe "fetch/2" do
    test "returns {:error, :no_http_client} when no http_client is provided" do
      assert {:error, :no_http_client} = ClientIdMetadata.fetch(@valid_client_id)
    end

    test "returns {:error, :no_http_client} when http_client option is nil" do
      assert {:error, :no_http_client} =
               ClientIdMetadata.fetch(@valid_client_id, http_client: nil)
    end

    test "returns {:ok, metadata} when http_client returns valid JSON" do
      http_client = mock_http_client(200, Jason.encode!(@valid_metadata))

      assert {:ok, metadata} = ClientIdMetadata.fetch(@valid_client_id, http_client: http_client)
      assert metadata["client_id"] == @valid_client_id
      assert metadata["client_name"] == "My Test App"
      assert metadata["redirect_uris"] == ["https://myapp.example.com/callback"]
    end

    test "returns error when http_client returns non-200 status" do
      http_client = mock_http_client(404, "Not Found")

      assert {:error, {:http_error, 404}} =
               ClientIdMetadata.fetch(@valid_client_id, http_client: http_client)
    end

    test "returns error when http_client returns invalid JSON" do
      http_client = mock_http_client(200, "not valid json{{{")

      assert {:error, :invalid_json} =
               ClientIdMetadata.fetch(@valid_client_id, http_client: http_client)
    end

    test "returns error when http_client returns non-map JSON" do
      http_client = mock_http_client(200, Jason.encode!(["not", "a", "map"]))

      assert {:error, :invalid_json} =
               ClientIdMetadata.fetch(@valid_client_id, http_client: http_client)
    end

    test "returns error when http_client returns an error" do
      http_client = mock_http_client_error(:econnrefused)

      assert {:error, :econnrefused} =
               ClientIdMetadata.fetch(@valid_client_id, http_client: http_client)
    end

    test "validates metadata after successful fetch" do
      # Metadata with mismatched client_id
      bad_metadata = Map.put(@valid_metadata, "client_id", "https://other.example.com/client")
      http_client = mock_http_client(200, Jason.encode!(bad_metadata))

      assert {:error, {:client_id_mismatch, _}} =
               ClientIdMetadata.fetch(@valid_client_id, http_client: http_client)
    end

    test "validates required fields after successful fetch" do
      # Metadata missing client_name
      incomplete_metadata = Map.delete(@valid_metadata, "client_name")
      http_client = mock_http_client(200, Jason.encode!(incomplete_metadata))

      assert {:error, {:missing_required_field, "client_name"}} =
               ClientIdMetadata.fetch(@valid_client_id, http_client: http_client)
    end
  end

  describe "validate/2" do
    test "returns :ok with valid metadata matching expected client_id" do
      assert :ok = ClientIdMetadata.validate(@valid_metadata, @valid_client_id)
    end

    test "returns error when client_id is missing" do
      metadata = Map.delete(@valid_metadata, "client_id")

      assert {:error, :missing_client_id} =
               ClientIdMetadata.validate(metadata, @valid_client_id)
    end

    test "returns error when client_id does not match expected" do
      metadata = Map.put(@valid_metadata, "client_id", "https://other.example.com/client")

      assert {:error, {:client_id_mismatch, details}} =
               ClientIdMetadata.validate(metadata, @valid_client_id)

      assert Keyword.get(details, :expected) == @valid_client_id
      assert Keyword.get(details, :actual) == "https://other.example.com/client"
    end

    test "returns error when client_name is missing" do
      metadata = Map.delete(@valid_metadata, "client_name")

      assert {:error, {:missing_required_field, "client_name"}} =
               ClientIdMetadata.validate(metadata, @valid_client_id)
    end

    test "returns error when redirect_uris is missing" do
      metadata = Map.delete(@valid_metadata, "redirect_uris")

      assert {:error, {:missing_required_field, "redirect_uris"}} =
               ClientIdMetadata.validate(metadata, @valid_client_id)
    end

    test "returns :ok when all required fields present with extra optional fields" do
      metadata =
        Map.merge(@valid_metadata, %{
          "client_uri" => "https://myapp.example.com",
          "logo_uri" => "https://myapp.example.com/logo.png",
          "scope" => "mcp:read mcp:write",
          "contacts" => ["admin@example.com"],
          "tos_uri" => "https://myapp.example.com/tos",
          "policy_uri" => "https://myapp.example.com/privacy"
        })

      assert :ok = ClientIdMetadata.validate(metadata, @valid_client_id)
    end
  end

  describe "build_metadata/1" do
    test "builds correct metadata from required opts" do
      metadata =
        ClientIdMetadata.build_metadata(
          client_id: @valid_client_id,
          client_name: "My App",
          redirect_uris: ["https://myapp.example.com/callback"]
        )

      assert metadata["client_id"] == @valid_client_id
      assert metadata["client_name"] == "My App"
      assert metadata["redirect_uris"] == ["https://myapp.example.com/callback"]
    end

    test "includes optional fields when provided" do
      metadata =
        ClientIdMetadata.build_metadata(
          client_id: @valid_client_id,
          client_name: "My App",
          redirect_uris: ["https://myapp.example.com/callback"],
          client_uri: "https://myapp.example.com",
          logo_uri: "https://myapp.example.com/logo.png",
          scope: "mcp:read mcp:write",
          contacts: ["admin@example.com"],
          tos_uri: "https://myapp.example.com/tos",
          policy_uri: "https://myapp.example.com/privacy"
        )

      assert metadata["client_id"] == @valid_client_id
      assert metadata["client_name"] == "My App"
      assert metadata["redirect_uris"] == ["https://myapp.example.com/callback"]
      assert metadata["client_uri"] == "https://myapp.example.com"
      assert metadata["logo_uri"] == "https://myapp.example.com/logo.png"
      assert metadata["scope"] == "mcp:read mcp:write"
      assert metadata["contacts"] == ["admin@example.com"]
      assert metadata["tos_uri"] == "https://myapp.example.com/tos"
      assert metadata["policy_uri"] == "https://myapp.example.com/privacy"
    end

    test "omits nil optional fields" do
      metadata =
        ClientIdMetadata.build_metadata(
          client_id: @valid_client_id,
          client_name: "My App",
          redirect_uris: ["https://myapp.example.com/callback"],
          client_uri: nil,
          logo_uri: nil,
          scope: nil
        )

      assert metadata["client_id"] == @valid_client_id
      assert metadata["client_name"] == "My App"
      assert metadata["redirect_uris"] == ["https://myapp.example.com/callback"]
      refute Map.has_key?(metadata, "client_uri")
      refute Map.has_key?(metadata, "logo_uri")
      refute Map.has_key?(metadata, "scope")
    end

    test "raises KeyError when required client_id is missing" do
      assert_raise KeyError, ~r/client_id/, fn ->
        ClientIdMetadata.build_metadata(
          client_name: "My App",
          redirect_uris: ["https://myapp.example.com/callback"]
        )
      end
    end

    test "raises KeyError when required client_name is missing" do
      assert_raise KeyError, ~r/client_name/, fn ->
        ClientIdMetadata.build_metadata(
          client_id: @valid_client_id,
          redirect_uris: ["https://myapp.example.com/callback"]
        )
      end
    end

    test "raises KeyError when required redirect_uris is missing" do
      assert_raise KeyError, ~r/redirect_uris/, fn ->
        ClientIdMetadata.build_metadata(
          client_id: @valid_client_id,
          client_name: "My App"
        )
      end
    end

    test "built metadata passes validation" do
      metadata =
        ClientIdMetadata.build_metadata(
          client_id: @valid_client_id,
          client_name: "My App",
          redirect_uris: ["https://myapp.example.com/callback"]
        )

      assert :ok = ClientIdMetadata.validate(metadata, @valid_client_id)
    end
  end

  # Helper to create a mock HTTP client module that returns a successful response
  defp mock_http_client(status, body) do
    module_name = :"MockHTTPClient_#{System.unique_integer([:positive])}"

    Module.create(
      module_name,
      quote do
        def get(_url, _headers) do
          {:ok, %{status: unquote(status), body: unquote(body)}}
        end
      end,
      Macro.Env.location(__ENV__)
    )

    module_name
  end

  # Helper to create a mock HTTP client that returns an error
  defp mock_http_client_error(reason) do
    module_name = :"MockHTTPClient_#{System.unique_integer([:positive])}"

    Module.create(
      module_name,
      quote do
        def get(_url, _headers) do
          {:error, unquote(reason)}
        end
      end,
      Macro.Env.location(__ENV__)
    )

    module_name
  end
end
