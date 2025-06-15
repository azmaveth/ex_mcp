defmodule ExMCP.Security.ClientRegistryTest do
  use ExUnit.Case

  @moduletag :security

  alias ExMCP.Security.ClientRegistry

  setup do
    {:ok, _pid} = ClientRegistry.start_link()
    :ok
  end

  describe "register_client/2" do
    test "registers static client" do
      client_data = %{
        client_id: "static-client",
        name: "Test Static Client",
        version: "1.0.0"
      }

      assert {:ok, client_info} = ClientRegistry.register_client(client_data, :static)
      assert client_info.client_id == "static-client"
      assert client_info.registration_type == :static
      assert %DateTime{} = client_info.registered_at
    end

    test "registers dynamic client with generated ID" do
      client_data = %{
        name: "Dynamic Client"
      }

      assert {:ok, client_info} = ClientRegistry.register_client(client_data, :dynamic)
      assert String.starts_with?(client_info.client_id, "mcp_client_")
      assert client_info.registration_type == :dynamic
    end

    test "prevents duplicate registration" do
      client_data = %{client_id: "duplicate-client"}

      assert {:ok, _} = ClientRegistry.register_client(client_data, :static)

      assert {:error, :client_already_registered} =
               ClientRegistry.register_client(client_data, :static)
    end
  end

  describe "validate_client/2" do
    test "validates registered client" do
      {:ok, _} =
        ClientRegistry.register_client(
          %{client_id: "valid-client"},
          :static
        )

      assert {:ok, client_info} = ClientRegistry.validate_client("valid-client")
      assert client_info.client_id == "valid-client"
    end

    test "rejects unknown client" do
      assert {:error, :unknown_client} = ClientRegistry.validate_client("unknown-client")
    end

    test "enforces trust boundary" do
      {:ok, _} =
        ClientRegistry.register_client(
          %{client_id: "boundary-client", trust_boundary: "production"},
          :static
        )

      assert {:ok, _} = ClientRegistry.validate_client("boundary-client", "production")

      assert {:error, :trust_boundary_violation} =
               ClientRegistry.validate_client("boundary-client", "staging")
    end
  end

  describe "record_request/3" do
    test "maintains request audit trail" do
      {:ok, _} =
        ClientRegistry.register_client(
          %{client_id: "audit-client"},
          :static
        )

      # Record some requests
      ClientRegistry.record_request("audit-client", "tools/list", "req-1")
      ClientRegistry.record_request("audit-client", "tools/call", "req-2")
      ClientRegistry.record_request("other-client", "resources/list", "req-3")

      # Get audit trail
      trail = ClientRegistry.get_client_audit_trail("audit-client")
      assert length(trail) == 2
      assert Enum.all?(trail, fn r -> r.client_id == "audit-client" end)
    end
  end

  describe "revoke_client/2" do
    test "removes client registration" do
      {:ok, _} =
        ClientRegistry.register_client(
          %{client_id: "revoke-client"},
          :static
        )

      assert :ok = ClientRegistry.revoke_client("revoke-client", "Security violation")
      assert {:error, :unknown_client} = ClientRegistry.validate_client("revoke-client")
    end
  end

  describe "list_clients/1" do
    test "filters by registration type" do
      {:ok, _} = ClientRegistry.register_client(%{client_id: "static-1"}, :static)
      {:ok, _} = ClientRegistry.register_client(%{client_id: "dynamic-1"}, :dynamic)
      {:ok, _} = ClientRegistry.register_client(%{client_id: "static-2"}, :static)

      static_clients = ClientRegistry.list_clients(registration_type: :static)
      assert length(static_clients) == 2
      assert Enum.all?(static_clients, fn c -> c.registration_type == :static end)
    end
  end
end
