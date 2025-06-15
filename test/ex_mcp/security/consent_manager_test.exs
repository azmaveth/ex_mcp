defmodule ExMCP.Security.ConsentManagerTest do
  use ExUnit.Case

  @moduletag :security

  alias ExMCP.Security.ConsentManager

  defmodule TestApprovalHandler do
    @behaviour ExMCP.Approval

    @impl true
    def request_approval(:dynamic_client_consent, data, _opts) do
      # Auto-approve for tests
      {:approved, data}
    end

    def request_approval(_, _, _), do: {:denied, "Not implemented"}
  end

  setup do
    # Start consent manager for each test
    {:ok, _pid} = ConsentManager.start_link(approval_handler: TestApprovalHandler)

    :ok
  end

  describe "request_consent/2" do
    test "creates consent record for dynamic client" do
      client_metadata = %{
        client_id: "test-client",
        client_name: "Test Client",
        scope: ["mcp:read", "mcp:write"]
      }

      assert {:ok, consent} = ConsentManager.request_consent(client_metadata, "user-123")
      assert consent.client_id == "test-client"
      assert consent.user_id == "user-123"
      assert consent.scopes == ["mcp:read", "mcp:write"]
      assert consent.revoked == false
    end

    test "returns existing valid consent" do
      client_metadata = %{
        client_id: "test-client",
        scope: ["mcp:read"]
      }

      # Create initial consent
      {:ok, consent1} = ConsentManager.request_consent(client_metadata, "user-123")

      # Request again - should return same consent
      {:ok, consent2} = ConsentManager.request_consent(client_metadata, "user-123")

      assert consent1.granted_at == consent2.granted_at
    end
  end

  describe "check_consent/3" do
    test "validates existing consent" do
      client_metadata = %{
        client_id: "test-client",
        scope: ["mcp:read", "mcp:write"]
      }

      {:ok, _} = ConsentManager.request_consent(client_metadata, "user-123")

      assert {:ok, :valid} =
               ConsentManager.check_consent("test-client", "user-123", ["mcp:read"])
    end

    test "rejects insufficient scope" do
      client_metadata = %{
        client_id: "test-client",
        scope: ["mcp:read"]
      }

      {:ok, _} = ConsentManager.request_consent(client_metadata, "user-123")

      assert {:error, :insufficient_scope} =
               ConsentManager.check_consent("test-client", "user-123", ["mcp:write"])
    end

    test "rejects revoked consent" do
      client_metadata = %{client_id: "test-client", scope: ["mcp:read"]}

      {:ok, _} = ConsentManager.request_consent(client_metadata, "user-123")
      :ok = ConsentManager.revoke_consent("test-client", "user-123")

      assert {:error, :revoked} =
               ConsentManager.check_consent("test-client", "user-123", ["mcp:read"])
    end
  end

  describe "list_user_consents/1" do
    test "returns all active consents for user" do
      # Create multiple consents
      {:ok, _} =
        ConsentManager.request_consent(
          %{client_id: "client-1", scope: ["mcp:read"]},
          "user-123"
        )

      {:ok, _} =
        ConsentManager.request_consent(
          %{client_id: "client-2", scope: ["mcp:write"]},
          "user-123"
        )

      {:ok, _} =
        ConsentManager.request_consent(
          %{client_id: "client-3", scope: ["mcp:read"]},
          # Different user
          "user-456"
        )

      consents = ConsentManager.list_user_consents("user-123")
      assert length(consents) == 2
      assert Enum.all?(consents, fn c -> c.user_id == "user-123" end)
    end
  end
end
