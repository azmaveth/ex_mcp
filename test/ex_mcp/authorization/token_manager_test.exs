defmodule ExMCP.Authorization.TokenManagerTest do
  @moduledoc """
  Tests for the OAuth token manager with automatic refresh.
  """
  use ExUnit.Case, async: true
  
  alias ExMCP.Authorization.TokenManager
  
  @moduletag :capture_log
  
  setup do
    # Mock auth config
    auth_config = %{
      client_id: "test-client",
      client_secret: "test-secret",
      token_endpoint: "https://auth.example.com/token"
    }
    
    {:ok, auth_config: auth_config}
  end
  
  describe "token storage and retrieval" do
    test "stores and retrieves access token", %{auth_config: auth_config} do
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      token_response = %{
        "access_token" => "test-token-123",
        "token_type" => "Bearer",
        "expires_in" => 3600
      }
      
      :ok = TokenManager.set_token(manager, token_response)
      
      assert {:ok, "test-token-123"} = TokenManager.get_token(manager)
    end
    
    test "returns error when no token set", %{auth_config: auth_config} do
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      assert {:error, :no_token} = TokenManager.get_token(manager)
    end
    
    test "detects expired tokens", %{auth_config: auth_config} do
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      # Set token that expires immediately
      token_response = %{
        "access_token" => "expired-token",
        "expires_in" => 0
      }
      
      :ok = TokenManager.set_token(manager, token_response)
      Process.sleep(100)
      
      assert {:error, :token_expired} = TokenManager.get_token(manager)
    end
    
    test "initializes with provided token", %{auth_config: auth_config} do
      initial_token = %{
        "access_token" => "initial-token",
        "refresh_token" => "refresh-123",
        "expires_in" => 3600
      }
      
      {:ok, manager} = TokenManager.start_link(
        auth_config: auth_config,
        initial_token: initial_token
      )
      
      assert {:ok, "initial-token"} = TokenManager.get_token(manager)
    end
  end
  
  describe "token information" do
    test "returns full token info", %{auth_config: auth_config} do
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      token_response = %{
        "access_token" => "test-token",
        "token_type" => "Bearer",
        "expires_in" => 3600,
        "scope" => "read write"
      }
      
      :ok = TokenManager.set_token(manager, token_response)
      
      assert {:ok, info} = TokenManager.get_token_info(manager)
      assert info.access_token == "test-token"
      assert info.token_type == "Bearer"
      assert info.scope == "read write"
      assert %DateTime{} = info.expires_at
    end
  end
  
  describe "automatic refresh" do
    @tag :skip
    test "schedules refresh before expiration", %{auth_config: _auth_config} do
      # This test would require mocking the authorization module
      # to simulate successful token refresh
    end
    
    test "preserves refresh token across updates", %{auth_config: auth_config} do
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      # Set initial token with refresh token
      initial = %{
        "access_token" => "access-1",
        "refresh_token" => "refresh-1",
        "expires_in" => 3600
      }
      
      :ok = TokenManager.set_token(manager, initial)
      
      # Update with new access token but no refresh token
      update = %{
        "access_token" => "access-2",
        "expires_in" => 3600
      }
      
      :ok = TokenManager.set_token(manager, update)
      
      # Refresh token should be preserved
      state = :sys.get_state(manager)
      assert state.refresh_token == "refresh-1"
    end
  end
  
  describe "subscription" do
    test "notifies subscribers on token update", %{auth_config: auth_config} do
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      :ok = TokenManager.subscribe(manager)
      
      token = %{
        "access_token" => "new-token",
        "expires_in" => 3600
      }
      
      :ok = TokenManager.set_token(manager, token)
      
      assert_receive {:token_updated, ^manager, info}
      assert info.access_token == "new-token"
    end
    
    test "removes subscribers on unsubscribe", %{auth_config: auth_config} do
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      :ok = TokenManager.subscribe(manager)
      :ok = TokenManager.unsubscribe(manager)
      
      token = %{
        "access_token" => "new-token",
        "expires_in" => 3600
      }
      
      :ok = TokenManager.set_token(manager, token)
      
      refute_receive {:token_updated, _, _}
    end
    
    test "handles subscriber process death", %{auth_config: auth_config} do
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      # Subscribe from a temporary process
      task = Task.async(fn ->
        TokenManager.subscribe(manager)
        :ok
      end)
      
      Task.await(task)
      
      # Subscriber process is now dead
      # This shouldn't crash the manager
      token = %{
        "access_token" => "new-token",
        "expires_in" => 3600
      }
      
      :ok = TokenManager.set_token(manager, token)
      
      # Manager should still be alive
      assert Process.alive?(manager)
    end
  end
  
  describe "error handling" do
    test "handles missing expires_in gracefully", %{auth_config: auth_config} do
      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)
      
      token = %{
        "access_token" => "no-expiry-token"
      }
      
      :ok = TokenManager.set_token(manager, token)
      
      # Should default to 1 hour expiration
      assert {:ok, "no-expiry-token"} = TokenManager.get_token(manager)
    end
  end
end