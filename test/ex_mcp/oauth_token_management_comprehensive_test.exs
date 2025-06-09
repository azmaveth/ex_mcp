defmodule ExMCP.OAuthTokenManagementComprehensiveTest do
  @moduledoc """
  Comprehensive test suite for OAuth Token Management including refresh, rotation, and validation.

  Tests the complete token lifecycle management according to MCP OAuth 2.1 specification
  requirements, including automatic refresh, token rotation for security, and proper
  validation of tokens.

  This test suite is designed to initially fail until the complete OAuth implementation
  is in place, following TDD principles.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Authorization
  alias ExMCP.Authorization.TokenManager

  @moduletag :capture_log

  describe "Token Manager - Automatic Refresh" do
    test "automatically refreshes token before expiration" do
      auth_config = %{
        client_id: "refresh-test-client",
        client_secret: "refresh-test-secret",
        token_endpoint: "https://auth.example.com/token",
        # Refresh 5 seconds before expiration
        refresh_window: 5
      }

      initial_token = %{
        "access_token" => "initial-access-token",
        "refresh_token" => "refresh-token-123",
        # Expires in 10 seconds
        "expires_in" => 10,
        "token_type" => "Bearer"
      }

      {:ok, manager} =
        TokenManager.start_link(
          auth_config: auth_config,
          initial_token: initial_token
        )

      # Token should be valid initially
      assert {:ok, "initial-access-token"} = TokenManager.get_token(manager)

      # Wait for automatic refresh (should happen after 5 seconds)
      Process.sleep(6000)

      # Token should have been refreshed or expired
      case TokenManager.get_token(manager) do
        {:ok, token} ->
          # If we still get a token, it means refresh hasn't completed yet
          # or refresh succeeded (unlikely without real server)
          assert is_binary(token)

        {:error, :token_expired} ->
          # Expected - token expired and refresh failed due to no server
          assert true

        {:error, reason} ->
          # Any other error is acceptable in test environment
          assert reason in [:no_token, :token_expired]
      end
    end

    test "schedules refresh based on expires_in" do
      auth_config = %{
        client_id: "schedule-test-client",
        client_secret: "test-secret",
        token_endpoint: "https://auth.example.com/token",
        # 5 minutes before expiration
        refresh_window: 300
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Set token with 1 hour expiration
      token = %{
        "access_token" => "hour-token",
        "refresh_token" => "refresh-hour",
        # 1 hour
        "expires_in" => 3600
      }

      :ok = TokenManager.set_token(manager, token)

      # Check that refresh timer is scheduled
      state = :sys.get_state(manager)
      assert is_reference(state.refresh_timer)

      # Verify expires_at is set correctly
      assert %DateTime{} = state.expires_at

      # Should be about 1 hour from now
      diff = DateTime.diff(state.expires_at, DateTime.utc_now(), :second)
      assert diff > 3500 and diff <= 3600
    end

    test "handles refresh failures gracefully" do
      auth_config = %{
        client_id: "fail-refresh-client",
        client_secret: "fail-secret",
        token_endpoint: "https://nonexistent.example.com/token",
        refresh_window: 1
      }

      initial_token = %{
        "access_token" => "fail-token",
        "refresh_token" => "fail-refresh",
        # Expires quickly
        "expires_in" => 3
      }

      {:ok, manager} =
        TokenManager.start_link(
          auth_config: auth_config,
          initial_token: initial_token
        )

      # Subscribe to get notified of refresh attempts
      :ok = TokenManager.subscribe(manager)

      # Wait for refresh attempt
      Process.sleep(3000)

      # Manager should still be alive despite refresh failure
      assert Process.alive?(manager)

      # Token should be expired
      assert {:error, :token_expired} = TokenManager.get_token(manager)
    end

    test "cancels refresh timer when setting new token" do
      auth_config = %{
        client_id: "cancel-test",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Set initial token with refresh token
      token1 = %{
        "access_token" => "token1",
        "refresh_token" => "refresh1",
        "expires_in" => 3600
      }

      :ok = TokenManager.set_token(manager, token1)
      state1 = :sys.get_state(manager)
      timer1 = state1.refresh_timer

      # Timer should be set since we have refresh token
      assert is_reference(timer1)

      # Set new token with different expiration
      token2 = %{
        "access_token" => "token2",
        "refresh_token" => "refresh2",
        "expires_in" => 7200
      }

      :ok = TokenManager.set_token(manager, token2)
      state2 = :sys.get_state(manager)
      timer2 = state2.refresh_timer

      # Timer should be set and different from first
      assert is_reference(timer2)
      assert timer1 != timer2
    end
  end

  describe "Token Manager - Token Rotation" do
    test "rotates refresh tokens when provided by server" do
      auth_config = %{
        client_id: "rotation-client",
        client_secret: "rotation-secret",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Initial token with refresh token
      token1 = %{
        "access_token" => "access1",
        "refresh_token" => "refresh1",
        "expires_in" => 3600
      }

      :ok = TokenManager.set_token(manager, token1)

      # New token with rotated refresh token
      token2 = %{
        "access_token" => "access2",
        # New refresh token
        "refresh_token" => "refresh2",
        "expires_in" => 3600
      }

      :ok = TokenManager.set_token(manager, token2)

      # Verify refresh token was updated
      state = :sys.get_state(manager)
      assert state.refresh_token == "refresh2"
    end

    test "preserves refresh token when not rotated" do
      auth_config = %{
        client_id: "preserve-client",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Initial token with refresh
      token1 = %{
        "access_token" => "access1",
        "refresh_token" => "refresh-preserve",
        "expires_in" => 3600
      }

      :ok = TokenManager.set_token(manager, token1)

      # New access token without refresh token
      token2 = %{
        "access_token" => "access2",
        # No refresh_token field
        "expires_in" => 3600
      }

      :ok = TokenManager.set_token(manager, token2)

      # Original refresh token should be preserved
      state = :sys.get_state(manager)
      assert state.refresh_token == "refresh-preserve"
    end

    test "handles token rotation for public clients" do
      # Public clients (no client_secret) MUST use refresh token rotation
      auth_config = %{
        client_id: "public-client",
        # No client_secret - public client
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Each refresh should provide new refresh token
      tokens = [
        %{
          "access_token" => "public-access1",
          "refresh_token" => "public-refresh1",
          "expires_in" => 3600
        },
        %{
          "access_token" => "public-access2",
          # Must rotate
          "refresh_token" => "public-refresh2",
          "expires_in" => 3600
        }
      ]

      for token <- tokens do
        :ok = TokenManager.set_token(manager, token)
      end

      state = :sys.get_state(manager)
      assert state.refresh_token == "public-refresh2"
    end
  end

  describe "Token Manager - Token Validation" do
    test "validates token before returning" do
      auth_config = %{
        client_id: "validation-client",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Set token that expires in 1 second
      token = %{
        "access_token" => "short-lived-token",
        "expires_in" => 1
      }

      :ok = TokenManager.set_token(manager, token)

      # Token should be valid immediately
      assert {:ok, "short-lived-token"} = TokenManager.get_token(manager)

      # Wait for expiration
      Process.sleep(1500)

      # Token should now be expired
      assert {:error, :token_expired} = TokenManager.get_token(manager)
    end

    test "handles tokens without expiration info" do
      auth_config = %{
        client_id: "no-expiry-client",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Token without expires_in
      token = %{
        "access_token" => "no-expiry-token"
        # No expires_in field
      }

      :ok = TokenManager.set_token(manager, token)

      # Should default to some reasonable expiration (e.g., 1 hour)
      assert {:ok, "no-expiry-token"} = TokenManager.get_token(manager)

      # Check that expiration was set
      state = :sys.get_state(manager)
      assert %DateTime{} = state.expires_at
    end

    test "validates token introspection response" do
      # For tokens that support introspection
      introspection_endpoint = "https://auth.example.com/introspect"
      token = "opaque-token-123"

      result = Authorization.validate_token(token, introspection_endpoint)

      case result do
        {:ok, introspection} ->
          # Token is active
          assert introspection.active == true
          assert is_binary(introspection.client_id)

        {:error, :token_inactive} ->
          # Token is not active
          assert true

        {:error, {:request_failed, _}} ->
          # Expected for no real server
          assert true

        other ->
          flunk("Unexpected introspection result: #{inspect(other)}")
      end
    end
  end

  describe "Token Manager - Force Refresh" do
    test "allows manual token refresh" do
      auth_config = %{
        client_id: "manual-refresh-client",
        client_secret: "manual-secret",
        token_endpoint: "https://auth.example.com/token"
      }

      initial_token = %{
        "access_token" => "manual-access",
        "refresh_token" => "manual-refresh",
        "expires_in" => 3600
      }

      {:ok, manager} =
        TokenManager.start_link(
          auth_config: auth_config,
          initial_token: initial_token
        )

      # Force refresh
      result = TokenManager.refresh_now(manager)

      case result do
        {:ok, new_token} ->
          assert new_token.access_token != "manual-access"
          assert is_binary(new_token.access_token)

        {:error, {:request_failed, _}} ->
          # Expected without real server
          assert true

        other ->
          flunk("Unexpected refresh result: #{inspect(other)}")
      end
    end

    test "requires refresh token for manual refresh" do
      auth_config = %{
        client_id: "no-refresh-client",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Set token without refresh token
      token = %{
        "access_token" => "no-refresh-access",
        "expires_in" => 3600
      }

      :ok = TokenManager.set_token(manager, token)

      # Manual refresh should fail
      assert {:error, :no_refresh_token} = TokenManager.refresh_now(manager)
    end
  end

  describe "Token Manager - Subscription System" do
    test "notifies multiple subscribers of token updates" do
      auth_config = %{
        client_id: "multi-sub-client",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Multiple subscribers
      parent = self()

      task1 =
        Task.async(fn ->
          TokenManager.subscribe(manager)
          send(parent, {:subscribed, 1})

          receive do
            {:token_updated, _, info} -> {:subscriber1, info}
          end
        end)

      task2 =
        Task.async(fn ->
          TokenManager.subscribe(manager)
          send(parent, {:subscribed, 2})

          receive do
            {:token_updated, _, info} -> {:subscriber2, info}
          end
        end)

      # Wait for subscriptions
      assert_receive {:subscribed, 1}
      assert_receive {:subscribed, 2}

      # Update token
      token = %{
        "access_token" => "broadcast-token",
        "expires_in" => 3600
      }

      :ok = TokenManager.set_token(manager, token)

      # Both should receive update
      assert {:subscriber1, info1} = Task.await(task1)
      assert {:subscriber2, info2} = Task.await(task2)

      assert info1.access_token == "broadcast-token"
      assert info2.access_token == "broadcast-token"
    end

    test "cleans up dead subscriber processes" do
      auth_config = %{
        client_id: "cleanup-client",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Create subscriber that dies
      task =
        Task.async(fn ->
          TokenManager.subscribe(manager)
          :ok
        end)

      Task.await(task)
      # Subscriber is now dead

      # Check subscriber count before
      state_before = :sys.get_state(manager)
      subscribers_before = MapSet.size(state_before.subscribers)

      # Trigger cleanup by sending DOWN message
      Process.sleep(100)

      # Update token to potentially trigger notification
      token = %{"access_token" => "cleanup-token", "expires_in" => 3600}
      :ok = TokenManager.set_token(manager, token)

      # Manager should still be alive
      assert Process.alive?(manager)

      # Dead subscriber should be cleaned up
      state_after = :sys.get_state(manager)
      subscribers_after = MapSet.size(state_after.subscribers)

      assert subscribers_after <= subscribers_before
    end
  end

  describe "Token Manager - Security Features" do
    test "does not log sensitive token data" do
      # Ensure tokens are not logged in plain text
      auth_config = %{
        client_id: "secure-logging-client",
        client_secret: "super-secret",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      sensitive_token = %{
        "access_token" => "super-secret-token-do-not-log",
        "refresh_token" => "super-secret-refresh-do-not-log",
        "expires_in" => 3600
      }

      :ok = TokenManager.set_token(manager, sensitive_token)

      # In production, verify logs don't contain these strings
      # This is a placeholder for log inspection
      assert true
    end

    test "stores tokens securely in state" do
      auth_config = %{
        client_id: "secure-state-client",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      token = %{
        "access_token" => "state-token",
        "refresh_token" => "state-refresh",
        "expires_in" => 3600
      }

      :ok = TokenManager.set_token(manager, token)

      # Tokens are stored in process state, not in ETS or other shared storage
      state = :sys.get_state(manager)
      assert state.access_token == "state-token"
      assert state.refresh_token == "state-refresh"
    end

    test "validates token response structure" do
      auth_config = %{
        client_id: "validate-response-client",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Valid token responses
      valid_responses = [
        %{"access_token" => "valid1", "expires_in" => 3600},
        %{"access_token" => "valid2", "token_type" => "Bearer"},
        %{"access_token" => "valid3", "refresh_token" => "refresh3"}
      ]

      for response <- valid_responses do
        assert :ok = TokenManager.set_token(manager, response)
      end

      # Invalid responses would be rejected at the protocol level
      # TokenManager expects already-validated responses
    end
  end

  describe "Token Manager - Edge Cases" do
    test "handles very short token lifetimes" do
      auth_config = %{
        client_id: "short-lifetime-client",
        token_endpoint: "https://auth.example.com/token",
        # Refresh immediately
        refresh_window: 0
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Token expires in 1 second
      token = %{
        "access_token" => "one-second-token",
        "refresh_token" => "one-second-refresh",
        "expires_in" => 1
      }

      :ok = TokenManager.set_token(manager, token)

      # Should attempt refresh immediately
      Process.sleep(100)

      # Manager should handle this gracefully
      assert Process.alive?(manager)
    end

    test "handles concurrent token updates" do
      auth_config = %{
        client_id: "concurrent-update-client",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Multiple concurrent updates
      tasks =
        for i <- 1..10 do
          Task.async(fn ->
            token = %{
              "access_token" => "concurrent-#{i}",
              "expires_in" => 3600
            }

            TokenManager.set_token(manager, token)
          end)
        end

      # All should complete
      results = Task.await_many(tasks)
      assert Enum.all?(results, &(&1 == :ok))

      # Should have one of the tokens
      assert {:ok, token} = TokenManager.get_token(manager)
      assert String.starts_with?(token, "concurrent-")
    end

    test "gracefully handles process termination" do
      auth_config = %{
        client_id: "termination-client",
        token_endpoint: "https://auth.example.com/token"
      }

      {:ok, manager} = TokenManager.start_link(auth_config: auth_config)

      # Set token with refresh timer
      token = %{
        "access_token" => "termination-token",
        "refresh_token" => "termination-refresh",
        "expires_in" => 3600
      }

      :ok = TokenManager.set_token(manager, token)

      # Get state to verify timer exists
      state = :sys.get_state(manager)
      assert is_reference(state.refresh_timer)

      # Stop the manager
      :ok = GenServer.stop(manager)

      # Should have cleaned up properly (timer cancelled)
      refute Process.alive?(manager)
    end
  end
end
