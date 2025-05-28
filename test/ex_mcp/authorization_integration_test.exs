defmodule ExMCP.AuthorizationIntegrationTest do
  use ExUnit.Case, async: true

  import Mox

  alias ExMCP.Authorization.TokenManager
  alias ExMCP.Transport.MockTransport

  setup :verify_on_exit!

  describe "authorization integration" do
    setup do
      # Set up mock transport
      Application.put_env(:ex_mcp, :transport_modules, %{
        mock: MockTransport
      })

      on_exit(fn ->
        Application.delete_env(:ex_mcp, :transport_modules)
      end)

      :ok
    end

    test "client adds authorization headers to HTTP requests" do
      # Start client with auth config
      {:ok, client} =
        ExMCP.Client.start_link(
          transport: :http,
          url: "https://api.example.com",
          auth_config: %{
            client_id: "test-client",
            client_secret: "secret",
            token_endpoint: "https://auth.example.com/token",
            initial_token: %{
              "access_token" => "test-token",
              "refresh_token" => "refresh-token",
              "expires_in" => 3600
            }
          }
        )

      # Get the token manager from client state
      state = :sys.get_state(client)
      assert state.token_manager != nil

      # Verify token is available
      {:ok, token} = TokenManager.get_token(state.token_manager)
      assert token == "test-token"
    end

    @tag :skip
    test "client refreshes token on 401 response" do
      # Create a test HTTP server that returns 401 on first request
      {:ok, server_pid} = start_test_http_server()

      # Start client with auth config
      {:ok, client} =
        ExMCP.Client.start_link(
          transport: :http,
          url: "http://localhost:#{get_server_port(server_pid)}",
          auth_config: %{
            client_id: "test-client",
            client_secret: "secret",
            token_endpoint: "http://localhost:#{get_server_port(server_pid)}/token",
            initial_token: %{
              "access_token" => "expired-token",
              "refresh_token" => "refresh-token",
              # Already expired
              "expires_in" => -1
            }
          }
        )

      # Make a request that will trigger token refresh
      set_server_response(server_pid, :auth_refresh_scenario)

      # This should succeed after automatic token refresh
      {:ok, _result} = ExMCP.Client.list_tools(client)

      # Verify new token was obtained
      state = :sys.get_state(client)
      {:ok, new_token} = TokenManager.get_token(state.token_manager)
      assert new_token == "new-access-token"

      stop_test_server(server_pid)
    end

    test "client handles 403 forbidden responses" do
      # Start client with auth config
      {:ok, client} =
        ExMCP.Client.start_link(
          transport: :http,
          url: "https://api.example.com",
          auth_config: %{
            client_id: "test-client",
            token_endpoint: "https://auth.example.com/token",
            initial_token: %{
              "access_token" => "valid-token",
              "expires_in" => 3600
            }
          }
        )

      # Mock transport to return 403
      MockTransport
      |> expect(:send_message, fn _json, _state ->
        {:error, {:http_error, 403, "Forbidden: Insufficient permissions"}}
      end)

      # Request should fail with permission error
      assert {:error, {:auth_error, _}} = ExMCP.Client.list_tools(client)
    end
  end

  # Test helpers

  defp start_test_http_server do
    # Would start a test HTTP server here
    # For now, return a fake pid
    pid = spawn(fn -> :timer.sleep(:infinity) end)
    {:ok, pid}
  end

  defp get_server_port(server_pid) do
    GenServer.call(server_pid, :get_port)
  end

  defp set_server_response(server_pid, scenario) do
    GenServer.call(server_pid, {:set_scenario, scenario})
  end

  defp stop_test_server(server_pid) do
    GenServer.stop(server_pid)
  end
end
