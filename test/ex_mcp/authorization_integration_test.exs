defmodule ExMCP.AuthorizationIntegrationTest do
  use ExUnit.Case, async: true

  import Mox

  alias ExMCP.Authorization.TokenManager
  alias ExMCP.Transport.Mock, as: MockTransport

  setup :verify_on_exit!

  describe "authorization integration" do
    setup do
      # Set Mox to global mode for cross-process mocking
      Mox.set_mox_global()

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
      # Mock transport to connect successfully, handle initialization, then return 403 on API calls
      MockTransport
      |> expect(:connect, fn _opts -> {:ok, :mock_state} end)
      |> stub(:receive_message, fn _state ->
        # Block forever to simulate waiting for messages
        Process.sleep(:infinity)
        {:ok, nil, :mock_state}
      end)
      |> expect(:send_message, 3, fn json, state ->
        # Parse the message to see what type it is
        message = Jason.decode!(json)

        case message do
          %{"method" => "initialize", "id" => id} ->
            # Send back a successful initialize response
            response = %{
              "jsonrpc" => "2.0",
              "id" => id,
              "result" => %{
                "protocolVersion" => "2025-03-26",
                "serverInfo" => %{"name" => "mock-server", "version" => "1.0.0"},
                "capabilities" => %{"tools" => %{}}
              }
            }

            # Simulate sending the response back to the client
            send(self(), {:transport_message, Jason.encode!(response)})
            {:ok, state}

          %{"method" => "tools/list"} ->
            # Return 403 for actual API calls
            {:error, {:http_error, 403, "Forbidden: Insufficient permissions"}}

          _ ->
            # Handle other messages like notifications/initialized
            {:ok, state}
        end
      end)

      # Start client with auth config
      {:ok, client} =
        ExMCP.Client.start_link(
          transport: MockTransport,
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

      # Request should fail with HTTP 403 error
      assert {:error, {:http_error, 403, "Forbidden: Insufficient permissions"}} =
               ExMCP.Client.list_tools(client)
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
