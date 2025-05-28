#!/usr/bin/env elixir

# OAuth Authorization Example
# 
# This example demonstrates how to use ExMCP with OAuth 2.1 authorization
# for connecting to MCP servers that require authentication.

defmodule OAuthExample do
  require Logger
  
  def run do
    # Configuration for OAuth-protected MCP server
    auth_config = %{
      # OAuth client credentials
      client_id: System.get_env("MCP_CLIENT_ID", "your-client-id"),
      client_secret: System.get_env("MCP_CLIENT_SECRET"),
      
      # Token endpoint for refreshing tokens
      token_endpoint: System.get_env("MCP_TOKEN_ENDPOINT", "https://auth.example.com/oauth/token"),
      
      # Initial token (could be loaded from storage)
      initial_token: get_stored_token(),
      
      # Refresh window - refresh token 5 minutes before expiry
      refresh_window: 300
    }
    
    # Start MCP client with authorization
    {:ok, client} = ExMCP.Client.start_link(
      transport: :http,
      url: System.get_env("MCP_SERVER_URL", "https://api.example.com"),
      auth_config: auth_config,
      name: :oauth_client
    )
    
    Logger.info("Connected to OAuth-protected MCP server")
    
    # The client will automatically:
    # 1. Add Authorization headers to all requests
    # 2. Refresh tokens before they expire
    # 3. Handle 401 responses by refreshing and retrying
    
    # Make authenticated requests
    case ExMCP.Client.list_tools(client) do
      {:ok, %{tools: tools}} ->
        Logger.info("Available tools: #{length(tools)}")
        Enum.each(tools, fn tool ->
          Logger.info("  - #{tool["name"]}: #{tool["description"]}")
        end)
        
      {:error, {:auth_error, reason}} ->
        Logger.error("Authentication failed: #{inspect(reason)}")
        
      {:error, reason} ->
        Logger.error("Request failed: #{inspect(reason)}")
    end
    
    # Example: Call a protected tool
    case ExMCP.Client.call_tool(client, "protected_operation", %{data: "sensitive"}) do
      {:ok, result} ->
        Logger.info("Tool result: #{inspect(result)}")
        
      {:error, {:auth_error, :forbidden}} ->
        Logger.error("Access denied - insufficient permissions")
        
      {:error, reason} ->
        Logger.error("Tool call failed: #{inspect(reason)}")
    end
    
    # Subscribe to token updates
    subscribe_to_token_updates(client)
    
    # Keep running to demonstrate token refresh
    Logger.info("Waiting for token refresh... (press Ctrl+C to exit)")
    Process.sleep(:infinity)
  end
  
  defp get_stored_token do
    # In a real application, load token from secure storage
    case System.get_env("MCP_ACCESS_TOKEN") do
      nil ->
        nil
        
      token ->
        %{
          "access_token" => token,
          "refresh_token" => System.get_env("MCP_REFRESH_TOKEN"),
          "expires_in" => 3600,
          "token_type" => "Bearer"
        }
    end
  end
  
  defp subscribe_to_token_updates(client) do
    # Get the token manager from client state
    state = :sys.get_state(client)
    
    if state.token_manager do
      # Subscribe to token updates
      ExMCP.Authorization.TokenManager.subscribe(state.token_manager)
      
      # Handle token update notifications
      spawn(fn -> 
        handle_token_updates()
      end)
    end
  end
  
  defp handle_token_updates do
    receive do
      {:token_refreshed, new_token} ->
        Logger.info("Token refreshed successfully")
        # In a real app, save the new token to secure storage
        save_token(new_token)
        handle_token_updates()
        
      {:token_refresh_failed, reason} ->
        Logger.error("Token refresh failed: #{inspect(reason)}")
        # Handle refresh failure - maybe prompt for re-authentication
        handle_token_updates()
    end
  end
  
  defp save_token(token) do
    # In a real application, save to secure storage
    Logger.info("Would save new token with expiry: #{token["expires_in"]}s")
  end
end

# Run the example
OAuthExample.run()