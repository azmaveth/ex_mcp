defmodule ExMCP.Authorization.ErrorHandlerTest do
  @moduledoc """
  Tests for authorization error handling according to MCP specification.
  """
  use ExUnit.Case, async: true
  
  alias ExMCP.Authorization.ErrorHandler
  
  describe "handle_auth_error/4" do
    test "handles 401 Unauthorized with WWW-Authenticate header" do
      headers = [
        {"www-authenticate", ~s(Bearer realm="https://auth.example.com", error="invalid_token")}
      ]
      
      result = ErrorHandler.handle_auth_error(401, headers, "", %{})
      
      assert {:retry, %{action: :authorize}} = result
    end
    
    test "handles 401 with refresh token available" do
      headers = [
        {"www-authenticate", ~s(Bearer error="invalid_token")}
      ]
      
      state = %{refresh_token: "refresh-token-123"}
      
      result = ErrorHandler.handle_auth_error(401, headers, "", state)
      
      assert {:retry, %{action: :refresh_token, refresh_token: "refresh-token-123"}} = result
    end
    
    test "handles 401 without WWW-Authenticate" do
      result = ErrorHandler.handle_auth_error(401, [], "", %{})
      
      assert {:error, :unauthorized_no_auth_info} = result
    end
    
    test "handles 403 Forbidden" do
      body = ~s({"error": "insufficient_scope", "error_description": "Need admin scope"})
      
      result = ErrorHandler.handle_auth_error(403, [], body, %{})
      
      assert {:error, {:forbidden, %{error: "insufficient_scope"}}} = result
    end
    
    test "handles other 4xx errors" do
      result = ErrorHandler.handle_auth_error(400, [], "", %{})
      
      assert {:error, {:auth_error, 400}} = result
    end
    
    test "ignores non-auth errors" do
      result = ErrorHandler.handle_auth_error(500, [], "", %{})
      
      assert :ok = result
    end
  end
  
  describe "handle_oauth_error/1" do
    test "parses standard OAuth error response" do
      response = %{
        "error" => "invalid_request",
        "error_description" => "Missing required parameter: code",
        "error_uri" => "https://example.com/docs/errors#invalid_request"
      }
      
      result = ErrorHandler.handle_oauth_error(response)
      
      assert {:error, {:invalid_request, "Missing required parameter: code"}} = result
    end
    
    test "handles error without description" do
      response = %{"error" => "server_error"}
      
      result = ErrorHandler.handle_oauth_error(response)
      
      assert {:error, {:server_error, ""}} = result
    end
    
    test "handles invalid OAuth response" do
      response = %{"message" => "Something went wrong"}
      
      result = ErrorHandler.handle_oauth_error(response)
      
      assert {:error, {:invalid_oauth_response, ^response}} = result
    end
  end
  
  describe "WWW-Authenticate parsing" do
    test "parses multiple parameters correctly" do
      headers = [
        {"www-authenticate", 
         ~s(Bearer realm="https://auth.example.com", error="invalid_token", scope="read write")}
      ]
      
      {:retry, params} = ErrorHandler.handle_auth_error(401, headers, "", %{})
      
      assert params[:action] == :authorize
    end
    
    test "handles malformed WWW-Authenticate header" do
      headers = [
        {"www-authenticate", "Bearer malformed"}
      ]
      
      result = ErrorHandler.handle_auth_error(401, headers, "", %{})
      
      assert {:error, :unauthorized_no_auth_info} = result
    end
  end
end