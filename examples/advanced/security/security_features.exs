#!/usr/bin/env elixir

# Security Example
# Demonstrates MCP security features including:
# - Origin validation for DNS rebinding protection
# - HTTPS enforcement
# - Authentication requirements
# - Secure localhost binding

defmodule SecurityExampleServer do
  @moduledoc """
  Example MCP server demonstrating security features.
  """
  
  use ExMCP.Server.Handler
  
  @impl true
  def init(_args) do
    {:ok, %{requests: 0}}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    result = %{
      protocolVersion: "2025-03-26",
      serverInfo: %{
        name: "security-example-server",
        version: "1.0.0",
        description: "Demonstrates MCP security features"
      },
      capabilities: %{
        tools: %{}
      }
    }
    
    {:ok, result, state}
  end
  
  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "secure_operation",
        description: "A tool that requires authentication",
        inputSchema: %{
          type: "object",
          properties: %{
            data: %{type: "string"}
          },
          required: ["data"]
        }
      }
    ]
    
    {:ok, tools, nil, state}
  end
  
  @impl true
  def handle_call_tool("secure_operation", %{"data" => data}, state) do
    # This tool would only be accessible with proper authentication
    new_state = %{state | requests: state.requests + 1}
    
    result = %{
      processed: data,
      requestCount: new_state.requests,
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }
    
    {:ok, result, new_state}
  end
  
  @impl true
  def handle_call_tool(_name, _args, state) do
    {:error, "Unknown tool", state}
  end
  
  @impl true
  def handle_list_resources(_cursor, state) do
    {:ok, [], nil, state}
  end
  
  @impl true
  def handle_read_resource(_uri, state) do
    {:error, "Resource not found", state}
  end
  
  @impl true
  def handle_list_prompts(_cursor, state) do
    {:ok, [], nil, state}
  end
  
  @impl true
  def handle_get_prompt(_name, _arguments, state) do
    {:error, "Prompt not found", state}
  end
  
  @impl true
  def handle_list_resource_templates(_cursor, state) do
    {:ok, [], nil, state}
  end
end

defmodule SecurityExampleRunner do
  def demo do
    IO.puts("""
    
    MCP Security Features Example
    =============================
    
    This example demonstrates various security configurations:
    """)
    
    # Example 1: Secure server with origin validation
    demo_origin_validation()
    
    # Example 2: HTTPS enforcement
    demo_https_enforcement()
    
    # Example 3: Localhost binding security
    demo_localhost_binding()
    
    # Example 4: Authentication requirements
    demo_authentication()
  end
  
  defp demo_origin_validation do
    IO.puts("\n1. Origin Validation (DNS Rebinding Protection)")
    IO.puts("   ============================================")
    
    # Start a secure server with origin validation
    {:ok, server} = ExMCP.SecureServer.start_link(
      server_id: "origin-validation-example",
      handler: SecurityExampleServer,
      transport: :stdio,
      name: :origin_validation_server,
      security: %{
        validate_origin: true,
        allowed_origins: ["http://localhost:3000", "https://app.example.com"],
        require_auth: false  # Disable auth for this example
      }
    )
    
    # Simulate client with allowed origin
    {:ok, client1} = ExMCP.Client.start_link(
      transport: :stdio,
      server: :origin_validation_server,
      client_info: %{
        name: "allowed-client",
        version: "1.0.0"
      },
      metadata: %{
        origin: "http://localhost:3000"
      }
    )
    
    Process.sleep(100)
    
    # This should succeed
    case ExMCP.Client.call_tool(client1, "secure_operation", %{"data" => "test"}) do
      {:ok, result} ->
        IO.puts("   ✅ Allowed origin succeeded: #{inspect(result)}")
      {:error, reason} ->
        IO.puts("   ❌ Unexpected error: #{inspect(reason)}")
    end
    
    # Clean up
    GenServer.stop(client1)
    GenServer.stop(server)
    
    IO.puts("   Origin validation protects against DNS rebinding attacks")
  end
  
  defp demo_https_enforcement do
    IO.puts("\n2. HTTPS Enforcement")
    IO.puts("   ==================")
    
    # Note: This is a conceptual example - actual HTTPS requires certificates
    security_config = ExMCP.Security.secure_defaults("https://api.example.com")
    
    IO.puts("   Security config for HTTPS URL:")
    IO.inspect(security_config, pretty: true, limit: :infinity)
    
    # Validate HTTPS requirement
    case ExMCP.Security.enforce_https_requirement("http://api.example.com") do
      {:error, :https_required} ->
        IO.puts("   ✅ HTTP rejected for non-localhost URL")
      _ ->
        IO.puts("   ❌ HTTPS not enforced")
    end
    
    # Localhost exception
    case ExMCP.Security.enforce_https_requirement("http://localhost:8080") do
      :ok ->
        IO.puts("   ✅ HTTP allowed for localhost")
      _ ->
        IO.puts("   ❌ Localhost exception not working")
    end
  end
  
  defp demo_localhost_binding do
    IO.puts("\n3. Localhost Binding Security")
    IO.puts("   ============================")
    
    # Secure localhost binding (allowed without auth)
    {:ok, server1} = ExMCP.SecureServer.start_link(
      server_id: "localhost-binding",
      handler: SecurityExampleServer,
      transport: :stdio,
      binding: "127.0.0.1",
      name: :localhost_server,
      security: %{
        require_auth: false
      }
    )
    
    IO.puts("   ✅ Server bound to localhost without authentication")
    
    GenServer.stop(server1)
    
    # Non-localhost binding requires authentication
    result = ExMCP.SecureServer.start_link(
      server_id: "public-binding",
      handler: SecurityExampleServer,
      transport: :stdio,
      binding: "0.0.0.0",
      security: %{
        require_auth: false  # This will be rejected
      }
    )
    
    case result do
      {:error, {:security_validation_failed, :authentication_required_for_non_localhost_binding}} ->
        IO.puts("   ✅ Non-localhost binding rejected without authentication")
      _ ->
        IO.puts("   ❌ Security validation not working properly")
    end
  end
  
  defp demo_authentication do
    IO.puts("\n4. Authentication Requirements")
    IO.puts("   ============================")
    
    # Start secure server requiring authentication
    {:ok, server} = ExMCP.SecureServer.start_link(
      server_id: "auth-required-server",
      handler: SecurityExampleServer,
      transport: :stdio,
      name: :auth_server,
      security: %{
        require_auth: true,
        trusted_issuers: ["https://auth.example.com"],
        validate_origin: false  # Disable for this example
      }
    )
    
    # Client without authentication
    {:ok, client} = ExMCP.Client.start_link(
      transport: :stdio,
      server: :auth_server
    )
    
    Process.sleep(100)
    
    # This should fail due to missing authentication
    case ExMCP.Client.call_tool(client, "secure_operation", %{"data" => "test"}) do
      {:error, %{"error" => %{"code" => -32001, "message" => message}}} ->
        IO.puts("   ✅ Unauthenticated request rejected: #{message}")
      _ ->
        IO.puts("   ❌ Authentication not enforced")
    end
    
    # Clean up
    GenServer.stop(client)
    GenServer.stop(server)
    
    IO.puts("""
    
       In production, you would:
       - Configure OAuth 2.1 or API key authentication
       - Set up trusted token issuers
       - Implement token introspection
       - Enable audit logging
    """)
  end
end

# Run the security demonstration
SecurityExampleRunner.demo()

IO.puts("""

Security Best Practices Summary:
================================

1. **Always use HTTPS** in production (except localhost)
2. **Enable origin validation** to prevent DNS rebinding
3. **Bind to localhost** (127.0.0.1) when possible
4. **Require authentication** for non-localhost deployments
5. **Validate all tokens** with trusted issuers
6. **Maintain audit logs** of all operations
7. **Use secure defaults** provided by ExMCP.Security

For more information, see the MCP specification security section.
""")