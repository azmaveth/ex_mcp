#!/usr/bin/env elixir

# Secure MCP Server Example
#
# Demonstrates security best practices implementation:
# - Token validation
# - Client registration and accountability
# - Consent management for dynamic clients
# - Audit trail maintenance

defmodule SecureServerExample do
  @behaviour ExMCP.Server.Handler
  
  require Logger
  
  # Server handler implementation
  
  @impl true
  def init(_args) do
    {:ok, %{
      tools: [
        %{
          name: "secure_operation",
          description: "A security-sensitive operation",
          inputSchema: %{
            type: "object",
            properties: %{
              action: %{type: "string"},
              data: %{type: "string"}
            }
          }
        }
      ]
    }}
  end
  
  @impl true
  def handle_initialize(params, state) do
    server_info = %{
      name: "Secure MCP Server",
      version: "1.0.0",
      protocolVersion: ExMCP.protocol_version()
    }
    
    {:ok, server_info, state}
  end
  
  @impl true
  def handle_list_tools(state) do
    {:ok, state.tools, state}
  end
  
  @impl true
  def handle_call_tool("secure_operation", arguments, state) do
    # This would be called only after security validation
    Logger.info("Executing secure operation: #{inspect(arguments)}")
    
    result = %{
      success: true,
      message: "Operation completed securely",
      audit_id: ExMCP.Protocol.generate_id()
    }
    
    {:ok, result, state}
  end
  
  # Implement other required callbacks...
  def handle_list_resources(_cursor, state), do: {:ok, [], state}
  def handle_read_resource(_uri, state), do: {:error, :not_found, state}
  def handle_list_prompts(_cursor, state), do: {:ok, [], state}
  def handle_get_prompt(_name, _args, state), do: {:error, :not_found, state}
  def handle_complete(_ref, _arg, state), do: {:ok, %{completion: %{values: []}}, state}
  # Draft feature: logging/setLevel
  def handle_set_log_level(_level, state), do: {:ok, %{}, state}
  
  @impl true
  def terminate(_reason, _state), do: :ok
end

defmodule ConsentApprovalHandler do
  @behaviour ExMCP.Approval
  
  @impl true
  def request_approval(:dynamic_client_consent, data, _opts) do
    IO.puts("\n=== CONSENT REQUEST ===")
    IO.puts("Client: #{data.client_name} (#{data.client_id})")
    IO.puts("Requested scopes: #{Enum.join(data.scopes, ", ")}")
    IO.puts("\nApprove this client? (y/n): ")
    
    case IO.gets("") |> String.trim() |> String.downcase() do
      "y" -> 
        {:approved, data}
        
      _ -> 
        {:denied, "User rejected consent"}
    end
  end
  
  def request_approval(_, _, _), do: {:denied, "Not supported"}
end

defmodule SecureServerDemo do
  def run do
    # Start security components
    {:ok, _} = ExMCP.Security.Supervisor.start_link(
      approval_handler: ConsentApprovalHandler
    )
    
    # Start secure server
    {:ok, server} = ExMCP.SecureServer.start_link(
      handler: SecureServerExample,
      transport: :stdio,
      server_id: "secure-mcp-demo",
      security: %{
        require_auth: true,
        trusted_issuers: ["https://auth.example.com"],
        approval_handler: ConsentApprovalHandler,
        trust_boundaries: ["production", "staging"]
      }
    )
    
    Logger.info("Secure MCP server started")
    Logger.info("Server ID: secure-mcp-demo")
    Logger.info("Security features enabled:")
    Logger.info("- Token validation (audience: secure-mcp-demo)")
    Logger.info("- Client registration and tracking")
    Logger.info("- Dynamic client consent management")
    Logger.info("- Request audit trail")
    
    # Keep server running
    Process.sleep(:infinity)
  end
  
  def test_client do
    # Example of connecting with proper authentication
    {:ok, client} = ExMCP.Client.start_link(
      transport: :stdio,
      command: ["elixir", "secure_server_example.exs"],
      client_info: %{
        name: "secure-test-client",
        version: "1.0.0"
      },
      auth_config: %{
        # In real usage, these would come from OAuth flow
        initial_token: %{
          "access_token" => generate_test_token("secure-mcp-demo"),
          "expires_in" => 3600
        }
      }
    )
    
    # Initialize connection
    {:ok, server_info} = ExMCP.Client.initialize(client)
    IO.puts("Connected to: #{server_info["name"]}")
    
    # List available tools
    {:ok, %{tools: tools}} = ExMCP.Client.list_tools(client)
    IO.puts("Available tools: #{inspect(Enum.map(tools, & &1["name"]))}")
    
    # Call secure operation
    {:ok, result} = ExMCP.Client.call_tool(client, "secure_operation", %{
      action: "test",
      data: "sensitive data"
    })
    
    IO.puts("Operation result: #{inspect(result)}")
  end
  
  defp generate_test_token(audience) do
    # In real implementation, this would be a proper JWT
    # with the correct audience claim
    Base.encode64("test-token-for-#{audience}")
  end
end

# Run the demo
case System.argv() do
  ["client"] -> SecureServerDemo.test_client()
  _ -> SecureServerDemo.run()
end