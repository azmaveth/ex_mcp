# Example of ExMCP.Server refactoring to use ResponseBuilder
# This shows how to replace manual response building with ResponseBuilder calls

# Add to imports section (around line 178):
defp generate_imports do
  quote do
    use GenServer
    import ExMCP.DSL.Tool
    import ExMCP.DSL.Resource
    import ExMCP.DSL.Prompt
    
    # ADD THIS LINE:
    alias ExMCP.Protocol.ResponseBuilder
    
    import ExMCP.ContentHelpers,
      only: [
        text: 1,
        text: 2,
        # ... rest of imports
      ]
  end
end

# Example replacements:

# BEFORE (line 829-835):
error_response = %{
  "jsonrpc" => "2.0",
  "id" => nil,
  "error" => %{
    "code" => ErrorCodes.invalid_request(),
    "message" => error_message
  }
}

# AFTER:
error_response = ResponseBuilder.build_batch_error(protocol_version)

# BEFORE (line 1010-1017):
error_response = %{
  "jsonrpc" => "2.0",
  "id" => id,
  "error" => %{
    "code" => ErrorCodes.server_error(),
    "message" => reason
  }
}

# AFTER:
error_response = ResponseBuilder.build_mcp_error(:server_error, id, reason)

# BEFORE (line 1037-1047):
error_response = %{
  "jsonrpc" => "2.0",
  "id" => id,
  "error" => %{
    "code" => ErrorCodes.invalid_request(),
    "message" => "Unsupported protocol version: #{client_version}",
    "data" => %{
      "supported_versions" => supported_versions
    }
  }
}

# AFTER:
error_response = ResponseBuilder.build_mcp_error(
  :invalid_request, 
  id, 
  "Unsupported protocol version: #{client_version}",
  %{"supported_versions" => supported_versions}
)

# BEFORE (line 1006):
response = %{"jsonrpc" => "2.0", "id" => id, "result" => result}

# AFTER:
response = ResponseBuilder.build_success_response(result, id)

# BEFORE (line 1195-1202):
error_response = %{
  "jsonrpc" => "2.0",
  "id" => id,
  "error" => %{
    "code" => ErrorCodes.method_not_found(),
    "message" => "Method not found: #{method}"
  }
}

# AFTER:
error_response = ResponseBuilder.build_mcp_error(:method_not_found, id, "Method not found: #{method}")

# Tool error example - BEFORE (line 1086-1090):
error_response = %{
  "jsonrpc" => "2.0",
  "id" => id,
  "error" => %{"code" => ErrorCodes.server_error(), "message" => "Tool not implemented"}
}

# AFTER:
error_response = ResponseBuilder.build_mcp_error(:server_error, id, "Tool not implemented")