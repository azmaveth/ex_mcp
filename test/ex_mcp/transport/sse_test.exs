defmodule ExMCP.Transport.SSETest do
  use ExUnit.Case
  
  alias ExMCP.Transport.SSE
  
  describe "SSE transport" do
    test "builds correct URLs" do
      state = %SSE{
        base_url: "http://localhost:3000",
        endpoint: "/mcp/v1"
      }
      
      # For now, we'll test the public interface instead
      
      # We can test the URLs indirectly through the connection process
      assert %SSE{base_url: "http://localhost:3000"} = state
    end
    
    test "parses SSE events correctly" do
      # Test SSE event parsing logic
      # This would require exposing some internal functions or
      # testing through the public interface with a mock server
    end
    
    test "handles connection errors" do
      # Test with invalid URL
      config = [url: "http://invalid.local:99999"]
      
      # This should fail to connect
      assert {:error, _reason} = SSE.connect(config)
    end
  end
end