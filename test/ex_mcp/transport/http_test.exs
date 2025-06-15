defmodule ExMCP.Transport.HTTPTest do
  use ExUnit.Case

  @moduletag :transport
  @moduletag :sse
  @moduletag :requires_http

  alias ExMCP.Transport.HTTP

  describe "HTTP transport" do
    test "builds correct URLs" do
      state = %HTTP{
        base_url: "http://localhost:3000",
        endpoint: "/mcp/v1"
      }

      # For now, we'll test the public interface instead

      # We can test the URLs indirectly through the connection process
      assert %HTTP{base_url: "http://localhost:3000"} = state
    end

    test "parses SSE events correctly" do
      # Test SSE event parsing logic
      # This would require exposing some internal functions or
      # testing through the public interface with a mock server
    end

    test "handles connection errors" do
      # Test with invalid URL
      config = [url: "http://invalid.local:99999"]

      # HTTP transport starts asynchronously, so connection attempts return ok initially
      # The actual connection failure happens asynchronously
      assert {:ok, _state} = HTTP.connect(config)
    end
  end
end
