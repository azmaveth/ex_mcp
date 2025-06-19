#!/usr/bin/env elixir

# Hello World Server - Native (BEAM) Transport
# MCP server using Erlang/BEAM native transport for in-process communication

# Add lib to path instead of using Mix.install to avoid HTTP dependencies
Code.prepend_path("_build/dev/lib/ex_mcp/ebin")
Code.prepend_path("_build/dev/lib/jason/ebin")

defmodule HelloNativeServer do
  use ExMCP.ServerV2
  
  @name "hello-native-server"
  @version "1.0.0"
  
  deftool "greet" do
    meta do
      name "Multilingual Greeter"
      description "Greets in different languages"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        name: %{type: "string"},
        language: %{
          type: "string",
          enum: ["en", "es", "fr", "de"],
          default: "en"
        }
      },
      required: ["name"]
    }
  end
  
  defresource "native://status" do
    meta do
      name "Server Status"
      description "Native transport status"
    end
    
    mime_type "application/json"
  end
  
  # Implement handlers using defhandler
  defhandler :tool, "greet", args, state do
    name = args["name"]
    greeting = case args["language"] || "en" do
      "es" -> "¡Hola, #{name}!"
      "fr" -> "Bonjour, #{name}!"
      "de" -> "Hallo, #{name}!"
      _ -> "Hello, #{name}!"
    end
    
    response = text(greeting)
    {:ok, [response], state}
  end
  
  # Resource handler showing process information
  defhandler :resource, "native://status", _uri, state do
    status = %{
      transport: "native/beam",
      pid: inspect(self()),
      node: node(),
      uptime_ms: :erlang.statistics(:wall_clock) |> elem(0)
    }
    content = json(status)
    {:ok, [content], state}
  end
end

# Start the server
IO.puts("Starting Hello World Native Server...")

# Check if we should run in distributed mode
distributed_mode = System.argv() |> Enum.member?("--distributed")

if distributed_mode do
  IO.puts("Running in distributed mode")
  IO.puts("Node: #{node()}")
  IO.puts("Cookie: #{Node.get_cookie()}")
end

{:ok, server} = HelloNativeServer.start_link(transport: :native)

# Register the server with a known name for distributed access
Process.register(server, :hello_native_server)
IO.puts("Server PID: #{inspect(server)} (registered as :hello_native_server)")

if distributed_mode do
  IO.puts("\nReady for distributed connections!")
  IO.puts("Clients can connect from other nodes using the registered name.")
else
  # Demonstrate in-process communication
  IO.puts("\nDemonstrating in-process communication...")
  
  # Create a client in the same process
  config = ExMCP.ClientConfig.new()
           |> ExMCP.ClientConfig.put_transport(:native, server_pid: server)

  {:ok, client} = ExMCP.connect(config)
  IO.puts("✓ Client connected via native transport")

  # Test the connection
  {:ok, resp} = ExMCP.call_tool(client, "greet", %{"name" => "BEAM", "language" => "en"})
  IO.puts("Greeting: #{ExMCP.Response.text_content(resp)}")

  {:ok, resp} = ExMCP.read_resource(client, "native://status")
  IO.puts("Status: #{inspect(ExMCP.Response.data_content(resp), pretty: true)}")

  ExMCP.disconnect(client)
  IO.puts("✓ Local demo complete")
end

IO.puts("\nServer running...")

if distributed_mode do
  IO.puts("For distributed testing, run:")
  IO.puts("  elixir --name client@127.0.0.1 --cookie hello_mcp_cookie hello_client_all.exs")
end

# Keep server running
Process.sleep(:infinity)