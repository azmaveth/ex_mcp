#!/usr/bin/env elixir

# Migration Example: Before and After
# Shows how to migrate from v1 to v2 API

IO.puts("""
==========================================
ExMCP Migration: v1 to v2
==========================================
""")

IO.puts("""
1. CLIENT CONNECTION
==================

# v1 (deprecated):
{:ok, client} = ExMCP.Client.connect(:stdio, 
  command: ["python", "server.py"])

# v2 (recommended):
config = ExMCP.ClientConfig.new()
         |> ExMCP.ClientConfig.put_transport(:stdio,
             command: ["python", "server.py"])
{:ok, client} = ExMCP.connect(config)
""")

IO.puts("""
2. CALLING TOOLS
================

# v1 (deprecated):
{:ok, response} = ExMCP.Client.call_tool(client, "my_tool", %{arg: "value"})
# Returns raw map

# v2 (recommended):
{:ok, response} = ExMCP.call_tool(client, "my_tool", %{arg: "value"})
# Returns ExMCP.Response struct
text = ExMCP.Response.text_content(response)
data = ExMCP.Response.data_content(response)
""")

IO.puts("""
3. ERROR HANDLING
=================

# v1 (deprecated):
case ExMCP.Client.call_tool(client, tool, args) do
  {:ok, %{"error" => error}} ->
    # Handle error
  {:ok, result} ->
    # Handle success
  {:error, reason} ->
    # Handle connection error
end

# v2 (recommended):
case ExMCP.call_tool(client, tool, args) do
  {:ok, response} ->
    if response.is_error do
      error_msg = ExMCP.Response.text_content(response)
      # Handle application error
    else
      # Handle success
    end
  {:error, error} ->
    # Handle protocol/transport error
end
""")

IO.puts("""
4. DSL CHANGES
==============

# v1 (deprecated):
defmodule MyHandler do
  use ExMCP.DSL.Tool
  
  tool "my_tool" do
    tool_description "Does something"
    args %{
      name: %{type: "string", required: true}
    }
    handler fn args ->
      # Return raw content
      [%{type: "text", text: "Hello"}]
    end
  end
end

# v2 (recommended):
defmodule MyHandler do
  use ExMCP.Server.Handler
  
  @impl true
  def handle_list_tools(state) do
    tools = [%{
      name: "my_tool",
      description: "Does something",
      input_schema: %{
        type: "object",
        properties: %{
          name: %{type: "string"}
        },
        required: ["name"]
      }
    }]
    {:ok, tools, state}
  end
  
  @impl true
  def handle_call_tool("my_tool", args, state) do
    content = [%{type: "text", text: "Hello"}]
    {:ok, content, state}
  end
end
""")

IO.puts("""
5. STRUCTURED RESPONSES
======================

# v1 (manual construction):
response = %{
  "content" => [%{
    "type" => "text",
    "text" => "Hello"
  }]
}

# v2 (type-safe):
response = ExMCP.Response.text("Hello", "my_tool")
# or
response = ExMCP.Response.json(%{data: "value"}, "my_tool")
# or
response = ExMCP.Response.error("Something went wrong", "my_tool")
""")

IO.puts("""
6. CONFIGURATION
================

# v1 (limited options):
{:ok, client} = ExMCP.Client.connect(:http,
  url: "http://localhost:8080",
  timeout: 5000
)

# v2 (builder pattern):
config = ExMCP.ClientConfig.new()
         |> ExMCP.ClientConfig.put_transport(:http,
             url: "http://localhost:8080",
             path: "/mcp/v1"
           )
         |> ExMCP.ClientConfig.put_timeout(
             connect: 5000,
             request: 30000
           )
         |> ExMCP.ClientConfig.put_retry_policy(
             max_attempts: 3,
             base_interval: 1000
           )
         |> ExMCP.ClientConfig.put_auth(:bearer,
             token: "secret-token"
           )

{:ok, client} = ExMCP.connect(config)
""")

IO.puts("""
==========================================
For more details, see MIGRATING_V2.md
==========================================
""")