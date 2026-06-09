#!/usr/bin/env elixir

# HTTP MCP server without SSE.

Mix.install([
  {:ex_mcp, path: Path.expand("../..", __DIR__)}
], verbose: false)

Logger.configure(level: :info)

defmodule HttpHelloServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "http-hello-server", version: "1.0.0"

  resource "hello://world", "A friendly greeting from the HTTP server" do
    title "Hello World"
    mime_type "text/plain"

    read fn %{uri: uri}, state ->
      {:ok,
       %{
         uri: uri,
         text: """
         Hello from the HTTP server.

         This endpoint uses regular HTTP JSON-RPC request/response handling.
         """
       }, state}
    end
  end

  resource "hello://stats", "Static demo server statistics" do
    title "Server Statistics"
    mime_type "application/json"

    read fn %{uri: uri}, state ->
      {:ok,
       %{
         uri: uri,
         text: Jason.encode!(%{transport: "http", server_time: DateTime.utc_now()})
       }, state}
    end
  end
end

if System.get_env("MCP_ENV") != "test" do
  port = String.to_integer(System.get_env("HTTP_PORT", "8080"))

  IO.puts("Starting HTTP MCP server on port #{port}.")
  IO.puts("Resources: hello://world, hello://stats")

  {:ok, _server} =
    HttpHelloServer.start_link(
      transport: :http,
      port: port,
      use_sse: false,
      name: :http_hello_server
    )

  Process.sleep(:infinity)
end
