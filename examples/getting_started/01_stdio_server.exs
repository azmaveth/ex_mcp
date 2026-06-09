#!/usr/bin/env elixir

# STDIO MCP server with a single hello tool.

Application.put_env(:ex_mcp, :stdio_mode, true)
Application.put_env(:ex_mcp, :stdio_startup_delay, 10)
System.put_env("ELIXIR_LOG_LEVEL", "emergency")
Application.put_env(:logger, :level, :emergency)
:logger.set_primary_config(:level, :emergency)

Mix.install([
  {:ex_mcp, path: Path.expand("../..", __DIR__)}
], verbose: false)

Logger.configure(level: :emergency)

defmodule StdioHelloServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "stdio-hello-server", version: "1.0.0"

  @impl true
  def init(_args), do: {:ok, %{call_count: 0}}

  tool "hello", "Says hello in a requested language" do
    title "Hello"
    param :name, :string, required: true
    param :language, :string, default: "english"

    run fn %{name: name, language: language}, state ->
      greeting =
        case language do
          "spanish" -> "Hola, #{name}."
          "french" -> "Bonjour, #{name}."
          "japanese" -> "Konnichiwa, #{name}."
          _ -> "Hello, #{name}."
        end

      new_state = %{state | call_count: state.call_count + 1}
      {:ok, "#{greeting} Greeting ##{new_state.call_count}.", new_state}
    end
  end
end

if System.get_env("MCP_ENV") != "test" do
  IO.puts(:stderr, "Starting STDIO hello server.")
  {:ok, _server} = StdioHelloServer.start_link(transport: :stdio)
  Process.sleep(:infinity)
end
