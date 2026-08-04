Application.put_env(:ex_mcp, :stdio_mode, true)
Application.put_env(:ex_mcp, :stdio_startup_delay, 0)
Logger.configure(level: :emergency)

defmodule ExMCP.Test.ModernStdioServer do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "modern-stdio-server", version: "1.0.0"

  alias ExMCP.Server.Context

  tool "echo", "Echo text" do
    param(:text, :string, required: true)

    run(fn %{"text" => text}, state ->
      {:ok, %{content: [%{type: "text", text: text}]}, state}
    end)
  end

  tool "onboard", "Collect a display name through MRTR" do
    run(fn _arguments, state ->
      case Context.input_responses() do
        nil ->
          input_requests = %{
            "profile" => %{
              "method" => "elicitation/create",
              "params" => %{
                "message" => "Choose a stdio display name",
                "requestedSchema" => %{"type" => "object"}
              }
            }
          }

          {:ok, ToolResult.input_required(input_requests, %{"transport" => "stdio"}), state}

        %{"profile" => %{"content" => %{"name" => name}}} ->
          request_state = Context.request_state()
          {:ok, ToolResult.text("#{name}:#{request_state["transport"]}"), state}
      end
    end)
  end
end

{:ok, _server} =
  ExMCP.Test.ModernStdioServer.start_link(
    transport: :stdio,
    protocol_mode: :modern_only,
    mrtr: true,
    request_state: [
      active_key_id: "stdio-test",
      keys: %{"stdio-test" => :binary.copy(<<73>>, 32)}
    ]
  )

Process.sleep(:infinity)
