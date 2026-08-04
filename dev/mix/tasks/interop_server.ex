defmodule Mix.Tasks.InteropServer do
  @moduledoc """
  Mix task to run a stdio MCP server for interop testing.

  ## Usage

      mix interop_server

  Starts a minimal MCP server on stdio for cross-language interop tests.
  """

  use Mix.Task

  @shortdoc "Runs a stdio MCP server for interop testing"

  def run(_args) do
    Mix.Task.run("app.start")

    # Configure for STDIO mode
    Application.put_env(:ex_mcp, :stdio_mode, true)
    Logger.configure(level: :emergency)

    Code.eval_string(~S"""
    defmodule InteropHandler do
      use ExMCP.Server.Handler
      use ExMCP.Server.DSL, name: "elixir-interop-server", version: "1.0.0"

      def __server_info__, do: %{name: "elixir-interop-server", version: "1.0.0"}

      tool "echo", "Echoes back the input message" do
        param :message, :string, required: true, description: "Message to echo"

        run fn %{message: message}, state ->
          {:ok, "Echo: #{message}", state}
        end
      end

      tool "add", "Adds two numbers" do
        param :a, :number, required: true, description: "First number"
        param :b, :number, required: true, description: "Second number"

        run fn %{a: a, b: b}, state ->
          {:ok, to_string(a + b), state}
        end
      end

      resource "test://greeting", "A test greeting resource" do
        name "Greeting"
        mime_type "text/plain"

        read fn _params, state ->
          {:ok, "Hello from Elixir!", state}
        end
      end

      prompt "simple_prompt", "A simple test prompt" do
        render fn _args, state ->
          {:ok, "This is a test prompt from Elixir", state}
        end
      end

      def handle_request("ping", _params, state) do
        {:reply, %{}, state}
      end

      def handle_request("notifications/initialized", _params, state) do
        {:noreply, state}
      end

      def handle_request(method, _params, state) do
        {:error, %{code: -32601, message: "Method not found: #{method}"}, state}
      end
    end
    """)

    # Start the server using StdioServer with the handler module
    {:ok, _server} =
      ExMCP.Server.StdioServer.start_link(module: InteropHandler)

    # Keep the process alive
    Process.sleep(:infinity)
  end
end
