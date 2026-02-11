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

    # Define the handler module dynamically
    # Uses the interface that StdioServer expects (get_tools/0, handle_tool_call/3, etc.)
    Code.eval_string(~S"""
    defmodule InteropHandler do
      def __server_info__ do
        %{name: "elixir-interop-server", version: "1.0.0"}
      end

      def get_capabilities do
        %{
          "tools" => %{"listChanged" => false},
          "resources" => %{"listChanged" => false, "subscribe" => false},
          "prompts" => %{"listChanged" => false}
        }
      end

      def get_tools do
        %{
          "echo" => %{
            name: "echo",
            description: "Echoes back the input message",
            input_schema: %{
              "type" => "object",
              "properties" => %{
                "message" => %{"type" => "string", "description" => "Message to echo"}
              },
              "required" => ["message"]
            }
          },
          "add" => %{
            name: "add",
            description: "Adds two numbers",
            input_schema: %{
              "type" => "object",
              "properties" => %{
                "a" => %{"type" => "number", "description" => "First number"},
                "b" => %{"type" => "number", "description" => "Second number"}
              },
              "required" => ["a", "b"]
            }
          }
        }
      end

      def get_resources do
        %{
          "test://greeting" => %{
            uri: "test://greeting",
            name: "Greeting",
            description: "A test greeting resource",
            mime_type: "text/plain"
          }
        }
      end

      def init(_args), do: {:ok, %{}}

      def handle_tool_call("echo", arguments, state) do
        message = Map.get(arguments, "message", "")
        {:ok, %{"content" => [%{"type" => "text", "text" => "Echo: #{message}"}]}, state}
      end

      def handle_tool_call("add", arguments, state) do
        a = Map.get(arguments, "a", 0)
        b = Map.get(arguments, "b", 0)
        {:ok, %{"content" => [%{"type" => "text", "text" => to_string(a + b)}]}, state}
      end

      def handle_tool_call(_name, _arguments, state) do
        {:error, %{code: -32601, message: "Tool not found"}, state}
      end

      # Catch-all for methods not directly handled by StdioServer
      def handle_request("resources/read", params, state) do
        uri = Map.get(params, "uri")

        case uri do
          "test://greeting" ->
            result = %{
              "contents" => [
                %{"uri" => "test://greeting", "mimeType" => "text/plain", "text" => "Hello from Elixir!"}
              ]
            }
            {:reply, result, state}

          _ ->
            {:error, %{code: -32602, message: "Resource not found"}, state}
        end
      end

      def handle_request("prompts/list", _params, state) do
        result = %{
          "prompts" => [
            %{"name" => "simple_prompt", "description" => "A simple test prompt"}
          ]
        }
        {:reply, result, state}
      end

      def handle_request("prompts/get", params, state) do
        case Map.get(params, "name") do
          "simple_prompt" ->
            result = %{
              "messages" => [
                %{"role" => "user", "content" => %{"type" => "text", "text" => "This is a test prompt from Elixir"}}
              ]
            }
            {:reply, result, state}

          _ ->
            {:error, %{code: -32602, message: "Prompt not found"}, state}
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
