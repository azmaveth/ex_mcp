defmodule Mix.Tasks.StdioServer do
  @moduledoc """
  Mix task to run a stdio MCP server for testing and development.

  ## Usage

      mix stdio_server

  This will start a stdio server that can be used for testing
  with MCP clients via stdin/stdout communication.
  """

  use Mix.Task

  @shortdoc "Runs a stdio MCP server"

  def run(_args) do
    Mix.Task.run("app.start")

    # Configure for STDIO mode
    Application.put_env(:ex_mcp, :stdio_mode, true)
    Application.put_env(:ex_mcp, :stdio_startup_delay, 10)
    Logger.configure(level: :emergency)

    # Define the server inline to avoid compilation issues
    Code.eval_string("""
    defmodule ExampleStdioServer do
      use ExMCP.Server

      deftool "say_hello" do
        meta do
          description "Say hello to someone via stdio"
        end

        args do
          field(:name, :string, required: true, description: "Name to greet")
        end
      end

      deftool "echo" do
        meta do
          description "Echo back the input message"
        end

        args do
          field(:message, :string, required: true, description: "Message to echo")
          field(:uppercase, :boolean, default: false, description: "Convert to uppercase")
        end
      end

      defresource "config://server/info" do
        meta do
          name "Server Information"
          description "Information about this stdio server"
        end
        mime_type "application/json"
      end

      defprompt "greeting_style" do
        meta do
          name "Greeting Style Prompt"
          description "Generate greetings in different styles"
        end

        arguments do
          arg(:style, required: true, description: "Greeting style (formal, casual, funny)")
          arg(:name, required: true, description: "Name to greet")
        end
      end

      @impl true
      def handle_tool_call("say_hello", %{"name" => name}, state) do
        content = [text("Hello, \#{name}! Welcome to ExMCP via stdio! 📝✨")]
        {:ok, %{content: content}, state}
      end

      @impl true
      def handle_tool_call("echo", %{"message" => message, "uppercase" => uppercase}, state) do
        result_message = if uppercase, do: String.upcase(message), else: message
        content = [text("Echo: \#{result_message}")]
        {:ok, %{content: content}, state}
      end

      def handle_tool_call("echo", %{"message" => message}, state) do
        handle_tool_call("echo", %{"message" => message, "uppercase" => false}, state)
      end

      @impl true
      def handle_resource_read("config://server/info", _uri, state) do
        server_info = %{
          name: "ExampleStdioServer",
          version: "1.0.0",
          capabilities: get_capabilities()
        }

        content = [json(server_info)]
        {:ok, content, state}
      end

      @impl true
      def handle_prompt_get("greeting_style", args, state) do
        style = Map.get(args, "style", "casual")
        name = Map.get(args, "name", "Friend")

        messages =
          case style do
            "formal" ->
              [
                system("You are a formal and professional assistant."),
                user("Please provide a formal greeting for \#{name}"),
                assistant("Good day, \#{name}. I hope this message finds you well.")
              ]

            "funny" ->
              [
                system("You are a humorous and playful assistant."),
                user("Give \#{name} a funny greeting"),
                assistant(
                  "Hey there, \#{name}! *tips hat* Ready to conquer the world... or at least this conversation? 😄"
                )
              ]

            # casual
            _ ->
              [
                system("You are a friendly and casual assistant."),
                user("Say hi to \#{name} in a casual way"),
                assistant("Hey \#{name}! How's it going? Great to see you here! 👋")
              ]
          end

        {:ok, %{messages: messages}, state}
      end
    end
    """)

    # Get the configured protocol version
    protocol_version = Application.get_env(:ex_mcp, :protocol_version, "2025-03-26")

    IO.puts(:stderr, """
    📡 ExMCP stdio Server Ready!

    Example usage with ExMCP client:
    {"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"#{protocol_version}","capabilities":{},"clientInfo":{"name":"test","version":"1.0.0"}},"id":1}
    {"jsonrpc":"2.0","method":"tools/list","params":{},"id":2}
    {"jsonrpc":"2.0","method":"tools/call","params":{"name":"say_hello","arguments":{"name":"World"}},"id":3}

    Press Ctrl+D to exit.
    """)

    # Start the server using the standard STDIO transport
    # The module is defined dynamically above, so we need to call it dynamically
    # credo:disable-for-next-line Credo.Check.Refactor.Apply
    {:ok, _server} = apply(ExampleStdioServer, :start_link, [[transport: :stdio]])

    # Keep the process alive
    Process.sleep(:infinity)
  end
end
