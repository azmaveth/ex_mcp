defmodule Mix.Tasks.StdioServer do
  @moduledoc """
  Mix task to run a stdio MCP server for testing and development.

  ## Usage

      mix stdio_server
      
  This will start a v2 stdio server that can be used for testing
  with MCP clients via stdin/stdout communication.
  """

  use Mix.Task

  @shortdoc "Runs a stdio MCP server"

  def run(_args) do
    Mix.Task.run("app.start")

    # Define the server inline to avoid Mix.install issues
    defmodule StdioServerV2 do
      use ExMCP.ServerV2

      deftool "say_hello" do
        tool_description("Say hello to someone via stdio")

        args do
          field(:name, :string, required: true, description: "Name to greet")
        end
      end

      deftool "echo" do
        tool_description("Echo back the input message")

        args do
          field(:message, :string, required: true, description: "Message to echo")
          field(:uppercase, :boolean, default: false, description: "Convert to uppercase")
        end
      end

      defresource "config://server/info" do
        resource_name("Server Information")
        resource_description("Information about this stdio server")
        mime_type("application/json")
      end

      defprompt "greeting_style" do
        prompt_name("Greeting Style Prompt")
        prompt_description("Generate greetings in different styles")

        arguments do
          arg(:style, required: true, description: "Greeting style (formal, casual, funny)")
          arg(:name, required: true, description: "Name to greet")
        end
      end

      @impl true
      def handle_tool_call("say_hello", %{"name" => name}, state) do
        content = [text("Hello, #{name}! Welcome to ExMCP v2 via stdio! 📝✨")]
        {:ok, %{content: content}, state}
      end

      @impl true
      def handle_tool_call("echo", %{"message" => message, "uppercase" => uppercase}, state) do
        result_message = if uppercase, do: String.upcase(message), else: message
        content = [text("Echo: #{result_message}")]
        {:ok, %{content: content}, state}
      end

      def handle_tool_call("echo", %{"message" => message}, state) do
        handle_tool_call("echo", %{"message" => message, "uppercase" => false}, state)
      end

      @impl true
      def handle_resource_read("config://server/info", _uri, state) do
        server_info = %{
          name: "StdioServerV2",
          version: "2.0.0",
          dsl_version: "v2",
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
                user("Please provide a formal greeting for #{name}"),
                assistant("Good day, #{name}. I hope this message finds you well.")
              ]

            "funny" ->
              [
                system("You are a humorous and playful assistant."),
                user("Give #{name} a funny greeting"),
                assistant(
                  "Hey there, #{name}! *tips hat* Ready to conquer the world... or at least this conversation? 😄"
                )
              ]

            # casual
            _ ->
              [
                system("You are a friendly and casual assistant."),
                user("Say hi to #{name} in a casual way"),
                assistant("Hey #{name}! How's it going? Great to see you here! 👋")
              ]
          end

        {:ok, %{messages: messages}, state}
      end
    end

    IO.puts(:stderr, """
    📡 ExMCP v2 stdio Server Ready!

    Example usage with ExMCP client:
    {"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test","version":"1.0.0"}},"id":1}
    {"jsonrpc":"2.0","method":"tools/list","params":{},"id":2}
    {"jsonrpc":"2.0","method":"tools/call","params":{"name":"say_hello","arguments":{"name":"World"}},"id":3}

    Press Ctrl+D to exit.
    """)

    # Start the v2 server
    {:ok, server} = StdioServerV2.start_link()

    # Handle stdio communication manually
    loop(server, "")
  end

  defp loop(server, buffer) do
    case IO.read(:stdio, :line) do
      :eof ->
        IO.puts(:stderr, "EOF received, shutting down")
        GenServer.stop(server)
        :ok

      {:error, reason} ->
        IO.puts(:stderr, "Read error: #{inspect(reason)}")
        GenServer.stop(server)
        :ok

      data ->
        buffer = buffer <> data

        case Jason.decode(buffer) do
          {:ok, message} ->
            handle_message(server, message)
            loop(server, "")

          {:error, _} ->
            loop(server, buffer)
        end
    end
  end

  defp handle_message(_server, %{"method" => "initialize", "id" => id}) do
    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{
        "protocolVersion" => "2025-03-26",
        "capabilities" => StdioServerV2.get_capabilities(),
        "serverInfo" => %{
          "name" => "ExMCP v2 stdio Server",
          "version" => "2.0.0"
        }
      }
    }

    IO.puts(Jason.encode!(response))
  end

  defp handle_message(_server, %{"method" => "tools/list", "id" => id}) do
    tools = StdioServerV2.list_tools()

    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{"tools" => tools}
    }

    IO.puts(Jason.encode!(response))
  end

  defp handle_message(server, %{
         "method" => "tools/call",
         "params" => %{"name" => tool_name, "arguments" => args},
         "id" => id
       }) do
    case GenServer.call(server, {:call_tool, tool_name, args}) do
      {:ok, result} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => result
        }

        IO.puts(Jason.encode!(response))

      {:error, reason} ->
        error_response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => -32603,
            "message" => "Internal error",
            "data" => inspect(reason)
          }
        }

        IO.puts(Jason.encode!(error_response))
    end
  end

  defp handle_message(_server, %{"method" => "resources/list", "id" => id}) do
    resources = StdioServerV2.list_resources()

    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{"resources" => resources}
    }

    IO.puts(Jason.encode!(response))
  end

  defp handle_message(server, %{
         "method" => "resources/read",
         "params" => %{"uri" => uri},
         "id" => id
       }) do
    case GenServer.call(server, {:read_resource, uri}) do
      {:ok, contents} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => %{"contents" => contents}
        }

        IO.puts(Jason.encode!(response))

      {:error, reason} ->
        error_response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => -32603,
            "message" => "Internal error",
            "data" => inspect(reason)
          }
        }

        IO.puts(Jason.encode!(error_response))
    end
  end

  defp handle_message(_server, %{"method" => "prompts/list", "id" => id}) do
    prompts = StdioServerV2.list_prompts()

    response = %{
      "jsonrpc" => "2.0",
      "id" => id,
      "result" => %{"prompts" => prompts}
    }

    IO.puts(Jason.encode!(response))
  end

  defp handle_message(server, %{
         "method" => "prompts/get",
         "params" => %{"name" => prompt_name, "arguments" => args},
         "id" => id
       }) do
    case GenServer.call(server, {:get_prompt, prompt_name, args}) do
      {:ok, result} ->
        response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "result" => result
        }

        IO.puts(Jason.encode!(response))

      {:error, reason} ->
        error_response = %{
          "jsonrpc" => "2.0",
          "id" => id,
          "error" => %{
            "code" => -32603,
            "message" => "Internal error",
            "data" => inspect(reason)
          }
        }

        IO.puts(Jason.encode!(error_response))
    end
  end

  defp handle_message(_server, %{"method" => "initialized"}) do
    # No response needed for initialized notification
  end

  defp handle_message(_server, message) do
    IO.puts(:stderr, "Unhandled message: #{inspect(message)}")
  end
end
