defmodule ExMCP.Server.Handler.Echo do
  @moduledoc """
  This module provides ExMCP extensions beyond the standard MCP specification.

  Simple echo handler for testing purposes.

  This handler echoes back tool calls and provides basic implementations
  for testing server functionality.
  """

  @behaviour ExMCP.Server.Handler

  @impl true
  def init(_args) do
    {:ok, %{}}
  end

  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "echo",
        description: "Echoes back the input",
        inputSchema: %{
          type: "object",
          properties: %{
            message: %{type: "string", description: "Message to echo"}
          },
          required: ["message"]
        }
      }
    ]

    {:ok, tools, nil, state}
  end

  @impl true
  def handle_call_tool("echo", arguments, state) do
    message = Map.get(arguments, "message", "")

    result = %{
      "content" => [
        %{"type" => "text", "text" => "Echo: #{message}"}
      ]
    }

    {:ok, result, state}
  end

  def handle_call_tool(_name, _arguments, state) do
    {:error, %{"code" => -32601, "message" => "Tool not found"}, state}
  end

  @impl true
  def handle_list_resources(_cursor, state) do
    {:ok, [], nil, state}
  end

  @impl true
  def handle_read_resource(_uri, state) do
    {:error, %{"code" => -32602, "message" => "Resource not found"}, state}
  end

  @impl true
  def handle_list_prompts(_cursor, state) do
    {:ok, [], nil, state}
  end

  @impl true
  def handle_get_prompt(_name, _arguments, state) do
    {:error, %{"code" => -32602, "message" => "Prompt not found"}, state}
  end

  @impl true
  def handle_list_resource_templates(_cursor, state) do
    {:ok, [], nil, state}
  end

  @impl true
  def handle_complete(_ref, _argument, state) do
    {:ok, %{completion: []}, state}
  end

  @impl true
  def handle_initialize(_params, state) do
    result = %{
      protocolVersion: "2025-03-26",
      serverInfo: %{
        name: "echo-server",
        version: "1.0.0"
      },
      capabilities: %{
        tools: %{}
      }
    }

    {:ok, result, state}
  end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end
end
