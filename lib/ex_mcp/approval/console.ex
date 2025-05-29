defmodule ExMCP.Approval.Console do
  @moduledoc """
  This module provides ExMCP extensions beyond the standard MCP specification.

  Console-based approval handler for human-in-the-loop interactions.

  This module implements the ExMCP.Approval behaviour and provides a simple
  console-based interface for approving or denying requests. It's suitable
  for development and testing scenarios where user interaction happens
  through the terminal.

  ## Example

      {:ok, client} = ExMCP.Client.start_link(
        transport: {:stdio, "my-server"},
        handler: {ExMCP.Client.DefaultHandler, [
          approval_handler: ExMCP.Approval.Console
        ]}
      )
  """

  @behaviour ExMCP.Approval

  require Logger

  @impl true
  def request_approval(type, data, opts) do
    case type do
      :sampling ->
        request_sampling_approval(data, opts)

      :response ->
        request_response_approval(data, opts)

      :tool_call ->
        request_tool_approval(data, opts)

      :resource_access ->
        request_resource_approval(data, opts)

      other ->
        Logger.warning("Unknown approval type: #{inspect(other)}")
        {:denied, "Unknown approval type"}
    end
  end

  # Private functions

  defp request_sampling_approval(params, _opts) do
    IO.puts("\n" <> String.duplicate("=", 60))
    IO.puts("🔔 APPROVAL REQUIRED: LLM Sampling Request")
    IO.puts(String.duplicate("=", 60))

    IO.puts("\nThe server is requesting to sample an LLM with the following parameters:")

    if messages = Map.get(params, "messages") do
      IO.puts("\n📝 Messages (#{length(messages)} total):")

      Enum.each(messages, fn msg ->
        role = Map.get(msg, "role", "unknown")
        content = extract_content(Map.get(msg, "content", ""))

        IO.puts(
          "  [#{role}]: #{String.slice(content, 0, 100)}#{if String.length(content) > 100, do: "...", else: ""}"
        )
      end)
    end

    if model_prefs = Map.get(params, "modelPreferences") do
      IO.puts("\n🤖 Model Preferences:")
      IO.puts("  #{inspect(model_prefs, pretty: true)}")
    end

    if system_prompt = Map.get(params, "systemPrompt") do
      IO.puts("\n💭 System Prompt:")

      IO.puts(
        "  #{String.slice(system_prompt, 0, 200)}#{if String.length(system_prompt) > 200, do: "...", else: ""}"
      )
    end

    IO.puts("\n" <> String.duplicate("-", 60))
    IO.write("\nApprove this sampling request? [y/N/m(odify)]: ")

    case IO.gets("") |> String.trim() |> String.downcase() do
      "y" ->
        IO.puts("✅ Approved")
        {:approved, params}

      "m" ->
        IO.puts("\n⚙️  Modification not implemented in this example")
        {:denied, "Modification not supported"}

      _ ->
        IO.puts("❌ Denied")
        {:denied, "User denied the request"}
    end
  end

  defp request_response_approval(result, opts) do
    IO.puts("\n" <> String.duplicate("=", 60))
    IO.puts("🔔 APPROVAL REQUIRED: LLM Response")
    IO.puts(String.duplicate("=", 60))

    IO.puts("\nThe LLM has generated the following response:")

    IO.puts("\n📤 Response:")
    IO.puts("  Role: #{Map.get(result, "role", "unknown")}")
    IO.puts("  Model: #{Map.get(result, "model", "unknown")}")

    content = extract_content(Map.get(result, "content", ""))
    IO.puts("  Content: #{content}")

    if sampling_params = Keyword.get(opts, :sampling_params) do
      IO.puts("\n📥 Original Request:")

      if messages = Map.get(sampling_params, "messages") do
        last_message = List.last(messages)

        if last_message do
          content = extract_content(Map.get(last_message, "content", ""))

          IO.puts(
            "  #{String.slice(content, 0, 200)}#{if String.length(content) > 200, do: "...", else: ""}"
          )
        end
      end
    end

    IO.puts("\n" <> String.duplicate("-", 60))
    IO.write("\nSend this response to the server? [y/N/m(odify)]: ")

    case IO.gets("") |> String.trim() |> String.downcase() do
      "y" ->
        IO.puts("✅ Approved")
        {:approved, result}

      "m" ->
        IO.puts("\n⚙️  Modification not implemented in this example")
        {:denied, "Modification not supported"}

      _ ->
        IO.puts("❌ Denied")
        {:denied, "User denied the response"}
    end
  end

  defp request_tool_approval(data, _opts) do
    IO.puts("\n🔧 Tool Call Approval Required")
    IO.puts("Tool: #{Map.get(data, "name", "unknown")}")
    IO.puts("Arguments: #{inspect(Map.get(data, "arguments", %{}))}")

    IO.write("\nApprove this tool call? [y/N]: ")

    case IO.gets("") |> String.trim() |> String.downcase() do
      "y" ->
        {:approved, data}

      _ ->
        {:denied, "User denied the tool call"}
    end
  end

  defp request_resource_approval(data, _opts) do
    IO.puts("\n📁 Resource Access Approval Required")
    IO.puts("URI: #{Map.get(data, "uri", "unknown")}")

    IO.write("\nApprove access to this resource? [y/N]: ")

    case IO.gets("") |> String.trim() |> String.downcase() do
      "y" ->
        {:approved, data}

      _ ->
        {:denied, "User denied resource access"}
    end
  end

  defp extract_content(content) when is_binary(content), do: content
  defp extract_content(%{"type" => "text", "text" => text}), do: text
  defp extract_content(%{"text" => text}), do: text
  defp extract_content(content), do: inspect(content)
end
