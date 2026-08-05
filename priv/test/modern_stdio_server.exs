Application.put_env(:ex_mcp, :stdio_mode, true)
Application.put_env(:ex_mcp, :stdio_startup_delay, 0)
Logger.configure(level: :emergency)

{:ok, _supervisor} =
  DynamicSupervisor.start_link(strategy: :one_for_one, name: ExMCP.DynamicSupervisor)

{:ok, _task_store} = ExMCP.Tasks.Store.ETS.start_link()
{:ok, _subscriptions} = ExMCP.Server.Subscriptions.start_link()

{:ok, _task} =
  ExMCP.Tasks.create(
    "stdio_task",
    %{},
    id: "stdio-task",
    owner: %{principal_id: nil, tenant_id: nil, audience: "stdio"},
    notify: false
  )

defmodule ExMCP.Test.ModernStdioServer do
  use ExMCP.Server.Handler, tasks: :store
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

  tool "publish_resource_update", "Publish a resource subscription event" do
    param(:uri, :string, required: true)

    run(fn %{"uri" => uri}, state ->
      ExMCP.Server.notify_resource_update(self(), uri)
      {:ok, ToolResult.text("published"), state}
    end)
  end

  tool "publish_tools_changed", "Publish a tools list-changed event" do
    run(fn _arguments, state ->
      ExMCP.Server.notify_tools_changed(self())
      {:ok, ToolResult.text("published"), state}
    end)
  end

  tool "complete_task", "Complete the fixed stdio task" do
    run(fn _arguments, state ->
      {:ok, _task} =
        ExMCP.Tasks.complete(
          "stdio-task",
          %{"content" => [%{"type" => "text", "text" => "stdio task complete"}]}
        )

      {:ok, ToolResult.text("completed"), state}
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
