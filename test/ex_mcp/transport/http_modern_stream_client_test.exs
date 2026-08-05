defmodule ExMCP.Transport.HTTP.ModernStreamClientTest do
  use ExUnit.Case, async: true

  alias ExMCP.Transport.HTTP.ModernStreamClient

  @request_id 42

  test "ordinary request streams accept only related notifications and their final response" do
    assert :ok =
             validate(:request, %{
               "jsonrpc" => "2.0",
               "method" => "notifications/progress",
               "params" => %{"progressToken" => "job", "progress" => 1}
             })

    assert :ok =
             validate(:request, %{
               "jsonrpc" => "2.0",
               "method" => "notifications/message",
               "params" => %{"level" => "info", "data" => "working"}
             })

    assert :ok = validate(:request, %{"jsonrpc" => "2.0", "id" => @request_id, "result" => %{}})

    assert :ok =
             validate(:request, %{
               "jsonrpc" => "2.0",
               "id" => @request_id,
               "error" => %{"code" => -32603, "message" => "failed"}
             })
  end

  test "ordinary request streams reject independent requests and cross-stream messages" do
    assert {:error, :invalid_stream_message} =
             validate(:request, %{
               "jsonrpc" => "2.0",
               "id" => 7,
               "method" => "sampling/createMessage",
               "params" => %{}
             })

    assert {:error, :invalid_stream_message} =
             validate(:request, %{
               "jsonrpc" => "2.0",
               "method" => "notifications/tools/list_changed",
               "params" => %{}
             })

    assert {:error, :response_id_mismatch} =
             validate(:request, %{"jsonrpc" => "2.0", "id" => 99, "result" => %{}})

    assert {:error, :invalid_stream_message} =
             validate(:request, %{
               "jsonrpc" => "2.0",
               "id" => @request_id,
               "result" => %{},
               "error" => %{}
             })
  end

  test "subscription streams reject request-scoped notifications" do
    assert :ok =
             validate(:subscription, %{
               "jsonrpc" => "2.0",
               "method" => "notifications/subscriptions/acknowledged",
               "params" => %{}
             })

    assert :ok =
             validate(:subscription, %{
               "jsonrpc" => "2.0",
               "method" => "notifications/resources/updated",
               "params" => %{"uri" => "file:///one"}
             })

    assert :ok =
             validate(:subscription, %{
               "jsonrpc" => "2.0",
               "method" => "notifications/tasks",
               "params" => %{"taskId" => "task-1"}
             })

    assert {:error, :invalid_stream_message} =
             validate(:subscription, %{
               "jsonrpc" => "2.0",
               "method" => "notifications/progress",
               "params" => %{"progressToken" => "job", "progress" => 1}
             })
  end

  defp validate(kind, message) do
    ModernStreamClient.validate_message(message, @request_id, kind)
  end
end
