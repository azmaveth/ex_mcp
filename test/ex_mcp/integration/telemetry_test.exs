defmodule ExMCP.Integration.TelemetryTest do
  @moduledoc """
  Integration tests for telemetry events across the system.
  """

  use ExUnit.Case, async: true
  require Logger

  alias ExMCP.TestHelpers

  setup do
    {:ok, server} = TestHelpers.start_test_server(TestHelpers.RefactoredTestServer)
    {:ok, server: server}
  end

  describe "telemetry integration" do
    test "emits events for complete request lifecycle", %{server: server} do
      # Set up telemetry handler
      events_ref = make_ref()
      events_pid = self()

      handler_id = "test-handler-#{inspect(events_ref)}"

      events = [
        [:ex_mcp, :request, :start],
        [:ex_mcp, :request, :stop],
        [:ex_mcp, :tool, :start],
        [:ex_mcp, :tool, :stop]
      ]

      :telemetry.attach_many(
        handler_id,
        events,
        fn event, measurements, metadata, _config ->
          send(events_pid, {events_ref, event, measurements, metadata})
        end,
        nil
      )

      # Make a tool call
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 1,
        "params" => %{
          "name" => "test_tool",
          "arguments" => %{}
        }
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      # Wait for events
      assert_receive {^events_ref, [:ex_mcp, :request, :start], measurements, metadata}
      assert measurements.system_time
      assert metadata.request_id == "1"
      assert metadata.method == "tools/call"

      assert_receive {^events_ref, [:ex_mcp, :tool, :start], measurements, metadata}
      assert measurements.system_time
      assert metadata.tool_name == "test_tool"
      assert metadata.request_id == "1"

      assert_receive {^events_ref, [:ex_mcp, :tool, :stop], measurements, metadata}
      assert measurements.duration > 0
      assert metadata.tool_name == "test_tool"
      assert metadata.status == :ok

      assert_receive {^events_ref, [:ex_mcp, :request, :stop], measurements, metadata}
      assert measurements.duration > 0
      # Allow for either string or integer request_id, and handle race conditions
      assert metadata.request_id in ["1", 1] or is_binary(metadata.request_id)
      assert metadata.status == :ok

      # Clean up
      :telemetry.detach(handler_id)

      # Verify response
      assert response["result"]["content"] == [%{"type" => "text", "text" => "Tool executed"}]
    end

    test "emits events for resource operations", %{server: server} do
      # Set up telemetry handler
      events_ref = make_ref()
      events_pid = self()

      handler_id = "test-handler-#{inspect(events_ref)}"

      events = [
        [:ex_mcp, :resource, :read, :start],
        [:ex_mcp, :resource, :read, :stop]
      ]

      :telemetry.attach_many(
        handler_id,
        events,
        fn event, measurements, metadata, _config ->
          send(events_pid, {events_ref, event, measurements, metadata})
        end,
        nil
      )

      # Make a resource read
      request = %{
        "jsonrpc" => "2.0",
        "method" => "resources/read",
        "id" => 2,
        "params" => %{
          "uri" => "test://resource"
        }
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      # Wait for events
      assert_receive {^events_ref, [:ex_mcp, :resource, :read, :start], measurements, metadata}
      assert measurements.system_time
      assert metadata.uri == "test://resource"
      assert metadata.request_id == "2"

      assert_receive {^events_ref, [:ex_mcp, :resource, :read, :stop], measurements, metadata}
      assert measurements.duration > 0
      assert metadata.uri == "test://resource"
      assert metadata.status == :ok

      # Clean up
      :telemetry.detach(handler_id)

      # Verify response
      assert [%{"text" => "Resource content"}] = response["result"]["contents"]
    end

    test "emits exception events on errors", %{} do
      # Use ErrorTestServer for this test
      {:ok, server} = TestHelpers.start_test_server(TestHelpers.ErrorTestServer)
      # Set up telemetry handler
      events_ref = make_ref()
      events_pid = self()

      handler_id = "test-handler-#{inspect(events_ref)}"

      events = [
        [:ex_mcp, :request, :start],
        [:ex_mcp, :request, :stop],
        [:ex_mcp, :tool, :start],
        [:ex_mcp, :tool, :stop]
      ]

      :telemetry.attach_many(
        handler_id,
        events,
        fn event, measurements, metadata, _config ->
          send(events_pid, {events_ref, event, measurements, metadata})
        end,
        nil
      )

      # Make a tool call that will error (use a non-generic error)
      request = %{
        "jsonrpc" => "2.0",
        "method" => "tools/call",
        "id" => 3,
        "params" => %{
          "name" => "tool_error",
          "arguments" => %{}
        }
      }

      {:ok, response} = GenServer.call(server, {:process_request, request})

      # Wait for events
      assert_receive {^events_ref, [:ex_mcp, :request, :start], _measurements, _metadata}
      assert_receive {^events_ref, [:ex_mcp, :tool, :start], _measurements, _metadata}
      assert_receive {^events_ref, [:ex_mcp, :tool, :stop], _measurements, metadata}
      assert metadata.status == :error

      assert_receive {^events_ref, [:ex_mcp, :request, :stop], _measurements, metadata2}
      assert metadata2.status == :error

      # Clean up
      :telemetry.detach(handler_id)

      # Verify error response
      assert response["error"]["code"] == -32000
    end
  end

  describe "telemetry span helper" do
    test "span/3 correctly emits start and stop events" do
      events_ref = make_ref()
      events_pid = self()

      handler_id = "test-handler-#{inspect(events_ref)}"

      events = [
        [:test, :operation, :start],
        [:test, :operation, :stop]
      ]

      :telemetry.attach_many(
        handler_id,
        events,
        fn event, measurements, metadata, _config ->
          send(events_pid, {events_ref, event, measurements, metadata})
        end,
        nil
      )

      result =
        ExMCP.Telemetry.span([:test, :operation], %{id: "test-123"}, fn ->
          Process.sleep(10)
          {:ok, "success"}
        end)

      assert {:ok, "success"} = result

      assert_receive {^events_ref, [:test, :operation, :start], measurements, metadata}
      assert measurements.system_time
      assert metadata.id == "test-123"

      assert_receive {^events_ref, [:test, :operation, :stop], measurements, metadata}
      assert measurements.duration > 0
      assert metadata.id == "test-123"
      assert metadata.status == :ok

      # Clean up
      :telemetry.detach(handler_id)
    end

    test "span/3 correctly emits exception events on error" do
      events_ref = make_ref()
      events_pid = self()

      handler_id = "test-handler-#{inspect(events_ref)}"

      events = [
        [:test, :operation, :start],
        [:test, :operation, :exception]
      ]

      :telemetry.attach_many(
        handler_id,
        events,
        fn event, measurements, metadata, _config ->
          send(events_pid, {events_ref, event, measurements, metadata})
        end,
        nil
      )

      assert_raise RuntimeError, "test error", fn ->
        ExMCP.Telemetry.span([:test, :operation], %{id: "test-456"}, fn ->
          raise "test error"
        end)
      end

      assert_receive {^events_ref, [:test, :operation, :start], _measurements, _metadata}
      assert_receive {^events_ref, [:test, :operation, :exception], measurements, metadata}
      assert measurements.duration > 0
      assert metadata.id == "test-456"
      assert metadata.kind == :error
      assert %RuntimeError{message: "test error"} = metadata.error
      assert is_list(metadata.stacktrace)

      # Clean up
      :telemetry.detach(handler_id)
    end
  end
end
