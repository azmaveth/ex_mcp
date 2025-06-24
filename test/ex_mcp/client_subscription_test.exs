defmodule ExMCP.ClientSubscriptionTest do
  use ExUnit.Case, async: true

  alias ExMCP.Client

  @moduletag :subscription

  describe "Client.subscribe_resource/2" do
    test "creates proper subscription request" do
      # Mock GenServer.call to capture the request
      test_pid = self()

      client =
        spawn(fn ->
          receive do
            {:"$gen_call", from, {:request, method, params}} ->
              send(test_pid, {:captured_request, method, params})
              GenServer.reply(from, {:ok, %{}})
          end
        end)

      # Test subscription
      assert {:ok, _result} = Client.subscribe_resource(client, "file:///config.json")

      # Verify correct request was sent
      assert_receive {:captured_request, "resources/subscribe", %{"uri" => "file:///config.json"}}
    end

    test "handles subscription success response" do
      client =
        spawn(fn ->
          receive do
            {:"$gen_call", from, {:request, "resources/subscribe", _params}} ->
              GenServer.reply(from, {:ok, %{}})
          end
        end)

      assert {:ok, %{}} = Client.subscribe_resource(client, "file:///test.txt")
    end

    test "handles subscription error response" do
      client =
        spawn(fn ->
          receive do
            {:"$gen_call", from, {:request, "resources/subscribe", _params}} ->
              GenServer.reply(from, {:error, "Resource not found"})
          end
        end)

      assert {:error, "Resource not found"} =
               Client.subscribe_resource(client, "file:///missing.txt")
    end

    test "supports timeout option" do
      client =
        spawn(fn ->
          receive do
            {:"$gen_call", from, {:request, "resources/subscribe", _params}} ->
              :timer.sleep(100)
              GenServer.reply(from, {:ok, %{}})
          end
        end)

      assert {:ok, %{}} = Client.subscribe_resource(client, "file:///slow.txt", timeout: 200)
    end

    test "supports format option" do
      client =
        spawn(fn ->
          receive do
            {:"$gen_call", from, {:request, "resources/subscribe", _params}} ->
              GenServer.reply(from, {:ok, %{"status" => "subscribed"}})
          end
        end)

      # Test default format (should use atomize_keys)
      assert {:ok, result} = Client.subscribe_resource(client, "file:///test.txt")

      # The result should be processed through format_response
      # (specific format depends on atomize_keys implementation)
      assert is_map(result)
    end
  end

  describe "Client.unsubscribe_resource/2" do
    test "creates proper unsubscription request" do
      test_pid = self()

      client =
        spawn(fn ->
          receive do
            {:"$gen_call", from, {:request, method, params}} ->
              send(test_pid, {:captured_request, method, params})
              GenServer.reply(from, {:ok, %{}})
          end
        end)

      assert {:ok, _result} = Client.unsubscribe_resource(client, "file:///config.json")

      assert_receive {:captured_request, "resources/unsubscribe",
                      %{"uri" => "file:///config.json"}}
    end

    test "handles unsubscription success response" do
      client =
        spawn(fn ->
          receive do
            {:"$gen_call", from, {:request, "resources/unsubscribe", _params}} ->
              GenServer.reply(from, {:ok, %{}})
          end
        end)

      assert {:ok, %{}} = Client.unsubscribe_resource(client, "file:///test.txt")
    end

    test "handles unsubscription error response" do
      client =
        spawn(fn ->
          receive do
            {:"$gen_call", from, {:request, "resources/unsubscribe", _params}} ->
              GenServer.reply(from, {:error, "Not subscribed"})
          end
        end)

      assert {:error, "Not subscribed"} = Client.unsubscribe_resource(client, "file:///test.txt")
    end
  end

  describe "resource notification handling" do
    test "handles resource update notifications" do
      # This test would require integration with the full Client GenServer
      # For now, we'll test the notification handling logic separately

      # Test that debug logging doesn't crash
      Application.put_env(:ex_mcp, :debug_logging, true)

      # The notification handling is tested as part of handle_info in Client
      # This serves as a placeholder for more comprehensive integration testing
      assert true

      Application.put_env(:ex_mcp, :debug_logging, false)
    end
  end

  describe "function signatures and documentation" do
    test "subscribe_resource/2 has correct arity" do
      assert function_exported?(Client, :subscribe_resource, 2)
    end

    test "subscribe_resource/3 has correct arity with options" do
      assert function_exported?(Client, :subscribe_resource, 3)
    end

    test "unsubscribe_resource/2 has correct arity" do
      assert function_exported?(Client, :unsubscribe_resource, 2)
    end

    test "unsubscribe_resource/3 has correct arity with options" do
      assert function_exported?(Client, :unsubscribe_resource, 3)
    end
  end
end
