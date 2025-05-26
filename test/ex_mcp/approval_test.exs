defmodule ExMCP.ApprovalTest do
  use ExUnit.Case, async: true

  defmodule TestApprovalHandler do
    @behaviour ExMCP.Approval

    @impl true
    def request_approval(type, data, opts) do
      # Send the approval request to the test process
      test_pid = Keyword.fetch!(opts, :test_pid)
      send(test_pid, {:approval_request, type, data, opts})

      # Wait for response from test
      receive do
        {:approval_response, response} -> response
      after
        1000 -> {:denied, "Timeout waiting for test response"}
      end
    end
  end

  defmodule TestHandler do
    @behaviour ExMCP.Client.Handler

    @impl true
    def init(args) do
      {:ok, args}
    end

    @impl true
    def handle_ping(state) do
      {:ok, %{}, state}
    end

    @impl true
    def handle_list_roots(state) do
      roots = Map.get(state, :roots, [])
      {:ok, roots, state}
    end

    @impl true
    def handle_create_message(params, state) do
      approval_handler = Map.fetch!(state, :approval_handler)
      test_pid = Map.fetch!(state, :test_pid)

      # Request approval for sampling
      case approval_handler.request_approval(:sampling, params, test_pid: test_pid) do
        {:approved, _} ->
          # Simulate LLM response
          result = %{
            "role" => "assistant",
            "content" => %{
              "type" => "text",
              "text" => "Test response"
            },
            "model" => "test-model"
          }

          # Request approval for response
          case approval_handler.request_approval(:response, result,
                 test_pid: test_pid,
                 sampling_params: params
               ) do
            {:approved, final_result} ->
              {:ok, final_result, state}

            {:denied, reason} ->
              {:error,
               %{
                 "code" => -32603,
                 "message" => "Response denied: #{reason}"
               }, state}

            {:modified, modified_result} ->
              {:ok, modified_result, state}
          end

        {:denied, reason} ->
          {:error,
           %{
             "code" => -32603,
             "message" => "Sampling denied: #{reason}"
           }, state}

        {:modified, modified_params} ->
          # Recurse with modified params
          handle_create_message(modified_params, state)
      end
    end

    @impl true
    def terminate(_reason, _state) do
      :ok
    end
  end

  describe "human-in-the-loop approval" do
    test "approves both sampling and response" do
      self_pid = self()

      handler_state = %{
        approval_handler: TestApprovalHandler,
        test_pid: self_pid,
        roots: []
      }

      # Initialize handler
      {:ok, state} = TestHandler.init(handler_state)

      # Test create_message request
      params = %{
        "messages" => [
          %{"role" => "user", "content" => %{"type" => "text", "text" => "Hello"}}
        ],
        "modelPreferences" => %{"temperature" => 0.7}
      }

      # Spawn a process to handle the request
      task =
        Task.async(fn ->
          TestHandler.handle_create_message(params, state)
        end)

      # Should receive sampling approval request
      assert_receive {:approval_request, :sampling, ^params, _opts}

      # Approve sampling
      send(task.pid, {:approval_response, {:approved, params}})

      # Should receive response approval request
      assert_receive {:approval_request, :response, result, opts}
      assert result["role"] == "assistant"
      assert result["model"] == "test-model"
      assert Keyword.get(opts, :sampling_params) == params

      # Approve response
      send(task.pid, {:approval_response, {:approved, result}})

      # Should get successful result
      assert {:ok, final_result, _state} = Task.await(task)
      assert final_result["role"] == "assistant"
    end

    test "denies sampling request" do
      self_pid = self()

      handler_state = %{
        approval_handler: TestApprovalHandler,
        test_pid: self_pid
      }

      {:ok, state} = TestHandler.init(handler_state)

      params = %{
        "messages" => [
          %{"role" => "user", "content" => "Test"}
        ]
      }

      task =
        Task.async(fn ->
          TestHandler.handle_create_message(params, state)
        end)

      # Should receive sampling approval request
      assert_receive {:approval_request, :sampling, ^params, _opts}

      # Deny sampling
      send(task.pid, {:approval_response, {:denied, "Test denial"}})

      # Should get error result
      assert {:error, error, _state} = Task.await(task)
      assert error["code"] == -32603
      assert error["message"] =~ "Test denial"
    end

    test "denies response after approved sampling" do
      self_pid = self()

      handler_state = %{
        approval_handler: TestApprovalHandler,
        test_pid: self_pid
      }

      {:ok, state} = TestHandler.init(handler_state)

      params = %{
        "messages" => [
          %{"role" => "user", "content" => "Test"}
        ]
      }

      task =
        Task.async(fn ->
          TestHandler.handle_create_message(params, state)
        end)

      # Approve sampling
      assert_receive {:approval_request, :sampling, ^params, _opts}
      send(task.pid, {:approval_response, {:approved, params}})

      # Deny response
      assert_receive {:approval_request, :response, _result, _opts}
      send(task.pid, {:approval_response, {:denied, "Response not appropriate"}})

      # Should get error result
      assert {:error, error, _state} = Task.await(task)
      assert error["code"] == -32603
      assert error["message"] =~ "Response not appropriate"
    end

    test "modifies response" do
      self_pid = self()

      handler_state = %{
        approval_handler: TestApprovalHandler,
        test_pid: self_pid
      }

      {:ok, state} = TestHandler.init(handler_state)

      params = %{
        "messages" => [
          %{"role" => "user", "content" => "Test"}
        ]
      }

      task =
        Task.async(fn ->
          TestHandler.handle_create_message(params, state)
        end)

      # Approve sampling
      assert_receive {:approval_request, :sampling, ^params, _opts}
      send(task.pid, {:approval_response, {:approved, params}})

      # Modify response
      assert_receive {:approval_request, :response, result, _opts}

      modified_result =
        Map.put(result, "content", %{
          "type" => "text",
          "text" => "Modified response"
        })

      send(task.pid, {:approval_response, {:modified, modified_result}})

      # Should get modified result
      assert {:ok, final_result, _state} = Task.await(task)
      assert final_result["content"]["text"] == "Modified response"
    end
  end

  describe "approval behaviour contract" do
    test "all approval types are handled" do
      types = [:sampling, :response, :tool_call, :resource_access]

      for type <- types do
        test_pid = self()

        # Spawn a task to handle the approval request
        task =
          Task.async(fn ->
            TestApprovalHandler.request_approval(type, %{}, test_pid: test_pid)
          end)

        # Should receive the request
        assert_receive {:approval_request, ^type, %{}, _opts}

        # Send a response
        send(task.pid, {:approval_response, {:approved, %{}}})

        # Should get the response
        assert {:approved, %{}} = Task.await(task)
      end
    end
  end
end
