defmodule ExMCP.SimpleClientTest do
  use ExUnit.Case, async: false

  alias ExMCP.SimpleClient

  # Working test transport
  defmodule TestTransport do
    use Agent

    @behaviour ExMCP.Transport

    def connect(opts) do
      if Keyword.get(opts, :fail) do
        {:error, :failed}
      else
        {:ok, agent} = Agent.start_link(fn -> %{messages: []} end)
        {:ok, agent}
      end
    end

    def send(agent, data) do
      Agent.update(agent, fn state ->
        %{state | messages: [data | state.messages]}
      end)

      {:ok, agent}
    end

    def recv(agent, _timeout) do
      msg =
        Agent.get_and_update(agent, fn state ->
          case state.messages do
            [msg | rest] -> {msg, %{state | messages: rest}}
            [] -> {nil, state}
          end
        end)

      if msg do
        # Parse and respond
        case Jason.decode!(msg) do
          %{"method" => "initialize", "id" => id} ->
            response =
              Jason.encode!(%{
                "jsonrpc" => "2.0",
                "id" => id,
                "result" => %{
                  "protocolVersion" => "2024-11-05",
                  "capabilities" => %{},
                  "serverInfo" => %{"name" => "test", "version" => "1.0"}
                }
              })

            {:ok, response, agent}

          %{"method" => "tools/list", "id" => id} ->
            response =
              Jason.encode!(%{
                "jsonrpc" => "2.0",
                "id" => id,
                "result" => %{"tools" => [%{"name" => "test_tool"}]}
              })

            {:ok, response, agent}

          %{"method" => "resources/list", "id" => id} ->
            response =
              Jason.encode!(%{
                "jsonrpc" => "2.0",
                "id" => id,
                "result" => %{"resources" => [%{"name" => "test_resource"}]}
              })

            {:ok, response, agent}

          %{"method" => "prompts/list", "id" => id} ->
            response =
              Jason.encode!(%{
                "jsonrpc" => "2.0",
                "id" => id,
                "result" => %{"prompts" => [%{"name" => "test_prompt"}]}
              })

            {:ok, response, agent}

          _ ->
            {:error, :unknown_message}
        end
      else
        {:error, :timeout}
      end
    end

    def close(agent) do
      Agent.stop(agent)
      {:ok, nil}
    end

    def controlling_process(_agent, _pid), do: :ok
    def send_message(_agent, _msg), do: {:error, :not_implemented}
    def receive_message(_agent), do: {:error, :not_implemented}
  end

  test "synchronous initialization" do
    # Client should be ready immediately after start_link
    assert {:ok, client} = SimpleClient.start_link(transport: TestTransport)

    # Should be able to use immediately
    assert {:ok, result} = SimpleClient.list_tools(client)
    assert [%{"name" => "test_tool"}] = result

    GenServer.stop(client)
  end

  test "connection failure" do
    # Should fail during start_link
    # Since init fails, the GenServer will exit
    Process.flag(:trap_exit, true)

    assert {:error, :failed} = SimpleClient.start_link(transport: TestTransport, fail: true)
  end

  test "status reporting" do
    assert {:ok, client} = SimpleClient.start_link(transport: TestTransport)

    assert {:ok, status} = SimpleClient.get_status(client)
    assert status.connection_status == :connected
    assert is_integer(status.last_activity)
    assert status.reconnect_attempts == 0

    GenServer.stop(client)
  end

  test "health check" do
    assert {:ok, client} = SimpleClient.start_link(transport: TestTransport)

    # Health check should succeed
    assert :ok = SimpleClient.health_check(client)

    GenServer.stop(client)
  end

  test "comprehensive API coverage" do
    assert {:ok, client} = SimpleClient.start_link(transport: TestTransport)

    # Test all API methods
    assert {:ok, _} = SimpleClient.list_tools(client)
    assert {:ok, _} = SimpleClient.list_resources(client)
    assert {:ok, _} = SimpleClient.list_prompts(client)

    GenServer.stop(client)
  end
end
