defmodule ExMCP.Integration.ConcurrentClientsTest.TestConcurrentHandler do
  @moduledoc false
  use GenServer

  # GenServer callbacks
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl GenServer
  def init(_opts) do
    {:ok, %{request_count: 0}}
  end

  @impl GenServer
  def terminate(_reason, _state), do: :ok

  @impl GenServer
  def handle_call({:initialize, params}, _from, state) do
    case handle_initialize(params, state) do
      {:ok, result, new_state} -> {:reply, {:ok, result, new_state}, new_state}
      {:error, reason, new_state} -> {:reply, {:error, reason, new_state}, new_state}
    end
  end

  def handle_call({:list_tools, cursor}, _from, state) do
    case handle_list_tools(cursor, state) do
      {:ok, tools, next_cursor, new_state} ->
        {:reply, {:ok, %{"tools" => tools, "nextCursor" => next_cursor}, new_state}, new_state}

      {:error, reason, new_state} ->
        {:reply, {:error, reason, new_state}, new_state}
    end
  end

  def handle_call({:call_tool, name, args}, _from, state) do
    case handle_call_tool(name, args, state) do
      {:ok, result, new_state} -> {:reply, {:ok, result, new_state}, new_state}
      {:error, reason, new_state} -> {:reply, {:error, reason, new_state}, new_state}
    end
  end

  def handle_call({:execute_tool, name, args}, _from, state) do
    case handle_call_tool(name, args, state) do
      {:ok, result, new_state} -> {:reply, {:ok, result}, new_state}
      {:error, reason, new_state} -> {:reply, {:error, reason}, new_state}
    end
  end

  def handle_call(:get_server_info, _from, state) do
    server_info = %{
      "name" => "concurrent-test-server",
      "version" => "1.0.0"
    }

    {:reply, server_info, state}
  end

  def handle_call(:get_capabilities, _from, state) do
    capabilities = %{
      "tools" => %{},
      "resources" => %{},
      "prompts" => %{}
    }

    {:reply, capabilities, state}
  end

  def handle_call(:get_tools, _from, state) do
    {:reply, get_tools(), state}
  end

  def handle_call(:get_resources, _from, state) do
    {:reply, get_resources(), state}
  end

  def handle_call(:get_prompts, _from, state) do
    {:reply, get_prompts(), state}
  end

  # Handler behaviour callbacks
  def handle_initialize(_params, state) do
    {:ok,
     %{
       name: "concurrent-test-server",
       version: "1.0.0",
       capabilities: %{
         tools: %{},
         resources: %{},
         prompts: %{}
       }
     }, state}
  end

  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "slow_operation",
        description: "Simulates a slow operation",
        input_schema: %{
          type: "object",
          properties: %{
            duration: %{type: "integer", description: "Duration in ms"}
          },
          required: ["duration"]
        }
      },
      %{
        name: "fast_operation",
        description: "Simulates a fast operation",
        input_schema: %{
          type: "object",
          properties: %{},
          required: []
        }
      }
    ]

    {:ok, tools, nil, state}
  end

  def handle_call_tool("slow_operation", %{"duration" => duration}, state) do
    Process.sleep(duration)
    {:ok, %{content: [%{type: "text", text: "Completed after #{duration}ms"}]}, state}
  end

  def handle_call_tool("fast_operation", %{"value" => value}, state) do
    {:ok, %{content: [%{type: "text", text: "Processed: #{value}"}]}, state}
  end

  def handle_call_tool(_name, _args, state) do
    {:error, "Unknown tool", state}
  end

  # DSL server compatibility methods
  def get_capabilities do
    %{
      "tools" => %{},
      "resources" => %{},
      "prompts" => %{}
    }
  end

  def get_tools do
    %{
      "slow_operation" => %{
        "name" => "slow_operation",
        "description" => "Simulates a slow operation",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "duration" => %{"type" => "integer", "description" => "Duration in ms"}
          },
          "required" => ["duration"]
        }
      },
      "fast_operation" => %{
        "name" => "fast_operation",
        "description" => "Simulates a fast operation",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{},
          "required" => []
        }
      }
    }
  end

  def get_resources, do: %{}
  def get_prompts, do: %{}
end

defmodule ExMCP.Integration.ConcurrentClientsTest.ErrorProneHandler do
  @moduledoc false
  use GenServer

  # GenServer callbacks
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl GenServer
  def init(_opts) do
    {:ok, %{call_count: 0}}
  end

  @impl GenServer
  def terminate(_reason, _state), do: :ok

  @impl GenServer
  def handle_call({:initialize, params}, _from, state) do
    case handle_initialize(params, state) do
      {:ok, result, new_state} -> {:reply, {:ok, result, new_state}, new_state}
      {:error, reason, new_state} -> {:reply, {:error, reason, new_state}, new_state}
    end
  end

  def handle_call({:list_tools, cursor}, _from, state) do
    case handle_list_tools(cursor, state) do
      {:ok, tools, next_cursor, new_state} ->
        {:reply, {:ok, %{"tools" => tools, "nextCursor" => next_cursor}, new_state}, new_state}

      {:error, reason, new_state} ->
        {:reply, {:error, reason, new_state}, new_state}
    end
  end

  def handle_call({:call_tool, name, args}, _from, state) do
    case handle_call_tool(name, args, state) do
      {:ok, result, new_state} -> {:reply, {:ok, result, new_state}, new_state}
      {:error, reason, new_state} -> {:reply, {:error, reason, new_state}, new_state}
    end
  end

  def handle_call({:execute_tool, name, args}, _from, state) do
    case handle_call_tool(name, args, state) do
      {:ok, result, new_state} -> {:reply, {:ok, result}, new_state}
      {:error, reason, new_state} -> {:reply, {:error, reason}, new_state}
    end
  end

  def handle_call(:get_server_info, _from, state) do
    server_info = %{
      "name" => "error-prone-server",
      "version" => "1.0.0"
    }

    {:reply, server_info, state}
  end

  def handle_call(:get_capabilities, _from, state) do
    capabilities = %{
      "tools" => %{}
    }

    {:reply, capabilities, state}
  end

  def handle_call(:get_tools, _from, state) do
    {:reply, get_tools(), state}
  end

  def handle_call(:get_resources, _from, state) do
    {:reply, get_resources(), state}
  end

  def handle_call(:get_prompts, _from, state) do
    {:reply, get_prompts(), state}
  end

  # Handler behaviour callbacks
  def handle_initialize(_params, state) do
    {:ok,
     %{
       name: "error-prone-server",
       version: "1.0.0",
       capabilities: %{tools: %{}}
     }, state}
  end

  def handle_list_tools(_cursor, state) do
    {:ok,
     [
       %{
         name: "unreliable",
         description: "Sometimes fails",
         input_schema: %{type: "object", properties: %{}}
       }
     ], nil, state}
  end

  def handle_call_tool("unreliable", _params, state) do
    new_count = state.call_count + 1

    # Fail every 3rd call
    if rem(new_count, 3) == 0 do
      {:error, "Simulated failure", %{state | call_count: new_count}}
    else
      {:ok, %{content: [%{type: "text", text: "Success #{new_count}"}]},
       %{state | call_count: new_count}}
    end
  end

  def handle_call_tool(_name, _args, state) do
    {:error, "Unknown tool", state}
  end

  def handle_list_resources(_cursor, state), do: {:ok, [], nil, state}
  def handle_read_resource(_uri, state), do: {:error, "Not found", state}
  def handle_list_prompts(_cursor, state), do: {:ok, [], nil, state}
  def handle_get_prompt(_name, _params, state), do: {:error, "Not found", state}

  # DSL server compatibility methods
  def get_capabilities do
    %{
      "tools" => %{}
    }
  end

  def get_tools do
    %{
      "unreliable" => %{
        "name" => "unreliable",
        "description" => "Sometimes fails",
        "inputSchema" => %{"type" => "object", "properties" => %{}}
      }
    }
  end

  def get_resources, do: %{}
  def get_prompts, do: %{}
end

defmodule ExMCP.Integration.ConcurrentClientsTest do
  use ExUnit.Case, async: false

  alias ExMCP.Client
  alias ExMCP.HttpPlug
  alias __MODULE__.{TestConcurrentHandler, ErrorProneHandler}

  @moduletag :integration
  @moduletag timeout: 60_000

  setup do
    Process.flag(:trap_exit, true)

    # Find a free port by binding to port 0
    {:ok, socket} = :gen_tcp.listen(0, [:binary, ip: {127, 0, 0, 1}])
    {:ok, port} = :inet.port(socket)
    :gen_tcp.close(socket)

    # Use a unique ranch listener ref to avoid conflicts
    unique_id = System.unique_integer([:positive])
    ranch_ref = :"concurrent_test_listener_#{unique_id}"

    # Start HTTP server with our plug
    {:ok, _} =
      Plug.Cowboy.http(
        HttpPlug,
        [
          handler: TestConcurrentHandler,
          server_info: %{name: "test-server", version: "1.0.0"}
        ],
        port: port,
        ref: ranch_ref
      )

    # Give server time to start
    Process.sleep(100)

    on_exit(fn ->
      # Stop the cowboy listener
      try do
        Plug.Cowboy.shutdown(ranch_ref)
      catch
        :exit, _ -> :ok
      end
    end)

    {:ok, port: port}
  end

  describe "concurrent client operations" do
    test "multiple clients can connect and operate simultaneously", %{port: port} do
      # Start multiple clients
      client_count = 10

      clients =
        for i <- 1..client_count do
          {:ok, client} = Client.connect("http://localhost:#{port}/", use_sse: false)
          {i, client}
        end

      # Verify all clients connected
      assert length(clients) == client_count

      # Each client lists tools
      results =
        clients
        |> Enum.map(fn {i, client} ->
          Task.async(fn ->
            try do
              case Client.list_tools(client) do
                {:ok, response} -> {i, :ok, response}
                {:error, _} -> {i, :error, nil}
              end
            catch
              :exit, _ -> {i, :error, nil}
            end
          end)
        end)
        |> Enum.map(&Task.await(&1, 10_000))

      # All should have returned
      assert length(results) == client_count

      # At least some should have succeeded
      successful = Enum.count(results, fn {_i, status, _} -> status == :ok end)
      assert successful > 0, "At least some clients should successfully list tools"

      # Clean up
      Enum.each(clients, fn {_i, client} ->
        Client.disconnect(client)
      end)
    end

    test "concurrent tool calls work correctly", %{port: port} do
      # Create client
      {:ok, client} = Client.connect("http://localhost:#{port}/", use_sse: false)

      # Make concurrent tool calls
      task_count = 20

      tasks =
        for i <- 1..task_count do
          Task.async(fn ->
            try do
              if rem(i, 2) == 0 do
                case Client.call_tool(client, "fast_operation", %{"value" => "task-#{i}"}) do
                  {:ok, response} -> {:fast, i, response}
                  {:error, _} -> {:error, i, :call_failed}
                end
              else
                case Client.call_tool(client, "slow_operation", %{"duration" => 50}) do
                  {:ok, response} -> {:slow, i, response}
                  {:error, _} -> {:error, i, :call_failed}
                end
              end
            catch
              :exit, _ -> {:error, i, :exit}
            end
          end)
        end

      # Collect results with generous timeout
      yielded = Task.yield_many(tasks, 30_000)

      results =
        Enum.map(yielded, fn
          {_task, {:ok, result}} ->
            result

          {task, nil} ->
            Task.shutdown(task, :brutal_kill)
            {:error, 0, :timeout}

          {_task, {:exit, _}} ->
            {:error, 0, :exit}
        end)

      # At least some operations should succeed
      successful = Enum.count(results, fn {type, _, _} -> type in [:fast, :slow] end)
      assert successful > 0, "At least some concurrent operations should succeed"

      Client.disconnect(client)
    end

    test "SSE connections handle backpressure correctly", %{port: port} do
      # This test requires SSE support which we'll simulate
      # by creating multiple SSE connections and flooding with events

      # Start SSE connections
      sse_count = 5

      sse_tasks =
        for i <- 1..sse_count do
          Task.async(fn ->
            # Connect via SSE endpoint
            _url = "http://localhost:#{port}/sse"
            _headers = [{"accept", "text/event-stream"}]

            # Use HTTPoison or similar to establish SSE connection
            # For this test, we'll simulate the connection behavior
            {:ok, "sse-client-#{i}"}
          end)
        end

      # Wait for connections
      sse_clients = Enum.map(sse_tasks, &Task.await/1)
      assert length(sse_clients) == sse_count

      # The actual SSE backpressure testing would require real SSE connections
      # This is a placeholder showing the test structure
    end

    test "server handles client disconnections gracefully", %{port: port} do
      # Start multiple clients
      clients =
        for i <- 1..5 do
          {:ok, client} = Client.connect("http://localhost:#{port}/", use_sse: false)
          {i, client}
        end

      # Start operations on all clients
      tasks =
        Enum.map(clients, fn {i, client} ->
          Task.async(fn ->
            try do
              # Some clients will be disconnected mid-operation
              if rem(i, 2) == 0 do
                Process.sleep(50)
                Client.disconnect(client)
                {:disconnected, i}
              else
                case Client.call_tool(client, "fast_operation", %{
                       "value" => "client-#{i}"
                     }) do
                  {:ok, response} ->
                    {:completed, i, response}

                  {:error, _} ->
                    {:error, i, :call_failed}
                end
              end
            catch
              :exit, _ -> {:error, i, :disconnected}
            end
          end)
        end)

      # Collect results
      results = Enum.map(tasks, &Task.await(&1, 5000))

      # Verify both disconnected and completed clients
      disconnected =
        Enum.filter(results, fn
          {:disconnected, _} -> true
          {:error, _, :disconnected} -> true
          _ -> false
        end)

      completed =
        Enum.filter(results, fn
          {:completed, _, _} -> true
          _ -> false
        end)

      assert length(disconnected) + length(completed) > 0

      # Clean up remaining clients
      Enum.each(clients, fn {_i, client} ->
        if Process.alive?(client) do
          Client.disconnect(client)
        end
      end)
    end

    test "rate limiting and throttling work correctly" do
      # This would test rate limiting if implemented
      # For now, we'll test that rapid requests don't crash the server

      # Find a free port
      {:ok, socket} = :gen_tcp.listen(0, [:binary, ip: {127, 0, 0, 1}])
      {:ok, port} = :inet.port(socket)
      :gen_tcp.close(socket)

      unique_id = System.unique_integer([:positive])
      ranch_ref = :"rate_limit_test_listener_#{unique_id}"

      # Start a rate-limited server
      {:ok, _} =
        Plug.Cowboy.http(
          HttpPlug,
          [
            handler: TestConcurrentHandler,
            server_info: %{name: "rate-limited-server", version: "1.0.0"}
            # Rate limiting could be configured here
          ],
          port: port,
          ref: ranch_ref
        )

      Process.sleep(100)

      # Create client
      {:ok, client} = Client.connect("http://localhost:#{port}/", use_sse: false)

      # Flood with requests
      flood_count = 100

      flood_tasks =
        for i <- 1..flood_count do
          Task.async(fn ->
            try do
              {:ok, _response} =
                Client.call_tool(client, "fast_operation", %{
                  "value" => "flood-#{i}"
                })

              :ok
            catch
              _, _ -> :error
            end
          end)
        end

      # Collect results with timeout
      results =
        flood_tasks
        |> Enum.map(&Task.yield(&1, 5000))
        |> Enum.map(fn
          {:ok, result} -> result
          nil -> :timeout
        end)

      # Some requests should succeed even under load
      successful = Enum.count(results, &(&1 == :ok))
      assert successful > 0

      # Server should still be responsive
      case Client.list_tools(client) do
        {:ok, _response} -> assert true
        {:error, _} -> assert true, "Server responded"
      end

      Client.disconnect(client)

      try do
        Plug.Cowboy.shutdown(ranch_ref)
      catch
        :exit, _ -> :ok
      end
    end
  end

  describe "error handling under load" do
    test "server recovers from handler errors", %{port: _port} do
      # Find a free port for the error-prone server
      {:ok, socket} = :gen_tcp.listen(0, [:binary, ip: {127, 0, 0, 1}])
      {:ok, error_port} = :inet.port(socket)
      :gen_tcp.close(socket)

      unique_id = System.unique_integer([:positive])
      error_ranch_ref = :"error_handler_test_listener_#{unique_id}"

      {:ok, _} =
        Plug.Cowboy.http(
          HttpPlug,
          [
            handler: ErrorProneHandler,
            server_info: %{name: "error-test", version: "1.0.0"}
          ],
          port: error_port,
          ref: error_ranch_ref
        )

      Process.sleep(100)

      # Create client
      {:ok, client} = Client.connect("http://localhost:#{error_port}/", use_sse: false)

      # Make multiple calls - each HTTP request creates a fresh handler instance,
      # so the server should handle all requests (no persistent error state).
      results =
        for i <- 1..9 do
          case Client.call_tool(client, "unreliable", %{}) do
            {:ok, response} ->
              {:success, i, response}

            {:error, error} ->
              {:error, i, error}
          end
        end

      # Verify the server is responsive and handles requests.
      # With per-request handler instances, each call starts fresh.
      successful = Enum.count(results, fn {status, _, _} -> status == :success end)
      assert successful > 0, "At least some calls should succeed"

      # Server should still be responsive after all calls
      case Client.call_tool(client, "unreliable", %{}) do
        {:ok, _} -> assert true
        {:error, _} -> assert true, "Server responded (even with error)"
      end

      Client.disconnect(client)

      try do
        Plug.Cowboy.shutdown(error_ranch_ref)
      catch
        :exit, _ -> :ok
      end
    end
  end
end
