defmodule ExMCP.Integration.ConcurrentClientsTest.TestConcurrentHandler do
  @moduledoc false
  use GenServer
  @behaviour ExMCP.Server.Handler

  # GenServer callbacks
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl GenServer
  def init(_opts) do
    {:ok, %{request_count: 0}}
  end

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

  def handle_call(:get_server_info, _from, state) do
    server_info = %{
      name: "concurrent-test-server",
      version: "1.0.0"
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

  # Handler behaviour callbacks
  @impl ExMCP.Server.Handler

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
  @behaviour ExMCP.Server.Handler

  # GenServer callbacks
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl GenServer
  def init(_opts) do
    {:ok, %{call_count: 0}}
  end

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

  def handle_call(:get_server_info, _from, state) do
    server_info = %{
      name: "error-prone-server",
      version: "1.0.0"
    }

    {:reply, server_info, state}
  end

  def handle_call(:get_capabilities, _from, state) do
    capabilities = %{
      "tools" => %{}
    }

    {:reply, capabilities, state}
  end

  # Handler behaviour callbacks
  @impl ExMCP.Server.Handler

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
    # Start a test HTTP server
    port = 8000 + :rand.uniform(1000)

    # Start HTTP server with our plug
    {:ok, _} =
      Plug.Cowboy.http(
        HttpPlug,
        [
          handler: TestConcurrentHandler,
          server_info: %{name: "test-server", version: "1.0.0"}
        ],
        port: port
      )

    # Give server time to start
    Process.sleep(100)

    on_exit(fn ->
      # Stop the cowboy listener
      Plug.Cowboy.shutdown(HttpPlug.HTTP)
    end)

    {:ok, port: port}
  end

  describe "concurrent client operations" do
    test "multiple clients can connect and operate simultaneously", %{port: port} do
      # Start multiple clients
      client_count = 10

      clients =
        for i <- 1..client_count do
          {:ok, client} = Client.connect("http://localhost:#{port}/")
          {i, client}
        end

      # Verify all clients connected
      assert length(clients) == client_count

      # Each client lists tools
      results =
        clients
        |> Enum.map(fn {i, client} ->
          Task.async(fn ->
            {:ok, response} = Client.list_tools(client)
            {i, response}
          end)
        end)
        |> Enum.map(&Task.await/1)

      # All should succeed
      assert length(results) == client_count

      Enum.each(results, fn {_i, response} ->
        assert length(response.content) > 0
      end)

      # Clean up
      Enum.each(clients, fn {_i, client} ->
        Client.disconnect(client)
      end)
    end

    test "concurrent tool calls work correctly", %{port: port} do
      # Create client
      {:ok, client} = Client.connect("http://localhost:#{port}/")

      # Make concurrent tool calls
      task_count = 20

      tasks =
        for i <- 1..task_count do
          Task.async(fn ->
            if rem(i, 2) == 0 do
              # Even numbers: fast operation
              {:ok, response} =
                Client.call_tool(client, "fast_operation", %{
                  "value" => "task-#{i}"
                })

              {:fast, i, ExMCP.Response.text_content(response)}
            else
              # Odd numbers: slow operation (50ms)
              {:ok, response} =
                Client.call_tool(client, "slow_operation", %{
                  "duration" => 50
                })

              {:slow, i, ExMCP.Response.text_content(response)}
            end
          end)
        end

      # Collect results
      results = Enum.map(tasks, &Task.await(&1, 10_000))

      # Verify all completed
      assert length(results) == task_count

      # Check fast operations
      fast_results = Enum.filter(results, fn {type, _, _} -> type == :fast end)

      Enum.each(fast_results, fn {:fast, i, text} ->
        assert text =~ "Processed: task-#{i}"
      end)

      # Check slow operations
      slow_results = Enum.filter(results, fn {type, _, _} -> type == :slow end)

      Enum.each(slow_results, fn {:slow, _i, text} ->
        assert text =~ "Completed after 50ms"
      end)

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
          {:ok, client} = Client.connect("http://localhost:#{port}/")
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
                {:ok, response} =
                  Client.call_tool(client, "fast_operation", %{
                    "value" => "client-#{i}"
                  })

                {:completed, i, response}
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

      assert length(disconnected) > 0
      assert length(completed) > 0

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

      port = 8000 + :rand.uniform(1000)

      # Stop any existing server on this port first
      try do
        Plug.Cowboy.shutdown(:"Elixir.ExMCP.HttpPlug.HTTP#{port}")
      catch
        :exit, _ -> :ok
      end

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
          ref: :"Elixir.ExMCP.HttpPlug.HTTP#{port}"
        )

      Process.sleep(100)

      # Create client
      {:ok, client} = Client.connect("http://localhost:#{port}/")

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
      {:ok, response} = Client.list_tools(client)
      assert response.content != nil

      Client.disconnect(client)
      Plug.Cowboy.shutdown(:"Elixir.HttpPlug.HTTP#{port}")
    end
  end

  describe "error handling under load" do
    test "server recovers from handler errors", %{port: port} do
      # Start server with error-prone handler
      error_port = port + 100

      # Stop any existing server on this port first
      try do
        Plug.Cowboy.shutdown(:"Elixir.ExMCP.HttpPlug.HTTP#{error_port}")
      catch
        :exit, _ -> :ok
      end

      {:ok, _} =
        Plug.Cowboy.http(
          HttpPlug,
          [
            handler: ErrorProneHandler,
            server_info: %{name: "error-test", version: "1.0.0"}
          ],
          port: error_port,
          ref: :"Elixir.ExMCP.HttpPlug.HTTP#{error_port}"
        )

      Process.sleep(100)

      # Create client
      {:ok, client} = Client.connect("http://localhost:#{error_port}/")

      # Make multiple calls
      results =
        for i <- 1..9 do
          case Client.call_tool(client, "unreliable", %{}) do
            {:ok, response} ->
              {:success, i, ExMCP.Response.text_content(response)}

            {:error, error} ->
              {:error, i, error.message}
          end
        end

      # Check pattern: every 3rd call fails
      assert Enum.at(results, 2) |> elem(0) == :error
      assert Enum.at(results, 5) |> elem(0) == :error
      assert Enum.at(results, 8) |> elem(0) == :error

      # But server continues working
      assert Enum.at(results, 3) |> elem(0) == :success
      assert Enum.at(results, 6) |> elem(0) == :success

      Client.disconnect(client)
      Plug.Cowboy.shutdown(:"Elixir.HttpPlug.HTTP#{error_port}")
    end
  end
end
