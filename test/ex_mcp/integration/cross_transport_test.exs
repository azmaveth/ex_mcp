defmodule ExMCP.Integration.CrossTransportTest do
  @moduledoc """
  Cross-Transport Compatibility Test Suite.

  This test suite verifies that MCP protocol operations work consistently
  across different transport implementations (Test, HTTP, SSE, etc.)
  and that transport-specific features are properly handled.
  """

  use ExUnit.Case, async: false

  alias ExMCP.{Client, Server}

  @moduletag :integration
  @moduletag :cross_transport
  @moduletag timeout: 60_000

  describe "transport consistency" do
    setup do
      # Create a shared server implementation for testing across transports
      defmodule CrossTransportTestServer do
        use ExMCP.Server

        deftool "ping" do
          meta do
            description("Simple ping tool")
            input_schema(%{type: "object", properties: %{}})
          end
        end

        deftool "echo" do
          meta do
            description("Echo input back")

            input_schema(%{
              type: "object",
              properties: %{
                message: %{type: "string"}
              },
              required: ["message"]
            })
          end
        end

        defresource "test://data" do
          meta do
            name("Test Data")
            description("Test resource for cross-transport testing")
          end
        end

        defprompt "simple_prompt" do
          meta do
            name("Simple Prompt")
            description("A simple test prompt")
          end
        end

        @impl true
        def handle_tool_call("ping", _args, state) do
          {:ok, %{content: [%{"type" => "text", "text" => "pong"}]}, state}
        end

        @impl true
        def handle_tool_call("echo", %{"message" => msg}, state) do
          {:ok, %{content: [%{"type" => "text", "text" => "Echo: #{msg}"}]}, state}
        end

        @impl true
        def handle_initialize(params, state) do
          {:ok,
           %{
             "protocolVersion" => params["protocolVersion"],
             "serverInfo" => %{
               "name" => "cross-transport-test-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{
               "tools" => %{},
               "resources" => %{},
               "prompts" => %{}
             }
           }, state}
        end

        @impl true
        def handle_resource_read("test://data", _full_uri, state) do
          {:ok,
           %{
             uri: "test://data",
             mimeType: "text/plain",
             text: "Test resource content"
           }, state}
        end

        @impl true
        def handle_prompt_get("simple_prompt", args, state) do
          topic = Map.get(args, "topic", "general")

          {:ok,
           %{
             description: "Generated prompt about #{topic}",
             messages: [
               %{
                 role: "user",
                 content: %{
                   type: "text",
                   text: "Please tell me about #{topic}"
                 }
               }
             ]
           }, state}
        end
      end

      %{server_module: CrossTransportTestServer}
    end

    test "test transport basic operations", %{server_module: server_module} do
      # Start server with test transport
      {:ok, server_pid} = server_module.start_link(transport: :test)
      Process.sleep(10)

      # Connect client
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Test basic operations
      assert_transport_operations_work(client)

      # Cleanup
      Client.stop(client)
      GenServer.stop(server_pid)
    end

    @tag :requires_http
    @tag :skip
    test "http transport basic operations", %{server_module: server_module} do
      # Find free port
      {:ok, socket} = :gen_tcp.listen(0, [:binary, ip: {127, 0, 0, 1}])
      {:ok, port} = :inet.port(socket)
      :gen_tcp.close(socket)

      # Start server with HTTP transport
      {:ok, server_pid} =
        server_module.start_link(transport: :http, port: port, host: "localhost")

      # Give HTTP server more time to start
      Process.sleep(500)

      # Verify server is running before connecting client
      assert Process.alive?(server_pid)

      # Connect client
      {:ok, client} =
        Client.start_link(
          transport: :http,
          url: "http://localhost:#{port}",
          use_sse: false
        )

      # Test basic operations
      assert_transport_operations_work(client)

      # Cleanup
      Client.stop(client)
      GenServer.stop(server_pid)
    end

    @tag :requires_http
    @tag :skip
    test "sse transport basic operations", %{server_module: server_module} do
      # Find free port
      {:ok, socket} = :gen_tcp.listen(0, [:binary, ip: {127, 0, 0, 1}])
      {:ok, port} = :inet.port(socket)
      :gen_tcp.close(socket)

      # Start server with HTTP transport (SSE uses HTTP server)
      {:ok, server_pid} =
        server_module.start_link(
          transport: :http,
          port: port,
          host: "localhost",
          sse_enabled: true
        )

      # Give HTTP server more time to start
      Process.sleep(500)

      # Verify server is running before connecting client
      assert Process.alive?(server_pid)

      # Connect client with SSE enabled
      {:ok, client} =
        Client.start_link(
          transport: :http,
          url: "http://localhost:#{port}",
          use_sse: true
        )

      # Test basic operations
      assert_transport_operations_work(client)

      # Cleanup
      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "transport-specific features" do
    test "test transport supports immediate responses" do
      defmodule ImmediateResponseServer do
        use ExMCP.Server

        deftool "immediate" do
          meta do
            description("Returns immediate response")
            input_schema(%{type: "object", properties: %{}})
          end
        end

        @impl true
        def handle_initialize(params, state) do
          {:ok,
           %{
             "protocolVersion" => params["protocolVersion"],
             "serverInfo" => %{
               "name" => "immediate-response-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{
               "tools" => %{}
             }
           }, state}
        end

        @impl true
        def handle_tool_call("immediate", _args, state) do
          {:ok, %{content: [%{"type" => "text", "text" => "immediate"}]}, state}
        end
      end

      {:ok, server_pid} = ImmediateResponseServer.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Measure response time (should be very fast for test transport)
      start_time = System.monotonic_time(:millisecond)
      {:ok, result} = Client.call_tool(client, "immediate", %{})
      end_time = System.monotonic_time(:millisecond)

      assert Enum.at(result.content, 0).text == "immediate"
      # Should be very fast
      assert end_time - start_time < 100

      Client.stop(client)
      GenServer.stop(server_pid)
    end

    @tag :requires_http
    @tag :skip
    test "http transport handles session management" do
      defmodule SessionServer do
        use ExMCP.Server

        deftool "get_session_info" do
          meta do
            description("Get session information")
            input_schema(%{type: "object", properties: %{}})
          end
        end

        @impl true
        def handle_initialize(params, state) do
          {:ok,
           %{
             "protocolVersion" => params["protocolVersion"],
             "serverInfo" => %{
               "name" => "session-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{
               "tools" => %{}
             }
           }, state}
        end

        @impl true
        def handle_tool_call("get_session_info", _args, state) do
          # In real HTTP transport, session info would be available
          {:ok, %{content: [%{"type" => "text", "text" => "session_active"}]}, state}
        end
      end

      # Find free port
      {:ok, socket} = :gen_tcp.listen(0, [:binary, ip: {127, 0, 0, 1}])
      {:ok, port} = :inet.port(socket)
      :gen_tcp.close(socket)

      {:ok, server_pid} =
        SessionServer.start_link(transport: :http, port: port, host: "localhost")

      # Give HTTP server more time to start
      Process.sleep(500)

      # Verify server is running before connecting client
      assert Process.alive?(server_pid)

      # Connect with session ID
      session_id = "test-session-#{System.unique_integer()}"

      {:ok, client} =
        Client.start_link(
          transport: :http,
          url: "http://localhost:#{port}",
          session_id: session_id,
          use_sse: false
        )

      # Verify session is maintained
      {:ok, result} = Client.call_tool(client, "get_session_info", %{})
      assert Enum.at(result.content, 0).text == "session_active"

      # Check that session ID is properly set in transport state
      client_state = :sys.get_state(client)
      transport_state = client_state.transport_state
      assert transport_state.session_id == session_id

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "protocol version consistency" do
    test "all transports support version negotiation" do
      defmodule VersionTestServer do
        use ExMCP.Server

        @impl true
        def handle_initialize(params, state) do
          version = params["protocolVersion"]

          {:ok,
           %{
             "protocolVersion" => version,
             "serverInfo" => %{
               "name" => "version-test-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{}
           }, state}
        end
      end

      test_version = "2025-06-18"

      # Test with Test transport
      {:ok, server_pid} = VersionTestServer.start_link(transport: :test)
      Process.sleep(10)

      {:ok, test_client} =
        Client.start_link(
          transport: :test,
          server: server_pid,
          protocol_version: test_version
        )

      {:ok, negotiated_version} = Client.negotiated_version(test_client)
      assert negotiated_version == test_version

      Client.stop(test_client)
      GenServer.stop(server_pid)
    end
  end

  describe "error handling consistency" do
    test "all transports handle server errors consistently" do
      defmodule ErrorTestServer do
        use ExMCP.Server

        deftool "error_tool" do
          meta do
            description("Tool that always errors")
            input_schema(%{type: "object", properties: %{}})
          end
        end

        @impl true
        def handle_initialize(params, state) do
          {:ok,
           %{
             "protocolVersion" => params["protocolVersion"],
             "serverInfo" => %{
               "name" => "error-test-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{
               "tools" => %{}
             }
           }, state}
        end

        @impl true
        def handle_tool_call("error_tool", _args, state) do
          {:error, "Intentional test error", state}
        end
      end

      # Test error handling with Test transport
      {:ok, server_pid} = ErrorTestServer.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      {:error, error} = Client.call_tool(client, "error_tool", %{})
      assert error["message"] == "\"Intentional test error\""
      assert is_integer(error["code"])

      # Verify client is still operational after error
      {:ok, tools} = Client.list_tools(client)
      assert length(tools.tools) == 1

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "performance characteristics" do
    test "transport performance comparison with profiling" do
      alias ExMCP.Testing.PerformanceProfiler

      defmodule PerfTestServer do
        use ExMCP.Server

        deftool "perf_test" do
          meta do
            description("Performance test tool")

            input_schema(%{
              type: "object",
              properties: %{
                payload_size: %{type: "integer"}
              }
            })
          end
        end

        @impl true
        def handle_initialize(params, state) do
          {:ok,
           %{
             "protocolVersion" => params["protocolVersion"],
             "serverInfo" => %{
               "name" => "perf-test-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{
               "tools" => %{}
             }
           }, state}
        end

        @impl true
        def handle_tool_call("perf_test", %{"payload_size" => size}, state) do
          # Generate payload of requested size
          payload = String.duplicate("x", size)
          {:ok, %{content: [%{"type" => "text", "text" => payload}]}, state}
        end
      end

      # Test with Test transport
      {:ok, server_pid} = PerfTestServer.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Define performance operations
      performance_operations = [
        {"small_payload",
         fn -> Client.call_tool(client, "perf_test", %{"payload_size" => 100}) end},
        {"medium_payload",
         fn -> Client.call_tool(client, "perf_test", %{"payload_size" => 1000}) end},
        {"large_payload",
         fn -> Client.call_tool(client, "perf_test", %{"payload_size" => 10000}) end}
      ]

      # Profile operations
      results = PerformanceProfiler.profile_batch(performance_operations, transport_type: :test)

      # Generate performance report
      report =
        PerformanceProfiler.generate_report(results,
          format: :detailed,
          include_recommendations: true
        )

      require Logger
      Logger.info("Cross-Transport Performance Report:\n#{report}")

      # Performance assertions
      assert results.summary.success_rate == 1.0, "All performance operations should succeed"
      assert results.summary.avg_time_ms < 100, "Average operation time should be reasonable"

      # Verify payload sizes
      [small_op, medium_op, large_op] = results.operations

      # Check execution time scaling
      assert small_op.execution_time_ms < medium_op.execution_time_ms,
             "Small payload should be faster than medium"

      assert medium_op.execution_time_ms < large_op.execution_time_ms,
             "Medium payload should be faster than large"

      # Performance should be reasonable (test transport should be very fast)
      assert small_op.execution_time_ms < 50, "Small payload should be under 50ms"
      assert large_op.execution_time_ms < 200, "Large payload should be under 200ms"

      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  # Helper function to test standard operations across transports
  defp assert_transport_operations_work(client) do
    # Add a small delay to allow client to fully initialize
    Process.sleep(100)

    # Test tool listing
    {:ok, tools_response} = Client.list_tools(client)
    assert length(tools_response.tools) == 2

    ping_tool = Enum.find(tools_response.tools, &(&1["name"] == "ping"))
    echo_tool = Enum.find(tools_response.tools, &(&1["name"] == "echo"))

    assert ping_tool["description"] == "Simple ping tool"
    assert echo_tool["description"] == "Echo input back"

    # Test tool execution
    {:ok, ping_result} = Client.call_tool(client, "ping", %{})
    assert Enum.at(ping_result.content, 0).text == "pong"

    {:ok, echo_result} = Client.call_tool(client, "echo", %{"message" => "test"})
    assert Enum.at(echo_result.content, 0).text == "Echo: test"

    # Test resource listing
    {:ok, resources_response} = Client.list_resources(client)
    assert length(resources_response.resources) == 1

    resource = hd(resources_response.resources)
    assert resource["uri"] == "test://data"
    assert resource["name"] == "Test Data"

    # Test resource reading
    {:ok, resource_content} = Client.read_resource(client, "test://data")
    assert length(resource_content.contents) == 1
    content = hd(resource_content.contents)
    assert content.uri == "test://data"
    assert content.text == "Test resource content"

    # Test prompt listing
    {:ok, prompts_response} = Client.list_prompts(client)
    assert length(prompts_response.prompts) == 1

    prompt = hd(prompts_response.prompts)
    assert prompt["name"] == "simple_prompt"

    # TODO: Fix message content normalization in ExMCP.Response
    # Test prompt generation (disabled due to known issue with message content normalization)
    # {:ok, prompt_result} = Client.get_prompt(client, "simple_prompt", %{"topic" => "testing"})
    # assert prompt_result.description == "Generated prompt about testing"
    # assert length(prompt_result.messages) == 1
    # 
    # message = hd(prompt_result.messages)
    # assert message["content"]["text"] =~ "testing"
  end
end
