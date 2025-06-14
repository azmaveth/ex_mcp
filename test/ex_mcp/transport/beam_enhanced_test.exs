defmodule ExMCP.Transport.BeamEnhancedTest do
  @moduledoc """
  Integration tests for the enhanced BEAM transport with ranch architecture.
  
  These tests validate that the new architecture maintains compatibility with
  the MCP protocol while delivering the promised performance improvements.
  """
  
  use ExUnit.Case, async: true
  require Logger

  alias ExMCP.Transport.Beam.{Server, Client, Acceptor, Supervisor}
  alias ExMCP.Transport.Beam.{Connection, Frame, Correlation, Serializer}

  @test_port_base 19000

  describe "Frame Protocol" do
    test "encodes and decodes messages correctly" do
      message = %{"method" => "ping", "params" => %{"data" => "test"}}
      
      assert {:ok, frame_data} = Frame.encode(message, 1, :rpc_request)
      assert is_binary(frame_data)
      assert byte_size(frame_data) > 0
      
      assert {:ok, decoded} = Frame.decode(frame_data)
      assert decoded.version == 1
      assert decoded.type == :rpc_request
      assert decoded.payload == message
    end

    test "validates frame size limits" do
      # Create a message that exceeds the 1MB limit (use random data that won't compress well)
      large_data = for _ <- 1..1_048_577, into: <<>>, do: <<:rand.uniform(256) - 1>>
      large_message = %{"data" => large_data}
      
      assert {:error, :frame_too_large} = Frame.encode(large_message)
    end

    test "handles different message types" do
      message = %{"type" => "heartbeat"}
      
      for msg_type <- [:rpc_request, :rpc_response, :notification, :heartbeat, :batch] do
        assert {:ok, frame_data} = Frame.encode(message, 1, msg_type)
        assert {:ok, decoded} = Frame.decode(frame_data)
        assert decoded.type == msg_type
      end
    end

    test "rejects invalid frames" do
      assert {:error, :invalid_frame_format} = Frame.decode(<<1, 2, 3>>)
      assert {:error, :frame_too_large} = Frame.decode(<<2_000_000::32, 1::8, 1::8, "data">>)
    end
  end

  describe "Serialization" do
    test "ETF serialization provides optimal performance" do
      message = %{
        "method" => "complex_operation",
        "params" => %{
          "numbers" => Enum.to_list(1..100),
          "nested" => %{"deeply" => %{"nested" => %{"data" => "value"}}},
          "timestamp" => System.system_time(:millisecond)
        }
      }

      # Test ETF
      assert {:ok, etf_data} = Serializer.serialize(message, :etf)
      assert {:ok, ^message} = Serializer.deserialize(etf_data, :etf)
      
      # Test JSON  
      assert {:ok, json_data} = Serializer.serialize(message, :json)
      assert {:ok, json_decoded} = Serializer.deserialize(json_data, :json)
      
      # ETF should be more compact for complex Elixir terms
      assert byte_size(etf_data) < byte_size(json_data)
    end

    test "format selection works correctly" do
      assert Serializer.optimal_format(:local, :beam_to_beam) == :etf
      assert Serializer.optimal_format(:local, :cross_platform) == :json
      assert Serializer.optimal_format(:distributed, :anything) == :json
    end

    test "format availability detection" do
      assert Serializer.format_available?(:etf) == true
      assert Serializer.format_available?(:json) == true
      assert Serializer.format_available?(:protobuf) == false
    end
  end

  describe "Registry Correlation" do
    setup do
      # Start correlation service for each test
      {:ok, correlation_pid} = Correlation.start_link([])
      on_exit(fn -> GenServer.stop(correlation_pid, :normal) end)
      :ok
    end

    test "registers and correlates requests efficiently" do
      # Register multiple requests
      correlation_ids = for i <- 1..10 do
        Correlation.register_request(self(), %{test_id: i})
      end

      assert length(correlation_ids) == 10
      assert Enum.all?(correlation_ids, &is_reference/1)
      
      # Send responses
      for {correlation_id, i} <- Enum.with_index(correlation_ids, 1) do
        response = %{"result" => "response_#{i}"}
        assert :ok = Correlation.send_response(correlation_id, response)
      end

      # Verify all responses received
      responses = for i <- 1..10 do
        receive do
          {:mcp_response, _correlation_id, response} -> response
        after
          1000 -> :timeout
        end
      end

      assert length(responses) == 10
      refute Enum.any?(responses, &(&1 == :timeout))
    end

    test "handles request timeouts correctly" do
      correlation_id = Correlation.register_request(self(), %{timeout: 100})
      
      # Wait for timeout
      assert {:error, :timeout} = Correlation.wait_for_response(correlation_id, 200)
    end

    test "cancels requests properly" do
      correlation_id = Correlation.register_request(self(), %{timeout: 5000})
      
      assert :ok = Correlation.cancel_request(correlation_id)
      assert {:error, :not_found} = Correlation.send_response(correlation_id, %{"result" => "test"})
    end

    test "tracks statistics accurately" do
      initial_stats = Correlation.get_stats()
      
      # Register some requests
      _id1 = Correlation.register_request(self(), %{})
      _id2 = Correlation.register_request(self(), %{})
      
      stats_after_register = Correlation.get_stats()
      assert stats_after_register.total_requests == initial_stats.total_requests + 2
      assert stats_after_register.active_requests == initial_stats.active_requests + 2
    end
  end

  describe "Server and Client Integration" do
    test "basic server startup and client connection" do
      port = @test_port_base + 1
      
      # Start server
      {:ok, server_pid, actual_port} = Server.start_test_server(port: port)
      
      # Verify server is running
      assert {:ok, ^actual_port} = Server.get_port(server_pid)
      assert {:ok, 0} = Server.get_connection_count(server_pid)
      
      # Start client
      {:ok, client_pid} = Client.connect([
        host: "localhost",
        port: actual_port,
        auto_reconnect: false
      ])

      # Wait for connection
      eventually(fn ->
        Client.status(client_pid) == :connected
      end)

      # Verify connection count increased
      assert {:ok, 1} = Server.get_connection_count(server_pid)
      
      # Clean up
      Client.close(client_pid)
      Server.stop(server_pid)
    end

    test "request-response cycle with correlation" do
      port = @test_port_base + 2
      
      {:ok, server_pid, actual_port} = Server.start_test_server(port: port)
      {:ok, client_pid} = Client.connect([
        host: "localhost", 
        port: actual_port,
        auto_reconnect: false
      ])

      eventually(fn ->
        Client.status(client_pid) == :connected
      end)

      # Send ping request
      start_time = System.monotonic_time(:microsecond)
      assert {:ok, response} = Client.call(client_pid, %{"method" => "ping"})
      end_time = System.monotonic_time(:microsecond)
      
      # Verify response
      assert %{"result" => %{"message" => "pong"}} = response
      
      # Verify performance (should be under 1ms for local connection)
      latency_us = end_time - start_time
      assert latency_us < 1000, "Latency too high: #{latency_us}μs"
      
      Logger.info("Request latency: #{latency_us}μs")
      
      # Clean up
      Client.close(client_pid)
      Server.stop(server_pid)
    end

    test "echo request with complex data" do
      port = @test_port_base + 3
      
      {:ok, server_pid, actual_port} = Server.start_test_server(port: port)
      {:ok, client_pid} = Client.connect([host: "localhost", port: actual_port])

      eventually(fn ->
        Client.status(client_pid) == :connected
      end)

      # Complex data structure
      complex_params = %{
        "numbers" => Enum.to_list(1..100),
        "nested" => %{
          "level1" => %{
            "level2" => %{
              "data" => "deep_value",
              "list" => ["a", "b", "c"]
            }
          }
        },
        "timestamp" => System.system_time(:millisecond)
      }

      assert {:ok, response} = Client.call(client_pid, %{
        "method" => "echo",
        "params" => complex_params
      })

      assert %{"result" => %{"echo" => ^complex_params}} = response
      
      Client.close(client_pid)
      Server.stop(server_pid)
    end

    test "notification handling" do
      port = @test_port_base + 4
      
      {:ok, server_pid, actual_port} = Server.start_test_server(port: port)
      {:ok, client_pid} = Client.connect([host: "localhost", port: actual_port])

      eventually(fn ->
        Client.status(client_pid) == :connected
      end)

      # Send notification
      notification = %{
        "method" => "log_message", 
        "params" => %{"message" => "test notification"}
      }
      
      assert :ok = Client.notify(client_pid, notification)
      
      # Verify server received it (check stats)
      Process.sleep(100)  # Give server time to process
      
      assert {:ok, stats_response} = Client.call(client_pid, %{"method" => "stats"})
      assert %{"result" => %{"notifications_received" => notifications}} = stats_response
      assert notifications >= 1
      
      Client.close(client_pid)
      Server.stop(server_pid)
    end

    test "multiple concurrent clients" do
      port = @test_port_base + 5
      
      {:ok, server_pid, actual_port} = Server.start_test_server(port: port, max_connections: 10)
      
      # Start multiple clients
      client_count = 5
      clients = for i <- 1..client_count do
        {:ok, client_pid} = Client.connect([
          host: "localhost", 
          port: actual_port,
          auto_reconnect: false
        ])
        {i, client_pid}
      end

      # Wait for all connections
      eventually(fn ->
        {:ok, count} = Server.get_connection_count(server_pid)
        count == client_count
      end)

      # Send requests from all clients concurrently
      tasks = for {i, client_pid} <- clients do
        Task.async(fn ->
          assert {:ok, response} = Client.call(client_pid, %{"method" => "ping"})
          {i, response}
        end)
      end

      # Collect all responses
      responses = Task.await_many(tasks, 5000)
      assert length(responses) == client_count
      
      # Verify all got pong responses
      for {_i, response} <- responses do
        assert %{"result" => %{"message" => "pong"}} = response
      end

      # Clean up
      for {_i, client_pid} <- clients do
        Client.close(client_pid)
      end
      Server.stop(server_pid)
    end

    test "connection statistics and monitoring" do
      port = @test_port_base + 6
      
      {:ok, server_pid, actual_port} = Server.start_test_server(port: port)
      
      # Initial stats
      initial_info = Server.get_info(server_pid)
      assert Map.has_key?(initial_info, :active_connections)
      assert initial_info.active_connections == 0

      # Connect client
      {:ok, client_pid} = Client.connect([host: "localhost", port: actual_port])
      
      eventually(fn ->
        Client.status(client_pid) == :connected
      end)

      # Check updated stats
      updated_info = Server.get_info(server_pid)
      assert updated_info.active_connections == 1

      # Check client stats
      client_stats = Client.get_stats(client_pid)
      assert client_stats.client_connected == true
      assert Map.has_key?(client_stats, :bytes_sent)
      assert Map.has_key?(client_stats, :messages_sent)

      Client.close(client_pid)
      Server.stop(server_pid)
    end
  end

  describe "Performance Benchmarks" do
    test "latency measurement under load" do
      port = @test_port_base + 7
      
      {:ok, server_pid, actual_port} = Server.start_test_server(port: port)
      {:ok, client_pid} = Client.connect([host: "localhost", port: actual_port])

      eventually(fn ->
        Client.status(client_pid) == :connected
      end)

      # Warm up
      for _ <- 1..10 do
        Client.call(client_pid, %{"method" => "ping"})
      end

      # Measure latency for multiple requests
      latencies = for _ <- 1..100 do
        start_time = System.monotonic_time(:microsecond)
        {:ok, _response} = Client.call(client_pid, %{"method" => "ping"})
        end_time = System.monotonic_time(:microsecond)
        end_time - start_time
      end

      avg_latency = Enum.sum(latencies) / length(latencies)
      max_latency = Enum.max(latencies)
      min_latency = Enum.min(latencies)

      Logger.info("Latency stats - Avg: #{Float.round(avg_latency, 1)}μs, " <>
                  "Min: #{min_latency}μs, Max: #{max_latency}μs")

      # Performance targets (should be much better than old architecture)
      assert avg_latency < 50, "Average latency too high: #{avg_latency}μs"
      assert max_latency < 200, "Max latency too high: #{max_latency}μs"
      
      Client.close(client_pid)
      Server.stop(server_pid)
    end

    test "throughput measurement" do
      port = @test_port_base + 8
      
      {:ok, server_pid, actual_port} = Server.start_test_server(port: port)
      {:ok, client_pid} = Client.connect([host: "localhost", port: actual_port])

      eventually(fn ->
        Client.status(client_pid) == :connected
      end)

      # Measure throughput
      request_count = 1000
      start_time = System.monotonic_time(:millisecond)
      
      # Send requests as fast as possible
      for i <- 1..request_count do
        spawn(fn ->
          Client.call(client_pid, %{"method" => "ping", "id" => i})
        end)
      end

      # Wait for all responses (simplified - just wait a reasonable time)
      Process.sleep(5000)
      
      end_time = System.monotonic_time(:millisecond)
      duration_ms = end_time - start_time
      throughput = request_count / (duration_ms / 1000)

      Logger.info("Throughput: #{Float.round(throughput, 1)} requests/second")

      # Should handle at least 200 requests/second locally
      assert throughput > 200, "Throughput too low: #{throughput} req/s"
      
      Client.close(client_pid)
      Server.stop(server_pid)
    end
  end

  describe "Error Handling and Resilience" do
    test "handles client disconnection gracefully" do
      port = @test_port_base + 9
      
      {:ok, server_pid, actual_port} = Server.start_test_server(port: port)
      {:ok, client_pid} = Client.connect([host: "localhost", port: actual_port])

      eventually(fn ->
        Client.status(client_pid) == :connected
      end)

      assert {:ok, 1} = Server.get_connection_count(server_pid)

      # Abruptly stop client
      Process.exit(client_pid, :kill)
      
      # Server should detect disconnection
      eventually(fn ->
        {:ok, count} = Server.get_connection_count(server_pid)
        count == 0
      end)

      Server.stop(server_pid)
    end

    test "handles server restart correctly" do
      port = @test_port_base + 10
      
      {:ok, server_pid, actual_port} = Server.start_test_server(port: port)
      {:ok, client_pid} = Client.connect([
        host: "localhost", 
        port: actual_port,
        auto_reconnect: true
      ])

      eventually(fn ->
        Client.status(client_pid) == :connected
      end)

      # Stop server
      Server.stop(server_pid)
      
      # Client should detect disconnection
      eventually(fn ->
        Client.status(client_pid) != :connected
      end)

      # Restart server on same port
      {:ok, _server_pid2, _} = Server.start_test_server(port: actual_port)
      
      # Client should reconnect automatically
      eventually(fn ->
        Client.status(client_pid) == :connected
      end, 10_000)

      Client.close(client_pid)
    end

    test "rejects requests when not connected" do
      {:ok, client_pid} = Client.connect([
        host: "localhost",
        port: 99999,  # Non-existent port
        auto_reconnect: false
      ])

      # Should not be connected
      assert Client.status(client_pid) != :connected
      
      # Requests should fail
      assert {:error, :not_connected} = Client.call(client_pid, %{"method" => "ping"})
      assert {:error, :not_connected} = Client.notify(client_pid, %{"method" => "log"})

      Client.close(client_pid)
    end
  end

  # Helper functions

  defp eventually(fun, timeout \\ 5_000) do
    eventually(fun, timeout, 50)
  end

  defp eventually(fun, timeout, interval) when timeout > 0 do
    if fun.() do
      :ok
    else
      Process.sleep(interval)
      eventually(fun, timeout - interval, interval)
    end
  end

  defp eventually(_fun, _timeout, _interval) do
    flunk("Eventually condition not met within timeout")
  end
end