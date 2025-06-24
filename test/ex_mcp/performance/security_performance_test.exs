defmodule ExMCP.Performance.SecurityPerformanceTest do
  @moduledoc """
  Performance tests for ExMCP security features.

  These tests validate that security validation meets the <100μs performance target
  and doesn't introduce significant overhead to transport operations.
  """

  use ExUnit.Case, async: false

  @moduletag :performance
  @moduletag :slow

  alias ExMCP.Transport.SecurityGuard
  alias ExMCP.Internal.SecurityConfig
  alias ExMCP.ConsentHandler.Test, as: TestConsentHandler

  @performance_target_microseconds 100

  setup_all do
    # Start test consent handler for performance tests
    {:ok, _} = TestConsentHandler.start_link()

    on_exit(fn ->
      TestConsentHandler.stop()
    end)

    :ok
  end

  setup do
    # Clear consent state between tests
    TestConsentHandler.clear_all_consents()
    :ok
  end

  describe "SecurityGuard Performance" do
    test "validates request within performance target" do
      # Setup test scenario
      request = %{
        url: "https://api.example.com/data",
        headers: [{"Authorization", "Bearer token"}],
        method: "GET",
        transport: :http,
        user_id: "test_user"
      }

      config = SecurityConfig.get_transport_config(:http)

      # Warm up
      for _ <- 1..10 do
        SecurityGuard.validate_request(request, config)
      end

      # Measure performance
      {time_microseconds, _result} =
        :timer.tc(fn ->
          SecurityGuard.validate_request(request, config)
        end)

      assert time_microseconds < @performance_target_microseconds,
             "Security validation took #{time_microseconds}μs, exceeds target of #{@performance_target_microseconds}μs"
    end

    test "internal URL validation is fast" do
      request = %{
        url: "https://localhost:8080/api",
        headers: [{"Authorization", "Bearer token"}],
        method: "GET",
        transport: :http,
        user_id: "test_user"
      }

      config = SecurityConfig.get_transport_config(:http)

      # Warm up
      for _ <- 1..10 do
        SecurityGuard.validate_request(request, config)
      end

      # Measure performance for internal URLs (should be faster)
      {time_microseconds, _result} =
        :timer.tc(fn ->
          SecurityGuard.validate_request(request, config)
        end)

      # Internal URLs should be even faster since they skip consent checks
      assert time_microseconds < @performance_target_microseconds / 2,
             "Internal URL validation took #{time_microseconds}μs, should be under #{@performance_target_microseconds / 2}μs"
    end

    test "consent cache hit performance" do
      # Pre-approve consent for fast cache hit
      TestConsentHandler.set_consent_response("test_user", "https://api.example.com", :approved)

      request = %{
        url: "https://api.example.com/data",
        headers: [],
        method: "GET",
        transport: :http,
        user_id: "test_user"
      }

      config = SecurityConfig.get_transport_config(:http)

      # First request to populate cache
      SecurityGuard.validate_request(request, config)

      # Warm up cache
      for _ <- 1..10 do
        SecurityGuard.validate_request(request, config)
      end

      # Measure cache hit performance
      {time_microseconds, _result} =
        :timer.tc(fn ->
          SecurityGuard.validate_request(request, config)
        end)

      assert time_microseconds < @performance_target_microseconds,
             "Consent cache hit took #{time_microseconds}μs, exceeds target of #{@performance_target_microseconds}μs"
    end

    test "concurrent validation performance" do
      request = %{
        url: "https://localhost:8080/api",
        headers: [{"Authorization", "Bearer token"}],
        method: "GET",
        transport: :http,
        user_id: "test_user"
      }

      config = SecurityConfig.get_transport_config(:http)

      # Test concurrent access doesn't cause significant slowdown
      tasks =
        for i <- 1..10 do
          Task.async(fn ->
            user_request = %{request | user_id: "user_#{i}"}

            {time_microseconds, _result} =
              :timer.tc(fn ->
                SecurityGuard.validate_request(user_request, config)
              end)

            time_microseconds
          end)
        end

      times = Task.await_many(tasks, 5000)
      avg_time = Enum.sum(times) / length(times)
      max_time = Enum.max(times)

      assert avg_time < @performance_target_microseconds,
             "Average concurrent validation time #{avg_time}μs exceeds target"

      assert max_time < @performance_target_microseconds * 2,
             "Max concurrent validation time #{max_time}μs exceeds reasonable threshold"
    end
  end

  describe "Transport Integration Performance" do
    test "HTTP transport security overhead is minimal" do
      # This would require actual HTTP transport integration
      # For now, we'll test the SecurityGuard component directly

      request = %{
        url: "https://api.example.com/data",
        headers: [
          {"Authorization", "Bearer token"},
          {"Content-Type", "application/json"},
          {"User-Agent", "ExMCP/1.0"}
        ],
        method: "POST",
        transport: :http,
        user_id: "http_user"
      }

      config = SecurityConfig.get_transport_config(:http)

      # Measure with multiple headers (realistic scenario)
      {time_microseconds, _result} =
        :timer.tc(fn ->
          SecurityGuard.validate_request(request, config)
        end)

      assert time_microseconds < @performance_target_microseconds,
             "HTTP security validation took #{time_microseconds}μs, exceeds target"
    end

    test "stdio transport security overhead is minimal" do
      request = %{
        url: "https://external-api.com/resource",
        headers: [],
        method: "GET",
        transport: :stdio,
        user_id: "stdio_user"
      }

      config = SecurityConfig.get_transport_config(:stdio)

      {time_microseconds, _result} =
        :timer.tc(fn ->
          SecurityGuard.validate_request(request, config)
        end)

      assert time_microseconds < @performance_target_microseconds,
             "Stdio security validation took #{time_microseconds}μs, exceeds target"
    end

    test "BEAM transport security overhead is minimal" do
      request = %{
        url: "beam://external_service/method",
        headers: [],
        method: "call",
        transport: :beam,
        user_id: "beam_user"
      }

      config = SecurityConfig.get_transport_config(:beam)

      {time_microseconds, _result} =
        :timer.tc(fn ->
          SecurityGuard.validate_request(request, config)
        end)

      assert time_microseconds < @performance_target_microseconds,
             "BEAM security validation took #{time_microseconds}μs, exceeds target"
    end
  end

  describe "Performance Regression Tests" do
    test "token stripping performance scales with header count" do
      base_headers = [{"Content-Type", "application/json"}]

      # Test with increasing numbers of sensitive headers
      for header_count <- [1, 5, 10, 20] do
        sensitive_headers =
          for i <- 1..header_count do
            {"X-Token-#{i}", "secret-#{i}"}
          end

        request = %{
          url: "https://api.example.com/data",
          headers: base_headers ++ sensitive_headers,
          method: "GET",
          transport: :http,
          user_id: "test_user"
        }

        config = SecurityConfig.get_transport_config(:http)

        {time_microseconds, _result} =
          :timer.tc(fn ->
            SecurityGuard.validate_request(request, config)
          end)

        # Performance should scale reasonably with header count
        max_time = @performance_target_microseconds * (1 + header_count / 10)

        assert time_microseconds < max_time,
               "Validation with #{header_count} headers took #{time_microseconds}μs, exceeds scaled target of #{max_time}μs"
      end
    end
  end
end
