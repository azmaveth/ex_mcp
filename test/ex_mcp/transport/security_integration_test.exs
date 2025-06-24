defmodule ExMcp.Transport.SecurityIntegrationTest do
  use ExUnit.Case, async: true

  # These tests assume the existence of several helper modules:
  #
  # - `ExMcp.Security.SecurityGuard`: The security middleware being tested.
  #
  # - `ExMcp.Test.Support.Transports`: A helper module with functions to start
  #   and interact with test versions of each transport (HTTP, Stdio, BEAM).
  #   - Each transport helper is expected to return `{:ok, response, received_command}`
  #     on success, where `received_command` is the command as seen by the
  #     final handler (after middleware processing).
  #
  # - `ExMcp.Test.ConsentHandler`: A mock consent handler that allows setting
  #   consent status for specific tokens during tests.

  alias ExMcp.Security.SecurityGuard
  alias ExMcp.Test.Support.Transports
  alias ExMcp.Test.ConsentHandler

  @command %{jsonrpc: "2.0", method: "test_method", params: %{foo: "bar"}, id: 1}
  @token "secret-test-token-for-integration"

  # Reset consent state before each test to ensure isolation.
  setup do
    ConsentHandler.clear_all_consent()
    :ok
  end

  # This macro defines a reusable suite of security tests that can be run
  # against any transport client configured in the test context.
  defmacrop run_security_tests() do
    quote do
      test "allows request and strips token when consent is granted", %{client: client} do
        ConsentHandler.grant_consent(@token)
        request_with_token = Map.put(@command, :meta, %{token: @token})

        {:ok, response, received_command} = client.(request_with_token)

        # Assert the operation was successful
        assert response.result == "ok"
        # Assert the security token was stripped and not passed to the handler
        refute get_in(received_command, [:meta, :token])
      end

      test "rejects request when consent is denied", %{client: client} do
        ConsentHandler.deny_consent(@token)
        request_with_token = Map.put(@command, :meta, %{token: @token})

        {:error, response} = client.(request_with_token)

        assert response.error.code == -32001
        assert response.error.message == "Access Denied: User consent not granted."
      end

      test "rejects request when security token is missing", %{client: client} do
        # A request without any token metadata
        {:error, response} = client.(@command)

        assert response.error.code == -32002
        assert response.error.message == "Access Denied: Missing security token."
      end

      test "rejects request when security token is invalid", %{client: client} do
        # The TestConsentHandler is not configured to know about this token,
        # so it will be treated as invalid/unconsented.
        request_with_invalid_token = Map.put(@command, :meta, %{token: "invalid-token"})

        {:error, response} = client.(request_with_invalid_token)

        assert response.error.code == -32001
        assert response.error.message == "Access Denied: User consent not granted."
      end
    end
  end

  describe "HTTP Transport Integration" do
    setup do
      # Assumes a helper that starts a test endpoint with the SecurityGuard.
      {:ok, endpoint} = Transports.Http.start_test_endpoint(security_guard: SecurityGuard)

      # The client function abstracts away the transport details (e.g., headers).
      client = fn command ->
        token = get_in(command, [:meta, :token])
        headers = if token, do: [{"authorization", "Bearer " <> token}], else: []
        # The SecurityGuard is expected to handle the token from the header.
        # The command sent over HTTP should not contain the meta field.
        command_to_send = Map.delete(command, :meta)
        Transports.Http.request(endpoint, command_to_send, headers)
      end

      %{client: client}
    end

    run_security_tests()
  end

  describe "Stdio Transport Integration" do
    setup do
      {:ok, pid} = Transports.Stdio.start_test_process(security_guard: SecurityGuard)
      client = &Transports.Stdio.request(pid, &1)
      %{client: client}
    end

    run_security_tests()
  end

  describe "BEAM Transport Integration" do
    setup do
      {:ok, pid} = Transports.Beam.start_test_server(security_guard: SecurityGuard)
      client = &Transports.Beam.request(pid, &1)
      %{client: client}
    end

    run_security_tests()
  end

  describe "Performance Benchmarks" do
    setup do
      # Use the BEAM transport for benchmarking as it has the lowest overhead.
      {:ok, pid} = Transports.Beam.start_test_server(security_guard: SecurityGuard)
      client = &Transports.Beam.request(pid, &1)
      # Pre-grant consent so we measure the "happy path" performance.
      ConsentHandler.grant_consent(@token)
      %{client: client}
    end

    @tag :benchmark
    test "SecurityGuard overhead is within target (<100µs)", %{client: client} do
      request_with_token = Map.put(@command, :meta, %{token: @token})

      # Benchee provides statistically sound measurements.
      # This test will only run if `mix test --include benchmark` is used.
      result =
        Benchee.run(
          %{"security_guard_check" => fn -> client.(request_with_token) end},
          time: 1,
          # Don't print results to console during the test run.
          formatters: []
        )

      avg_time = result["security_guard_check"].statistics.average
      # Ensure the average execution time is less than 100 microseconds.
      assert avg_time < 100.0, "Expected average time < 100µs, but got #{avg_time}µs"
    end
  end
end
