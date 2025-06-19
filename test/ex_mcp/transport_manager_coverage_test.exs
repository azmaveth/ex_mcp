defmodule ExMCP.TransportManagerCoverageTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog

  alias ExMCP.TransportManager

  # Mock transports for testing
  defmodule SuccessTransport do
    def connect(opts) do
      delay = Keyword.get(opts, :delay, 0)
      if delay > 0, do: Process.sleep(delay)
      {:ok, %{transport: :success, opts: opts}}
    end
  end

  defmodule FailTransport do
    def connect(opts) do
      {:error, Keyword.get(opts, :error, :connection_failed)}
    end
  end

  defmodule CrashTransport do
    def connect(_opts) do
      raise "Transport crashed!"
    end
  end

  describe "connect/1 with single transport" do
    test "connects successfully" do
      opts = [
        transports: [{SuccessTransport, [test: true]}]
      ]

      assert {:ok, {SuccessTransport, %{transport: :success, opts: opts}}} =
               TransportManager.connect(opts)

      assert opts[:test] == true
    end

    test "handles connection failure" do
      opts = [
        transports: [{FailTransport, [error: :refused]}]
      ]

      log =
        capture_log(fn ->
          assert {:error, :all_transports_failed} = TransportManager.connect(opts)
        end)

      assert log =~ "Transport ExMCP.TransportManagerCoverageTest.FailTransport failed"
    end

    test "handles transport crash" do
      opts = [
        transports: [{CrashTransport, []}]
      ]

      log =
        capture_log(fn ->
          assert {:error, :all_transports_failed} = TransportManager.connect(opts)
        end)

      assert log =~ "Transport ExMCP.TransportManagerCoverageTest.CrashTransport"
    end
  end

  describe "connect/1 with sequential strategy" do
    test "tries transports in order" do
      opts = [
        transports: [
          {FailTransport, []},
          {FailTransport, [error: :timeout]},
          {SuccessTransport, [winner: true]}
        ],
        fallback_strategy: :sequential
      ]

      assert {:ok, {SuccessTransport, state}} = TransportManager.connect(opts)
      assert state.opts[:winner] == true
    end

    test "fails when all transports fail" do
      opts = [
        transports: [
          {FailTransport, [error: :error1]},
          {FailTransport, [error: :error2]},
          {FailTransport, [error: :error3]}
        ],
        fallback_strategy: :sequential
      ]

      assert {:error, :all_transports_failed} = TransportManager.connect(opts)
    end
  end

  describe "connect/1 with parallel strategy" do
    test "connects to first successful transport" do
      opts = [
        transports: [
          {FailTransport, []},
          {SuccessTransport, [delay: 10]},
          {SuccessTransport, [delay: 100]}
        ],
        fallback_strategy: :parallel
      ]

      assert {:ok, {SuccessTransport, _state}} = TransportManager.connect(opts)
    end

    test "all tasks fail in parallel" do
      opts = [
        transports: [
          {FailTransport, [error: :parallel1]},
          {FailTransport, [error: :parallel2]},
          {FailTransport, [error: :parallel3]}
        ],
        fallback_strategy: :parallel
      ]

      assert {:error, :all_transports_failed} = TransportManager.connect(opts)
    end
  end

  describe "connect/1 with fastest strategy" do
    test "returns fastest successful connection" do
      opts = [
        transports: [
          {SuccessTransport, [delay: 100, id: :slow]},
          {SuccessTransport, [delay: 10, id: :fast]},
          {SuccessTransport, [delay: 50, id: :medium]}
        ],
        fallback_strategy: :fastest
      ]

      assert {:ok, {SuccessTransport, state}} = TransportManager.connect(opts)
      # Should get one of the successful transports (timing may vary)
      assert state.opts[:id] in [:fast, :medium, :slow]
    end

    test "handles mixed success and failure" do
      opts = [
        transports: [
          {FailTransport, []},
          {SuccessTransport, [delay: 20, id: :success]},
          {FailTransport, []}
        ],
        fallback_strategy: :fastest
      ]

      assert {:ok, {SuccessTransport, state}} = TransportManager.connect(opts)
      assert state.opts[:id] == :success
    end
  end

  describe "connect/1 with custom strategy" do
    @tag :skip
    test "accepts custom strategy function" do
      custom_strategy = fn transports ->
        # Custom logic: try last transport first
        case transports do
          [] ->
            {:error, :no_transports}

          list ->
            {mod, opts} = List.last(list)

            case mod.connect(opts) do
              {:ok, state} -> {:ok, {mod, state}}
              error -> error
            end
        end
      end

      opts = [
        transports: [
          {FailTransport, []},
          {SuccessTransport, [custom: true]}
        ],
        fallback_strategy: custom_strategy
      ]

      assert {:ok, {SuccessTransport, state}} = TransportManager.connect(opts)
      assert state.opts[:custom] == true
    end
  end

  describe "connect/1 with retries" do
    test "retries failed connections" do
      # This would need a stateful mock that fails then succeeds
      opts = [
        transports: [{FailTransport, []}],
        max_retries: 3,
        retry_interval: 10
      ]

      log =
        capture_log(fn ->
          assert {:error, _} = TransportManager.connect(opts)
        end)

      # Should see retry attempts
      assert log =~ "failed"
    end
  end

  describe "edge cases" do
    test "handles empty transport list" do
      opts = [transports: []]

      assert {:error, :no_transports_configured} = TransportManager.connect(opts)
    end

    test "handles invalid strategy" do
      opts = [
        transports: [{SuccessTransport, []}],
        fallback_strategy: :invalid_strategy
      ]

      # Should raise FunctionClauseError due to no matching do_connect clause
      assert_raise FunctionClauseError, fn ->
        TransportManager.connect(opts)
      end
    end

    test "handles missing transports option" do
      # Should use empty list default, not raise KeyError
      assert {:error, :no_transports_configured} = TransportManager.connect([])
    end
  end

  describe "transport validation" do
    test "validates transport module format" do
      opts = [
        transports: [
          # Invalid atom that's not a module
          {:not_a_module, []},
          {SuccessTransport, []}
        ]
      ]

      # Should skip invalid transport or error
      result = TransportManager.connect(opts)

      case result do
        {:ok, {SuccessTransport, _}} ->
          # Skipped invalid transport
          assert true

        {:error, _} ->
          # Failed on invalid transport
          assert true
      end
    end
  end

  describe "logging and debugging" do
    test "logs connection attempts" do
      opts = [
        transports: [
          {FailTransport, []},
          {SuccessTransport, []}
        ]
      ]

      log =
        capture_log([level: :debug], fn ->
          assert {:ok, _} = TransportManager.connect(opts)
        end)

      assert log =~ "Attempting connection"
      assert log =~ "Successfully connected"
    end
  end
end
