defmodule ExMCP.Server.StdioServerRequestIdTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias ExMCP.Server.StdioServer

  defmodule CountingHandler do
    use ExMCP.Server.Handler

    @impl true
    def init(_opts), do: {:ok, %{list_calls: 0}}

    @impl true
    def handle_list_tools(_cursor, state) do
      {:ok, [], nil, Map.update!(state, :list_calls, &(&1 + 1))}
    end
  end

  setup do
    logger_level = Logger.level()
    otp_logger_level = :logger.get_primary_config()[:level]
    logger_app_level = Application.get_env(:logger, :level)
    stdio_mode = Application.get_env(:ex_mcp, :stdio_mode)
    startup_delay = Application.get_env(:ex_mcp, :stdio_startup_delay)

    Application.put_env(:ex_mcp, :stdio_startup_delay, 60_000)

    on_exit(fn ->
      Logger.configure(level: logger_level)
      :logger.set_primary_config(:level, otp_logger_level)
      restore_env(:logger, :level, logger_app_level)
      restore_env(:ex_mcp, :stdio_mode, stdio_mode)
      restore_env(:ex_mcp, :stdio_startup_delay, startup_delay)
    end)

    :ok
  end

  test "stdio rejects a duplicate process-lifetime request ID before dispatch" do
    output =
      capture_io("", fn ->
        {:ok, server} = StdioServer.start_link(module: CountingHandler)
        Process.unlink(server)

        request = %{"jsonrpc" => "2.0", "id" => "stdio-duplicate", "method" => "tools/list"}
        encoded = Jason.encode!(request)

        send(server, {:stdin_line, encoded})
        send(server, {:stdin_line, encoded})

        state = :sys.get_state(server)
        assert state.handler_state.list_calls == 1
        assert MapSet.size(state.validation_state.seen_request_ids) == 1

        monitor = Process.monitor(server)
        Process.exit(server, :kill)
        assert_receive {:DOWN, ^monitor, :process, ^server, :killed}
      end)

    [first, duplicate] =
      output
      |> String.split("\n", trim: true)
      |> Enum.map(&Jason.decode!/1)

    assert %{"id" => "stdio-duplicate", "result" => %{"tools" => []}} = first

    assert %{
             "id" => "stdio-duplicate",
             "error" => %{
               "code" => -32600,
               "data" => %{"type" => "duplicate_request_id"}
             }
           } = duplicate
  end

  defp restore_env(app, key, nil), do: Application.delete_env(app, key)
  defp restore_env(app, key, value), do: Application.put_env(app, key, value)
end
