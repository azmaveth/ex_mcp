defmodule ExMCP.MessageProcessorTest do
  use ExUnit.Case, async: true

  alias ExMCP.MessageProcessor
  alias ExMCP.MessageProcessor.Conn

  defmodule TestPlug do
    @behaviour ExMCP.MessageProcessor

    def init(opts), do: opts

    def call(conn, opts) do
      value = Keyword.get(opts, :assign_value, "test")
      MessageProcessor.assign(conn, :test_key, value)
    end
  end

  defmodule HaltingPlug do
    @behaviour ExMCP.MessageProcessor

    def init(opts), do: opts

    def call(conn, _opts) do
      conn
      |> MessageProcessor.assign(:halted_here, true)
      |> MessageProcessor.halt()
    end
  end

  defmodule HandlerServer do
    use ExMCP.Server.Handler

    @impl true
    def init(_args), do: {:ok, %{}}

    @impl true
    def handle_initialize(_params, state) do
      {:ok, %{name: "handler-server", version: "1.0.0", capabilities: %{completions: %{}}}, state}
    end

    @impl true
    def handle_list_tools(_cursor, state), do: {:ok, [], nil, state}

    @impl true
    def handle_list_prompts(_cursor, state), do: {:ok, [], nil, state}

    @impl true
    def handle_list_resources(_cursor, state), do: {:ok, [], nil, state}

    @impl true
    def handle_complete(ref, argument, state) do
      {:ok,
       %{
         completion: %{
           values: ["#{ref["name"]}:#{argument["value"]}"],
           total: 1,
           hasMore: false
         }
       }, state}
    end
  end

  defmodule InitOptsHandlerServer do
    use ExMCP.Server.Handler

    @impl true
    def init(opts), do: {:ok, Map.new(opts)}

    @impl true
    def handle_initialize(_params, state) do
      {:ok,
       %{
         name: Map.fetch!(state, :name),
         version: Map.fetch!(state, :version),
         capabilities: %{}
       }, state}
    end
  end

  defmodule TrackedHandlerServer do
    use ExMCP.Server.Handler

    @impl true
    def init(opts) do
      state = Map.new(opts)
      send(state.test_pid, {:handler_started, self()})
      {:ok, state}
    end

    @impl true
    def handle_list_tools(_cursor, state), do: {:ok, [], nil, state}
  end

  defmodule RaisingHandlerServer do
    use ExMCP.Server.Handler

    @impl true
    def init(opts) do
      state = Map.new(opts)
      send(state.test_pid, {:handler_started, self()})
      {:ok, state}
    end

    @impl true
    def handle_list_tools(_cursor, _state) do
      raise "handler blew up: s3cr3t-detail"
    end
  end

  defmodule BlockingHandlerServer do
    use ExMCP.Server.Handler

    @impl true
    def init(opts) do
      state = Map.new(opts)
      send(state.test_pid, {:handler_started, self()})
      {:ok, state}
    end

    @impl true
    def handle_list_tools(_cursor, state) do
      # Blocks until told otherwise; the caller's timeout fires first.
      receive do
        :unblock -> {:ok, [], nil, state}
      end
    end
  end

  defmodule ErrorReturningHandlerServer do
    use ExMCP.Server.Handler

    @impl true
    def handle_list_tools(_cursor, state) do
      {:error, %{secret: "s3cr3t-detail"}, state}
    end
  end

  defmodule FailingInitHandlerServer do
    use ExMCP.Server.Handler

    @impl true
    def init(_opts), do: {:stop, {:boom, "s3cr3t-detail"}}
  end

  describe "new/2" do
    test "creates a new connection with request" do
      request = %{"method" => "test", "params" => %{}}
      conn = MessageProcessor.new(request, transport: :http, session_id: "123")

      assert %Conn{} = conn
      assert conn.request == request
      assert conn.transport == :http
      assert conn.session_id == "123"
      assert conn.response == nil
      assert conn.assigns == %{}
      assert conn.halted == false
    end
  end

  describe "assign/3" do
    test "assigns a value to the connection" do
      conn = MessageProcessor.new(%{})
      updated_conn = MessageProcessor.assign(conn, :key, "value")

      assert updated_conn.assigns[:key] == "value"
    end
  end

  describe "halt/1" do
    test "halts the connection" do
      conn = MessageProcessor.new(%{})
      halted_conn = MessageProcessor.halt(conn)

      assert halted_conn.halted == true
    end
  end

  describe "put_response/2" do
    test "sets the response" do
      conn = MessageProcessor.new(%{})
      response = %{"result" => "success"}
      updated_conn = MessageProcessor.put_response(conn, response)

      assert updated_conn.response == response
    end
  end

  describe "run/2" do
    test "runs a series of plugs" do
      conn = MessageProcessor.new(%{})

      plugs = [
        {TestPlug, [assign_value: "first"]},
        {TestPlug, [assign_value: "second"]}
      ]

      result_conn = MessageProcessor.run(plugs, conn)

      # Second plug should overwrite the first
      assert result_conn.assigns[:test_key] == "second"
    end

    test "stops processing when halted" do
      conn = MessageProcessor.new(%{})

      plugs = [
        {HaltingPlug, []},
        {TestPlug, [assign_value: "should_not_run"]}
      ]

      result_conn = MessageProcessor.run(plugs, conn)

      assert result_conn.halted == true
      assert result_conn.assigns[:halted_here] == true
      assert result_conn.assigns[:test_key] == nil
    end
  end

  describe "process/2 with Server.Handler modules" do
    test "normalizes initialize replies from handler GenServers" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "initialize",
        "params" => %{
          "protocolVersion" => "2025-11-25",
          "capabilities" => %{},
          "clientInfo" => %{"name" => "test", "version" => "1.0.0"}
        },
        "id" => 1
      }

      conn =
        request |> MessageProcessor.new() |> MessageProcessor.process(%{handler: HandlerServer})

      assert conn.response["result"]["serverInfo"] == %{
               "name" => "handler-server",
               "version" => "1.0.0"
             }

      assert conn.response["result"]["protocolVersion"] == "2025-11-25"
    end

    test "dispatches completion params as ref and argument" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "completion/complete",
        "params" => %{
          "ref" => %{"type" => "ref/prompt", "name" => "prompt"},
          "argument" => %{"name" => "arg", "value" => "par"}
        },
        "id" => 2
      }

      conn =
        request |> MessageProcessor.new() |> MessageProcessor.process(%{handler: HandlerServer})

      assert conn.response["result"]["completion"]["values"] == ["prompt:par"]
    end

    test "passes handler_opts to temporary handler GenServers" do
      request = %{
        "jsonrpc" => "2.0",
        "method" => "initialize",
        "params" => %{
          "protocolVersion" => "2025-11-25",
          "capabilities" => %{},
          "clientInfo" => %{"name" => "test", "version" => "1.0.0"}
        },
        "id" => 3
      }

      conn =
        request
        |> MessageProcessor.new()
        |> MessageProcessor.process(%{
          handler: InitOptsHandlerServer,
          handler_opts: [name: "configured-handler", version: "2.0.0"]
        })

      assert conn.response["result"]["serverInfo"] == %{
               "name" => "configured-handler",
               "version" => "2.0.0"
             }
    end
  end

  describe "process/2 handler fault tolerance" do
    @describetag capture_log: true

    test "handler crash returns -32603 and does not kill the request process" do
      Process.flag(:trap_exit, true)

      conn =
        10
        |> tools_list_request()
        |> MessageProcessor.new()
        |> MessageProcessor.process(%{
          handler: RaisingHandlerServer,
          handler_opts: [test_pid: self()]
        })

      assert %{"error" => error} = conn.response
      assert error["code"] == -32603
      assert error["message"] == "Tools list failed"
      assert error["data"] == %{"type" => "handler_crash"}
      refute inspect(conn.response) =~ "s3cr3t-detail"

      # The handler must not be linked to the request process, so no exit
      # signal is delivered even while trapping exits.
      refute_received {:EXIT, _pid, _reason}

      assert_receive {:handler_started, handler_pid}
      assert_handler_down(handler_pid)
    end

    test "handler blocking past :handler_call_timeout returns -32603 timeout error" do
      conn =
        11
        |> tools_list_request()
        |> MessageProcessor.new()
        |> MessageProcessor.process(%{
          handler: BlockingHandlerServer,
          handler_opts: [test_pid: self()],
          handler_call_timeout: 100
        })

      assert %{"error" => error} = conn.response
      assert error["code"] == -32603
      assert error["data"] == %{"type" => "handler_timeout"}

      # The stuck handler must be reaped after the request, not leaked.
      assert_receive {:handler_started, handler_pid}
      assert_handler_down(handler_pid)
    end

    test "handler is stopped after a successful request (no process leak)" do
      conn =
        12
        |> tools_list_request()
        |> MessageProcessor.new()
        |> MessageProcessor.process(%{
          handler: TrackedHandlerServer,
          handler_opts: [test_pid: self()]
        })

      assert conn.response["result"] == %{"tools" => []}

      assert_receive {:handler_started, handler_pid}
      assert_handler_down(handler_pid)
    end

    test "handler error returns do not leak error details in the response" do
      conn =
        13
        |> tools_list_request()
        |> MessageProcessor.new()
        |> MessageProcessor.process(%{handler: ErrorReturningHandlerServer})

      assert %{"error" => error} = conn.response
      assert error["code"] == -32603
      assert error["message"] == "Tools list failed"
      assert error["data"] == %{"type" => "handler_error"}
      refute inspect(conn.response) =~ "s3cr3t-detail"
    end

    test "handler start failure returns a generic -32603 error" do
      conn =
        14
        |> tools_list_request()
        |> MessageProcessor.new()
        |> MessageProcessor.process(%{handler: FailingInitHandlerServer})

      assert %{"error" => error} = conn.response
      assert error["code"] == -32603
      assert error["message"] == "Internal server error"
      assert error["data"] == %{"type" => "handler_start_failed"}
      refute inspect(conn.response) =~ "s3cr3t-detail"
    end
  end

  defp tools_list_request(id) do
    %{"jsonrpc" => "2.0", "method" => "tools/list", "params" => %{}, "id" => id}
  end

  defp assert_handler_down(handler_pid) do
    ref = Process.monitor(handler_pid)
    assert_receive {:DOWN, ^ref, :process, ^handler_pid, _reason}, 2_000
  end
end
