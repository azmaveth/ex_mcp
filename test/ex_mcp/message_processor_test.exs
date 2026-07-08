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
end
