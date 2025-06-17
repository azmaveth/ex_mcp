defmodule ExMCP.PlugTest do
  use ExUnit.Case, async: true

  alias ExMCP.Plug
  alias ExMCP.Plug.Conn

  defmodule TestPlug do
    @behaviour ExMCP.Plug

    def init(opts), do: opts

    def call(conn, opts) do
      value = Keyword.get(opts, :assign_value, "test")
      Plug.assign(conn, :test_key, value)
    end
  end

  defmodule HaltingPlug do
    @behaviour ExMCP.Plug

    def init(opts), do: opts

    def call(conn, _opts) do
      conn
      |> Plug.assign(:halted_here, true)
      |> Plug.halt()
    end
  end

  describe "new/2" do
    test "creates a new connection with request" do
      request = %{"method" => "test", "params" => %{}}
      conn = Plug.new(request, transport: :http, session_id: "123")

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
      conn = Plug.new(%{})
      updated_conn = Plug.assign(conn, :key, "value")

      assert updated_conn.assigns[:key] == "value"
    end
  end

  describe "halt/1" do
    test "halts the connection" do
      conn = Plug.new(%{})
      halted_conn = Plug.halt(conn)

      assert halted_conn.halted == true
    end
  end

  describe "put_response/2" do
    test "sets the response" do
      conn = Plug.new(%{})
      response = %{"result" => "success"}
      updated_conn = Plug.put_response(conn, response)

      assert updated_conn.response == response
    end
  end

  describe "run/2" do
    test "runs a series of plugs" do
      conn = Plug.new(%{})

      plugs = [
        {TestPlug, [assign_value: "first"]},
        {TestPlug, [assign_value: "second"]}
      ]

      result_conn = Plug.run(plugs, conn)

      # Second plug should overwrite the first
      assert result_conn.assigns[:test_key] == "second"
    end

    test "stops processing when halted" do
      conn = Plug.new(%{})

      plugs = [
        {HaltingPlug, []},
        {TestPlug, [assign_value: "should_not_run"]}
      ]

      result_conn = Plug.run(plugs, conn)

      assert result_conn.halted == true
      assert result_conn.assigns[:halted_here] == true
      assert result_conn.assigns[:test_key] == nil
    end
  end
end
