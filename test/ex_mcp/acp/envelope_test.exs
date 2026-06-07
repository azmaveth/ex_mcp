defmodule ExMCP.ACP.EnvelopeTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Envelope

  test "builds pipe-friendly requests" do
    request =
      "session/new"
      |> Envelope.request()
      |> Envelope.with_params(%{"cwd" => "/tmp/project"})
      |> Envelope.with_id(1)

    assert request == %{
             "jsonrpc" => "2.0",
             "method" => "session/new",
             "params" => %{"cwd" => "/tmp/project"},
             "id" => 1
           }
  end

  test "builds notifications without ids" do
    assert Envelope.notification("initialized") == %{
             "jsonrpc" => "2.0",
             "method" => "initialized",
             "params" => %{}
           }
  end

  test "builds responses" do
    assert Envelope.response("abc", %{"ok" => true}) == %{
             "jsonrpc" => "2.0",
             "result" => %{"ok" => true},
             "id" => "abc"
           }
  end

  test "builds errors and preserves false data" do
    assert Envelope.error(nil, -32_600, "Invalid", false) == %{
             "jsonrpc" => "2.0",
             "error" => %{"code" => -32_600, "message" => "Invalid", "data" => false},
             "id" => nil
           }
  end
end
