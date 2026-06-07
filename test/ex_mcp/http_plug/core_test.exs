defmodule ExMCP.HttpPlug.CoreTest do
  use ExUnit.Case, async: true

  alias ExMCP.HttpPlug.Core

  describe "parse_json/1" do
    test "accepts JSON objects and rejects scalars" do
      assert {:ok, %{"jsonrpc" => "2.0"}} = Core.parse_json(~s({"jsonrpc":"2.0"}))
      assert {:error, :invalid_json_rpc_envelope} = Core.parse_json(~s(["not", "an", "object"]))
      assert {:error, :parse_error} = Core.parse_json("{")
    end
  end

  describe "origin_allowed?/2" do
    test "allows same-origin browser requests" do
      context = %{origin: "http://localhost:4000", scheme: "http", host: "localhost", port: 4000}

      assert Core.origin_allowed?(context, %{allowed_origins: []})
    end

    test "allows explicit origins and rejects others" do
      opts = %{allowed_origins: ["https://allowed.example"]}

      assert Core.origin_allowed?(%{origin: "https://allowed.example"}, opts)
      refute Core.origin_allowed?(%{origin: "https://blocked.example"}, opts)
    end
  end
end
