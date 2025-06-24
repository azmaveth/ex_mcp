defmodule ExMCP.ClientCompleteTest do
  @moduledoc """
  Tests for the Client.complete/3 function added in Phase 2.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Client
  alias ExMCP.Internal.Protocol

  describe "complete/3 basic functionality" do
    test "function exists with correct arity" do
      # Verify the function is exported
      assert function_exported?(Client, :complete, 3)
      assert function_exported?(Client, :complete, 4)
    end

    test "returns error when not connected" do
      {:ok, client} =
        GenServer.start_link(Client,
          transport: :test,
          _skip_connect: true
        )

      ref = %{"type" => "ref/prompt", "name" => "test"}
      argument = %{"name" => "arg", "value" => "val"}

      assert {:error, :not_connected} = Client.complete(client, ref, argument)

      GenServer.stop(client)
    end
  end

  describe "protocol encoding" do
    test "encode_complete generates correct request format" do
      ref = %{"type" => "ref/prompt", "name" => "test_prompt"}
      argument = %{"name" => "language", "value" => "py"}

      request = Protocol.encode_complete(ref, argument)

      assert request["jsonrpc"] == "2.0"
      assert request["method"] == "completion/complete"
      assert request["params"]["ref"] == ref
      assert request["params"]["argument"] == argument
      assert is_binary(request["id"]) or is_integer(request["id"])
    end
  end

  describe "parameter validation" do
    test "accepts valid ref types" do
      {:ok, client} =
        GenServer.start_link(Client,
          transport: :test,
          _skip_connect: true
        )

      # These should not crash - they'll return :not_connected
      prompt_ref = %{"type" => "ref/prompt", "name" => "test"}
      resource_ref = %{"type" => "ref/resource", "uri" => "file:///"}
      argument = %{"name" => "test", "value" => "val"}

      assert {:error, :not_connected} = Client.complete(client, prompt_ref, argument)
      assert {:error, :not_connected} = Client.complete(client, resource_ref, argument)

      GenServer.stop(client)
    end

    test "accepts timeout option" do
      {:ok, client} =
        GenServer.start_link(Client,
          transport: :test,
          _skip_connect: true
        )

      ref = %{"type" => "ref/prompt", "name" => "test"}
      argument = %{"name" => "arg", "value" => "val"}

      # Should accept timeout option
      assert {:error, :not_connected} = Client.complete(client, ref, argument, timeout: 1000)

      GenServer.stop(client)
    end
  end
end
