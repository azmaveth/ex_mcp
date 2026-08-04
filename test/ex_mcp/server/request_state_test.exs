defmodule ExMCP.Server.RequestStateTest do
  use ExUnit.Case, async: true

  alias ExMCP.Server.{RequestContext, RequestState}

  @key :binary.copy(<<7>>, 32)

  test "seals bounded JSON state and verifies the complete request binding" do
    context = context()
    params = params()
    opts = options(now: 1_000)

    assert {:ok, binding} = RequestState.binding(context, params, ["answer"], 1, opts)
    assert {:ok, token} = RequestState.seal(%{"step" => 1}, binding, opts)

    assert {:ok, payload} =
             RequestState.unseal(
               token,
               context,
               Map.put(params, "inputResponses", %{"answer" => %{}}),
               %{"answer" => %{"action" => "accept"}},
               opts
             )

    assert payload["applicationState"] == %{"step" => 1}
    assert payload["binding"]["expectedInputIds"] == ["answer"]
    assert payload["binding"]["round"] == 1
    assert is_binary(payload["jti"])
  end

  test "rejects tampering, changed immutable params, and mismatched response IDs" do
    context = context()
    opts = options(now: 1_000)
    assert {:ok, binding} = RequestState.binding(context, params(), ["answer"], 1, opts)
    assert {:ok, token} = RequestState.seal(nil, binding, opts)

    [prefix, header, nonce, ciphertext, tag] = String.split(token, ".")
    <<first, rest::binary>> = ciphertext
    changed = if first == ?A, do: ?B, else: ?A
    tampered = Enum.join([prefix, header, nonce, <<changed, rest::binary>>, tag], ".")

    assert {:error, :invalid_request_state} =
             RequestState.unseal(tampered, context, params(), %{"answer" => %{}}, opts)

    changed = put_in(params(), ["arguments", "city"], "Chicago")

    assert {:error, :request_state_binding_mismatch} =
             RequestState.unseal(token, context, changed, %{"answer" => %{}}, opts)

    assert {:error, :input_response_ids_mismatch} =
             RequestState.unseal(token, context, params(), %{"other" => %{}}, opts)
  end

  test "supports decrypt-only rotation keys and expiry" do
    old_opts = options(now: 1_000)
    assert {:ok, binding} = RequestState.binding(context(), params(), [], 1, old_opts)
    assert {:ok, token} = RequestState.seal(%{}, binding, old_opts)

    rotated =
      options(
        now: 1_010,
        active_key_id: "new",
        keys: %{"new" => :binary.copy(<<8>>, 32), "current" => @key}
      )

    assert {:ok, _payload} = RequestState.unseal(token, context(), params(), %{}, rotated)

    expired = Keyword.put(rotated, :request_state_now, 2_000)

    assert {:error, :request_state_expired} =
             RequestState.unseal(token, context(), params(), %{}, expired)
  end

  test "rejects unsafe or node-local application state" do
    opts = options(now: 1_000)
    assert {:ok, binding} = RequestState.binding(context(), params(), [], 1, opts)
    assert {:error, :request_state_not_json} = RequestState.seal(self(), binding, opts)
  end

  test "validates declared MRTR key rings before a server starts" do
    assert :ok = RequestState.validate_configuration(options(now: 1_000))

    assert {:error, :invalid_request_state_configuration} =
             RequestState.validate_configuration(
               request_state: [active_key_id: "bad", keys: %{"bad" => "too short"}]
             )

    assert_raise ArgumentError, ~r/invalid MRTR requestState configuration/, fn ->
      ExMCP.HttpPlug.init(
        mrtr: true,
        request_state: [active_key_id: "bad", keys: %{"bad" => "too short"}]
      )
    end
  end

  defp context do
    %RequestContext{
      method: "tools/call",
      request_id: 1,
      request?: true,
      era: :modern,
      protocol_version: "2026-07-28",
      client_capabilities: %{"elicitation" => %{}},
      principal_id: "user-1",
      tenant_id: "tenant-1",
      endpoint: "/mcp"
    }
  end

  defp params do
    %{
      "name" => "weather",
      "arguments" => %{"city" => "Austin"},
      "_meta" => %{
        "io.modelcontextprotocol/protocolVersion" => "2026-07-28",
        "io.modelcontextprotocol/clientCapabilities" => %{"elicitation" => %{}}
      }
    }
  end

  defp options(overrides) do
    request_state = [
      active_key_id: Keyword.get(overrides, :active_key_id, "current"),
      keys: Keyword.get(overrides, :keys, %{"current" => @key}),
      ttl_seconds: 60,
      max_ttl_seconds: 60,
      clock_skew_seconds: 0
    ]

    [
      request_state: request_state,
      request_state_now: Keyword.fetch!(overrides, :now),
      endpoint: "/mcp",
      principal_id: "user-1",
      tenant_id: "tenant-1"
    ]
  end
end
