defmodule ExMCP.Server.MRTRDispatchTest do
  use ExUnit.Case, async: true

  alias ExMCP.Server.{Context, Dispatch}

  @key :binary.copy(<<42>>, 32)

  defmodule Handler do
    use ExMCP.Server.Handler

    @impl true
    def handle_initialize(_params, state), do: {:ok, %{}, state}

    @impl true
    def handle_list_tools(_cursor, state), do: {:ok, [], nil, state}

    @impl true
    def handle_call_tool("collect", _arguments, state) do
      case Context.input_responses() do
        nil ->
          requests = %{
            "profile" => %{
              "method" => "elicitation/create",
              "params" => %{
                "message" => "Choose a display name",
                "requestedSchema" => %{"type" => "object"}
              }
            }
          }

          {:input_required, requests, %{"step" => "profile"}, %{state | calls: state.calls + 1}}

        %{"profile" => response} ->
          result = %{
            content: [
              %{
                type: "text",
                text: response["content"]["name"] <> ":" <> Context.request_state()["step"]
              }
            ]
          }

          {:ok, result, %{state | calls: state.calls + 1}}
      end
    end

    def handle_call_tool(_name, _arguments, state), do: {:error, "unknown", state}
  end

  test "a tool suspends and resumes through authenticated requestState" do
    initial = request(1, base_params())

    assert {:response, %{"result" => interim}, state} =
             Dispatch.dispatch(initial, Handler, %{calls: 0}, options())

    assert interim["resultType"] == "input_required"
    assert Map.keys(interim["inputRequests"]) == ["profile"]
    assert is_binary(interim["requestState"])
    assert state.calls == 1

    retry_params =
      base_params()
      |> Map.put("inputResponses", %{
        "profile" => %{"action" => "accept", "content" => %{"name" => "Ada"}}
      })
      |> Map.put("requestState", interim["requestState"])

    assert {:response, %{"result" => complete}, final_state} =
             Dispatch.dispatch(request(99, retry_params), Handler, state, options())

    assert complete["resultType"] == "complete"
    assert complete["content"] == [%{"type" => "text", "text" => "Ada:profile"}]
    assert final_state.calls == 2
  end

  test "tampered state and cross-request replay are rejected before handler dispatch" do
    assert {:response, %{"result" => interim}, state} =
             Dispatch.dispatch(request(1, base_params()), Handler, %{calls: 0}, options())

    token = interim["requestState"]
    [prefix, header, nonce, ciphertext, tag] = String.split(token, ".")
    <<first, rest::binary>> = ciphertext
    changed = if first == ?A, do: ?B, else: ?A
    tampered = Enum.join([prefix, header, nonce, <<changed, rest::binary>>, tag], ".")
    responses = %{"profile" => %{"action" => "decline"}}

    tampered_params =
      base_params()
      |> Map.put("inputResponses", responses)
      |> Map.put("requestState", tampered)

    assert {:response, %{"error" => tamper_error}, unchanged_state} =
             Dispatch.dispatch(request(2, tampered_params), Handler, state, options())

    assert tamper_error["code"] == -32602
    assert unchanged_state.calls == 1

    changed_params =
      base_params()
      |> Map.put("arguments", %{"scope" => "different"})
      |> Map.put("inputResponses", responses)
      |> Map.put("requestState", token)

    assert {:response, %{"error" => binding_error}, unchanged_state} =
             Dispatch.dispatch(request(3, changed_params), Handler, state, options())

    assert binding_error["code"] == -32602
    assert unchanged_state.calls == 1
  end

  test "server refuses to request an undeclared client capability" do
    params = put_in(base_params(), ["_meta", "io.modelcontextprotocol/clientCapabilities"], %{})

    assert {:response, %{"error" => error}, state} =
             Dispatch.dispatch(request(1, params), Handler, %{calls: 0}, options())

    assert error["code"] == -32021
    assert error["data"]["requiredCapabilities"] == %{"elicitation" => %{"form" => %{}}}
    assert state.calls == 1
  end

  defp request(id, params) do
    %{"jsonrpc" => "2.0", "id" => id, "method" => "tools/call", "params" => params}
  end

  defp base_params do
    %{
      "name" => "collect",
      "arguments" => %{"scope" => "same"},
      "_meta" => %{
        "io.modelcontextprotocol/protocolVersion" => "2026-07-28",
        "io.modelcontextprotocol/clientCapabilities" => %{"elicitation" => %{}}
      }
    }
  end

  defp options do
    [
      request_state: [
        active_key_id: "test",
        keys: %{"test" => @key},
        ttl_seconds: 60
      ]
    ]
  end
end
