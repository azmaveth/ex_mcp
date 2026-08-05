defmodule ExMCP.Server.ResultNormalizerProtocolTest do
  use ExUnit.Case, async: true

  alias ExMCP.Server.ResultNormalizer

  test "leaves legacy results unchanged" do
    result = %{"tools" => []}

    assert ResultNormalizer.protocol_result(result, %{era: :legacy},
             server_info: %{name: "example", version: "1"}
           ) == result
  end

  test "preserves handler ordering for legacy tool lists" do
    result = %{"tools" => [%{"name" => "zebra"}, %{"name" => "alpha"}]}

    assert ResultNormalizer.protocol_result(result, %{era: :legacy, method: "tools/list"}) ==
             result
  end

  test "sorts modern tool lists deterministically by name" do
    tools = [
      %{name: "zebra", description: "last"},
      %{name: "alpha", description: "first"},
      %{name: "middle", description: "second"}
    ]

    expected_names = ["alpha", "middle", "zebra"]

    for permutation <- [
          tools,
          Enum.reverse(tools),
          [Enum.at(tools, 1), hd(tools), List.last(tools)]
        ] do
      result =
        ResultNormalizer.protocol_result(
          %{tools: permutation},
          %{era: :modern, method: "tools/list"},
          server_info: %{name: "example", version: "1"}
        )

      assert Enum.map(result["tools"], & &1["name"]) == expected_names
      assert result["ttlMs"] == 0
      assert result["cacheScope"] == "private"
    end
  end

  test "adds conservative cache defaults and preserves valid handler overrides" do
    defaulted =
      ResultNormalizer.protocol_result(
        %{contents: []},
        %{era: :modern, method: "resources/read"}
      )

    assert defaulted["ttlMs"] == 0
    assert defaulted["cacheScope"] == "private"

    overridden =
      ResultNormalizer.protocol_result(
        %{prompts: [], ttl_ms: 30_000, cache_scope: :public},
        %{era: :modern, method: "prompts/list"}
      )

    assert overridden["ttlMs"] == 30_000
    assert overridden["cacheScope"] == "public"
  end

  test "repairs invalid cache hints and removes them from non-complete results" do
    repaired =
      ResultNormalizer.protocol_result(
        %{"tools" => [], "ttlMs" => -1, "cacheScope" => "shared"},
        %{era: :modern, method: "tools/list"}
      )

    assert repaired["ttlMs"] == 0
    assert repaired["cacheScope"] == "private"

    interim =
      ResultNormalizer.protocol_result(
        %{"resultType" => "input_required", "ttlMs" => 10, "cacheScope" => "public"},
        %{era: :modern, method: "resources/read"}
      )

    refute Map.has_key?(interim, "ttlMs")
    refute Map.has_key?(interim, "cacheScope")
  end

  test "stamps complete modern results with canonical server metadata" do
    result =
      ResultNormalizer.protocol_result(
        %{tools: [], _meta: %{"com.example/value" => true}},
        %{era: :modern},
        server_info: %{name: "example", version: "1"}
      )

    assert result["resultType"] == "complete"
    assert result["tools"] == []
    assert result["_meta"]["com.example/value"]

    assert result["_meta"]["io.modelcontextprotocol/serverInfo"] == %{
             "name" => "example",
             "version" => "1"
           }
  end

  test "preserves input-required and extension result discriminators" do
    for result_type <- ["input_required", "com.example/custom"] do
      result =
        ResultNormalizer.protocol_result(
          %{"resultType" => result_type},
          %{era: :modern}
        )

      assert result["resultType"] == result_type
    end
  end

  test "connection-owned server identity replaces handler-supplied identity" do
    result =
      ResultNormalizer.protocol_result(
        %{
          "_meta" => %{
            "io.modelcontextprotocol/serverInfo" => %{"name" => "spoofed"}
          }
        },
        %{era: :modern},
        server_info: %{"name" => "configured", "version" => "1"}
      )

    assert result["_meta"]["io.modelcontextprotocol/serverInfo"]["name"] == "configured"
  end
end
