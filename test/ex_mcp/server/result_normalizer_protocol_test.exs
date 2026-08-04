defmodule ExMCP.Server.ResultNormalizerProtocolTest do
  use ExUnit.Case, async: true

  alias ExMCP.Server.ResultNormalizer

  test "leaves legacy results unchanged" do
    result = %{"tools" => []}

    assert ResultNormalizer.protocol_result(result, %{era: :legacy},
             server_info: %{name: "example", version: "1"}
           ) == result
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
