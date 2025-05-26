defmodule ExMCP.ProtocolSamplingTest do
  use ExUnit.Case
  alias ExMCP.Protocol

  describe "sampling/createMessage encoding" do
    test "encodes create_message request correctly" do
      params = %{
        "messages" => [
          %{
            "role" => "user",
            "content" => "Hello"
          }
        ]
      }

      encoded = Protocol.encode_create_message(params)

      assert encoded["jsonrpc"] == "2.0"
      assert encoded["method"] == "sampling/createMessage"
      assert encoded["params"] == params
      assert is_binary(encoded["id"]) or is_integer(encoded["id"])
    end

    test "encodes create_message with all parameters" do
      params = %{
        "messages" => [
          %{
            "role" => "user",
            "content" => "Hello"
          }
        ],
        "modelPreferences" => %{
          "hints" => ["fast"],
          "costPriority" => 0.5
        },
        "systemPrompt" => "Be helpful",
        "includeContext" => "all",
        "metadata" => %{
          "requestId" => "123"
        }
      }

      encoded = Protocol.encode_create_message(params)
      assert encoded["params"]["modelPreferences"]["hints"] == ["fast"]
      assert encoded["params"]["systemPrompt"] == "Be helpful"
      assert encoded["params"]["metadata"]["requestId"] == "123"
    end

    test "encodes create_message with sampling parameters" do
      params = %{
        "messages" => [
          %{
            "role" => "user",
            "content" => "Tell me a story"
          }
        ],
        "modelPreferences" => %{
          "hints" => ["creative", "long"],
          "costPriority" => 0.2,
          "speedPriority" => 0.3,
          "intelligencePriority" => 0.5
        },
        "samplingParams" => %{
          "temperature" => 0.8,
          "topP" => 0.95,
          "maxTokens" => 2000
        }
      }

      encoded = Protocol.encode_create_message(params)

      assert encoded["method"] == "sampling/createMessage"
      assert encoded["params"]["messages"] |> Enum.count() == 1
      assert encoded["params"]["modelPreferences"]["intelligencePriority"] == 0.5
      assert encoded["params"]["samplingParams"]["temperature"] == 0.8
    end
  end
end
