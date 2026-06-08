defmodule ExMCP.Server.Tools.ResponseNormalizerTest do
  use ExUnit.Case, async: true

  alias ExMCP.Server.Tools.ResponseNormalizer

  test "normalizes text responses" do
    assert ResponseNormalizer.normalize("hello") == %{content: [%{type: "text", text: "hello"}]}

    assert ResponseNormalizer.normalize(%{text: "hello"}) == %{
             content: [%{type: "text", text: "hello"}]
           }

    assert ResponseNormalizer.normalize(text: "hello") == %{
             content: [%{type: "text", text: "hello"}]
           }
  end

  test "preserves normalized maps and maps legacy structured content" do
    assert ResponseNormalizer.normalize(%{structuredContent: %{answer: 42}}) == %{
             content: [],
             structuredOutput: %{answer: 42}
           }

    assert ResponseNormalizer.normalize(%{content: [], resourceLinks: []}) == %{
             content: [],
             resourceLinks: []
           }
  end

  test "normalizes error responses" do
    assert ResponseNormalizer.normalize_error("failed") == %{
             content: [%{type: "text", text: "failed"}],
             isError: true
           }

    assert ResponseNormalizer.normalize_error(:badarg) == %{
             content: [%{type: "text", text: ":badarg"}],
             isError: true
           }
  end
end
