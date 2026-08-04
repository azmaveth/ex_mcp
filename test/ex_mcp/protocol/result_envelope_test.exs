defmodule ExMCP.Protocol.ResultEnvelopeTest do
  use ExUnit.Case, async: true

  alias ExMCP.Protocol.ResultEnvelope

  test "treats an absent discriminator as complete only for legacy peers" do
    assert {:ok, :complete} = ResultEnvelope.classify(%{"tools" => []}, :legacy)

    assert {:error, :missing_result_type} =
             ResultEnvelope.classify(%{"tools" => []}, :modern)

    assert {:error, :missing_result_type} =
             ResultEnvelope.classify(%{"tools" => []}, :unknown)

    assert {:error, :missing_result_type} =
             ResultEnvelope.classify(%{"tools" => []}, nil)
  end

  test "recognizes complete and input-required results" do
    assert {:ok, :complete} =
             ResultEnvelope.classify(%{"resultType" => "complete"}, "2026-07-28")

    assert {:ok, :input_required} =
             ResultEnvelope.classify(%{"resultType" => "input_required"}, :modern)
  end

  test "accepts only explicitly negotiated extension result types" do
    result = %{"resultType" => "com.example/custom"}

    assert {:error, {:unknown_result_type, "com.example/custom"}} =
             ResultEnvelope.classify(result, :modern)

    assert {:ok, {:extension, "com.example/custom"}} =
             ResultEnvelope.classify(result, :modern,
               allowed_result_types: ["com.example/custom"]
             )
  end

  test "rejects non-string discriminators and non-object results" do
    assert {:error, {:invalid_result_type, 1}} =
             ResultEnvelope.classify(%{"resultType" => 1}, :modern)

    assert {:error, :result_must_be_object} = ResultEnvelope.classify([], :modern)
  end

  test "returns the validated result unchanged" do
    result = %{"resultType" => "complete", "tools" => []}
    assert {:ok, :complete, ^result} = ResultEnvelope.validate(result, :modern)
  end
end
