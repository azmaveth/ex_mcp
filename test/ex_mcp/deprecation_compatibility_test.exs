defmodule ExMCP.DeprecationCompatibilityTest do
  use ExUnit.Case, async: true

  @retained_tools_modules [
    ExMCP.Server.Tools,
    ExMCP.Server.Tools.Simplified,
    ExMCP.Server.Tools.Builder,
    ExMCP.Server.Tools.Helpers,
    ExMCP.Server.Tools.Registry,
    ExMCP.Server.Tools.ResponseNormalizer,
    ExMCP.Server.Tools.ASTValidator
  ]

  @deprecated_metadata_modules [
    ExMCP.Server.Tools,
    ExMCP.Content.Builders,
    ExMCP.Content.Sanitizer,
    ExMCP.Content.Transformer
  ]

  test "deprecated Server.Tools modules remain public throughout 1.x" do
    for module <- @retained_tools_modules do
      assert Code.ensure_loaded?(module)
      assert {:docs_v1, _, _, _, %{"en" => module_doc}, _, _} = Code.fetch_docs(module)
      assert module_doc =~ "2.0.0"
      refute module_doc =~ "1.1.0"
    end
  end

  test "compiled deprecation metadata schedules public removals for 2.0" do
    for module <- @deprecated_metadata_modules do
      assert {:docs_v1, _, _, _, _, _, docs} = Code.fetch_docs(module)

      messages =
        for {_identifier, _line, _signatures, _doc, metadata} <- docs,
            message = metadata[:deprecated],
            is_binary(message),
            do: message

      assert messages != []
      assert Enum.all?(messages, &String.contains?(&1, "2.0.0"))
      refute Enum.any?(messages, &String.contains?(&1, "1.1.0"))
    end
  end
end
