defmodule ExMCP.DSL.AdvancedSimpleTest do
  use ExUnit.Case, async: true
  
  alias ExMCP.DSL.Advanced
  
  describe "public functions" do
    test "validate_enhanced_input/2 validates input" do
      input = %{"name" => "test", "value" => 42}
      schema = %{}
      
      assert {:ok, ^input} = Advanced.validate_enhanced_input(input, schema)
    end
    
    test "apply_middleware/3 applies middleware pipeline" do
      middleware = []
      request = %{"method" => "test", "params" => %{}}
      context = %{}
      
      assert {:ok, ^request} = Advanced.apply_middleware(middleware, request, context)
    end
    
    test "validate_output/2 validates output" do
      output = %{"result" => "success"}
      schema = %{}
      
      assert {:ok, ^output} = Advanced.validate_output(output, schema)
    end
  end
  
  describe "compilation helpers" do
    test "compile_enhanced_input_schema/1 returns map" do
      assert %{} = Advanced.compile_enhanced_input_schema(nil)
    end
    
    test "compile_middleware_pipeline/2 returns list" do
      assert [] = Advanced.compile_middleware_pipeline([], %{})
    end
    
    test "compile_annotations/1 returns map" do
      assert %{} = Advanced.compile_annotations(%{})
    end
    
    test "cleanup_tool_attributes/1 returns nil" do
      assert nil == Advanced.cleanup_tool_attributes(nil)
    end
  end
  
  describe "edge cases" do
    test "validate_enhanced_input handles nil input" do
      assert {:ok, nil} = Advanced.validate_enhanced_input(nil, %{})
    end
    
    test "validate_enhanced_input handles empty input" do
      assert {:ok, %{}} = Advanced.validate_enhanced_input(%{}, %{})
    end
    
    test "apply_middleware handles empty middleware list" do
      request = %{"data" => "test"}
      assert {:ok, ^request} = Advanced.apply_middleware([], request, %{})
    end
    
    test "apply_middleware handles nil context" do
      request = %{"data" => "test"}
      assert {:ok, ^request} = Advanced.apply_middleware([], request, nil)
    end
    
    test "validate_output handles list output" do
      output = [%{"type" => "text", "text" => "Hello"}]
      assert {:ok, ^output} = Advanced.validate_output(output, %{})
    end
    
    test "validate_output handles string output" do
      output = "simple string"
      assert {:ok, ^output} = Advanced.validate_output(output, %{})
    end
  end
end