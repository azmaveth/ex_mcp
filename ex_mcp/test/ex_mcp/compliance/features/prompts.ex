defmodule ExMCP.Compliance.Features.Prompts do
  @moduledoc """
  Shared prompt compliance tests across all MCP versions.
  """
  
  defmacro __using__(version) do
    quote do
      import ExMCP.Compliance.Features.Prompts
      @version unquote(version)
      
      # Basic functionality (all versions)
      test "prompts/list returns valid prompt definitions" do
        ExMCP.Compliance.Features.Prompts.test_basic_prompts_list(@version)
      end
      
      test "prompts/get returns valid prompt content" do
        ExMCP.Compliance.Features.Prompts.test_prompt_get(@version)
      end
      
      # Version-specific features
      if @version in ["2025-03-26", "2025-06-18"] do
        test "prompt list change notifications work" do
          ExMCP.Compliance.Features.Prompts.test_list_change_notifications(@version)
        end
      end
    end
  end
  
  def test_basic_prompts_list(version) do
    assert true # Placeholder
  end
  
  def test_prompt_get(version) do
    assert true # Placeholder
  end
  
  def test_list_change_notifications(version) do
    assert true # Placeholder
  end
end
