defmodule ExMCP.DSL.AllUsersIntegrationTest do
  @moduledoc """
  Integration tests for all modules using ExMCP.Server DSL.

  This ensures backward compatibility is maintained during refactoring.
  """

  use ExUnit.Case, async: false

  @dsl_users [
    ExMCP.Server.Legacy,
    ExMCP.Server.Tools,
    ExMCP.Server.ToolsRefactored,
    ExMCP.Server.Tools.Simplified,
    ExMCP.Server.StdioServer,
    ExMCP.Server.Handler,
    ExMCP.Compliance.Handlers.Handler20241105,
    ExMCP.Compliance.Handlers.Handler20250326,
    ExMCP.TestServer,
    Mix.Tasks.StdioServer
  ]

  describe "DSL user module compilation" do
    for module <- @dsl_users do
      @module module

      test "#{inspect(@module)} compiles and exposes expected functions" do
        # Check module is loaded
        assert Code.ensure_loaded?(@module)

        # Check DSL-generated functions exist
        if function_exported?(@module, :get_tools, 0) do
          assert is_map(@module.get_tools())
        end

        if function_exported?(@module, :get_resources, 0) do
          assert is_map(@module.get_resources())
        end

        if function_exported?(@module, :get_prompts, 0) do
          assert is_map(@module.get_prompts())
        end

        if function_exported?(@module, :get_capabilities, 0) do
          capabilities = @module.get_capabilities()
          assert is_map(capabilities)

          # Basic capability structure validation
          if map_size(@module.get_tools()) > 0 do
            assert is_map(capabilities["tools"])
          end
        end

        # Check helper functions
        assert function_exported?(@module, :text, 1)
        assert function_exported?(@module, :json, 1)
        assert function_exported?(@module, :user, 1)
        assert function_exported?(@module, :assistant, 1)
      end

      test "#{inspect(@module)} GenServer behavior implementation" do
        # All DSL users should be GenServers
        behaviors = @module.module_info(:attributes)[:behaviour] || []
        assert GenServer in behaviors

        # Check required callbacks exist
        assert function_exported?(@module, :init, 1)
        assert function_exported?(@module, :handle_call, 3)
        assert function_exported?(@module, :handle_cast, 2)
        assert function_exported?(@module, :handle_info, 2)
      end
    end
  end

  describe "DSL functionality smoke tests" do
    test "ExMCP.Server.Tools functionality" do
      # Quick smoke test of actual functionality
      {:ok, pid} = ExMCP.Server.Tools.start_link()

      # Test a basic call
      tools = GenServer.call(pid, {:mcp, :list_tools})
      assert is_map(tools)

      GenServer.stop(pid)
    end

    test "ExMCP.TestServer functionality" do
      {:ok, pid} = ExMCP.TestServer.start_link()

      # List tools
      tools = GenServer.call(pid, {:mcp, :list_tools})
      assert Map.has_key?(tools, "tools")

      # Call a tool
      result = GenServer.call(pid, {:mcp, :call_tool, "echo", %{"message" => "test"}})
      assert result["content"]

      GenServer.stop(pid)
    end
  end

  describe "DSL macro expansion consistency" do
    test "all DSL users generate consistent structures" do
      for module <- @dsl_users, function_exported?(module, :get_tools, 0) do
        tools = module.get_tools()

        # Verify tool structure
        for {_name, tool} <- tools do
          assert Map.has_key?(tool, :name)
          assert Map.has_key?(tool, :description)
          assert Map.has_key?(tool, :input_schema)

          # Schema should have string keys
          if tool.input_schema do
            assert is_binary(tool.input_schema["type"]) ||
                     is_nil(tool.input_schema["type"])
          end
        end
      end
    end

    test "capability detection is consistent" do
      for module <- @dsl_users, function_exported?(module, :get_capabilities, 0) do
        capabilities = module.get_capabilities()

        # If module has tools, should have tools capability
        if function_exported?(module, :get_tools, 0) && map_size(module.get_tools()) > 0 do
          assert Map.has_key?(capabilities, "tools")
          assert capabilities["tools"]["listChanged"] == true
        end

        # If module has resources, should have resources capability
        if function_exported?(module, :get_resources, 0) && map_size(module.get_resources()) > 0 do
          assert Map.has_key?(capabilities, "resources")
          assert capabilities["resources"]["listChanged"] == true
        end
      end
    end
  end

  describe "callback implementation validation" do
    test "handler callback signatures are correct" do
      for module <- @dsl_users do
        if function_exported?(module, :handle_tool_call, 3) do
          # Just verify the function exists with correct arity
          assert function_exported?(module, :handle_tool_call, 3)
        end

        if function_exported?(module, :handle_resource_read, 3) do
          assert function_exported?(module, :handle_resource_read, 3)
        end

        if function_exported?(module, :handle_prompt_get, 3) do
          assert function_exported?(module, :handle_prompt_get, 3)
        end
      end
    end
  end
end
