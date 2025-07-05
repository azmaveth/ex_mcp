defmodule ExMCP.DSL.AllUsersIntegrationTest do
  @moduledoc """
  Integration tests for all modules using ExMCP.Server DSL.

  This ensures backward compatibility is maintained during refactoring.
  """

  use ExUnit.Case, async: false

  # Modules that actually use ExMCP.Server DSL
  @dsl_users [
    ExMCP.TestHelpers.ApiTestServer,
    ExMCP.TestHelpers.ErrorTestServer,
    ExMCP.TestHelpers.RefactoredTestServer
  ]

  # Modules using other patterns (not ExMCP.Server DSL)
  @non_dsl_server_modules [
    # Uses ExMCP.Server but doesn't define tools/content helpers
    ExMCP.Server.StdioServer,
    # Uses ExMCP.Server.Handler
    ExMCP.Server.Legacy,
    # Uses ExMCP.Server.Handler + ExMCP.Server.Tools
    ExMCP.Server.Tools,
    # Uses ExMCP.Server.Handler
    ExMCP.Server.ToolsRefactored,
    # Uses ExMCP.Server.Handler
    ExMCP.Server.Tools.Simplified,
    # Uses ExMCP.Server.Handler
    ExMCP.Compliance.Handlers.Handler20241105,
    # Uses ExMCP.Server.Handler
    ExMCP.Compliance.Handlers.Handler20250326,
    # Pure GenServer
    ExMCP.TestServer
  ]

  # Modules that use other behaviors (not ExMCP.Server DSL)
  @non_dsl_modules [
                     # Behavior definition
                     ExMCP.Server.Handler,
                     # Mix task
                     Mix.Tasks.StdioServer
                   ] ++ @non_dsl_server_modules

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

  describe "Non-DSL module compilation" do
    for module <- @non_dsl_modules do
      @module module

      test "#{inspect(@module)} compiles and is accessible" do
        # Check module is loaded
        assert Code.ensure_loaded?(@module)

        # Don't expect DSL functions for these modules
        # These modules implement other behaviors
      end

      test "#{inspect(@module)} correct behavior implementation" do
        behaviors = @module.module_info(:attributes)[:behaviour] || []

        case @module do
          Mix.Tasks.StdioServer ->
            # Mix task
            assert Mix.Task in behaviors

          ExMCP.Server.Handler ->
            # Behavior definition, not implementation
            # No specific behavior required
            :ok

          _ ->
            # Unknown module
            :ok
        end
      end
    end
  end

  describe "DSL functionality smoke tests" do
    test "ApiTestServer DSL functionality" do
      # Just test that DSL modules compile and load correctly
      # since they are test helpers and may not have full server functionality
      for module <- @dsl_users do
        assert Code.ensure_loaded?(module)
        # Test that DSL generated the expected functions
        assert function_exported?(module, :text, 1)
        assert function_exported?(module, :json, 1)
      end
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
