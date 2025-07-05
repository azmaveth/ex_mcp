defmodule ExMCP.DSL.ActualUsersTest do
  @moduledoc """
  Tests for modules that actually use ExMCP.Server DSL.

  This test identifies true DSL users vs behavior definitions.
  """

  use ExUnit.Case, async: false

  # Helper function to check if a module has DSL-generated functions
  defp module_has_dsl_functions?(module) do
    exports = module.module_info(:exports)

    Enum.any?(exports, fn {name, arity} ->
      name in [:get_tools, :get_resources, :get_prompts] and arity == 0
    end)
  rescue
    _ -> false
  end

  describe "identify actual DSL users" do
    test "categorize all potential DSL users" do
      potential_users = [
        ExMCP.TestHelpers.RefactoredTestServer,
        ExMCP.TestHelpers.ApiTestServer,
        ExMCP.TestHelpers.ErrorTestServer,
        ExMCP.Server.Handler,
        ExMCP.Compliance.Handlers.Handler20241105,
        ExMCP.Compliance.Handlers.Handler20250326,
        ExMCP.TestServer,
        Mix.Tasks.StdioServer
      ]

      # Categorize modules
      {dsl_users, behavior_modules, other_modules} =
        Enum.reduce(potential_users, {[], [], []}, fn module, {dsl, behav, other} ->
          cond do
            # Check if it's a behavior definition
            function_exported?(module, :behaviour_info, 1) ->
              {dsl, [module | behav], other}

            # Check if it has DSL-generated functions (use module_info instead of function_exported?)
            module_has_dsl_functions?(module) ->
              {[module | dsl], behav, other}

            # Otherwise it's something else
            true ->
              {dsl, behav, [module | other]}
          end
        end)

      IO.puts("\nActual DSL Users:")
      Enum.each(dsl_users, &IO.puts("  - #{inspect(&1)}"))

      IO.puts("\nBehavior Modules:")
      Enum.each(behavior_modules, &IO.puts("  - #{inspect(&1)}"))

      IO.puts("\nOther Modules:")
      Enum.each(other_modules, &IO.puts("  - #{inspect(&1)}"))

      # Verify we found some DSL users
      assert length(dsl_users) > 0
    end
  end

  describe "test actual DSL users" do
    # These are confirmed DSL users from the categorization above
    @confirmed_dsl_users [
      ExMCP.TestHelpers.RefactoredTestServer,
      ExMCP.TestHelpers.ApiTestServer,
      ExMCP.TestHelpers.ErrorTestServer
    ]

    for module <- @confirmed_dsl_users do
      @module module

      test "#{inspect(@module)} has DSL-generated functions" do
        # Should have at least one of these
        has_dsl_functions = module_has_dsl_functions?(@module)

        assert has_dsl_functions

        # If it has tools, check the structure
        try do
          tools = @module.get_tools()
          assert is_map(tools)

          for {_name, tool} <- tools do
            assert Map.has_key?(tool, :name)
            assert Map.has_key?(tool, :description)
          end
        rescue
          # Module doesn't have tools, that's fine
          UndefinedFunctionError -> :ok
        end
      end

      test "#{inspect(@module)} is a GenServer" do
        behaviors = @module.module_info(:attributes)[:behaviour] || []
        assert GenServer in behaviors || Enum.any?(behaviors, &(&1 == GenServer))
      end
    end
  end

  describe "manual DSL user test" do
    test "create and test a simple DSL server" do
      defmodule TestDSLServer do
        use ExMCP.Server

        deftool "test_tool" do
          meta do
            description("A test tool")
          end

          input_schema(%{
            type: "object",
            properties: %{
              message: %{type: "string"}
            },
            required: ["message"]
          })
        end

        defresource "test://resource" do
          meta do
            name("Test Resource")
            description("A test resource")
          end

          mime_type("text/plain")
        end

        defprompt "test_prompt" do
          meta do
            name("Test Prompt")
            description("A test prompt")
          end

          arguments do
            arg(:text, required: true, description: "Text input")
          end
        end

        @impl true
        def handle_tool_call("test_tool", %{"message" => msg}, state) do
          {:ok, %{content: [text("Echo: #{msg}")]}, state}
        end

        @impl true
        def handle_resource_read("test://resource", _uri, state) do
          {:ok, [text("Test resource content")], state}
        end

        @impl true
        def handle_prompt_get("test_prompt", %{"text" => text}, state) do
          {:ok, %{messages: [user(text), assistant("I understand.")]}, state}
        end
      end

      # Test the DSL server
      assert Map.has_key?(TestDSLServer.get_tools(), "test_tool")
      assert Map.has_key?(TestDSLServer.get_resources(), "test://resource")
      assert Map.has_key?(TestDSLServer.get_prompts(), "test_prompt")

      # Test capabilities
      capabilities = TestDSLServer.get_capabilities()
      assert capabilities["tools"]["listChanged"] == true
      assert capabilities["resources"]["listChanged"] == true
      assert capabilities["prompts"]["listChanged"] == true

      # Test GenServer start
      {:ok, pid} = TestDSLServer.start_link()
      assert Process.alive?(pid)
      GenServer.stop(pid)
    end
  end
end
