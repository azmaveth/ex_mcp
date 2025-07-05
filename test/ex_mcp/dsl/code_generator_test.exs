defmodule ExMCP.DSL.CodeGeneratorTest do
  use ExUnit.Case, async: true

  alias ExMCP.DSL.CodeGenerator

  describe "generate/1" do
    test "generates complete server code structure" do
      code = CodeGenerator.generate()

      # Convert to string to check structure
      code_str = Macro.to_string(code)

      # Should include imports
      assert code_str =~ "use GenServer"
      assert code_str =~ "import ExMCP.DSL.Tool"
      assert code_str =~ "import ExMCP.DSL.Resource"
      assert code_str =~ "import ExMCP.DSL.Prompt"

      # Should include behavior
      assert code_str =~ "@behaviour ExMCP.Server"

      # Should include module attributes
      assert code_str =~ "@__tools__"
      assert code_str =~ "@__resources__"
      assert code_str =~ "@__prompts__"
    end

    test "includes all necessary functions" do
      code = CodeGenerator.generate()
      code_str = Macro.to_string(code)

      # Public API functions
      assert code_str =~ "def start_link"
      assert code_str =~ "def child_spec"
      assert code_str =~ "def get_capabilities"
      assert code_str =~ "def get_tools"
      assert code_str =~ "def get_resources"
      assert code_str =~ "def get_prompts"

      # GenServer callbacks
      assert code_str =~ "def init"
      assert code_str =~ "def handle_call"
      assert code_str =~ "def handle_cast"
      assert code_str =~ "def handle_info"

      # Default callbacks
      assert code_str =~ "def handle_tool_call"
      assert code_str =~ "def handle_resource_read"
      assert code_str =~ "def handle_prompt_get"
    end

    test "generates with custom server options" do
      opts = [server_info: %{name: "TestServer", version: "2.0.0"}]
      code = CodeGenerator.generate(opts)
      code_str = Macro.to_string(code)

      assert code_str =~ "@server_opts"
    end
  end

  describe "generate_imports/0" do
    test "generates all required imports" do
      imports = CodeGenerator.generate_imports()
      imports_str = Macro.to_string(imports)

      # Core imports
      assert imports_str =~ "use GenServer"

      # DSL imports
      assert imports_str =~ "import ExMCP.DSL.Tool"
      assert imports_str =~ "import ExMCP.DSL.Resource"
      assert imports_str =~ "import ExMCP.DSL.Prompt"

      # Aliases - check for grouped alias syntax
      assert imports_str =~
               "alias ExMCP.Protocol.{RequestProcessor, RequestTracker, ResponseBuilder}"

      assert imports_str =~ "alias ExMCP.Server.Transport"

      # Note: Content helpers are now delegated in helper functions generation
      # rather than imported to avoid conflicts
    end
  end

  describe "generate_setup/1" do
    test "generates module attributes and behavior" do
      setup = CodeGenerator.generate_setup([])
      setup_str = Macro.to_string(setup)

      # Behavior declaration
      assert setup_str =~ "@behaviour ExMCP.Server"

      # Module attributes
      assert setup_str =~ "Module.register_attribute(__MODULE__, :__tools__"
      assert setup_str =~ "Module.register_attribute(__MODULE__, :__resources__"
      assert setup_str =~ "Module.register_attribute(__MODULE__, :__prompts__"

      # Default values
      assert setup_str =~ "@__tools__ %{}"
      assert setup_str =~ "@__resources__ %{}"
      assert setup_str =~ "@__prompts__ %{}"
    end

    test "includes server options when provided" do
      opts = [server_info: %{name: "Test", version: "1.0"}]
      setup = CodeGenerator.generate_setup(opts)
      setup_str = Macro.to_string(setup)

      assert setup_str =~ "@server_opts"
    end
  end

  describe "generate_functions/0" do
    test "generates all required function groups" do
      functions = CodeGenerator.generate_functions()
      functions_str = Macro.to_string(functions)

      # Check for key function signatures
      assert functions_str =~ "def start_link"
      assert functions_str =~ "def child_spec"
      assert functions_str =~ "def get_capabilities"
      assert functions_str =~ "def get_tools"
      assert functions_str =~ "def init"
      assert functions_str =~ "def handle_call"
      assert functions_str =~ "defoverridable"
    end
  end

  describe "integration test" do
    defmodule TestServerUsingGenerator do
      use ExMCP.Server

      # Add a simple tool for testing
      deftool "test_tool" do
        meta do
          description("Test tool")
        end

        input_schema(%{type: "object"})
      end

      @impl true
      def handle_tool_call("test_tool", _args, state) do
        {:ok, %{content: [%{"type" => "text", "text" => "Test result"}]}, state}
      end
    end

    test "generated server can be started and used" do
      {:ok, pid} = TestServerUsingGenerator.start_link()

      # Test that the server is running
      assert Process.alive?(pid)

      # Test getter functions
      tools = TestServerUsingGenerator.get_tools()
      assert map_size(tools) == 1
      assert Map.has_key?(tools, "test_tool")

      # Test capabilities
      capabilities = TestServerUsingGenerator.get_capabilities()
      assert Map.has_key?(capabilities, "tools")

      # Stop the server
      GenServer.stop(pid)
    end

    test "generated server handles GenServer calls" do
      {:ok, pid} = TestServerUsingGenerator.start_link()

      # Test server info call
      server_info = GenServer.call(pid, :get_server_info)
      assert is_map(server_info)
      assert Map.has_key?(server_info, :name)

      # Test capabilities call
      capabilities = GenServer.call(pid, :get_capabilities)
      assert is_map(capabilities)

      # Test tools call
      tools = GenServer.call(pid, :get_tools)
      assert is_map(tools)

      GenServer.stop(pid)
    end

    test "generated server processes MCP requests" do
      {:ok, pid} = TestServerUsingGenerator.start_link()

      # Connect test transport
      send(pid, {:test_transport_connect, self()})

      # Send a tools/list request
      request = %{
        "jsonrpc" => "2.0",
        "id" => 123,
        "method" => "tools/list"
      }

      send(pid, {:transport_message, Jason.encode!(request)})

      # Should receive response
      assert_receive {:transport_message, response_json}, 1000

      response = Jason.decode!(response_json)
      assert response["id"] == 123
      assert is_list(response["result"]["tools"])
      assert length(response["result"]["tools"]) == 1

      GenServer.stop(pid)
    end
  end

  describe "transport handling" do
    test "start_link supports different transports" do
      code = CodeGenerator.generate()
      code_str = Macro.to_string(code)

      # Check transport handling
      assert code_str =~ "do_start_link(:native, opts)"
      assert code_str =~ "do_start_link(:stdio, opts)"
      assert code_str =~ "do_start_link(:test, opts)"
      assert code_str =~ "Transport.start_server"
    end
  end

  describe "helper function generation" do
    test "includes request tracking helpers" do
      code = CodeGenerator.generate()
      code_str = Macro.to_string(code)

      assert code_str =~ "track_pending_request"
      assert code_str =~ "complete_pending_request"
      assert code_str =~ "request_cancelled?"
      assert code_str =~ "RequestTracker.track_request"
      assert code_str =~ "RequestTracker.complete_request"
      assert code_str =~ "RequestTracker.cancelled?"
    end

    test "includes response sending helpers" do
      code = CodeGenerator.generate()
      code_str = Macro.to_string(code)

      assert code_str =~ "send_response"
      assert code_str =~ "process_request"
      assert code_str =~ "RequestProcessor.process"
    end
  end
end
