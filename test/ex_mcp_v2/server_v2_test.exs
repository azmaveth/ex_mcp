defmodule ExMCP.ServerV2Test do
  use ExUnit.Case, async: true

  alias ExMCP.ServerV2

  # Test server implementation using the DSL
  defmodule TestServer do
    use ExMCP.ServerV2

    deftool "hello" do
      tool_description("Says hello")

      args do
        field(:name, :string, required: true, description: "Name to greet")
        field(:style, :string, required: false, description: "Greeting style")
      end
    end

    deftool "error_tool" do
      tool_description("Tool that always errors")
    end

    defresource "config://app" do
      resource_name("App Config")
      resource_description("Application configuration")
      mime_type("application/json")
    end

    defresource "data://users" do
      resource_name("User Data")
      resource_description("User information")
      mime_type("application/json")
      subscribable(true)
    end

    defprompt "greeting" do
      prompt_name("Greeting Template")
      prompt_description("Generate a greeting message")

      arguments do
        arg(:style, description: "Greeting style", required: false)
        arg(:language, description: "Language for greeting", required: false)
      end
    end

    defprompt "farewell" do
      prompt_name("Farewell Template")
      prompt_description("Generate a farewell message")
    end

    # Handler implementations
    @impl true
    def handle_tool_call("hello", args, state) do
      name = Map.get(args, "name", "World")
      style = Map.get(args, "style", "friendly")
      greeting = "#{style |> String.capitalize()} hello, #{name}!"
      {:ok, %{content: [text(greeting)]}, state}
    end

    def handle_tool_call("error_tool", _args, state) do
      {:error, "This tool always fails", state}
    end

    @impl true
    def handle_resource_read("config://app", _uri, state) do
      content = %{
        "type" => "text",
        "text" => Jason.encode!(%{debug: true, port: 8080}),
        "format" => "json"
      }

      {:ok, [content], state}
    end

    def handle_resource_read("data://users", _uri, state) do
      users = [
        %{id: 1, name: "Alice"},
        %{id: 2, name: "Bob"}
      ]

      content = %{
        "type" => "text",
        "text" => Jason.encode!(users),
        "format" => "json"
      }

      {:ok, [content], state}
    end

    @impl true
    def handle_prompt_get("greeting", args, state) do
      style = Map.get(args, "style", "friendly")
      language = Map.get(args, "language", "english")

      messages = [
        %{
          "role" => "user",
          "content" => %{
            "type" => "text",
            "text" => "Please greet me in a #{style} way in #{language}"
          }
        }
      ]

      {:ok, %{messages: messages}, state}
    end

    def handle_prompt_get("farewell", _args, state) do
      messages = [
        %{
          "role" => "user",
          "content" => %{
            "type" => "text",
            "text" => "Please say goodbye politely"
          }
        }
      ]

      {:ok, %{messages: messages}, state}
    end

    @impl true
    def handle_resource_list(state) do
      resources = [
        %{
          uri: "config://app",
          name: "App Config",
          description: "Application configuration",
          mimeType: "application/json"
        },
        %{
          uri: "data://users",
          name: "User Data",
          description: "User information",
          mimeType: "application/json"
        }
      ]

      {:ok, resources, state}
    end

    @impl true
    def handle_prompt_list(state) do
      prompts = [
        %{
          name: "greeting",
          description: "Generate a greeting message",
          arguments: [
            %{name: "style", description: "Greeting style", required: false},
            %{name: "language", description: "Language for greeting", required: false}
          ]
        },
        %{
          name: "farewell",
          description: "Generate a farewell message",
          arguments: []
        }
      ]

      {:ok, prompts, state}
    end

    @impl true
    def handle_resource_subscribe(uri, state) do
      subscriptions = Map.get(state, :subscriptions, MapSet.new())
      new_subscriptions = MapSet.put(subscriptions, uri)
      {:ok, Map.put(state, :subscriptions, new_subscriptions)}
    end

    @impl true
    def handle_resource_unsubscribe(uri, state) do
      subscriptions = Map.get(state, :subscriptions, MapSet.new())
      new_subscriptions = MapSet.delete(subscriptions, uri)
      {:ok, Map.put(state, :subscriptions, new_subscriptions)}
    end

    @impl true
    def handle_request("custom/ping", _params, state) do
      {:reply, %{"result" => "pong"}, state}
    end

    def handle_request("custom/noreply", _params, state) do
      {:noreply, state}
    end

    def handle_request("custom/error", _params, state) do
      {:error, "Custom error occurred", state}
    end

    @impl true
    def init(args) do
      initial_state = %{
        debug: Keyword.get(args, :debug, false),
        subscriptions: MapSet.new()
      }

      {:ok, initial_state}
    end

    @impl GenServer
    def handle_cast({:notify_resource_update, _uri}, state) do
      {:noreply, state}
    end
  end

  # Minimal server for testing default implementations
  defmodule MinimalServer do
    use ExMCP.ServerV2

    @impl true
    def handle_tool_call(_name, _args, state) do
      {:error, "No tools implemented", state}
    end

    @impl true
    def handle_resource_read(_uri, _full_uri, state) do
      {:error, "No resources implemented", state}
    end

    @impl true
    def handle_prompt_get(_name, _args, state) do
      {:error, "No prompts implemented", state}
    end

    @impl GenServer
    def handle_cast(_msg, state) do
      {:noreply, state}
    end
  end

  describe "__using__ macro" do
    test "injects required behaviour and imports" do
      # Check that GenServer behaviour is implemented (from use GenServer)
      behaviours = TestServer.__info__(:attributes)[:behaviour] || []
      assert GenServer in behaviours

      # Check that required callback functions are exported
      exports = TestServer.__info__(:functions)
      assert Keyword.has_key?(exports, :handle_tool_call)
      assert Keyword.has_key?(exports, :handle_resource_read)
      assert Keyword.has_key?(exports, :handle_prompt_get)
    end

    test "creates module attributes for DSL definitions" do
      # Test that the module has the expected attributes
      assert TestServer.__info__(:attributes)[:__tools__] != nil
      assert TestServer.__info__(:attributes)[:__resources__] != nil
      assert TestServer.__info__(:attributes)[:__prompts__] != nil
    end

    test "defines start_link/1 function" do
      assert function_exported?(TestServer, :start_link, 0)
      assert function_exported?(TestServer, :start_link, 1)
    end

    test "defines capability getter functions" do
      assert function_exported?(TestServer, :get_capabilities, 0)
      assert function_exported?(TestServer, :get_tools, 0)
      assert function_exported?(TestServer, :get_resources, 0)
      assert function_exported?(TestServer, :get_prompts, 0)
    end
  end

  describe "capability detection" do
    test "detects tools capabilities" do
      capabilities = TestServer.get_capabilities()

      assert Map.has_key?(capabilities, "tools")
      assert capabilities["tools"]["listChanged"] == true
    end

    test "detects resources capabilities" do
      capabilities = TestServer.get_capabilities()

      assert Map.has_key?(capabilities, "resources")
      assert capabilities["resources"]["listChanged"] == true
      assert capabilities["resources"]["subscribe"] == true
    end

    test "detects prompts capabilities" do
      capabilities = TestServer.get_capabilities()

      assert Map.has_key?(capabilities, "prompts")
      assert capabilities["prompts"]["listChanged"] == true
    end

    test "minimal server has no capabilities" do
      capabilities = MinimalServer.get_capabilities()

      refute Map.has_key?(capabilities, "tools")
      refute Map.has_key?(capabilities, "resources")
      refute Map.has_key?(capabilities, "prompts")
    end
  end

  describe "DSL definition getters" do
    test "get_tools/0 returns defined tools" do
      tools = TestServer.get_tools()

      assert is_map(tools)
      assert Map.has_key?(tools, "hello")
      assert Map.has_key?(tools, "error_tool")

      hello_tool = tools["hello"]
      assert hello_tool.name == "hello"
      assert hello_tool.description == "Says hello"
      assert is_map(hello_tool.input_schema)
    end

    test "get_resources/0 returns defined resources" do
      resources = TestServer.get_resources()

      assert is_map(resources)
      assert Map.has_key?(resources, "config://app")
      assert Map.has_key?(resources, "data://users")

      config_resource = resources["config://app"]
      assert config_resource.uri == "config://app"
      assert config_resource.name == "App Config"
      assert config_resource.mime_type == "application/json"

      users_resource = resources["data://users"]
      assert users_resource.subscribable == true
    end

    test "get_prompts/0 returns defined prompts" do
      prompts = TestServer.get_prompts()

      assert is_map(prompts)
      assert Map.has_key?(prompts, "greeting")
      assert Map.has_key?(prompts, "farewell")

      greeting_prompt = prompts["greeting"]
      assert greeting_prompt.name == "greeting"
      assert greeting_prompt.description == "Generate a greeting message"
      assert is_list(greeting_prompt.arguments)
    end

    test "minimal server returns empty maps" do
      assert TestServer.get_tools() != %{}
      assert MinimalServer.get_tools() == %{}
      assert MinimalServer.get_resources() == %{}
      assert MinimalServer.get_prompts() == %{}
    end
  end

  describe "GenServer behavior" do
    test "can start server with default state" do
      {:ok, pid} = TestServer.start_link()
      assert Process.alive?(pid)

      state = :sys.get_state(pid)
      assert is_map(state)

      GenServer.stop(pid)
    end

    test "can start server with custom initialization" do
      {:ok, pid} = TestServer.start_link(debug: true, custom_value: "test")

      state = :sys.get_state(pid)
      assert state.debug == true
      assert Map.has_key?(state, :subscriptions)

      GenServer.stop(pid)
    end

    test "default init creates map state from args" do
      {:ok, pid} = MinimalServer.start_link(key1: "value1", key2: "value2")

      state = :sys.get_state(pid)
      assert state == %{key1: "value1", key2: "value2"}

      GenServer.stop(pid)
    end
  end

  describe "callback implementations" do
    setup do
      {:ok, pid} = TestServer.start_link()

      on_exit(fn ->
        if Process.alive?(pid) do
          GenServer.stop(pid)
        end
      end)

      %{server: pid}
    end

    test "handle_tool_call with valid arguments", %{server: server} do
      state = :sys.get_state(server)

      {:ok, result, new_state} = TestServer.handle_tool_call("hello", %{"name" => "Alice"}, state)

      assert %{content: [content]} = result
      assert content["type"] == "text"
      assert String.contains?(content["text"], "hello, Alice")
      assert new_state == state
    end

    test "handle_tool_call with optional arguments", %{server: server} do
      state = :sys.get_state(server)

      {:ok, result, _state} =
        TestServer.handle_tool_call("hello", %{"name" => "Bob", "style" => "formal"}, state)

      assert %{content: [content]} = result
      assert String.contains?(content["text"], "Formal hello, Bob")
    end

    test "handle_tool_call with error", %{server: server} do
      state = :sys.get_state(server)

      {:error, message, new_state} = TestServer.handle_tool_call("error_tool", %{}, state)

      assert message == "This tool always fails"
      assert new_state == state
    end

    test "handle_resource_read for config", %{server: server} do
      state = :sys.get_state(server)

      {:ok, contents, new_state} =
        TestServer.handle_resource_read("config://app", "config://app", state)

      assert is_list(contents)
      assert length(contents) == 1

      [content] = contents
      assert content["type"] == "text"
      assert content["format"] == "json"
      assert new_state == state
    end

    test "handle_resource_read for users", %{server: server} do
      state = :sys.get_state(server)

      {:ok, contents, _state} =
        TestServer.handle_resource_read("data://users", "data://users", state)

      assert is_list(contents)
      [content] = contents
      assert content["type"] == "text"
      assert content["format"] == "json"
    end

    test "handle_prompt_get with arguments", %{server: server} do
      state = :sys.get_state(server)

      {:ok, result, new_state} =
        TestServer.handle_prompt_get(
          "greeting",
          %{"style" => "casual", "language" => "spanish"},
          state
        )

      assert %{messages: messages} = result
      assert is_list(messages)
      assert length(messages) == 1

      [message] = messages
      assert message["role"] == "user"
      assert String.contains?(message["content"]["text"], "casual")
      assert String.contains?(message["content"]["text"], "spanish")
      assert new_state == state
    end

    test "handle_prompt_get with defaults", %{server: server} do
      state = :sys.get_state(server)

      {:ok, result, _state} = TestServer.handle_prompt_get("farewell", %{}, state)

      assert %{messages: messages} = result
      assert length(messages) == 1
    end

    test "handle_resource_list returns defined resources", %{server: server} do
      state = :sys.get_state(server)

      {:ok, resources, new_state} = TestServer.handle_resource_list(state)

      assert is_list(resources)
      assert length(resources) == 2

      uris = Enum.map(resources, & &1.uri)
      assert "config://app" in uris
      assert "data://users" in uris
      assert new_state == state
    end

    test "handle_prompt_list returns defined prompts", %{server: server} do
      state = :sys.get_state(server)

      {:ok, prompts, new_state} = TestServer.handle_prompt_list(state)

      assert is_list(prompts)
      assert length(prompts) == 2

      names = Enum.map(prompts, & &1.name)
      assert "greeting" in names
      assert "farewell" in names
      assert new_state == state
    end

    test "handle_resource_subscribe manages subscriptions", %{server: server} do
      state = :sys.get_state(server)

      {:ok, new_state} = TestServer.handle_resource_subscribe("data://users", state)

      assert MapSet.member?(new_state.subscriptions, "data://users")
    end

    test "handle_resource_unsubscribe manages subscriptions", %{server: server} do
      state = :sys.get_state(server)

      # First subscribe
      {:ok, state_with_sub} = TestServer.handle_resource_subscribe("data://users", state)
      assert MapSet.member?(state_with_sub.subscriptions, "data://users")

      # Then unsubscribe
      {:ok, final_state} = TestServer.handle_resource_unsubscribe("data://users", state_with_sub)
      refute MapSet.member?(final_state.subscriptions, "data://users")
    end

    test "handle_request with custom methods", %{server: server} do
      state = :sys.get_state(server)

      # Test reply response
      {:reply, response, new_state} = TestServer.handle_request("custom/ping", %{}, state)
      assert response == %{"result" => "pong"}
      assert new_state == state

      # Test noreply response
      {:noreply, new_state} = TestServer.handle_request("custom/noreply", %{}, state)
      assert new_state == state

      # Test error response
      {:error, error, new_state} = TestServer.handle_request("custom/error", %{}, state)
      assert error == "Custom error occurred"
      assert new_state == state
    end
  end

  describe "default callback implementations" do
    test "minimal server uses default implementations" do
      {:ok, pid} = MinimalServer.start_link()
      state = :sys.get_state(pid)

      # Test default resource list
      {:ok, resources, new_state} = MinimalServer.handle_resource_list(state)
      assert resources == []
      assert new_state == state

      # Test default prompt list
      {:ok, prompts, new_state} = MinimalServer.handle_prompt_list(state)
      assert prompts == []
      assert new_state == state

      # Test default resource subscription
      {:ok, new_state} = MinimalServer.handle_resource_subscribe("test://uri", state)
      assert new_state == state

      # Test default resource unsubscription
      {:ok, new_state} = MinimalServer.handle_resource_unsubscribe("test://uri", state)
      assert new_state == state

      # Test default request handler
      {:noreply, new_state} = MinimalServer.handle_request("unknown/method", %{}, state)
      assert new_state == state

      GenServer.stop(pid)
    end
  end

  describe "notify_resource_update/2" do
    test "sends cast message to server" do
      {:ok, pid} = TestServer.start_link()

      # This tests that the function doesn't crash - actual notification
      # handling would require the full MCP server infrastructure
      assert :ok = ServerV2.notify_resource_update(pid, "data://users")

      GenServer.stop(pid)
    end

    test "handles named server processes" do
      {:ok, _pid} = TestServer.start_link()

      # Test with module name
      assert :ok = ServerV2.notify_resource_update(TestServer, "data://users")

      GenServer.stop(TestServer)
    end
  end

  describe "edge cases and error handling" do
    test "handles missing tool definitions gracefully" do
      tools = MinimalServer.get_tools()
      assert tools == %{}

      capabilities = MinimalServer.get_capabilities()
      refute Map.has_key?(capabilities, "tools")
    end

    test "handles invalid callback results" do
      defmodule BadServer do
        use ExMCP.ServerV2

        @impl true
        def handle_tool_call(_name, _args, _state) do
          # Invalid return format
          :invalid_return
        end

        @impl true
        def handle_resource_read(_uri, _full_uri, _state) do
          {:ok, "not_a_list", %{}}
        end

        @impl true
        def handle_prompt_get(_name, _args, _state) do
          {:ok, %{messages: "not_a_list"}, %{}}
        end
      end

      {:ok, pid} = BadServer.start_link()
      state = :sys.get_state(pid)

      # These should return whatever the implementation returns,
      # error handling is done at the protocol level
      result = BadServer.handle_tool_call("test", %{}, state)
      assert result == :invalid_return

      GenServer.stop(pid)
    end

    test "resource subscribable detection works correctly" do
      # TestServer has one subscribable resource (data://users)
      capabilities = TestServer.get_capabilities()
      assert capabilities["resources"]["subscribe"] == true

      # Create a server with no subscribable resources
      defmodule NoSubscribeServer do
        use ExMCP.ServerV2

        defresource "static://data" do
          resource_name("Static Data")
          subscribable(false)
        end

        @impl true
        def handle_tool_call(_name, _args, state), do: {:error, "not implemented", state}
        @impl true
        def handle_resource_read(_uri, _full_uri, state), do: {:error, "not implemented", state}
        @impl true
        def handle_prompt_get(_name, _args, state), do: {:error, "not implemented", state}
      end

      capabilities = NoSubscribeServer.get_capabilities()
      refute capabilities["resources"]["subscribe"]
    end

    test "handles attribute retrieval edge cases" do
      # Test the attribute access pattern used in the getters
      defmodule EdgeCaseServer do
        use ExMCP.ServerV2

        @impl true
        def handle_tool_call(_name, _args, state), do: {:error, "not implemented", state}
        @impl true
        def handle_resource_read(_uri, _full_uri, state), do: {:error, "not implemented", state}
        @impl true
        def handle_prompt_get(_name, _args, state), do: {:error, "not implemented", state}
      end

      # Should handle missing or malformed attributes gracefully
      assert EdgeCaseServer.get_tools() == %{}
      assert EdgeCaseServer.get_resources() == %{}
      assert EdgeCaseServer.get_prompts() == %{}
      assert EdgeCaseServer.get_resource_templates() == %{}
    end
  end

  describe "integration with content helpers" do
    test "content helper functions are available" do
      {:ok, pid} = TestServer.start_link()
      state = :sys.get_state(pid)

      # Test that the content helpers work in the callback
      {:ok, result, _state} = TestServer.handle_tool_call("hello", %{"name" => "Test"}, state)

      assert %{content: [content]} = result
      assert content["type"] == "text"
      assert is_binary(content["text"])

      GenServer.stop(pid)
    end

    test "json content helper works" do
      {:ok, pid} = TestServer.start_link()
      state = :sys.get_state(pid)

      {:ok, contents, _state} =
        TestServer.handle_resource_read("config://app", "config://app", state)

      [content] = contents
      assert content["type"] == "text"
      assert content["format"] == "json"
      assert String.contains?(content["text"], "debug")

      GenServer.stop(pid)
    end

    test "user message helper works" do
      {:ok, pid} = TestServer.start_link()
      state = :sys.get_state(pid)

      {:ok, result, _state} = TestServer.handle_prompt_get("greeting", %{}, state)

      %{messages: [message]} = result
      assert message["role"] == "user"
      assert message["content"]["type"] == "text"

      GenServer.stop(pid)
    end
  end
end
