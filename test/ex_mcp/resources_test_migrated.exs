defmodule ExMCP.ResourcesTestMigrated do
  use ExUnit.Case, async: true

  @moduletag :resources

  # Note: Client and Server aliases removed as they're not needed in migrated test

  # DSL-based server replacing TestResourcesHandler
  defmodule TestResourcesServer do
    use ExMCP.Server

    # Resource definitions using DSL
    defresource "file:///config.json" do
      meta do
        name("Application Config")
        description("Main configuration file")
      end

      mime_type("application/json")
    end

    defresource "file:///data.csv" do
      meta do
        name("Sample Data")
        description("CSV data file")
      end

      mime_type("text/csv")
    end

    defresource "file:///image.png" do
      meta do
        name("Logo Image")
        description("Company logo")
      end

      mime_type("image/png")
      subscribable(true)
    end

    # Note: Resource templates don't have DSL macros yet,
    # they're handled via handle_resource_template_list/2 callback

    # Custom initialization to maintain state compatibility
    @impl true
    def init(args) do
      initial_state = %{
        subscriptions: MapSet.new(),
        resource_data: %{
          "file:///config.json" => ~s({"version": "1.0.0", "debug": true}),
          "file:///data.csv" => "name,age\nAlice,30\nBob,25",
          "file:///image.png" => Base.encode64("fake png data")
        }
      }

      {:ok, Map.merge(Map.new(args), initial_state)}
    end

    # Resource reading implementations
    @impl true
    def handle_resource_read("file:///config.json", uri, state) do
      data = state.resource_data[uri]

      content = %{
        uri: uri,
        mime_type: "application/json",
        text: data
      }

      {:ok, [content], state}
    end

    def handle_resource_read("file:///data.csv", uri, state) do
      data = state.resource_data[uri]

      content = %{
        uri: uri,
        text: data
      }

      {:ok, [content], state}
    end

    def handle_resource_read("file:///image.png", uri, state) do
      data = state.resource_data[uri]

      content = %{
        uri: uri,
        mime_type: "image/png",
        # Already base64 encoded
        blob: data
      }

      {:ok, [content], state}
    end

    def handle_resource_read(uri, _full_uri, state) do
      {:error, "Resource not found: #{uri}", state}
    end

    # Subscription management
    @impl true
    def handle_resource_subscribe(uri, state) do
      if Map.has_key?(state.resource_data, uri) do
        new_subscriptions = MapSet.put(state.subscriptions, uri)
        {:ok, %{state | subscriptions: new_subscriptions}}
      else
        {:error, "Resource not found: #{uri}", state}
      end
    end

    @impl true
    def handle_resource_unsubscribe(uri, state) do
      new_subscriptions = MapSet.delete(state.subscriptions, uri)
      {:ok, %{state | subscriptions: new_subscriptions}}
    end

    # Override default resource list with pagination
    @impl true
    def handle_resource_list(state) do
      resources = [
        %{
          uri: "file:///config.json",
          name: "Application Config",
          description: "Main configuration file",
          mime_type: "application/json"
        },
        %{
          uri: "file:///data.csv",
          name: "Sample Data",
          description: "CSV data file",
          mime_type: "text/csv"
        },
        %{
          uri: "file:///image.png",
          name: "Logo Image",
          description: "Company logo",
          mime_type: "image/png"
        }
      ]

      {:ok, resources, state}
    end

    # Custom pagination function for tests
    def handle_resource_list_with_pagination(cursor, state) do
      resources = [
        %{
          uri: "file:///config.json",
          name: "Application Config",
          description: "Main configuration file",
          mime_type: "application/json"
        },
        %{
          uri: "file:///data.csv",
          name: "Sample Data",
          description: "CSV data file",
          mime_type: "text/csv"
        },
        %{
          uri: "file:///image.png",
          name: "Logo Image",
          description: "Company logo",
          mime_type: "image/png"
        }
      ]

      # Simple pagination: return 2 resources at a time
      case cursor do
        nil ->
          {:ok, Enum.take(resources, 2), "page2", state}

        "page2" ->
          {:ok, Enum.drop(resources, 2), nil, state}

        _ ->
          {:error, "Invalid cursor", state}
      end
    end

    # Resource templates list (not a required callback)
    def handle_list_resource_templates(_cursor, state) do
      templates = [
        %{
          uriTemplate: "file:///{path}",
          name: "File Resource",
          description: "Access files by path",
          mime_type: "text/plain"
        },
        %{
          uriTemplate: "db://{table}/{id}",
          name: "Database Record",
          description: "Access database records",
          mime_type: "application/json"
        }
      ]

      {:ok, templates, nil, state}
    end

    # Tool implementations (required but not used)
    @impl true
    def handle_tool_call(_name, _arguments, state) do
      {:error, "No tools available", state}
    end

    # Prompt implementations (required but not used)
    @impl true
    def handle_prompt_get(_name, _args, state) do
      {:error, "No prompts available", state}
    end
  end

  setup do
    # Start DSL server directly (no more handler pattern)
    {:ok, server} = TestResourcesServer.start_link(transport: :native)

    on_exit(fn ->
      if Process.alive?(server), do: GenServer.stop(server)
    end)

    %{server: server}
  end

  describe "migrated resources functionality" do
    test "DSL server exposes resources correctly" do
      resources = TestResourcesServer.get_resources()
      assert Map.has_key?(resources, "file:///config.json")
      assert Map.has_key?(resources, "file:///data.csv")
      assert Map.has_key?(resources, "file:///image.png")

      config_resource = resources["file:///config.json"]
      assert config_resource.name == "Application Config"
      assert config_resource.mime_type == "application/json"
    end

    test "DSL server has correct capabilities" do
      capabilities = TestResourcesServer.get_capabilities()
      assert Map.has_key?(capabilities, "resources")
      assert capabilities["resources"]["listChanged"] == true
      assert capabilities["resources"]["subscribe"] == true
    end

    test "can list resources with pagination", %{server: server} do
      state = :sys.get_state(server)

      # First page
      {:ok, page1, cursor, _new_state} =
        TestResourcesServer.handle_resource_list_with_pagination(nil, state)

      assert length(page1) == 2
      assert cursor == "page2"

      # Check first page resources
      assert Enum.at(page1, 0).uri == "file:///config.json"
      assert Enum.at(page1, 0).mime_type == "application/json"
      assert Enum.at(page1, 1).uri == "file:///data.csv"

      # Second page
      {:ok, page2, next_cursor, _new_state} =
        TestResourcesServer.handle_resource_list_with_pagination(cursor, state)

      assert is_nil(next_cursor)
      assert length(page2) == 1
      assert Enum.at(page2, 0).uri == "file:///image.png"
    end

    test "can read text resources", %{server: server} do
      state = :sys.get_state(server)

      # Read JSON resource
      {:ok, contents, _new_state} =
        TestResourcesServer.handle_resource_read(
          "file:///config.json",
          "file:///config.json",
          state
        )

      assert length(contents) == 1
      resource = hd(contents)
      assert resource.uri == "file:///config.json"
      assert resource.mime_type == "application/json"
      assert resource.text =~ "version"
      assert resource.text =~ "1.0.0"

      # Read CSV resource
      {:ok, contents2, _new_state} =
        TestResourcesServer.handle_resource_read(
          "file:///data.csv",
          "file:///data.csv",
          state
        )

      resource2 = hd(contents2)
      assert resource2.text =~ "name,age"
      assert resource2.text =~ "Alice,30"
    end

    test "can read binary resources", %{server: server} do
      state = :sys.get_state(server)

      {:ok, contents, _new_state} =
        TestResourcesServer.handle_resource_read(
          "file:///image.png",
          "file:///image.png",
          state
        )

      resource = hd(contents)

      assert resource.uri == "file:///image.png"
      assert resource.mime_type == "image/png"
      # Base64 encoded
      assert resource.blob
      # Binary resources don't have text
      refute Map.has_key?(resource, :text)
    end

    test "read_resource returns error for unknown resource", %{server: server} do
      state = :sys.get_state(server)

      {:error, message, _state} =
        TestResourcesServer.handle_resource_read(
          "file:///unknown.txt",
          "file:///unknown.txt",
          state
        )

      assert message =~ "Resource not found"
    end

    test "can subscribe to resources", %{server: server} do
      state = :sys.get_state(server)

      # Subscribe to a resource
      {:ok, new_state} =
        TestResourcesServer.handle_resource_subscribe("file:///config.json", state)

      assert MapSet.member?(new_state.subscriptions, "file:///config.json")

      # Subscribe to another resource
      {:ok, new_state2} =
        TestResourcesServer.handle_resource_subscribe("file:///data.csv", new_state)

      assert MapSet.member?(new_state2.subscriptions, "file:///data.csv")

      # Try to subscribe to non-existent resource
      {:error, message, _state} =
        TestResourcesServer.handle_resource_subscribe("file:///nonexistent.txt", state)

      assert message =~ "Resource not found"
    end

    test "can unsubscribe from resources", %{server: server} do
      state = :sys.get_state(server)

      # First subscribe
      {:ok, subscribed_state} =
        TestResourcesServer.handle_resource_subscribe("file:///config.json", state)

      assert MapSet.member?(subscribed_state.subscriptions, "file:///config.json")

      # Then unsubscribe
      {:ok, unsubscribed_state} =
        TestResourcesServer.handle_resource_unsubscribe("file:///config.json", subscribed_state)

      refute MapSet.member?(unsubscribed_state.subscriptions, "file:///config.json")

      # Unsubscribing from non-subscribed resource should still succeed
      {:ok, final_state} =
        TestResourcesServer.handle_resource_unsubscribe("file:///data.csv", state)

      assert final_state == state
    end

    test "can list resource templates", %{server: server} do
      state = :sys.get_state(server)

      {:ok, templates, _cursor, _new_state} =
        TestResourcesServer.handle_list_resource_templates(nil, state)

      assert length(templates) == 2

      file_template = Enum.find(templates, &(&1.uriTemplate == "file:///{path}"))
      assert file_template.name == "File Resource"
      assert file_template.mime_type == "text/plain"

      db_template = Enum.find(templates, &(&1.uriTemplate == "db://{table}/{id}"))
      assert db_template.name == "Database Record"
      assert db_template.description =~ "database records"
    end

    test "multiple resources can be read in sequence", %{server: server} do
      state = :sys.get_state(server)

      # Read all three resources
      {:ok, config_contents, _} =
        TestResourcesServer.handle_resource_read(
          "file:///config.json",
          "file:///config.json",
          state
        )

      {:ok, data_contents, _} =
        TestResourcesServer.handle_resource_read("file:///data.csv", "file:///data.csv", state)

      {:ok, image_contents, _} =
        TestResourcesServer.handle_resource_read("file:///image.png", "file:///image.png", state)

      # Verify each has correct content type
      assert hd(config_contents).mime_type == "application/json"
      assert hd(data_contents).text =~ "name,age"
      # Binary data
      assert hd(image_contents).blob
    end

    test "subscription state management works", %{server: server} do
      initial_state = :sys.get_state(server)
      assert MapSet.size(initial_state.subscriptions) == 0

      # Subscribe to multiple resources
      {:ok, state1} =
        TestResourcesServer.handle_resource_subscribe("file:///config.json", initial_state)

      {:ok, state2} = TestResourcesServer.handle_resource_subscribe("file:///data.csv", state1)

      assert MapSet.size(state2.subscriptions) == 2
      assert MapSet.member?(state2.subscriptions, "file:///config.json")
      assert MapSet.member?(state2.subscriptions, "file:///data.csv")

      # Unsubscribe from one
      {:ok, state3} =
        TestResourcesServer.handle_resource_unsubscribe("file:///config.json", state2)

      assert MapSet.size(state3.subscriptions) == 1
      refute MapSet.member?(state3.subscriptions, "file:///config.json")
      assert MapSet.member?(state3.subscriptions, "file:///data.csv")
    end
  end
end
