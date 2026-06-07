defmodule ExMCP.ACP.CapabilitiesTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Capabilities

  defmodule HandlerWithOptionalCallbacks do
    def handle_load_session(_params, _ctx, state), do: {:reply, %{}, state}
    def handle_list_sessions(_params, _ctx, state), do: {:reply, [], state}
    def handle_delete_session(_session_id, _ctx, state), do: {:reply, %{}, state}
    def handle_logout(_ctx, state), do: {:reply, %{}, state}
  end

  describe "supported?/2" do
    test "reads JSON-shaped capabilities" do
      caps = %{
        "loadSession" => true,
        "sessionCapabilities" => %{
          "list" => %{},
          "resume" => %{},
          "additionalDirectories" => %{}
        },
        "auth" => %{"logout" => %{}}
      }

      assert Capabilities.supported?(caps, :load_session)
      assert Capabilities.supported?(caps, :session_list)
      assert Capabilities.supported?(caps, :session_resume)
      assert Capabilities.supported?(caps, :additional_directories)
      assert Capabilities.supported?(caps, :logout)
      refute Capabilities.supported?(caps, :session_delete)
    end

    test "also reads atom-keyed maps" do
      caps = %{sessionCapabilities: %{delete: %{}}}

      assert Capabilities.supported?(caps, :session_delete)
    end
  end

  describe "from_handler/1" do
    test "constructs capabilities from exported callbacks" do
      caps = Capabilities.from_handler(HandlerWithOptionalCallbacks)

      assert Capabilities.supported?(caps, :load_session)
      assert Capabilities.supported?(caps, :session_list)
      assert Capabilities.supported?(caps, :session_delete)
      assert Capabilities.supported?(caps, :logout)
      refute Capabilities.supported?(caps, :session_resume)
    end
  end
end
