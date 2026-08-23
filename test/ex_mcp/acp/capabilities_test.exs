defmodule ExMCP.ACP.CapabilitiesTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Capabilities

  defmodule HandlerWithOptionalCallbacks do
    def handle_load_session(_params, _ctx, state), do: {:reply, %{}, state}
    def handle_list_sessions(_params, _ctx, state), do: {:reply, [], state}
    def handle_fork_session(_params, _ctx, state), do: {:reply, %{}, state}
    def handle_delete_session(_session_id, _ctx, state), do: {:reply, %{}, state}
    def handle_logout(_ctx, state), do: {:reply, %{}, state}
  end

  defmodule AdapterWithListSessions2 do
    def list_sessions(_params, state), do: {:ok, [], state}
  end

  defmodule AdapterWithLegacyListSessions1 do
    def list_sessions(state), do: {:ok, [], state}
  end

  defmodule AdapterWithForkSession2 do
    def fork_session(_params, state), do: {:ok, %{"sessionId" => "forked"}, state}
  end

  describe "supported?/2" do
    test "reads JSON-shaped capabilities" do
      caps = %{
        "loadSession" => true,
        "sessionCapabilities" => %{
          "list" => %{},
          "resume" => %{},
          "fork" => %{},
          "additionalDirectories" => %{}
        },
        "mcpCapabilities" => %{
          "_meta" => %{"ex_mcp.mcpCapabilities" => %{"beam" => true}}
        },
        "session" => %{"configOptions" => %{"boolean" => %{}}},
        "auth" => %{"logout" => %{}}
      }

      assert Capabilities.supported?(caps, :load_session)
      assert Capabilities.supported?(caps, :session_list)
      assert Capabilities.supported?(caps, :session_resume)
      assert Capabilities.supported?(caps, :session_fork)
      assert Capabilities.supported?(caps, :additional_directories)
      assert Capabilities.supported?(caps, :mcp_beam)
      assert Capabilities.supported?(caps, :logout)
      assert Capabilities.supported?(caps, :boolean_config_options)
      refute Capabilities.supported?(caps, :session_delete)
    end

    test "also reads atom-keyed maps" do
      caps = %{sessionCapabilities: %{delete: %{}}}

      assert Capabilities.supported?(caps, :session_delete)
    end
  end

  describe "put/3 client capabilities" do
    test "explicitly opts a v1 client into boolean config options" do
      caps =
        %{"session" => %{"existing" => %{}}}
        |> Capabilities.put(:boolean_config_options, true)

      assert caps == %{
               "session" => %{
                 "existing" => %{},
                 "configOptions" => %{"boolean" => %{}}
               }
             }

      assert Capabilities.supported?(caps, :boolean_config_options)
      refute Capabilities.supported?(%{}, :boolean_config_options)
      assert Capabilities.put(%{}, :boolean_config_options, false) == %{}
    end

    test "preserves existing config option capabilities" do
      caps = %{"session" => %{"configOptions" => %{"_meta" => %{"ui" => "native"}}}}

      assert get_in(Capabilities.put(caps, :boolean_config_options, true), [
               "session",
               "configOptions"
             ]) == %{"_meta" => %{"ui" => "native"}, "boolean" => %{}}
    end
  end

  describe "from_handler/1" do
    test "constructs capabilities from exported callbacks" do
      caps = Capabilities.from_handler(HandlerWithOptionalCallbacks)

      assert Capabilities.supported?(caps, :load_session)
      assert Capabilities.supported?(caps, :session_list)
      assert Capabilities.supported?(caps, :session_delete)
      assert Capabilities.supported?(caps, :session_fork)
      assert Capabilities.supported?(caps, :logout)
      refute Capabilities.supported?(caps, :session_resume)
    end
  end

  describe "advertise_adapter_session_list/2" do
    test "advertises only the param-aware adapter callback" do
      caps = Capabilities.advertise_adapter_session_list(%{}, AdapterWithListSessions2)

      assert Capabilities.supported?(caps, :session_list)

      legacy_caps =
        Capabilities.advertise_adapter_session_list(%{}, AdapterWithLegacyListSessions1)

      refute Capabilities.supported?(legacy_caps, :session_list)
    end
  end

  describe "advertise_adapter_session_fork/2" do
    test "advertises adapters with fork_session/2" do
      caps = Capabilities.advertise_adapter_session_fork(%{}, AdapterWithForkSession2)

      assert Capabilities.supported?(caps, :session_fork)
    end
  end
end
