defmodule ExMCP.ClientV2SimpleTest do
  use ExUnit.Case

  test "client module exists and has expected functions" do
    # Basic sanity check that the module exists
    assert {:module, ExMCP.ClientV2} = Code.ensure_compiled(ExMCP.ClientV2)

    # Check that expected functions are exported
    functions = ExMCP.ClientV2.__info__(:functions)

    expected_functions = [
      {:start_link, 1},
      {:list_tools, 1},
      {:list_tools, 2},
      {:call_tool, 3},
      {:call_tool, 4},
      {:list_resources, 1},
      {:list_resources, 2},
      {:read_resource, 2},
      {:read_resource, 3},
      {:list_prompts, 1},
      {:list_prompts, 2},
      {:get_prompt, 2},
      {:get_prompt, 3},
      {:get_prompt, 4},
      {:get_status, 1}
    ]

    for func <- expected_functions do
      assert func in functions, "Expected function #{inspect(func)} not found"
    end
  end

  test "client State struct has expected fields" do
    # Check the State module exists
    assert {:module, ExMCP.ClientV2.State} = Code.ensure_compiled(ExMCP.ClientV2.State)

    # Create a State struct and verify fields
    state = %ExMCP.ClientV2.State{}

    expected_fields = [
      :transport_mod,
      :transport_state,
      :transport_opts,
      :server_info,
      :server_capabilities,
      :connection_status,
      :pending_requests,
      :last_activity,
      :session_id,
      :receiver_task,
      :reconnect_timer,
      :reconnect_attempts,
      :max_reconnect_attempts,
      :reconnect_interval
    ]

    for field <- expected_fields do
      assert Map.has_key?(state, field), "Expected field #{field} not found in State struct"
    end
  end
end
