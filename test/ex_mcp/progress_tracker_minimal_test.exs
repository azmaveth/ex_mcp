defmodule ExMCP.ProgressTrackerMinimalTest do
  use ExUnit.Case, async: false

  alias ExMCP.ProgressTracker

  setup do
    # Start the application to ensure ProgressTracker is available
    {:ok, _} = Application.ensure_all_started(:ex_mcp)

    on_exit(fn ->
      # Stop the application after test
      Application.stop(:ex_mcp)
    end)

    :ok
  end

  test "basic progress tracker functionality" do
    # This is a minimal test to verify the ProgressTracker works
    # with proper application setup

    sender_pid = self()

    # Test that we can call functions without errors
    tokens = ProgressTracker.list_active_tokens()
    assert is_list(tokens)

    # Test basic start/complete cycle
    result = ProgressTracker.start_progress("test-token", sender_pid)

    case result do
      {:ok, _state} ->
        # Clean up
        ProgressTracker.complete_progress("test-token")
        assert true

      {:error, _reason} ->
        # ProgressTracker may not be started correctly
        flunk("ProgressTracker should be available when application is started")
    end
  end
end
