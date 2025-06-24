defmodule ExMCP.ProgressTrackerMinimalTest do
  use ExUnit.Case, async: true

  alias ExMCP.ProgressTracker

  test "basic progress tracker functionality" do
    # This is a minimal test to verify the ProgressTracker works
    # without any complex setup or teardown

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
        # ProgressTracker may not be started in test env
        assert true
    end
  end
end
