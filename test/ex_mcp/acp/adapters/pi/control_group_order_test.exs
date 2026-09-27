defmodule ExMCP.ACP.Adapters.Pi.ControlGroupOrderTest do
  @moduledoc """
  Regression test for the order in which subprocess exit fails pending
  control groups.

  Control-group ids are minted as `"group-N"` from the VM-wide
  `System.unique_integer([:positive, :monotonic])` counter. Exit handling
  used to take `Map.values/1` of the groups, i.e. the lexicographic order of
  those strings, so two groups straddling a digit boundary (`"group-999"`,
  `"group-1000"`) were failed in reverse order. The golden
  `exit_fails_active_prompt_and_control_groups_then_cancels_queue` scenario
  only hit that when concurrent tests happened to push the counter across a
  boundary between its two `session/new` requests.

  Not async: the test positions the shared counter right below a power of
  ten, so no other test may consume monotonic integers concurrently.
  """

  use ExUnit.Case, async: false

  alias ExMCP.Test.PiGolden
  alias ExMCP.Test.PiGolden.Flows

  test "exit fails control groups in creation order across a digit boundary" do
    # open_session mints one group (10^k - 2, settled before the exit); the
    # two session/new groups still pending at exit get 10^k - 1 and 10^k.
    advance_monotonic_counter_to_boundary()

    steps =
      Flows.open_session(1) ++
        [
          Flows.session_new(5),
          Flows.session_new(6),
          {:port_exit, 137}
        ]

    assert %{messages: [%{"id" => 5, "error" => _}, %{"id" => 6, "error" => _}]} =
             PiGolden.last_result(PiGolden.run(steps))
  end

  defp advance_monotonic_counter_to_boundary do
    current = System.unique_integer([:positive, :monotonic])
    target = next_power_of_ten(current + 3) - 3
    burn_until(target)
  end

  defp burn_until(target) do
    if System.unique_integer([:positive, :monotonic]) < target, do: burn_until(target)
  end

  defp next_power_of_ten(n, p \\ 10), do: if(p > n, do: p, else: next_power_of_ten(n, p * 10))
end
