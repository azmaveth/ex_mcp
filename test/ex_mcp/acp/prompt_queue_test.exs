defmodule ExMCP.ACP.PromptQueueTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.PromptQueue

  test "enqueues and pops in FIFO order" do
    queue =
      PromptQueue.new()
      |> PromptQueue.enqueue(:first)
      |> PromptQueue.enqueue(:second)

    assert PromptQueue.len(queue) == 2
    assert {:value, :first, queue} = PromptQueue.pop(queue)
    assert {:value, :second, queue} = PromptQueue.pop(queue)
    assert PromptQueue.pop(queue) == :empty
    assert PromptQueue.empty?(queue)
  end

  test "splits matching items and preserves remaining order" do
    queue =
      PromptQueue.from_list([
        %{session_id: "a", id: 1},
        %{session_id: "b", id: 2},
        %{session_id: "a", id: 3}
      ])

    assert {matches, remaining} = PromptQueue.split(queue, &(&1.session_id == "a"))
    assert Enum.map(matches, & &1.id) == [1, 3]
    assert PromptQueue.to_list(remaining) == [%{session_id: "b", id: 2}]
  end

  test "drains all items" do
    queue = PromptQueue.from_list([1, 2, 3])

    assert PromptQueue.drain(queue) == {[1, 2, 3], PromptQueue.new()}
  end
end
