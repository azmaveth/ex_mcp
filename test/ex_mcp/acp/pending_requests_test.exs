defmodule ExMCP.ACP.PendingRequestsTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.PendingRequests

  test "tracks pending request values" do
    pending =
      PendingRequests.empty()
      |> PendingRequests.put(1, {:from, :prompt})
      |> PendingRequests.put("agent", :request)

    assert MapSet.new(PendingRequests.values(pending)) == MapSet.new([{:from, :prompt}, :request])
    assert PendingRequests.pop(pending, 1) == {{:from, :prompt}, %{"agent" => :request}}
    assert PendingRequests.delete(pending, "agent") == %{1 => {:from, :prompt}}
  end
end
