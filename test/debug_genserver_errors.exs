defmodule DebugGenServerErrorsTest do
  use ExUnit.Case, async: true

  defmodule TestGenServer do
    use GenServer

    def start_link(should_fail) do
      GenServer.start_link(__MODULE__, should_fail)
    end

    def init(true) do
      {:stop, {:connection_failed, "test error"}}
    end

    def init(false) do
      {:ok, %{}}
    end
  end

  test "GenServer.start_link error behavior" do
    # Test what happens when init returns {:stop, reason}
    # This should return {:error, reason} according to GenServer docs
    result = TestGenServer.start_link(true)
    IO.puts("GenServer start_link result when init returns {:stop, reason}: #{inspect(result)}")

    # According to GenServer docs, this should be {:error, {:connection_failed, "test error"}}
    case result do
      {:error, reason} ->
        IO.puts("✅ SUCCESS: Got expected {:error, reason}: #{inspect(reason)}")
        assert reason == {:connection_failed, "test error"}

      {:ok, pid} ->
        GenServer.stop(pid)
        flunk("Expected error but got success")

      other ->
        flunk("Unexpected result: #{inspect(other)}")
    end

    # Test successful case
    result2 = TestGenServer.start_link(false)
    IO.puts("GenServer start_link result when init returns {:ok, state}: #{inspect(result2)}")

    case result2 do
      {:ok, pid} -> GenServer.stop(pid)
      _ -> :ok
    end
  end
end
