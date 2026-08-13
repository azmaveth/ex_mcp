defmodule ExMCP.Internal.PortEnvironmentTest do
  use ExUnit.Case, async: false

  alias ExMCP.Internal.PortEnvironment

  test "validates supported child environment policies" do
    assert :ok = PortEnvironment.validate_policy([])
    assert :ok = PortEnvironment.validate_policy(environment_policy: :isolated)
    assert :ok = PortEnvironment.validate_policy(environment_policy: :inherit)

    assert {:error, {:invalid_environment_policy, :unsafe}} =
             PortEnvironment.validate_policy(environment_policy: :unsafe)
  end

  test "normalizes supported environment shapes without losing explicit removals" do
    assert PortEnvironment.normalize(%{"BAR" => false, FOO: 1}) == %{
             "FOO" => "1",
             "BAR" => false
           }

    assert PortEnvironment.normalize([
             {"TUPLE", 2},
             %{name: :ATOM_MAP, value: true},
             %{"name" => "STRING_MAP", "value" => "value"}
           ]) == %{
             "TUPLE" => "2",
             "ATOM_MAP" => "true",
             "STRING_MAP" => "value"
           }

    assert PortEnvironment.normalize(:invalid) == %{}
  end

  test "encodes normalized values for Port.open/2" do
    assert PortEnvironment.to_port(%{"SET" => "value", "UNSET" => false})
           |> Map.new() == %{~c"SET" => ~c"value", ~c"UNSET" => false}
  end

  test "isolated base removes ambient variables while inherit starts empty" do
    sentinel = "EX_MCP_PORT_ENVIRONMENT_TEST_SECRET"
    previous = System.get_env(sentinel)
    System.put_env(sentinel, "ambient")

    on_exit(fn ->
      if is_binary(previous),
        do: System.put_env(sentinel, previous),
        else: System.delete_env(sentinel)
    end)

    assert PortEnvironment.base([])[sentinel] == false
    assert PortEnvironment.base(environment_policy: :inherit) == %{}
  end
end
