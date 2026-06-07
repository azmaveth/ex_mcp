defmodule ExMCP.ACP.NameValueTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.NameValue

  test "normalizes maps and lists into ACP name/value lists" do
    assert NameValue.list(%{PATH: "/bin"}) == [%{"name" => "PATH", "value" => "/bin"}]

    assert NameValue.list([%{name: :TERM, value: :dumb}, {"HOME", "/tmp"}]) == [
             %{"name" => "TERM", "value" => "dumb"},
             %{"name" => "HOME", "value" => "/tmp"}
           ]
  end

  test "normalizes ACP name/value lists into maps" do
    assert NameValue.map([%{"name" => :TERM, "value" => :dumb}, {:HOME, "/tmp"}]) == %{
             "TERM" => "dumb",
             "HOME" => "/tmp"
           }
  end
end
