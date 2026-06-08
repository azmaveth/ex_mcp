defmodule ExMCP.Internal.RequestParamsTest do
  use ExUnit.Case, async: true

  alias ExMCP.Internal.RequestParams

  describe "cursor/1" do
    test "omits cursor params when no cursor is present" do
      assert RequestParams.cursor(nil) == %{}
    end

    test "builds cursor params when a cursor is present" do
      assert RequestParams.cursor("next-page") == %{"cursor" => "next-page"}
    end
  end

  describe "cursor_from_opts/1" do
    test "omits cursor params when options do not contain a cursor" do
      assert RequestParams.cursor_from_opts([]) == %{}
      assert RequestParams.cursor_from_opts(timeout: 5_000) == %{}
    end

    test "builds cursor params from options" do
      assert RequestParams.cursor_from_opts(cursor: "next-page") == %{"cursor" => "next-page"}
    end
  end

  describe "take_cursor/1" do
    test "returns cursor params and remaining options" do
      assert RequestParams.take_cursor(cursor: "next-page", timeout: 5_000) ==
               {%{"cursor" => "next-page"}, [timeout: 5_000]}
    end

    test "returns empty params when cursor is absent" do
      assert RequestParams.take_cursor(timeout: 5_000) == {%{}, [timeout: 5_000]}
    end
  end

  describe "uri/1" do
    test "builds URI params" do
      assert RequestParams.uri("file:///tmp/example.txt") == %{"uri" => "file:///tmp/example.txt"}
    end
  end

  describe "named/2" do
    test "builds named operation params with arguments" do
      assert RequestParams.named("weather", %{"location" => "Chicago"}) == %{
               "name" => "weather",
               "arguments" => %{"location" => "Chicago"}
             }
    end

    test "defaults arguments to an empty map" do
      assert RequestParams.named("weather") == %{"name" => "weather", "arguments" => %{}}
    end
  end

  describe "completion/2" do
    test "builds completion params" do
      ref = %{"type" => "ref/prompt", "name" => "code"}
      argument = %{"name" => "language", "value" => "elixir"}

      assert RequestParams.completion(ref, argument) == %{"ref" => ref, "argument" => argument}
    end
  end

  describe "metadata helpers" do
    test "adds request metadata when meta is a map" do
      params = RequestParams.named("weather")

      assert RequestParams.with_meta(params, %{"progressToken" => "token-1"}) == %{
               "name" => "weather",
               "arguments" => %{},
               "_meta" => %{"progressToken" => "token-1"}
             }
    end

    test "preserves client option behavior by adding empty metadata maps" do
      params = RequestParams.named("weather")

      assert RequestParams.with_opts_meta(params, meta: %{}) == %{
               "name" => "weather",
               "arguments" => %{},
               "_meta" => %{}
             }
    end

    test "ignores metadata values that are not maps" do
      params = RequestParams.named("weather")

      assert RequestParams.with_meta(params, "token-1") == params
      assert RequestParams.with_opts_meta(params, []) == params
    end

    test "omits empty metadata maps for protocol encoding" do
      params = RequestParams.named("weather")

      assert RequestParams.with_non_empty_meta(params, %{}) == params

      assert RequestParams.with_non_empty_meta(params, %{"requestId" => "req-1"}) == %{
               "name" => "weather",
               "arguments" => %{},
               "_meta" => %{"requestId" => "req-1"}
             }
    end

    test "normalizes progress tokens into metadata" do
      params = RequestParams.named("weather")

      assert RequestParams.with_progress_or_meta(params, "token-1") == %{
               "name" => "weather",
               "arguments" => %{},
               "_meta" => %{"progressToken" => "token-1"}
             }

      assert RequestParams.with_progress_or_meta(params, 123) == %{
               "name" => "weather",
               "arguments" => %{},
               "_meta" => %{"progressToken" => 123}
             }
    end
  end
end
