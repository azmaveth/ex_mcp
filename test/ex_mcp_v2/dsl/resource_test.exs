defmodule ExMCP.DSL.ResourceTest do
  use ExUnit.Case, async: true

  alias ExMCP.DSL.Resource

  describe "uri_matches?/2" do
    test "matches exact URIs" do
      assert Resource.uri_matches?("file://app.log", "file://app.log")
      refute Resource.uri_matches?("file://app.log", "file://other.log")
    end

    test "matches wildcard patterns" do
      pattern = "file://logs/*.log"

      assert Resource.uri_matches?("file://logs/app.log", pattern)
      assert Resource.uri_matches?("file://logs/error.log", pattern)
      refute Resource.uri_matches?("file://logs/app.txt", pattern)
      refute Resource.uri_matches?("file://data/app.log", pattern)
    end

    test "handles multiple wildcards" do
      pattern = "file://*/logs/*.log"

      assert Resource.uri_matches?("file://app/logs/error.log", pattern)
      assert Resource.uri_matches?("file://api/logs/debug.log", pattern)
      refute Resource.uri_matches?("file://app/data/error.log", pattern)
    end

    test "handles invalid regex patterns gracefully" do
      # Test with pattern that would create invalid regex
      pattern = "file://logs/[.log"

      refute Resource.uri_matches?("file://logs/app.log", pattern)
    end
  end

  describe "extract_variables/2" do
    test "extracts variables from URI templates" do
      template = "repos/{owner}/{repo}/issues/{id}"
      uri = "repos/octocat/Hello-World/issues/123"

      variables = Resource.extract_variables(uri, template)

      assert variables == %{
               "owner" => "octocat",
               "repo" => "Hello-World",
               "id" => "123"
             }
    end

    test "returns empty map for non-matching URIs" do
      template = "repos/{owner}/{repo}/issues/{id}"
      uri = "different/path/structure"

      variables = Resource.extract_variables(uri, template)

      assert variables == %{}
    end

    test "handles templates without variables" do
      template = "static/path/file.txt"
      uri = "static/path/file.txt"

      variables = Resource.extract_variables(uri, template)

      assert variables == %{}
    end

    test "handles complex variable patterns" do
      template = "api/v{version}/users/{user_id}/posts/{post_id}"
      uri = "api/v2/users/123/posts/456"

      variables = Resource.extract_variables(uri, template)

      assert variables == %{
               "version" => "2",
               "user_id" => "123",
               "post_id" => "456"
             }
    end

    test "handles invalid template patterns gracefully" do
      template = "invalid/{pattern"
      uri = "invalid/test"

      variables = Resource.extract_variables(uri, template)

      assert variables == %{}
    end
  end
end
