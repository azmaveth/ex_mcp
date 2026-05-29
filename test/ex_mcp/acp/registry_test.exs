defmodule ExMCP.ACP.RegistryTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Registry

  @registry %{
    "version" => "1.0.0",
    "agents" => [
      %{
        "id" => "codex-acp",
        "name" => "Codex CLI",
        "description" => "ACP adapter for OpenAI's coding assistant",
        "distribution" => %{
          "npx" => %{"package" => "@zed-industries/codex-acp@0.15.0"}
        }
      },
      %{
        "id" => "gemini-cli",
        "name" => "Gemini CLI",
        "description" => "Google's official CLI for Gemini",
        "distribution" => %{
          "npx" => %{"package" => "@google/gemini-cli@0.44.1", "args" => ["--acp"]}
        }
      }
    ]
  }

  describe "default_url/0" do
    test "points at the public latest registry" do
      assert Registry.default_url() ==
               "https://cdn.agentclientprotocol.com/registry/v1/latest/registry.json"
    end
  end

  describe "parse/1" do
    test "decodes registry JSON" do
      assert {:ok, registry} = Registry.parse(Jason.encode!(@registry))
      assert registry["version"] == "1.0.0"
      assert length(registry["agents"]) == 2
    end
  end

  describe "agents/1" do
    test "returns agents or an empty list" do
      assert length(Registry.agents(@registry)) == 2
      assert Registry.agents(%{}) == []
    end
  end

  describe "get_agent/2" do
    test "finds by id, name, and case-insensitive fallback" do
      assert Registry.get_agent(@registry, "codex-acp")["name"] == "Codex CLI"
      assert Registry.get_agent(@registry, "Gemini CLI")["id"] == "gemini-cli"
      assert Registry.get_agent(@registry, "codex cli")["id"] == "codex-acp"
    end
  end

  describe "find_agents/2" do
    test "searches id, name, and description" do
      assert [%{"id" => "codex-acp"}] = Registry.find_agents(@registry, "openai")
      assert [%{"id" => "gemini-cli"}] = Registry.find_agents(@registry, "google")
    end
  end

  describe "npx_command/1" do
    test "builds an npx command and preserves registry args" do
      agent = Registry.get_agent(@registry, "gemini-cli")

      assert Registry.npx_command(agent) ==
               {:ok, ["npx", "-y", "@google/gemini-cli@0.44.1", "--acp"]}
    end

    test "returns an error when npx distribution is unavailable" do
      assert Registry.npx_command(%{"distribution" => %{}}) ==
               {:error, :npx_distribution_not_found}
    end
  end

  describe "fetch/1" do
    test "uses the configured http client and parses the response body" do
      http_client = fn url, _headers, timeout ->
        assert url == "https://example.test/registry.json"
        assert timeout == 123
        {:ok, Jason.encode!(@registry)}
      end

      assert {:ok, registry} =
               Registry.fetch(
                 url: "https://example.test/registry.json",
                 timeout: 123,
                 http_client: http_client
               )

      assert Registry.get_agent(registry, "codex-acp")["name"] == "Codex CLI"
    end
  end
end
