defmodule ExMCP.ACPCompatTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACPCompat

  @sha_a String.duplicate("a", 40)
  @sha_b String.duplicate("b", 40)

  @manifest %{
    "schemaVersion" => 1,
    "catalog" => %{
      "source" => "https://example.test/agents.md",
      "agents" => ["Agent One", "Agent Two"]
    },
    "registry" => %{
      "source" => "https://example.test/registry.json",
      "agents" => [
        %{"id" => "agent-one", "version" => "1.0.0"},
        %{"id" => "agent-two", "version" => "2.0.0"}
      ]
    },
    "adapterUpstreams" => [
      %{
        "id" => "reference-adapter",
        "adapter" => "ExMCP.ACP.Adapters.Reference",
        "repository" => "https://github.com/example/reference-adapter",
        "branch" => "main",
        "commit" => @sha_a
      }
    ],
    "interopAgents" => [
      %{
        "id" => "agent-one",
        "catalogName" => "Agent One",
        "tier" => "initialize",
        "command" => ["agent-one", "--acp"]
      }
    ]
  }

  describe "validate_manifest/1" do
    test "accepts a complete reviewed manifest" do
      assert :ok = ACPCompat.validate_manifest(@manifest)
    end

    test "rejects duplicate executable entries" do
      duplicate = hd(@manifest["interopAgents"])
      manifest = put_in(@manifest, ["interopAgents"], [duplicate, duplicate])

      assert {:error, :duplicate_interop_agents} = ACPCompat.validate_manifest(manifest)
    end

    test "requires HTTPS drift sources" do
      manifest = put_in(@manifest, ["catalog", "source"], "http://example.test/agents.md")

      assert {:error, {:invalid_source, "catalog"}} = ACPCompat.validate_manifest(manifest)
    end

    test "requires full immutable upstream revisions" do
      manifest = put_in(@manifest, ["adapterUpstreams", Access.at(0), "commit"], "main")

      assert {:error, :invalid_adapter_upstreams} = ACPCompat.validate_manifest(manifest)
    end

    test "rejects mutable npx package commands" do
      manifest =
        put_in(
          @manifest,
          ["interopAgents", Access.at(0), "command"],
          ["npx", "-y", "agent-one"]
        )

      assert {:error, :invalid_interop_agents} = ACPCompat.validate_manifest(manifest)
    end
  end

  describe "parse_catalog/1" do
    test "extracts only the primary link from agent bullets" do
      markdown = """
      # Agents

      The following agents can be used with an ACP Client:

      - [Agent Two](https://example.test/two)
      - [Agent One](https://example.test/one) (via [adapter](https://example.test/adapter))
      """

      assert {:ok, ["Agent One", "Agent Two"]} = ACPCompat.parse_catalog(markdown)
    end

    test "fails closed when the page shape no longer contains agent bullets" do
      assert {:error, :no_catalog_agents_found} = ACPCompat.parse_catalog("# Agents\nNo list")
    end
  end

  describe "registry_snapshot/1" do
    test "keeps stable identity and version fields in id order" do
      registry = %{
        "agents" => [
          %{"id" => "z", "version" => "2.0.0", "description" => "ignored"},
          %{"id" => "a", "version" => "1.0.0", "distribution" => %{}}
        ]
      }

      assert {:ok,
              [
                %{"id" => "a", "version" => "1.0.0"},
                %{"id" => "z", "version" => "2.0.0"}
              ]} = ACPCompat.registry_snapshot(registry)
    end
  end

  describe "compare/2" do
    test "reports catalog, registry, and adapter upstream drift" do
      snapshots = %{
        catalog: ["Agent One", "Agent Three"],
        registry: [
          %{"id" => "agent-one", "version" => "1.1.0"},
          %{"id" => "agent-three", "version" => "3.0.0"}
        ],
        upstreams: %{"reference-adapter" => @sha_b}
      }

      drift = ACPCompat.compare(@manifest, snapshots)

      assert %{
               "kind" => "catalog",
               "added" => ["Agent Three"],
               "removed" => ["Agent Two"]
             } in drift

      assert registry = Enum.find(drift, &(&1["kind"] == "registry"))
      assert registry["added"] == ["agent-three"]
      assert registry["removed"] == ["agent-two"]

      assert registry["versionChanges"] == [
               %{
                 "id" => "agent-one",
                 "expectedVersion" => "1.0.0",
                 "actualVersion" => "1.1.0"
               }
             ]

      assert upstream = Enum.find(drift, &(&1["kind"] == "adapter_upstream"))
      assert upstream["expected"] == @sha_a
      assert upstream["actual"] == @sha_b
      assert upstream["compareUrl"] =~ "/compare/#{@sha_a}...#{@sha_b}"
    end

    test "returns no drift for matching snapshots" do
      snapshots = %{
        catalog: @manifest["catalog"]["agents"],
        registry: @manifest["registry"]["agents"],
        upstreams: %{"reference-adapter" => @sha_a}
      }

      assert ACPCompat.compare(@manifest, snapshots) == []
    end
  end

  describe "check_remote/2" do
    test "fetches all source types through the injected HTTP client" do
      parent = self()

      http_client = fn url, headers, _timeout, _options ->
        send(parent, {:request, url, headers})

        case url do
          "https://example.test/agents.md" ->
            {:ok,
             "- [Agent One](https://example.test/one)\n" <>
               "- [Agent Two](https://example.test/two)\n"}

          "https://example.test/registry.json" ->
            {:ok, Jason.encode!(%{"agents" => @manifest["registry"]["agents"]})}

          "https://api.github.com/repos/example/reference-adapter/commits/main" ->
            {:ok, Jason.encode!(%{"sha" => @sha_a})}
        end
      end

      report = ACPCompat.check_remote(@manifest, http_client: http_client)

      assert report.drift == []
      assert report.errors == []
      assert_receive {:request, "https://example.test/agents.md", _headers}
      assert_receive {:request, "https://example.test/registry.json", _headers}

      assert_receive {:request,
                      "https://api.github.com/repos/example/reference-adapter/commits/main",
                      _headers}
    end

    test "reports source failures without treating empty fetches as authoritative removals" do
      http_client = fn _url, _headers, _timeout, _options -> {:error, :offline} end

      report = ACPCompat.check_remote(@manifest, http_client: http_client)

      assert length(report.errors) == 3
      assert Enum.any?(report.errors, &(&1["source"] == "catalog"))
      assert Enum.any?(report.errors, &(&1["source"] == "registry"))
      assert Enum.any?(report.errors, &(&1["source"] == "adapter_upstream:reference-adapter"))
    end
  end

  describe "interop_agent/2" do
    test "returns only explicitly reviewed executable entries" do
      assert {:ok, %{"command" => ["agent-one", "--acp"]}} =
               ACPCompat.interop_agent(@manifest, "agent-one")

      assert {:error, :unknown_agent} = ACPCompat.interop_agent(@manifest, "not-reviewed")
    end
  end

  describe "checked-in manifest" do
    test "tracks the full catalog baseline and all adapter reference repositories" do
      assert {:ok, manifest} = ACPCompat.load_manifest()
      assert length(manifest["catalog"]["agents"]) == 40
      assert length(manifest["registry"]["agents"]) == 39

      repositories = Enum.map(manifest["adapterUpstreams"], & &1["repository"])

      assert "https://github.com/agentclientprotocol/claude-agent-acp" in repositories
      assert "https://github.com/zed-industries/codex-acp" in repositories
      assert "https://github.com/svkozak/pi-acp" in repositories

      assert Enum.sort(Enum.map(manifest["interopAgents"], & &1["id"])) ==
               ~w(claude-acp codex-acp gemini pi-acp)
    end
  end
end
