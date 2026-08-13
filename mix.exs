defmodule ExMCP.MixProject do
  use Mix.Project

  @version "1.0.0-rc.8"
  @github_url "https://github.com/azmaveth/ex_mcp"

  def project do
    [
      app: :ex_mcp,
      version: @version,
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.env()),
      test_ignore_filters: [~r"^test/conformance/(client|server)\.exs$"],
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package(),
      docs: docs(),
      source_url: @github_url,
      homepage_url: @github_url,
      test_coverage: [tool: ExCoveralls],
      dialyzer: [
        plt_add_apps: [:mix, :ex_unit],
        ignore_warnings: ".dialyzer_ignore.exs",
        list_unused_filters: false,
        plt_local_path: "priv/plts",
        plt_core_path: "priv/plts"
      ],
      # Cowlib 2.19.0 is the newest compatible release. These remaining
      # advisories are mitigated by Plug/Cowboy response-header validation,
      # and ExMCP does not call cow_cookie:cookie/1. Those assumptions are
      # locked by dependency_advisory_mitigation_test.exs. Security owner:
      # project maintainers; review/remove these exceptions by 2026-09-12 or
      # immediately when a patched Cowlib is published. Keep the exceptions
      # exact so `mix hex.audit` still fails on every new advisory.
      hex: [
        ignore_advisories: [
          "EEF-CVE-2026-43966",
          "EEF-CVE-2026-43969"
        ]
      ],
      aliases: aliases()
    ]
  end

  defp aliases do
    [
      # Quick entry points for examples (see examples/README.md).
      # Individual .exs files do Mix.install and can be slow on first run.
      examples: [
        "run -e 'IO.puts(\"ExMCP Examples — see examples/README.md\") ; IO.puts(\"Quick starts: elixir examples/utilities/*.exs or examples/getting_started/demo_client.exs\") ; IO.puts(\"Full demo: cd examples/getting_started && ./run_demo.sh\") ; IO.puts(\"Fast alias: mix examples.getting_started\")'"
      ],
      # Fast (no re-Mix.install) version of the getting-started patterns.
      # See examples/getting_started/README.md and the main examples/README.md.
      "examples.getting_started": ["run -r examples/support/getting_started.exs"]
    ]
  end

  def cli do
    [
      preferred_envs: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        "coveralls.github": :test
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :crypto, :ssl, :inets],
      mod: {ExMCP.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:jason, "~> 1.4"},
      {:mint, "~> 1.6"},
      {:mint_web_socket, "~> 1.0"},
      {:castore, "~> 1.0"},
      {:telemetry, "~> 1.2"},
      {:ex_doc, "~> 0.40", only: :dev, runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:sobelow, "~> 0.13", only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.18", only: :test},
      {:git_hooks, "~> 0.7", only: [:dev], runtime: false},
      {:plug_cowboy, "~> 2.7"},
      {:plug, "~> 1.16"},
      {:fuse, "~> 2.4", optional: true},
      # MCP protocol support
      {:ex_json_schema, "~> 0.10"},
      {:html_entities, "~> 0.5", only: [:dev, :test]},
      {:propcheck, "~> 1.4", only: :test},
      {:benchee, "~> 1.0", only: [:dev, :test]},
      {:bypass, "~> 2.0", only: :test},
      {:jose, "~> 1.11"}
    ]
  end

  defp description do
    """
    Elixir implementation of MCP and ACP. Build MCP clients/servers with tools, resources, prompts over stdio, HTTP/SSE, and BEAM. Control coding agents via ACP with adapters for Claude Code, Codex, and more.
    """
  end

  defp package do
    [
      licenses: ["MIT"],
      links: %{
        "GitHub" => @github_url,
        "Changelog" => "#{@github_url}/blob/master/CHANGELOG.md",
        "MCP Spec" => "https://modelcontextprotocol.io",
        "ACP Spec" => "https://agentclientprotocol.com"
      },
      # NOTE: `dev/` (repo-only mix tasks + ExMCP.SpecSync) is intentionally
      # not listed, so it never ships to Hex.
      files: ~w(
          lib
          .formatter.exs
          mix.exs
          README.md
          LICENSE
          CHANGELOG.md
          docs/ACP_GUIDE.md
          docs/ARCHITECTURE.md
          docs/CONFIGURATION.md
          docs/DEVELOPMENT.md
          docs/DSL_GUIDE.md
          docs/SECURITY.md
          docs/TRANSPORT_GUIDE.md
          docs/TROUBLESHOOTING.md
          docs/getting-started
          docs/guides
        )
    ]
  end

  # Specifies which paths to compile per environment.
  #
  # `dev/` holds repo-only tooling (the `mix test.suite` / `mix mcp.sync_spec`
  # family and `ExMCP.SpecSync.*`). It is compiled for local development and
  # tests but is deliberately absent from `package.files`, so it never reaches
  # consumers' `mix help` (audit L1). `ExMCP.Testing.*` stays under `lib/` as a
  # documented, published test kit.
  defp elixirc_paths(:test),
    do: [
      "lib",
      "dev",
      "test/support",
      "test/ex_mcp/compliance",
      "test/ex_mcp/compliance/features",
      "test/ex_mcp/compliance/handlers"
    ]

  defp elixirc_paths(:dev), do: ["lib", "dev"]

  defp elixirc_paths(_), do: ["lib"]

  defp docs do
    [
      main: "readme",
      name: "ExMCP",
      canonical: "https://hexdocs.pm/ex_mcp",
      warnings_as_errors: true,
      skip_undefined_reference_warnings_on: ["CHANGELOG.md"],
      extras: [
        "README.md",
        "docs/guides/USER_GUIDE.md",
        "docs/guides/PHOENIX_GUIDE.md",
        "docs/DSL_GUIDE.md",
        "docs/TRANSPORT_GUIDE.md",
        "docs/CONFIGURATION.md",
        "docs/getting-started/MIGRATION.md",
        "docs/SECURITY.md",
        "docs/ARCHITECTURE.md",
        "docs/DEVELOPMENT.md",
        "docs/TROUBLESHOOTING.md",
        "docs/ACP_GUIDE.md",
        "CHANGELOG.md"
      ],
      extra_section: "GUIDES",
      source_ref: "v#{@version}",
      groups_for_extras: [
        Introduction: ~r/README/,
        Guides:
          ~r/USER_GUIDE|PHOENIX_GUIDE|DSL_GUIDE|TRANSPORT_GUIDE|ACP_GUIDE|CONFIGURATION|getting-started\/MIGRATION|SECURITY|ARCHITECTURE|DEVELOPMENT|TROUBLESHOOTING/,
        Changelog: ~r/CHANGELOG/
      ],
      groups_for_modules: [
        "MCP Core": [
          ExMCP,
          ExMCP.Client,
          ExMCP.Server,
          ExMCP.Server.Handler,
          ExMCP.Server.DSL,
          ExMCP.Server.DSL.Result,
          ExMCP.Server.MRTR.InputRequired,
          ExMCP.HttpPlug,
          ExMCP.Types,
          ExMCP.Content,
          ExMCP.Error,
          ExMCP.Response
        ],
        "MCP Transports": [
          ExMCP.Transport,
          ExMCP.Transport.Stdio,
          ExMCP.Transport.HTTP,
          ExMCP.Transport.SSEClient,
          ExMCP.Transport.Local
        ],
        Authorization: [
          ExMCP.Authorization
        ],
        "Agent Client Protocol (ACP)": [
          ExMCP.ACP,
          ExMCP.ACP.Agent,
          ExMCP.ACP.Agent.Handler,
          ExMCP.ACP.Client,
          ExMCP.ACP.Client.Handler,
          ExMCP.ACP.Client.DefaultHandler,
          ExMCP.ACP.Protocol,
          ExMCP.ACP.Types,
          ExMCP.ACP.Registry,
          ExMCP.ACP.Adapter,
          ExMCP.ACP.AdapterBridge,
          ExMCP.ACP.AdapterTransport,
          ExMCP.ACP.Adapters.ClaudeSDK,
          ExMCP.ACP.Adapters.ClaudeSDK.SessionStore,
          ExMCP.ACP.Adapters.Codex,
          ExMCP.ACP.Adapters.Pi
        ],
        "Deprecated (planned removal in 2.0)": [
          ExMCP.Server.Tools,
          ExMCP.Server.Tools.Simplified,
          ExMCP.Server.Tools.Builder,
          ExMCP.Server.Tools.Helpers,
          ExMCP.Server.Tools.Registry,
          ExMCP.Server.Tools.ResponseNormalizer,
          ExMCP.Server.Tools.ASTValidator
        ]
      ],
      filter_modules: fn mod, _ ->
        # Hide pure internals and repo-only tooling from the sidebar.
        # Deprecated Tools stay visible.
        name = inspect(mod)

        not String.starts_with?(name, "ExMCP.Internal.") and
          not String.starts_with?(name, "ExMCP.SpecSync.") and
          not String.starts_with?(name, "Mix.Tasks.") and
          not String.contains?(name, ".Test.")
      end,
      before_closing_body_tag: fn
        :html ->
          """
          <script>
            // Add copy button to code blocks
            document.addEventListener('DOMContentLoaded', function() {
              var blocks = document.querySelectorAll('pre code');
              blocks.forEach(function(block) {
                var button = document.createElement('button');
                button.className = 'copy-button';
                button.textContent = 'Copy';
                button.addEventListener('click', function() {
                  navigator.clipboard.writeText(block.textContent);
                  button.textContent = 'Copied!';
                  setTimeout(function() { button.textContent = 'Copy'; }, 2000);
                });
                block.parentNode.insertBefore(button, block);
              });
            });
          </script>
          <style>
            .copy-button {
              position: absolute;
              top: 5px;
              right: 5px;
              padding: 2px 8px;
              font-size: 12px;
              background: #f0f0f0;
              border: 1px solid #ccc;
              border-radius: 3px;
              cursor: pointer;
            }
            pre { position: relative; }
          </style>
          """

        _ ->
          ""
      end
    ]
  end
end
