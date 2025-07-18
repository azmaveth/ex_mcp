defmodule ExMCP.MixProject do
  use Mix.Project

  @version "0.6.0"
  @github_url "https://github.com/azmaveth/ex_mcp"

  def project do
    [
      app: :ex_mcp,
      version: @version,
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package(),
      docs: docs(),
      source_url: @github_url,
      homepage_url: @github_url,
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        "coveralls.github": :test
      ],
      dialyzer: [
        plt_add_apps: [:mix, :ex_unit],
        ignore_warnings: ".dialyzer_ignore.exs",
        list_unused_filters: false,
        plt_local_path: "priv/plts",
        plt_core_path: "priv/plts"
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
      {:horde, "~> 0.8"},
      {:telemetry, "~> 1.2"},
      {:ex_doc, "~> 0.36", only: :dev, runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:mox, "~> 1.2", only: :test},
      {:sobelow, "~> 0.13", only: [:dev, :test], runtime: false},
      {:excoveralls, "~> 0.18", only: :test},
      {:git_hooks, "~> 0.7", only: [:dev], runtime: false},
      {:plug_cowboy, "~> 2.7"},
      {:plug, "~> 1.16"},
      {:fuse, "~> 2.4", optional: true},
      # ExMCP v2 dependencies
      {:ex_json_schema, "~> 0.10"},
      {:html_entities, "~> 0.5", only: [:dev, :test]},
      {:propcheck, "~> 1.4", only: :test},
      {:benchee, "~> 1.0", only: [:dev, :test]},
      {:bypass, "~> 2.0", only: :test},
      {:gen_state_machine, "~> 3.0"}
    ]
  end

  defp description do
    """
    Complete Elixir implementation of the Model Context Protocol (MCP) v2025-03-26.
    Build MCP clients and servers with support for tools, resources, prompts, sampling,
    roots, and subscriptions. Includes both spec-compliant features and Elixir-specific
    extensions like BEAM transport, auto-reconnection, and server discovery.
    """
  end

  defp package do
    [
      licenses: ["MIT"],
      links: %{
        "GitHub" => @github_url,
        "Changelog" => "#{@github_url}/blob/master/CHANGELOG.md",
        "MCP Spec" => "https://modelcontextprotocol.io"
      },
      files:
        ~w(lib .formatter.exs mix.exs README.md LICENSE CHANGELOG.md USER_GUIDE.md EXTENSIONS.md)
    ]
  end

  # Specifies which paths to compile per environment
  defp elixirc_paths(:test),
    do: [
      "lib",
      "test/support",
      "test/ex_mcp/compliance",
      "test/ex_mcp/compliance/features",
      "test/ex_mcp/compliance/handlers"
    ]

  defp elixirc_paths(_), do: ["lib"]

  defp docs do
    [
      main: "readme",
      name: "ExMCP",
      canonical: "https://hexdocs.pm/ex_mcp",
      extras: [
        "README.md",
        "docs/PHOENIX_GUIDE.md",
        "USER_GUIDE.md",
        "API_REFERENCE.md",
        "EXTENSIONS.md",
        "CHANGELOG.md"
      ],
      extra_section: "GUIDES",
      source_ref: "v#{@version}",
      groups_for_extras: [
        Introduction: ~r/README/,
        Guides: ~r/PHOENIX_GUIDE|USER_GUIDE|API_REFERENCE|EXTENSIONS/,
        Changelog: ~r/CHANGELOG/
      ],
      groups_for_modules: [
        "MCP Specification": [
          ExMCP.Protocol,
          ExMCP.Types,
          ExMCP.Server,
          ExMCP.Server.Handler,
          ExMCP.Transport.Stdio,
          ExMCP.Transport.SSE
        ],
        "MCP + Extensions": [ExMCP.Client],
        "ExMCP Extensions": [
          ExMCP.Transport.Beam,
          ExMCP.ServerManager,
          ExMCP.Discovery
        ],
        Internal: [ExMCP.Transport, ExMCP.Application]
      ],
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
