defmodule ExMCP.MixProject do
  use Mix.Project

  @version "0.1.0"
  @github_url "https://github.com/azmaveth/ex_mcp"

  def project do
    [
      app: :ex_mcp,
      version: @version,
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package(),
      docs: docs(),
      source_url: @github_url,
      homepage_url: @github_url
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
      {:ex_doc, "~> 0.36", only: :dev, runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:mox, "~> 1.2", only: :test}
    ]
  end

  defp description do
    """
    A comprehensive Elixir implementation of the Model Context Protocol (MCP).
    Build MCP clients and servers with support for tools, resources, prompts,
    and multiple transport layers (stdio, SSE, WebSocket).
    """
  end

  defp package do
    [
      licenses: ["MIT"],
      links: %{"GitHub" => @github_url},
      files: ~w(lib .formatter.exs mix.exs README.md LICENSE CHANGELOG.md)
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md", "CHANGELOG.md"],
      source_ref: "v#{@version}",
      groups_for_modules: [
        "Core Protocol": [ExMCP.Protocol, ExMCP.Types],
        "Client": [ExMCP.Client, ExMCP.Client.StdioTransport, ExMCP.Client.SSETransport],
        "Server": [ExMCP.Server, ExMCP.Server.Handler, ExMCP.Server.StdioTransport, ExMCP.Server.SSETransport],
        "Management": [ExMCP.ServerManager, ExMCP.Discovery],
        "Utilities": [ExMCP.Transport, ExMCP.ServerPersistence]
      ]
    ]
  end
end