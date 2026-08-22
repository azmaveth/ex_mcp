defmodule Mix.Tasks.Acp.Compat.Check do
  @moduledoc """
  Checks the reviewed ACP ecosystem manifest for upstream drift.

      mix acp.compat.check
      mix acp.compat.check --offline
      mix acp.compat.check --json
      mix acp.compat.check --manifest path/to/manifest.json

  The task never installs agents or mutates the manifest. A non-zero exit means
  the catalog, registry, or an adapter reference repository needs review.
  """

  use Mix.Task

  @shortdoc "Check ACP catalog, registry, and adapter upstream drift"
  @switches [offline: :boolean, json: :boolean, manifest: :string]

  @impl Mix.Task
  def run(args) do
    {opts, _rest, invalid} = OptionParser.parse(args, strict: @switches)

    if invalid != [], do: Mix.raise("Invalid options: #{inspect(invalid)}")

    manifest_path = Path.expand(opts[:manifest] || ExMCP.ACPCompat.default_manifest())

    manifest =
      case ExMCP.ACPCompat.load_manifest(manifest_path) do
        {:ok, manifest} -> manifest
        {:error, reason} -> Mix.raise("Invalid ACP compatibility manifest: #{inspect(reason)}")
      end

    if opts[:offline] do
      print_offline(manifest, manifest_path, opts)
    else
      report =
        ExMCP.ACPCompat.check_remote(manifest,
          manifest_path: manifest_path,
          github_token: System.get_env("GITHUB_TOKEN")
        )

      print_report(report, opts)

      if report.drift != [] or report.errors != [] do
        Mix.raise("ACP compatibility drift detected")
      end
    end
  end

  defp print_offline(manifest, path, opts) do
    summary = %{
      "manifest" => path,
      "catalogAgents" => length(manifest["catalog"]["agents"]),
      "registryAgents" => length(manifest["registry"]["agents"]),
      "adapterUpstreams" => length(manifest["adapterUpstreams"]),
      "interopAgents" => length(manifest["interopAgents"])
    }

    if opts[:json] do
      Mix.shell().info(Jason.encode!(summary, pretty: true))
    else
      Mix.shell().info(
        "ACP compatibility manifest is valid: " <>
          "#{summary["catalogAgents"]} catalog agents, " <>
          "#{summary["registryAgents"]} registry agents, " <>
          "#{summary["adapterUpstreams"]} adapter upstreams, " <>
          "#{summary["interopAgents"]} executable smoke entries"
      )
    end
  end

  defp print_report(report, opts) do
    if opts[:json] do
      Mix.shell().info(Jason.encode!(report, pretty: true))
    else
      Mix.shell().info(ExMCP.ACPCompat.format_report(report))
    end
  end
end
