defmodule Mix.Tasks.Acp.GenShims do
  @moduledoc """
  Generates the deprecated `ExMCP.ACP.*` forwarding modules from `ex_acp`.

      mix acp.gen_shims            # rewrite lib/ex_mcp/acp.ex and lib/ex_mcp/acp/
      mix acp.gen_shims --check    # exit non-zero if the shims are stale (CI)
      mix acp.gen_shims --out DIR  # write the tree under DIR instead

  The generator owns `lib/ex_mcp/acp.ex` and everything under
  `lib/ex_mcp/acp/`: files it did not generate are deleted. Requires `:ex_acp`
  as a dependency. See `ExMCP.ACPShimGenerator`.
  """

  use Mix.Task

  alias ExMCP.ACPShimGenerator

  @shortdoc "Generate deprecated ExMCP.ACP.* shims that forward to ex_acp"
  @switches [check: :boolean, out: :string]

  @impl Mix.Task
  def run(args) do
    {opts, _rest, invalid} = OptionParser.parse(args, strict: @switches)
    if invalid != [], do: Mix.raise("Invalid options: #{inspect(invalid)}")

    Mix.Task.run("compile")

    root = Path.expand(opts[:out] || "lib/ex_mcp")
    generated = ACPShimGenerator.generate()
    existing = existing_files(root)

    if opts[:check] do
      check(root, generated, existing)
    else
      write(root, generated, existing)
    end
  end

  defp existing_files(root) do
    [Path.join(root, "acp.ex") | Path.wildcard(Path.join(root, "acp/**/*.ex"))]
    |> Enum.filter(&File.regular?/1)
    |> Enum.map(&Path.relative_to(&1, root))
  end

  defp check(root, generated, existing) do
    stale =
      for {path, source} <- generated,
          File.read(Path.join(root, path)) != {:ok, source},
          do: {:changed, path}

    extra = for path <- existing, not Map.has_key?(generated, path), do: {:extra, path}

    case stale ++ extra do
      [] ->
        Mix.shell().info("ACP shims are up to date (#{map_size(generated)} modules)")

      problems ->
        Enum.each(problems, fn {kind, path} -> Mix.shell().error("  #{kind}: #{path}") end)
        Mix.raise("ACP shims are stale; run `mix acp.gen_shims`")
    end
  end

  defp write(root, generated, existing) do
    for path <- existing, not Map.has_key?(generated, path) do
      File.rm!(Path.join(root, path))
    end

    for {path, source} <- generated do
      full = Path.join(root, path)
      File.mkdir_p!(Path.dirname(full))
      File.write!(full, source)
    end

    # Drop directories emptied by removed modules.
    root
    |> Path.join("acp/**")
    |> Path.wildcard()
    |> Enum.filter(&File.dir?/1)
    |> Enum.sort_by(&String.length/1, :desc)
    |> Enum.each(&File.rmdir/1)

    Mix.shell().info("Wrote #{map_size(generated)} ACP shims under #{Path.relative_to_cwd(root)}")
  end
end
