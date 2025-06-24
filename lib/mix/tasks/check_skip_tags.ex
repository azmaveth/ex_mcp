defmodule Mix.Tasks.CheckSkipTags do
  @moduledoc """
  Checks test files for @tag :skip annotations.

  This task helps maintain test coverage by preventing tests from being
  permanently skipped, which can hide compliance issues and create false
  confidence in test coverage.

  ## Usage

      # Check all test files
      mix check_skip_tags

      # Check only staged files (for git hooks)
      mix check_skip_tags --staged

      # Check files changed in branch compared to main
      mix check_skip_tags --branch main

  ## Options

    * `--staged` - Check only files staged for commit
    * `--branch` - Check files changed compared to specified branch (default: main)
    * `--quiet` - Suppress output, only return exit code

  ## Exit codes

    * 0 - No @tag :skip found
    * 1 - Found @tag :skip in test files
  """

  use Mix.Task

  @shortdoc "Checks for @tag :skip in test files"

  @switches [
    staged: :boolean,
    branch: :string,
    quiet: :boolean
  ]

  @impl Mix.Task
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: @switches)

    script_path = Path.join([File.cwd!(), "scripts", "check_skip_tags.sh"])

    if not File.exists?(script_path) do
      Mix.raise("Skip tag check script not found at: #{script_path}")
    end

    cmd_args = build_command_args(opts)
    {output, exit_code} = System.cmd(script_path, cmd_args, stderr_to_stdout: true)

    unless opts[:quiet] do
      # Remove ANSI color codes for Mix output
      clean_output = remove_ansi_codes(output)
      Mix.shell().info(clean_output)
    end

    if exit_code != 0 do
      exit({:shutdown, exit_code})
    end
  end

  defp build_command_args(opts) do
    cond do
      opts[:staged] -> ["staged"]
      opts[:branch] -> ["branch", opts[:branch]]
      true -> ["all"]
    end
  end

  defp remove_ansi_codes(text) do
    # Remove ANSI escape sequences
    text
    |> String.replace(~r/\e\[[0-9;]*m/, "")
    |> String.trim()
  end
end
