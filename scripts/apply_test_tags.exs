#!/usr/bin/env elixir

# Script to apply tags to test files based on patterns

defmodule TestTagger do
  @moduledoc """
  Applies tags to test files based on patterns in filenames and content.
  """

  # Tag rules based on file patterns
  def file_rules do
    [
    # Integration tests
    {~r/_integration_test\.exs$/, [:integration]},
    {~r/integration.*_test\.exs$/, [:integration]},
    
    # Transport tests
    {~r/transport.*_test\.exs$/, [:transport]},
    {~r/stdio.*_test\.exs$/, [:transport, :stdio, :requires_stdio]},
    {~r/sse.*_test\.exs$/, [:transport, :sse, :requires_http]},
    {~r/beam.*_test\.exs$/, [:transport, :beam, :requires_beam]},
    {~r/http.*_test\.exs$/, [:transport, :requires_http]},
    
    # Feature tests
    {~r/batch.*_test\.exs$/, [:batch]},
    {~r/progress.*_test\.exs$/, [:progress]},
    {~r/cancellation.*_test\.exs$/, [:cancellation]},
    {~r/roots.*_test\.exs$/, [:roots]},
    {~r/resources.*_test\.exs$/, [:resources]},
    {~r/tools.*_test\.exs$/, [:tools]},
    {~r/prompts.*_test\.exs$/, [:prompts]},
    {~r/logging.*_test\.exs$/, [:logging]},
    {~r/completion.*_test\.exs$/, [:completion]},
    {~r/security.*_test\.exs$/, [:security]},
    {~r/protocol.*_test\.exs$/, [:protocol]},
    
    # Performance tests
    {~r/performance.*_test\.exs$/, [:performance, :slow]},
    {~r/stress.*_test\.exs$/, [:stress, :slow]},
    {~r/comprehensive.*_test\.exs$/, [:slow]}
    ]
  end

  # Content rules - check for patterns in file content
  def content_rules do
    [
    # Integration indicators
    {~r/Server\.start_link.*Client\.start_link/s, [:integration]},
    {~r/MockSSEServer|TestHTTPServer/, [:integration, :requires_http]},
    {~r/Process\.sleep\(\d{4,}\)/, [:slow]},
    {~r/Enum\.each.*1\.\.1000/, [:performance, :slow]},
    {~r/@tag\s+:capture_log/, [:capture_log]},
    {~r/timeout:\s*:infinity/, [:slow]}
    ]
  end

  def run do
    test_files = Path.wildcard("test/**/*_test.exs")
    
    results = Enum.map(test_files, fn file ->
      tags = analyze_file(file)
      {file, tags}
    end)
    
    # Group by suggested tags
    by_tags = Enum.group_by(results, fn {_file, tags} -> tags end)
    
    # Print results
    IO.puts("\n## Test Tagging Analysis\n")
    
    for {tags, files} <- Enum.sort(by_tags) do
      if tags != [] do
        IO.puts("\n### Files that should have tags: #{inspect(tags)}")
        for {file, _} <- files do
          IO.puts("  - #{file}")
        end
      end
    end
    
    # Generate tagging commands
    IO.puts("\n## Suggested Tagging Commands\n")
    
    for {file, tags} <- results, tags != [] do
      relative_path = Path.relative_to_cwd(file)
      _tag_string = tags |> Enum.map(&inspect/1) |> Enum.join(", ")
      
      # Check if it already has @moduletag
      content = File.read!(file)
      if String.contains?(content, "@moduletag") do
        IO.puts("# #{relative_path} already has @moduletag, review manually")
      else
        IO.puts("# Add to #{relative_path}:")
        for tag <- tags do
          IO.puts("  @moduletag #{inspect(tag)}")
        end
      end
    end
  end
  
  defp analyze_file(file) do
    content = File.read!(file)
    
    # Skip compliance tests - they already have proper tags
    if String.contains?(file, "/compliance/") do
      []
    else
      # Apply file rules
      file_tags = file_rules()
        |> Enum.filter(fn {pattern, _} -> Regex.match?(pattern, file) end)
        |> Enum.flat_map(fn {_, tags} -> tags end)
      
      # Apply content rules
      content_tags = content_rules()
        |> Enum.filter(fn {pattern, _} -> Regex.match?(pattern, content) end)
        |> Enum.flat_map(fn {_, tags} -> tags end)
      
      # Combine and deduplicate
      (file_tags ++ content_tags)
      |> Enum.uniq()
      |> Enum.sort()
    end
  end
end

TestTagger.run()