# Ensure support files are compiled
Code.require_file("support/test_http_server.ex", __DIR__)
Code.require_file("support/test_server.ex", __DIR__)
Code.require_file("support/test_helpers.ex", __DIR__)

# Start required applications for HTTP tests
{:ok, _} = Application.ensure_all_started(:inets)
{:ok, _} = Application.ensure_all_started(:ssl)
{:ok, _} = Application.ensure_all_started(:ranch)
{:ok, _} = Application.ensure_all_started(:cowboy)

# Enable test mode for SSE handlers to prevent blocking in tests
Application.put_env(:ex_mcp, :test_mode, true)

# Clean up any stray test processes before running tests
if System.get_env("SKIP_TEST_CLEANUP") != "true" do
  Mix.Task.run("test.cleanup")
end

# Configure default exclusions for fast local development
# These can be overridden with --include flags
default_exclusions = [
  # Tests requiring external services
  integration: true,
  external: true,
  live_server: true,

  # Performance and slow tests
  slow: true,
  performance: true,
  stress: true,

  # Work in progress
  wip: true,
  skip: true,

  # Tests requiring specific setup
  requires_http: true,
  requires_stdio: true,
  requires_beam: true
]

# Print exclusion summary
excluded_tags =
  default_exclusions
  |> Enum.filter(fn {_tag, excluded} -> excluded end)
  |> Enum.map(fn {tag, _} -> tag end)

if length(excluded_tags) > 0 do
  IO.puts("\n⚠️  Test tags excluded by default: #{inspect(excluded_tags)}")
  IO.puts("   Use --include <tag> to run specific test categories\n")
end

ExUnit.configure(exclude: default_exclusions)
ExUnit.start()

# Define mocks
# Note: Transport mocks removed since v1 Transport module was deleted
