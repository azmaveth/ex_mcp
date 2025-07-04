# Support files are compiled via elixirc_paths(:test) in mix.exs
# No need to explicitly require them here as they're already available

# Configure logger before starting applications
Logger.configure(level: :warning)

# Import test helpers after compilation (in ExUnit.start callback)
ExUnit.after_suite(fn _results ->
  # Safety net: cleanup any truly orphaned test processes
  ExMCP.TestSupport.cleanup_orphans()
  :ok
end)

# Start required applications for HTTP tests
{:ok, _} = Application.ensure_all_started(:inets)
{:ok, _} = Application.ensure_all_started(:ssl)
{:ok, _} = Application.ensure_all_started(:ranch)
{:ok, _} = Application.ensure_all_started(:cowboy)

# Start test consent handler agent
{:ok, _} = ExMCP.ConsentHandler.Test.start_link()

# Start ValidatorRegistry for content validation tests
{:ok, _} = ExMCP.Content.ValidatorRegistry.start_link(name: ExMCP.Content.ValidatorRegistry)

# Ensure compliance version modules are generated
Code.ensure_loaded(ExMCP.Compliance.VersionGenerator)

# Enable test mode for SSE handlers to prevent blocking in tests
Application.put_env(:ex_mcp, :test_mode, true)

# Don't stop the application - let tests that need it have access to it
# Individual tests can stop/restart if needed for isolation
# Application.stop(:ex_mcp)

# Ensure the application is started for tests that need it
{:ok, _} = Application.ensure_all_started(:ex_mcp)

# Safe cleanup: Only handle network resources that might block new tests
# Application processes are handled by OTP supervision - don't force kill them
ExMCP.TestSupport.safe_cleanup_network_resources()

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
  requires_beam: true,
  requires_bypass: true
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
