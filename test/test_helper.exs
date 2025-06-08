# Ensure support files are compiled
Code.require_file("support/mock_sse_server.ex", __DIR__)
Code.require_file("support/test_http_server.ex", __DIR__)

# Start required applications for HTTP tests
{:ok, _} = Application.ensure_all_started(:inets)
{:ok, _} = Application.ensure_all_started(:ssl)

ExUnit.start()

# Define mocks
Mox.defmock(ExMCP.Transport.Mock, for: ExMCP.Transport)
