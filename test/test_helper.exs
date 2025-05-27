# Ensure support files are compiled
Code.require_file("support/mock_sse_server.ex", __DIR__)

ExUnit.start()

# Define mocks
Mox.defmock(ExMCP.Transport.Mock, for: ExMCP.Transport)
