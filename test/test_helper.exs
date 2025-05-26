ExUnit.start()

# Define mocks
Mox.defmock(ExMCP.Transport.Mock, for: ExMCP.Transport)
