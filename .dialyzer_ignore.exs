[
  # Ignore warnings in test support files that depend on ExUnit
  {"lib/ex_mcp/testing/assertions.ex"},
  {"lib/ex_mcp/testing/mock_server.ex"},
  
  # Ignore false positive pattern match warnings
  {"lib/ex_mcp/transport/http_server.ex", :pattern_match_cov},
  {"lib/ex_mcp/reliability/supervisor.ex", :pattern_match}
]