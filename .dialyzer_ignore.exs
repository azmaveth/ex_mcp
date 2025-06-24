[
  # Ignore warnings in test support files that depend on ExUnit
  {"lib/ex_mcp/testing/assertions.ex"},
  {"lib/ex_mcp/testing/mock_server.ex"},
  
  # Ignore testing module type resolution issues
  {"lib/ex_mcp/testing/mock_server.ex", :invalid_contract},
  {"lib/ex_mcp/testing/mock_server.ex", :no_return},
  {"lib/ex_mcp/testing/mock_server.ex", :call},
  {"lib/ex_mcp/testing/assertions.ex", :invalid_contract}
]