[
  # Ignore warnings in test support files that depend on ExUnit
  {"lib/ex_mcp/testing/assertions.ex"},
  {"lib/ex_mcp/testing/mock_server.ex"},
  
  # Ignore testing module type resolution issues
  {"lib/ex_mcp/testing/mock_server.ex", :no_return},
  {"lib/ex_mcp/testing/mock_server.ex", :call},
  {"lib/ex_mcp/testing/assertions.ex", :invalid_contract},
  
  # Ignore macro-related warnings in tools_refactored.ex
  {"lib/ex_mcp/server/tools_refactored.ex", :no_return},
  {"lib/ex_mcp/server/tools_refactored.ex", :pattern_match},
  {"lib/ex_mcp/server/tools_refactored.ex", :call},
  
  # Ignore @spec issue in builder.ex - Dialyzer and Credo have conflicting requirements
  {"lib/ex_mcp/server/tools/builder.ex", :invalid_contract},
  
  # Ignore pattern match warnings for deprecated batch support
  # Batch support is deprecated but still needed for backward compatibility
  {"lib/ex_mcp/client/request_handler.ex", :pattern_match},
  
  # Ignore Transport.Error contract warnings in stdio.ex - these functions are used correctly
  # but Dialyzer expects different return patterns in some call contexts
  {"lib/ex_mcp/transport/stdio.ex", :call},

  # Ignore unreachable pattern warning in SecurityGuard - this is a defensive pattern
  # for robustness against malformed consent handlers
  {"lib/ex_mcp/transport/security_guard.ex", :pattern_match_cov},

  # NOTE: Test files are not analyzed in dev environment
  # These entries may be needed when running dialyzer in test environment (CI)
]