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

  # Test environment specific warnings - these files are only analyzed when MIX_ENV=test
  
  # Test support files with intentional pattern mismatches for error testing
  {"test/support/error_test_server.ex", :pattern_match},
  {"test/support/refactored_test_server.ex", :pattern_match},
  {"test/support/test_helpers.ex", :pattern_match},
  
  # Test support files with intentional unreachable clauses for comprehensive error handling
  {"test/support/error_test_server.ex", :pattern_match_cov},
  {"test/support/refactored_test_server.ex", :pattern_match_cov},
  {"test/support/test_helpers.ex", :pattern_match_cov},
  
  # Test support callback type mismatches - these are intentional for testing edge cases
  {"test/support/consent_handler/test.ex", :callback_type_mismatch},
  {"test/support/refactored_test_server.ex", :callback_type_mismatch},
  
  # Test functions with no return - these are generated test functions that don't need returns
  {"test/ex_mcp/compliance/batch_test.ex", :no_return},
  {"test/ex_mcp/compliance/completion_test.ex", :no_return},
  {"test/ex_mcp/compliance/version_generator.ex", :no_return},
  
  # Test support files - guard clause and call warnings from test helper patterns
  {"test/support/test_helpers.ex", :guard_fail},
  {"test/support/test_helpers.ex", :call},
  {"test/ex_mcp/compliance/batch_test.ex", :guard_fail},
  {"test/ex_mcp/compliance/completion_test.ex", :guard_fail},
  
  # Test pattern match warnings that are false positives
  {"test/ex_mcp/compliance/batch_test.ex", :pattern_match},
  {"test/ex_mcp/compliance/completion_test.ex", :pattern_match},
  
  # Compliance test feature files - these contain generated test functions and intentional mismatches
  {"test/ex_mcp/compliance/features/batch.ex", :no_return},
  {"test/ex_mcp/compliance/features/batch.ex", :call},
  {"test/ex_mcp/compliance/features/completion.ex", :no_return},
  {"test/ex_mcp/compliance/features/completion.ex", :call},
  {"test/ex_mcp/compliance/features/cancellation.ex", :guard_fail},
  {"test/ex_mcp/compliance/features/roots.ex", :guard_fail},
  {"test/ex_mcp/compliance/features/transport.ex", :pattern_match},
  
  # Compliance handler files with intentional callback mismatches for testing different protocol versions
  {"test/ex_mcp/compliance/handlers/handler20241105.ex", :callback_type_mismatch},
  {"test/ex_mcp/compliance/handlers/handler20250326.ex", :callback_type_mismatch},
  {"test/ex_mcp/compliance/handlers/handler20250618.ex", :callback_type_mismatch}
]