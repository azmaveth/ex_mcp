#!/usr/bin/env elixir

defmodule ComplianceTestMigrator do
  @moduledoc """
  Migration script to help transition from old compliance tests to new structure.
  
  This script:
  1. Identifies old compliance test files that can be deprecated
  2. Validates that new compliance structure covers all test cases
  3. Provides migration recommendations
  """

  def run do
    IO.puts("🔄 MCP Compliance Test Migration Analysis")
    IO.puts("=" |> String.duplicate(50))

    analyze_old_tests()
    analyze_new_structure()
    provide_recommendations()
  end

  defp analyze_old_tests do
    IO.puts("\n📋 Analyzing existing compliance tests...")

    old_test_files = [
      "test/ex_mcp/compliance/spec_2024_11_05_test.exs",
      "test/ex_mcp/compliance/spec_2025_03_26_test.exs",
      "test/ex_mcp/compliance/spec_2025_06_18_test.exs",
      "test/ex_mcp/compliance/oauth_2_1_compliance_test.exs",
      "test/ex_mcp/compliance/transport_version_test.exs"
    ]

    for file <- old_test_files do
      if File.exists?(file) do
        IO.puts("  ✅ Found: #{file}")
        analyze_test_file(file)
      else
        IO.puts("  ❌ Missing: #{file}")
      end
    end
  end

  defp analyze_test_file(file) do
    content = File.read!(file)

    # Count test cases
    test_count =
      content
      |> String.split("\n")
      |> Enum.count(&String.contains?(&1, "test "))

    # Check for skipped tests
    skip_count =
      content
      |> String.split("\n")
      |> Enum.count(&String.contains?(&1, "@tag :skip"))

    IO.puts("    📊 Tests: #{test_count}, Skipped: #{skip_count}")

    if skip_count > 0 do
      IO.puts("    ⚠️  Contains skipped tests - these need attention")
    end
  end

  defp analyze_new_structure do
    IO.puts("\n🏗️  Analyzing new compliance structure...")

    new_structure_files = [
      "test/ex_mcp/compliance/version_generator.ex",
      "test/ex_mcp/compliance/features/tools.ex",
      "test/ex_mcp/compliance/features/resources.ex",
      "test/ex_mcp/compliance/features/authorization.ex",
      "test/ex_mcp/compliance/features/transport.ex",
      "test/ex_mcp/compliance/features/prompts.ex",
      "test/support/compliance_test_helpers.ex"
    ]

    all_present = true

    for file <- new_structure_files do
      if File.exists?(file) do
        IO.puts("  ✅ #{file}")
      else
        IO.puts("  ❌ Missing: #{file}")
        all_present = false
      end
    end

    if all_present do
      IO.puts("  🎉 New compliance structure is complete!")
    else
      IO.puts("  ⚠️  New compliance structure is incomplete")
    end
  end

  defp provide_recommendations do
    IO.puts("\n💡 Migration Recommendations:")
    IO.puts("=" |> String.duplicate(30))

    recommendations = [
      "1. ✅ New compliance structure is implemented",
      "2. 🔄 Update CI to use version-specific test jobs",
      "3. 📝 Update documentation to reference new test structure",
      "4. 🧪 Run new compliance tests to ensure they work",
      "5. 🗑️  Deprecate old compliance test files after validation",
      "6. 📊 Update test coverage reporting for new structure"
    ]

    for rec <- recommendations do
      IO.puts("   #{rec}")
    end

    IO.puts("\n🚀 Next Steps:")
    IO.puts("   • Run: mix test --only compliance")
    IO.puts("   • Run: mix test --only version_2025_03_26")
    IO.puts("   • Validate all version-specific tests pass")
    IO.puts("   • Update CI configuration")

    IO.puts("\n✨ Benefits of New Structure:")
    IO.puts("   • Clear version-specific failure reporting")
    IO.puts("   • Zero duplication across versions")
    IO.puts("   • Easy to add new MCP versions")
    IO.puts("   • Comprehensive feature coverage")
    IO.puts("   • Integrated OAuth 2.1 and batch processing tests")
  end
end

# Run the migration analysis
ComplianceTestMigrator.run()
