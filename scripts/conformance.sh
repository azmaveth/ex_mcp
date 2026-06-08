#!/usr/bin/env bash
# Run MCP conformance tests against ExMCP.
# Uses the official modelcontextprotocol/conformance framework.
#
# Usage:
#   ./scripts/conformance.sh                    # Run both client and server (latest version)
#   ./scripts/conformance.sh server             # Server tests only
#   ./scripts/conformance.sh client             # Client tests only
#   ./scripts/conformance.sh server <scenario>  # Single server scenario
#   ./scripts/conformance.sh client <scenario>  # Single client scenario
#   ./scripts/conformance.sh all-versions       # Test ALL protocol versions
#
# Environment variables:
#   CONFORMANCE_SPEC_VERSION  — Test a specific version (e.g., 2025-06-18)
#   CONFORMANCE_PACKAGE_VERSION — Stable conformance package version (default: 0.1.16)
#   CONFORMANCE_ALPHA_VERSION   — Alpha conformance package version (default: 0.2.0-alpha.2)
#   CONFORMANCE_PORT          — Server port (default: 3099)
#   CONFORMANCE_TIMEOUT       — Client timeout in ms (default: 120000)
#
# Results saved to: tmp/conformance_output.txt

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
CONFORMANCE_PACKAGE_VERSION="${CONFORMANCE_PACKAGE_VERSION:-0.1.16}"
CONFORMANCE="npx @modelcontextprotocol/conformance@$CONFORMANCE_PACKAGE_VERSION"
SERVER_PORT="${CONFORMANCE_PORT:-3099}"
SERVER_SCRIPT="$PROJECT_DIR/test/conformance/server.exs"
CLIENT_SCRIPT="$PROJECT_DIR/test/conformance/client.exs"
OUTPUT_FILE="$PROJECT_DIR/tmp/conformance_output.txt"
BASELINE_FILE="$PROJECT_DIR/test/conformance/expected-failures.yml"
TIMEOUT="${CONFORMANCE_TIMEOUT:-120000}"
SPEC_VERSION="${CONFORMANCE_SPEC_VERSION:-}"

mkdir -p "$PROJECT_DIR/tmp"

cd "$PROJECT_DIR"

start_server() {
  echo "Starting ExMCP server on port $SERVER_PORT..."
  elixir "$SERVER_SCRIPT" "$SERVER_PORT" > "$PROJECT_DIR/tmp/conformance_server.log" 2>&1 &
  SERVER_PID=$!

  echo "Waiting for server..."
  for i in $(seq 1 30); do
    if curl -s "http://localhost:$SERVER_PORT/mcp" > /dev/null 2>&1; then
      echo "Server ready."
      return 0
    fi
    sleep 1
  done
  echo "Server failed to start!"
  return 1
}

stop_server() {
  kill $SERVER_PID 2>/dev/null || true
  wait $SERVER_PID 2>/dev/null || true
}

run_server_tests() {
  local scenario="${1:-}"
  local version="${2:-$SPEC_VERSION}"
  echo "=== Server Conformance Tests${version:+ (spec $version)} ==="
  echo ""

  start_server || return 1

  local args="server --url http://localhost:$SERVER_PORT/mcp --verbose"
  if [ -n "$scenario" ]; then
    args="$args --scenario $scenario"
  else
    args="$args --suite active"
  fi
  if [ -n "$version" ]; then
    args="$args --spec-version $version"
  fi
  if [ -f "$BASELINE_FILE" ]; then
    args="$args --expected-failures $BASELINE_FILE"
  fi

  echo "Running: $CONFORMANCE $args"
  echo ""
  $CONFORMANCE $args 2>&1 | tee -a "$OUTPUT_FILE"
  local exit_code=${PIPESTATUS[0]}

  stop_server
  return $exit_code
}

run_client_tests() {
  local scenario="${1:-}"
  local version="${2:-$SPEC_VERSION}"
  local suite="${3:-core}"
  local target="suite $suite"
  if [ -n "$scenario" ]; then
    target="scenario $scenario"
  fi
  echo "=== Client Conformance Tests${version:+ (spec $version)} ($target) ==="
  echo ""

  local args="client --command 'elixir $CLIENT_SCRIPT' --timeout $TIMEOUT --verbose"
  if [ -n "$scenario" ]; then
    args="$args --scenario $scenario"
  else
    args="$args --suite $suite"
  fi
  if [ -n "$version" ]; then
    args="$args --spec-version $version"
  fi
  if [ -f "$BASELINE_FILE" ]; then
    args="$args --expected-failures $BASELINE_FILE"
  fi

  echo "Running: $CONFORMANCE $args"
  echo ""
  eval $CONFORMANCE $args 2>&1 | tee -a "$OUTPUT_FILE"
  return ${PIPESTATUS[0]}
}

run_all_versions() {
  # All spec versions the conformance framework supports (newest first).
  # 2024-11-05 is not a valid spec-version in the framework.
  # We also test draft and extension scenarios.
  local versions=("2025-11-25" "2025-06-18" "2025-03-26" "draft" "extension")
  local total_passed=0
  local total_failed=0

  echo "========================================" | tee -a "$OUTPUT_FILE"
  echo "Running conformance for ALL protocol versions" | tee -a "$OUTPUT_FILE"
  echo "========================================" | tee -a "$OUTPUT_FILE"
  echo ""

  for version in "${versions[@]}"; do
    echo "" | tee -a "$OUTPUT_FILE"
    echo "╔══════════════════════════════════════╗" | tee -a "$OUTPUT_FILE"
    echo "║  Protocol Version: $version        ║" | tee -a "$OUTPUT_FILE"
    echo "╚══════════════════════════════════════╝" | tee -a "$OUTPUT_FILE"
    echo "" | tee -a "$OUTPUT_FILE"

    # Server tests for this version
    run_server_tests "" "$version" || true

    # Client tests for this version.
    run_client_tests "" "$version" || true

    # The official suite has no core scenarios for 2025-03-26, but it does
    # include auth backcompat scenarios for that version. The auth suite
    # selector currently returns zero, so run the scenarios by name.
    if [ "$version" = "2025-03-26" ]; then
      run_client_tests "auth/2025-03-26-oauth-metadata-backcompat" "$version" || true
      run_client_tests "auth/2025-03-26-oauth-endpoint-fallback" "$version" || true
    fi
  done

  echo ""
  echo "========================================" | tee -a "$OUTPUT_FILE"
  echo "All-versions conformance run complete." | tee -a "$OUTPUT_FILE"
  echo "========================================" | tee -a "$OUTPUT_FILE"
}

run_draft_alpha() {
  CONFORMANCE_PACKAGE_VERSION="${CONFORMANCE_ALPHA_VERSION:-0.2.0-alpha.2}"
  CONFORMANCE="npx @modelcontextprotocol/conformance@$CONFORMANCE_PACKAGE_VERSION"
  SPEC_VERSION="${CONFORMANCE_SPEC_VERSION:-draft}"

  echo "========================================" | tee -a "$OUTPUT_FILE"
  echo "Running non-gating draft conformance with $CONFORMANCE" | tee -a "$OUTPUT_FILE"
  echo "========================================" | tee -a "$OUTPUT_FILE"

  run_server_tests "" "$SPEC_VERSION" || true
  run_client_tests "" "$SPEC_VERSION" || true

  echo "Draft alpha conformance run complete (non-gating)." | tee -a "$OUTPUT_FILE"
}

# Clear output file
> "$OUTPUT_FILE"
echo "MCP Conformance Test Run — $(date)" >> "$OUTPUT_FILE"
echo "========================================" >> "$OUTPUT_FILE"

MODE="${1:-all}"
SCENARIO="${2:-}"

case "$MODE" in
  server)
    run_server_tests "$SCENARIO"
    ;;
  client)
    run_client_tests "$SCENARIO"
    ;;
  all)
    echo "Running server tests..." >> "$OUTPUT_FILE"
    run_server_tests "$SCENARIO" || true
    echo "" >> "$OUTPUT_FILE"
    echo "Running client tests..." >> "$OUTPUT_FILE"
    run_client_tests "$SCENARIO" || true
    ;;
  all-versions)
    run_all_versions
    ;;
  draft-alpha)
    run_draft_alpha
    ;;
  *)
    echo "Usage: $0 [server|client|all|all-versions|draft-alpha] [scenario]"
    echo ""
    echo "Modes:"
    echo "  server        Run server conformance tests"
    echo "  client        Run client conformance tests"
    echo "  all           Run both (default)"
    echo "  all-versions  Test conformance-supported versions through 2025-11-25"
    echo "  draft-alpha   Non-gating draft run using conformance 0.2.0-alpha.2"
    echo ""
    echo "Environment:"
    echo "  CONFORMANCE_SPEC_VERSION=2025-06-18  Test a specific version"
    echo "  CONFORMANCE_PACKAGE_VERSION=0.1.16   Pin stable conformance package"
    echo "  CONFORMANCE_ALPHA_VERSION=0.2.0-alpha.2  Override draft-alpha package"
    exit 1
    ;;
esac

echo ""
echo "--- Results saved to $OUTPUT_FILE ---"
