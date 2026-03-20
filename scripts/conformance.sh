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
#   CONFORMANCE_PORT          — Server port (default: 3099)
#   CONFORMANCE_TIMEOUT       — Client timeout in ms (default: 120000)
#
# Results saved to: tmp/conformance_output.txt

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
CONFORMANCE="npx @modelcontextprotocol/conformance"
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
  echo "=== Client Conformance Tests${version:+ (spec $version)} ==="
  echo ""

  local args="client --command 'elixir $CLIENT_SCRIPT' --timeout $TIMEOUT --verbose"
  if [ -n "$scenario" ]; then
    args="$args --scenario $scenario"
  else
    args="$args --suite core"
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

    # Client tests for this version
    run_client_tests "" "$version" || true
  done

  echo ""
  echo "========================================" | tee -a "$OUTPUT_FILE"
  echo "All-versions conformance run complete." | tee -a "$OUTPUT_FILE"
  echo "========================================" | tee -a "$OUTPUT_FILE"
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
  *)
    echo "Usage: $0 [server|client|all|all-versions] [scenario]"
    echo ""
    echo "Modes:"
    echo "  server        Run server conformance tests"
    echo "  client        Run client conformance tests"
    echo "  all           Run both (default)"
    echo "  all-versions  Test all protocol versions (2024-11-05 through 2025-11-25)"
    echo ""
    echo "Environment:"
    echo "  CONFORMANCE_SPEC_VERSION=2025-06-18  Test a specific version"
    exit 1
    ;;
esac

echo ""
echo "--- Results saved to $OUTPUT_FILE ---"
