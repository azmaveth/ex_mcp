#!/usr/bin/env bash
# Run MCP conformance tests against ExMCP.
# Uses the official modelcontextprotocol/conformance framework.
#
# Usage:
#   ./scripts/conformance.sh                    # Run both client and server tests
#   ./scripts/conformance.sh server             # Server tests only
#   ./scripts/conformance.sh client             # Client tests only
#   ./scripts/conformance.sh server <scenario>  # Single server scenario
#   ./scripts/conformance.sh client <scenario>  # Single client scenario
#
# Prerequisites:
#   npm (for npx @modelcontextprotocol/conformance)
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

mkdir -p "$PROJECT_DIR/tmp"

cd "$PROJECT_DIR"

run_server_tests() {
  local scenario="${1:-}"
  echo "=== Server Conformance Tests ==="
  echo ""

  # Start the conformance server
  echo "Starting ExMCP server on port $SERVER_PORT..."
  elixir "$SERVER_SCRIPT" "$SERVER_PORT" > "$PROJECT_DIR/tmp/conformance_server.log" 2>&1 &
  SERVER_PID=$!

  # Wait for server to be ready
  echo "Waiting for server..."
  for i in $(seq 1 30); do
    if curl -s "http://localhost:$SERVER_PORT/mcp" > /dev/null 2>&1; then
      echo "Server ready."
      break
    fi
    sleep 1
  done

  # Run conformance tests
  local args="server --url http://localhost:$SERVER_PORT/mcp --verbose"
  if [ -n "$scenario" ]; then
    args="$args --scenario $scenario"
  else
    args="$args --suite active"
  fi
  if [ -f "$BASELINE_FILE" ]; then
    args="$args --expected-failures $BASELINE_FILE"
  fi

  echo "Running: $CONFORMANCE $args"
  echo ""
  $CONFORMANCE $args 2>&1 | tee -a "$OUTPUT_FILE"
  local exit_code=${PIPESTATUS[0]}

  # Cleanup
  kill $SERVER_PID 2>/dev/null || true
  wait $SERVER_PID 2>/dev/null || true

  return $exit_code
}

run_client_tests() {
  local scenario="${1:-}"
  echo "=== Client Conformance Tests ==="
  echo ""

  local args="client --command 'elixir $CLIENT_SCRIPT' --timeout $TIMEOUT --verbose"
  if [ -n "$scenario" ]; then
    args="$args --scenario $scenario"
  else
    args="$args --suite core"
  fi
  if [ -f "$BASELINE_FILE" ]; then
    args="$args --expected-failures $BASELINE_FILE"
  fi

  echo "Running: $CONFORMANCE $args"
  echo ""
  eval $CONFORMANCE $args 2>&1 | tee -a "$OUTPUT_FILE"
  return ${PIPESTATUS[0]}
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
  *)
    echo "Usage: $0 [server|client|all] [scenario]"
    exit 1
    ;;
esac

echo ""
echo "--- Results saved to $OUTPUT_FILE ---"
