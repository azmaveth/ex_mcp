#!/bin/bash

# Run the complete ExMCP getting started demo

echo "🚀 ExMCP Getting Started Demo"
echo "=============================="
echo ""
echo "This demo will show all 4 transport types:"
echo "  1. STDIO - Simple subprocess communication"
echo "  2. HTTP - Standard request/response" 
echo "  3. HTTP+SSE - With real-time streaming"
echo "  4. Native BEAM - Direct process communication"
echo ""
echo "Starting in 3 seconds..."
sleep 3

# Run the demo client which will start all servers
elixir demo_client.exs

echo ""
echo "Demo completed! 🎉"
echo ""
echo "To run individual servers, see the README.md"