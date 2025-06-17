#!/usr/bin/env python3
"""
Python MCP Calculator Server

A simple calculator server using the Python MCP SDK that ExMCP clients can connect to.
This demonstrates Python-side MCP server implementation that's compatible with ExMCP.

Usage:
    python calculator_server.py

The server will communicate via stdio (JSON-RPC over stdin/stdout).
"""

import asyncio
import json
import sys
from typing import Dict, Any, List
from mcp.server import Server
from mcp.server.stdio import stdio_server
from mcp.types import (
    Tool,
    TextContent,
    CallToolRequest,
    CallToolResult,
)


class CalculatorServer:
    """Python MCP Calculator Server compatible with ExMCP clients."""
    
    def __init__(self):
        self.server = Server("python-calculator")
        self.operation_count = 0
        self.history = []
        
        # Register tools
        self._register_tools()
    
    def _register_tools(self):
        """Register all calculator tools with the MCP server."""
        
        @self.server.list_tools()
        async def list_tools() -> List[Tool]:
            """List all available calculator tools."""
            return [
                Tool(
                    name="add",
                    description="Add two numbers",
                    inputSchema={
                        "type": "object",
                        "properties": {
                            "a": {"type": "number", "description": "First number"},
                            "b": {"type": "number", "description": "Second number"}
                        },
                        "required": ["a", "b"]
                    }
                ),
                Tool(
                    name="subtract",
                    description="Subtract two numbers",
                    inputSchema={
                        "type": "object",
                        "properties": {
                            "a": {"type": "number", "description": "First number"},
                            "b": {"type": "number", "description": "Second number"}
                        },
                        "required": ["a", "b"]
                    }
                ),
                Tool(
                    name="multiply",
                    description="Multiply two numbers",
                    inputSchema={
                        "type": "object",
                        "properties": {
                            "a": {"type": "number", "description": "First number"},
                            "b": {"type": "number", "description": "Second number"}
                        },
                        "required": ["a", "b"]
                    }
                ),
                Tool(
                    name="divide",
                    description="Divide two numbers",
                    inputSchema={
                        "type": "object",
                        "properties": {
                            "a": {"type": "number", "description": "Dividend"},
                            "b": {"type": "number", "description": "Divisor (cannot be zero)"}
                        },
                        "required": ["a", "b"]
                    }
                ),
                Tool(
                    name="power",
                    description="Raise a number to a power",
                    inputSchema={
                        "type": "object",
                        "properties": {
                            "base": {"type": "number", "description": "Base number"},
                            "exponent": {"type": "number", "description": "Exponent"}
                        },
                        "required": ["base", "exponent"]
                    }
                ),
                Tool(
                    name="get_history",
                    description="Get calculation history",
                    inputSchema={
                        "type": "object",
                        "properties": {
                            "limit": {"type": "integer", "description": "Maximum number of history items", "default": 10}
                        }
                    }
                ),
                Tool(
                    name="get_stats",
                    description="Get server statistics",
                    inputSchema={
                        "type": "object",
                        "properties": {}
                    }
                )
            ]
        
        @self.server.call_tool()
        async def call_tool(name: str, arguments: Dict[str, Any]) -> CallToolResult:
            """Handle tool calls from MCP clients."""
            
            try:
                if name == "add":
                    result = self._add(arguments["a"], arguments["b"])
                elif name == "subtract":
                    result = self._subtract(arguments["a"], arguments["b"])
                elif name == "multiply":
                    result = self._multiply(arguments["a"], arguments["b"])
                elif name == "divide":
                    result = self._divide(arguments["a"], arguments["b"])
                elif name == "power":
                    result = self._power(arguments["base"], arguments["exponent"])
                elif name == "get_history":
                    limit = arguments.get("limit", 10)
                    result = self._get_history(limit)
                elif name == "get_stats":
                    result = self._get_stats()
                else:
                    raise ValueError(f"Unknown tool: {name}")
                
                # Record operation in history (except for get_history and get_stats)
                if name not in ["get_history", "get_stats"]:
                    self.operation_count += 1
                    self.history.append({
                        "operation": name,
                        "arguments": arguments,
                        "result": result,
                        "timestamp": self._get_timestamp()
                    })
                    
                    # Keep only last 50 operations
                    if len(self.history) > 50:
                        self.history = self.history[-50:]
                
                return CallToolResult(
                    content=[
                        TextContent(
                            type="text",
                            text=f"Python Calculator: {result}"
                        )
                    ]
                )
                
            except Exception as e:
                return CallToolResult(
                    content=[
                        TextContent(
                            type="text", 
                            text=f"Error: {str(e)}"
                        )
                    ],
                    isError=True
                )
    
    def _add(self, a: float, b: float) -> str:
        """Add two numbers."""
        result = a + b
        return f"{a} + {b} = {result}"
    
    def _subtract(self, a: float, b: float) -> str:
        """Subtract two numbers."""
        result = a - b
        return f"{a} - {b} = {result}"
    
    def _multiply(self, a: float, b: float) -> str:
        """Multiply two numbers."""
        result = a * b
        return f"{a} × {b} = {result}"
    
    def _divide(self, a: float, b: float) -> str:
        """Divide two numbers."""
        if b == 0:
            raise ValueError("Division by zero is not allowed")
        result = a / b
        return f"{a} ÷ {b} = {result}"
    
    def _power(self, base: float, exponent: float) -> str:
        """Raise a number to a power."""
        result = base ** exponent
        return f"{base}^{exponent} = {result}"
    
    def _get_history(self, limit: int) -> str:
        """Get calculation history."""
        if not self.history:
            return "No calculations performed yet"
        
        recent_history = self.history[-limit:]
        history_lines = []
        
        for i, entry in enumerate(recent_history, 1):
            op = entry["operation"]
            args = entry["arguments"]
            result = entry["result"]
            timestamp = entry["timestamp"]
            
            history_lines.append(f"{i}. [{timestamp}] {op}({args}) → {result}")
        
        return "Calculation History:\n" + "\n".join(history_lines)
    
    def _get_stats(self) -> str:
        """Get server statistics."""
        stats = {
            "server_type": "Python MCP Calculator",
            "total_operations": self.operation_count,
            "history_entries": len(self.history),
            "available_operations": ["add", "subtract", "multiply", "divide", "power"],
            "python_version": sys.version.split()[0],
            "mcp_compatible": True
        }
        
        return f"Server Statistics:\n{json.dumps(stats, indent=2)}"
    
    def _get_timestamp(self) -> str:
        """Get current timestamp."""
        import datetime
        return datetime.datetime.now().strftime("%H:%M:%S")
    
    async def run(self):
        """Run the MCP server using stdio transport."""
        async with stdio_server() as streams:
            await self.server.run(
                streams[0], streams[1], 
                self.server.create_initialization_options()
            )


def main():
    """Main entry point for the Python MCP Calculator Server."""
    calculator = CalculatorServer()
    
    # Add some debug logging to stderr so it doesn't interfere with stdio transport
    import logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[logging.StreamHandler(sys.stderr)]
    )
    
    logger = logging.getLogger(__name__)
    logger.info("Starting Python MCP Calculator Server...")
    logger.info("Server will communicate via stdio (JSON-RPC)")
    logger.info("Connect with ExMCP client using transport: :stdio")
    
    try:
        asyncio.run(calculator.run())
    except KeyboardInterrupt:
        logger.info("Server shutting down...")
    except Exception as e:
        logger.error(f"Server error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()