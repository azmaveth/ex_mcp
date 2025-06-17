#!/usr/bin/env python3
"""
Python MCP Client connecting to ExMCP (Elixir) Server

This demonstrates how Python applications can use Elixir MCP servers
for high-performance data processing and concurrent operations.

Prerequisites:
    pip install mcp

Usage:
    python elixir_client.py

Make sure the Elixir server is available either by:
1. Running: iex -S mix, then ExMCP.Examples.Interop.ElixirServerForPython.run()
2. Or using the CLI command shown in the script
"""

import asyncio
import json
import time
import sys
from typing import List, Dict, Any
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client


class ElixirMCPClient:
    """Python client for connecting to ExMCP (Elixir) servers."""
    
    def __init__(self):
        self.session = None
        
    async def connect(self):
        """Connect to the Elixir MCP data processing server."""
        
        # Command to start Elixir MCP server
        server_params = StdioServerParameters(
            command=[
                "elixir", 
                "-e", 
                "Application.ensure_all_started(:ex_mcp); "
                "ExMCP.Examples.Interop.ElixirServerForPython.start_cli_server()"
            ]
        )
        
        print("🚀 Starting Elixir MCP server...")
        
        try:
            self.client_context = stdio_client(server_params)
            read, write = await self.client_context.__aenter__()
            
            self.session_context = ClientSession(read, write)
            self.session = await self.session_context.__aenter__()
            
            # Initialize the connection
            await self.session.initialize()
            print("✅ Connected to Elixir MCP server!")
            
            return True
            
        except Exception as e:
            print(f"❌ Failed to connect to Elixir server: {e}")
            print("Make sure Elixir and ExMCP are installed:")
            print("  1. Install Elixir: https://elixir-lang.org/install.html")
            print("  2. Clone ExMCP: git clone <repo>")
            print("  3. Run: mix deps.get")
            return False
    
    async def disconnect(self):
        """Disconnect from the Elixir server."""
        if hasattr(self, 'session_context'):
            await self.session_context.__aexit__(None, None, None)
        if hasattr(self, 'client_context'):
            await self.client_context.__aexit__(None, None, None)
        print("🔌 Disconnected from Elixir server")
    
    async def list_capabilities(self):
        """List all capabilities of the Elixir server."""
        print("\n=== Elixir Server Capabilities ===")
        
        # List tools
        tools_response = await self.session.list_tools()
        print(f"📋 Available tools: {len(tools_response.tools)}")
        for tool in tools_response.tools:
            print(f"  • {tool.name}: {tool.description}")
            
            # Show input schema details
            if hasattr(tool, 'inputSchema') and tool.inputSchema:
                required = tool.inputSchema.get('required', [])
                properties = tool.inputSchema.get('properties', {})
                if properties:
                    params = [f"{k}{'*' if k in required else ''}" for k in properties.keys()]
                    print(f"    Parameters: {', '.join(params)}")
        
        # List resources
        try:
            resources_response = await self.session.list_resources()
            print(f"📁 Available resources: {len(resources_response.resources)}")
            for resource in resources_response.resources:
                print(f"  • {resource.name}: {resource.description}")
        except Exception:
            print("📁 Resources: Not available or empty")
        
        # List prompts
        try:
            prompts_response = await self.session.list_prompts()
            print(f"💬 Available prompts: {len(prompts_response.prompts)}")
            for prompt in prompts_response.prompts:
                print(f"  • {prompt.name}: {prompt.description}")
        except Exception:
            print("💬 Prompts: Not available or empty")
    
    async def demo_basic_operations(self):
        """Demonstrate basic data processing operations."""
        print("\n=== Basic Data Processing Demo ===")
        
        # Test data processing
        sample_data = [1, 2, 3, 4, 5, 10, 15, 20, 25, 30]
        
        print(f"Processing data: {sample_data}")
        start_time = time.time()
        
        result = await self.session.call_tool(
            "process_data",
            {
                "numbers": sample_data,
                "operations": ["sum", "mean", "median", "std_dev", "min", "max"]
            }
        )
        
        elapsed = (time.time() - start_time) * 1000  # Convert to milliseconds
        print(f"⚡ Processing completed in {elapsed:.2f}ms")
        print("📊 Results:")
        print(result.content[0].text)
    
    async def demo_dataset_management(self):
        """Demonstrate dataset creation and management."""
        print("\n=== Dataset Management Demo ===")
        
        # Create multiple datasets
        datasets = [
            {
                "name": "sales_data",
                "data": [100, 150, 200, 175, 300, 250, 400, 350, 500, 450],
                "metadata": {
                    "description": "Monthly sales figures",
                    "source": "sales_department",
                    "created_by": "python_client"
                }
            },
            {
                "name": "temperature_data", 
                "data": [20.5, 22.1, 19.8, 25.3, 21.7, 23.9, 18.4, 26.1],
                "metadata": {
                    "description": "Daily temperature readings",
                    "source": "weather_station",
                    "created_by": "python_client"
                }
            }
        ]
        
        print("Creating datasets...")
        for dataset in datasets:
            result = await self.session.call_tool("create_dataset", dataset)
            print(f"  ✓ {result.content[0].text}")
        
        # List all datasets
        print("\nListing all datasets:")
        result = await self.session.call_tool("list_datasets", {})
        print(result.content[0].text)
        
        # Compare datasets
        print("\nComparing datasets:")
        result = await self.session.call_tool(
            "compare_datasets", 
            {"dataset1": "sales_data", "dataset2": "temperature_data"}
        )
        print(result.content[0].text)
    
    async def demo_performance_test(self):
        """Demonstrate the performance of Elixir server."""
        print("\n=== Performance Test ===")
        
        # Test concurrent operations
        print("Testing concurrent data processing...")
        
        # Create multiple tasks
        tasks = []
        operation_count = 10
        
        start_time = time.time()
        
        for i in range(operation_count):
            # Generate different data for each operation
            data = [j * (i + 1) for j in range(1, 11)]
            
            task = self.session.call_tool(
                "process_data",
                {
                    "numbers": data,
                    "operations": ["sum", "mean", "std_dev"]
                }
            )
            tasks.append(task)
        
        # Wait for all operations to complete
        results = await asyncio.gather(*tasks)
        
        total_time = (time.time() - start_time) * 1000
        avg_time = total_time / operation_count
        
        print(f"🚀 Completed {operation_count} concurrent operations")
        print(f"⚡ Total time: {total_time:.2f}ms")
        print(f"⚡ Average per operation: {avg_time:.2f}ms")
        print(f"🔥 Elixir's Actor Model enables efficient concurrent processing!")
        
        # Show a few results
        print("\nSample results:")
        for i, result in enumerate(results[:3]):
            print(f"  Operation {i+1}: {result.content[0].text.split('\\n')[0]}")
    
    async def demo_server_stats(self):
        """Get and display Elixir server statistics."""
        print("\n=== Elixir Server Statistics ===")
        
        result = await self.session.call_tool("get_server_stats", {})
        stats_text = result.content[0].text
        
        print(stats_text)
        
        # Parse and highlight key metrics
        try:
            # Extract JSON from the text
            json_start = stats_text.find('{')
            if json_start != -1:
                stats_json = json.loads(stats_text[json_start:])
                
                print("\n🔍 Key Performance Metrics:")
                print(f"  • Server Type: {stats_json.get('server_type', 'Unknown')}")
                print(f"  • Uptime: {stats_json.get('uptime_seconds', 0)} seconds")
                print(f"  • Operations Processed: {stats_json.get('processing_operations', 0)}")
                print(f"  • Elixir Version: {stats_json.get('elixir_version', 'Unknown')}")
                
                memory = stats_json.get('memory_usage', {})
                if memory:
                    total_mb = memory.get('total', 0) / (1024 * 1024)
                    print(f"  • Memory Usage: {total_mb:.1f} MB")
                
                capabilities = stats_json.get('capabilities', [])
                if capabilities:
                    print(f"  • Capabilities: {', '.join(capabilities)}")
                    
        except Exception as e:
            print(f"Could not parse detailed stats: {e}")
    
    async def demo_resources_and_prompts(self):
        """Demonstrate resource and prompt capabilities."""
        print("\n=== Resources and Prompts Demo ===")
        
        # First, make sure we have some datasets to use as resources
        await self.session.call_tool(
            "create_dataset", 
            {
                "name": "resource_demo",
                "data": [1, 2, 3, 4, 5],
                "metadata": {"description": "Demo dataset for resource testing"}
            }
        )
        
        # List and read resources
        try:
            resources_response = await self.session.list_resources()
            print(f"📁 Available resources: {len(resources_response.resources)}")
            
            for resource in resources_response.resources:
                print(f"  • {resource.name} ({resource.uri})")
                
                # Read the first resource
                if resources_response.resources:
                    first_resource = resources_response.resources[0]
                    print(f"\nReading resource: {first_resource.name}")
                    
                    resource_content = await self.session.read_resource(first_resource.uri)
                    print("Resource content (first 200 chars):")
                    content_text = resource_content.contents[0].text
                    print(f"  {content_text[:200]}{'...' if len(content_text) > 200 else ''}")
                    
        except Exception as e:
            print(f"Resource demo failed: {e}")
        
        # Test prompts
        try:
            prompts_response = await self.session.list_prompts()
            print(f"\n💬 Available prompts: {len(prompts_response.prompts)}")
            
            if prompts_response.prompts:
                first_prompt = prompts_response.prompts[0]
                print(f"Using prompt: {first_prompt.name}")
                
                prompt_result = await self.session.get_prompt(
                    first_prompt.name,
                    {"dataset_name": "resource_demo", "analysis_type": "comprehensive"}
                )
                
                print("Generated prompt:")
                for message in prompt_result.messages:
                    print(f"  Role: {message.role}")
                    print(f"  Content: {message.content.text[:200]}...")
                    
        except Exception as e:
            print(f"Prompt demo failed: {e}")


async def main():
    """Main demo function."""
    print("🐍 Python MCP Client → 🧪 Elixir MCP Server Demo")
    print("=" * 60)
    
    client = ElixirMCPClient()
    
    try:
        # Connect to Elixir server
        if not await client.connect():
            return
        
        # Run all demos
        await client.list_capabilities()
        await client.demo_basic_operations()
        await client.demo_dataset_management()
        await client.demo_performance_test()
        await client.demo_server_stats()
        await client.demo_resources_and_prompts()
        
        print("\n✨ Demo completed successfully!")
        print("\n🔍 Key Benefits of Python → Elixir MCP Integration:")
        print("  ✓ Leverage Elixir's Actor Model for concurrent processing")
        print("  ✓ Fault-tolerant architecture with supervisor trees")
        print("  ✓ High-performance numerical computations")
        print("  ✓ Seamless JSON-RPC communication")
        print("  ✓ Rich MCP capabilities (tools, resources, prompts)")
        print("  ✓ Cross-language interoperability")
        
    except Exception as e:
        print(f"❌ Demo failed: {e}")
        import traceback
        traceback.print_exc()
        
    finally:
        await client.disconnect()


if __name__ == "__main__":
    asyncio.run(main())