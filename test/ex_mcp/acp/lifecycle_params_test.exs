defmodule ExMCP.ACP.LifecycleParamsTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.LifecycleParams

  describe "pipeline" do
    test "normalizes lifecycle params with mcp servers and additional directories" do
      opts = [
        mcp_servers: [%{"type" => "stdio", "name" => "tools"}],
        additional_directories: ["/tmp/work"]
      ]

      params =
        %{"cwd" => "/tmp/project"}
        |> LifecycleParams.normalize(opts)

      assert params["mcpServers"] == [%{"type" => "stdio", "name" => "tools"}]
      assert params["additionalDirectories"] == ["/tmp/work"]
    end
  end

  describe "validate/2" do
    test "rejects additional directories unless capability is advertised" do
      assert {:error, {:unsupported_capability, :additional_directories}} =
               LifecycleParams.validate([additional_directories: ["/tmp/work"]], %{})
    end

    test "accepts absolute additional directories when advertised" do
      caps = %{"sessionCapabilities" => %{"additionalDirectories" => %{}}}

      assert :ok = LifecycleParams.validate([additional_directories: ["/tmp/work"]], caps)
    end

    test "rejects relative additional directories" do
      caps = %{"sessionCapabilities" => %{"additionalDirectories" => %{}}}

      assert {:error, {:invalid_params, :additional_directories_must_be_absolute_paths}} =
               LifecycleParams.validate([additional_directories: ["relative"]], caps)
    end

    test "accepts official MCP server descriptors without ExMCP metadata" do
      opts = [mcp_servers: [%{"type" => "http", "name" => "docs", "url" => "http://localhost"}]]

      assert :ok = LifecycleParams.validate(opts, %{})
    end

    test "rejects removed native MCP descriptors" do
      opts = [mcp_servers: [%{"type" => "native", "name" => "local", "service" => :docs}]]

      assert {:error, {:invalid_params, :native_mcp_removed}} =
               LifecycleParams.validate(opts, %{})
    end

    test "accepts BEAM MCP descriptors when ExMCP BEAM support is advertised" do
      opts = [mcp_servers: [%{"type" => "beam", "name" => "local", "service" => :docs}]]

      caps = %{
        "mcpCapabilities" => %{
          "_meta" => %{"ex_mcp.mcpCapabilities" => %{"beam" => true}}
        }
      }

      assert :ok = LifecycleParams.validate(opts, caps)
    end
  end
end
