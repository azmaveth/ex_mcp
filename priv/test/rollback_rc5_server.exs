Application.put_env(:ex_mcp, :stdio_mode, true)
Application.put_env(:logger, :level, :emergency)
:logger.set_primary_config(:level, :emergency)
{:ok, _applications} = Application.ensure_all_started(:ex_mcp)

defmodule ExMCP.Test.RollbackRC5Server do
  use ExMCP.Server.Handler
  use ExMCP.Server.DSL, name: "rc5-rollback-target", version: "1.0.0-rc.5"

  tool "reconcile", "Read the state reconciled before rollback" do
    param(:completed, :string, required: true)

    run(fn %{completed: completed}, state ->
      {:ok, "rc5-reconciled=#{completed}", state}
    end)
  end
end

{:ok, _server} = ExMCP.Test.RollbackRC5Server.start_link(transport: :stdio)
Process.sleep(:infinity)
