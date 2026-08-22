defmodule ExMCP.ACP.RequestValidationTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.RequestValidation

  @session_id "session-1"

  test "accepts each ACP v1 session update discriminator with its required shape" do
    updates = [
      %{
        "sessionUpdate" => "user_message_chunk",
        "messageId" => nil,
        "content" => %{"type" => "text", "text" => "hello", "annotations" => %{}}
      },
      %{
        "sessionUpdate" => "agent_message_chunk",
        "content" => %{"type" => "image", "data" => "AA==", "mimeType" => "image/png"}
      },
      %{
        "sessionUpdate" => "agent_thought_chunk",
        "content" => %{"type" => "text", "text" => "thinking"}
      },
      %{
        "sessionUpdate" => "tool_call",
        "toolCallId" => "tool-1",
        "title" => "Read file",
        "kind" => "read",
        "status" => "in_progress",
        "content" => [
          %{"type" => "content", "content" => %{"type" => "text", "text" => "started"}}
        ]
      },
      %{
        "sessionUpdate" => "tool_call_update",
        "toolCallId" => "tool-1",
        "status" => "completed",
        "locations" => [%{"path" => "/tmp/file", "line" => 1}]
      },
      %{
        "sessionUpdate" => "plan",
        "entries" => [%{"content" => "Audit", "priority" => "high", "status" => "pending"}]
      },
      %{
        "sessionUpdate" => "plan_update",
        "plan" => %{"type" => "markdown", "planId" => "plan-1", "content" => "# Plan"}
      },
      %{"sessionUpdate" => "plan_removed", "planId" => "plan-1"},
      %{
        "sessionUpdate" => "available_commands_update",
        "availableCommands" => [
          %{"name" => "audit", "description" => "Audit code", "input" => %{"hint" => "path"}}
        ]
      },
      %{
        "sessionUpdate" => "current_mode_update",
        "currentModeId" => "auto"
      },
      %{
        "sessionUpdate" => "config_option_update",
        "configOptions" => [
          %{
            "id" => "mode",
            "name" => "Mode",
            "type" => "select",
            "currentValue" => "safe",
            "options" => [%{"value" => "safe", "name" => "Safe"}]
          },
          %{
            "id" => "verbose",
            "name" => "Verbose",
            "type" => "boolean",
            "currentValue" => false
          }
        ]
      },
      %{"sessionUpdate" => "session_info_update", "title" => nil},
      %{
        "sessionUpdate" => "usage_update",
        "used" => 10,
        "size" => 100,
        "cost" => %{"amount" => 0.01, "currency" => "USD"}
      }
    ]

    for update <- updates do
      assert :ok =
               RequestValidation.validate_session_update(%{
                 "sessionId" => @session_id,
                 "update" => update
               })
    end
  end

  test "rejects unknown discriminators and malformed variant fields" do
    invalid_updates = [
      %{"sessionUpdate" => "unknown"},
      %{"sessionUpdate" => "agent_message_chunk", "content" => %{"type" => "text"}},
      %{"sessionUpdate" => "tool_call", "toolCallId" => "tool-1"},
      %{"sessionUpdate" => "tool_call_update", "toolCallId" => "", "status" => "done"},
      %{"sessionUpdate" => "plan", "entries" => [%{"content" => "missing fields"}]},
      %{"sessionUpdate" => "usage_update", "used" => -1, "size" => 100},
      %{"sessionUpdate" => "session_info_update", "_meta" => "not-an-object"}
    ]

    for update <- invalid_updates do
      assert {:error, :invalid_params} =
               RequestValidation.validate_session_update(%{
                 "sessionId" => @session_id,
                 "update" => update
               })
    end

    assert {:error, :invalid_params} = RequestValidation.validate_session_update(%{})
  end

  test "validates form and URL elicitation scopes and responses" do
    form = %{
      "mode" => "form",
      "sessionId" => @session_id,
      "toolCallId" => "tool-1",
      "message" => "Choose a color",
      "requestedSchema" => %{
        "type" => "object",
        "properties" => %{
          "color" => %{
            "type" => "string",
            "oneOf" => [%{"const" => "blue", "title" => "Blue"}]
          }
        }
      }
    }

    assert :ok = RequestValidation.validate_agent_request("elicitation/create", form)

    url = %{
      "mode" => "url",
      "requestId" => 9,
      "message" => "Authorize",
      "url" => "https://example.com/oauth",
      "elicitationId" => "oauth-1"
    }

    assert :ok = RequestValidation.validate_agent_request("elicitation/create", url)

    assert {:error, :invalid_params} =
             RequestValidation.validate_agent_request(
               "elicitation/create",
               Map.put(url, "url", "https://user:password@example.com/oauth")
             )

    assert {:error, :invalid_params} =
             RequestValidation.validate_agent_request(
               "elicitation/create",
               Map.put(form, "requestId", 9)
             )

    password_form =
      put_in(form, ["requestedSchema", "properties", "color"], %{
        "type" => "string",
        "format" => "password"
      })

    assert {:error, :invalid_params} =
             RequestValidation.validate_agent_request("elicitation/create", password_form)

    codex_secret_form =
      put_in(form, ["requestedSchema", "properties", "color"], %{
        "type" => "string",
        "_meta" => %{"codex" => %{"isSecret" => true}}
      })

    assert {:error, :invalid_params} =
             RequestValidation.validate_agent_request("elicitation/create", codex_secret_form)

    assert :ok =
             RequestValidation.validate_elicitation_response(%{
               "action" => "accept",
               "content" => %{"color" => "blue"}
             })

    assert {:error, :invalid_params} =
             RequestValidation.validate_elicitation_response(%{
               "action" => "accept",
               "content" => "not an object"
             })
  end
end
