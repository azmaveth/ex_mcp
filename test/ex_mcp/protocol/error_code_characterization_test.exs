defmodule ExMCP.Protocol.ErrorCodeCharacterizationTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Types, as: ACPTypes
  alias ExMCP.Protocol.ErrorCodes
  alias ExMCP.Transport.SecurityError

  test "public error constructors retain their numeric codes" do
    error_codes = %{
      parse_error: ErrorCodes.parse_error(),
      invalid_request: ErrorCodes.invalid_request(),
      method_not_found: ErrorCodes.method_not_found(),
      invalid_params: ErrorCodes.invalid_params(),
      internal_error: ErrorCodes.internal_error(),
      request_cancelled: ErrorCodes.request_cancelled(),
      # Invoke dynamically because this characterization deliberately covers a deprecated API.
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      consent_required: apply(ErrorCodes, :consent_required, []),
      consent_denied: ErrorCodes.consent_denied(),
      server_error: ErrorCodes.server_error(),
      # credo:disable-for-next-line Credo.Check.Refactor.Apply
      resource_not_found: apply(ErrorCodes, :resource_not_found, []),
      url_elicitation_required: ErrorCodes.url_elicitation_required()
    }

    errors = %{
      parse_error: ExMCP.Error.parse_error().code,
      invalid_request: ExMCP.Error.invalid_request("details").code,
      method_not_found: ExMCP.Error.method_not_found("method").code,
      invalid_params: ExMCP.Error.invalid_params("details").code,
      internal_error: ExMCP.Error.internal_error("details").code,
      prompt_error: ExMCP.Error.prompt_error("details", "prompt").code
    }

    security_errors =
      Map.new(
        [
          :token_passthrough_blocked,
          :consent_required,
          :consent_denied,
          :consent_error,
          :security_violation,
          :unknown
        ],
        fn type ->
          error = SecurityError.new(type, "message")
          {type, SecurityError.format_for_transport(error, :stdio).code}
        end
      )

    assert error_codes == %{
             parse_error: -32700,
             invalid_request: -32600,
             method_not_found: -32601,
             invalid_params: -32602,
             internal_error: -32603,
             request_cancelled: -32001,
             consent_required: -32002,
             consent_denied: -32003,
             server_error: -32000,
             resource_not_found: -32002,
             url_elicitation_required: -32042
           }

    assert errors == %{
             parse_error: -32700,
             invalid_request: -32600,
             method_not_found: -32601,
             invalid_params: -32602,
             internal_error: -32603,
             prompt_error: -32002
           }

    assert security_errors == %{
             token_passthrough_blocked: -32001,
             consent_required: -32002,
             consent_denied: -32003,
             consent_error: -32004,
             security_violation: -32000,
             unknown: -32603
           }

    assert %{
             auth_required: ACPTypes.auth_required_code(),
             resource_not_found: ACPTypes.resource_not_found_code(),
             request_cancelled: ACPTypes.request_cancelled_code()
           } == %{
             auth_required: -32000,
             resource_not_found: -32002,
             request_cancelled: -32800
           }
  end

  test "the legacy -32002 collision remains explicit" do
    assert ErrorCodes.error_message(:resource_not_found) == "Consent required"
    assert ExMCP.Error.category(ExMCP.Error.prompt_error("details", "prompt")) == "Prompt Error"
  end

  test "future protocol errors are additive and unused by legacy constructors" do
    assert ErrorCodes.header_mismatch() == -32020
    assert ErrorCodes.missing_required_client_capability() == -32021
    assert ErrorCodes.unsupported_protocol_version() == -32022
  end
end
