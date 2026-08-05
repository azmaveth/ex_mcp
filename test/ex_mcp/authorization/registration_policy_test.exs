defmodule ExMCP.Authorization.RegistrationPolicyTest do
  use ExUnit.Case, async: false

  alias ExMCP.Authorization.RegistrationPolicy

  @cimd_url "https://client.example/oauth/metadata.json"

  test "pre-registered credentials have highest priority and resolve secret references" do
    System.put_env("EX_MCP_TEST_CLIENT_SECRET", "secret")
    on_exit(fn -> System.delete_env("EX_MCP_TEST_CLIENT_SECRET") end)

    config = %{
      client_registration: {:pre_registered, "client-1", {:env, "EX_MCP_TEST_CLIENT_SECRET"}},
      client_metadata_url: @cimd_url,
      application_type: :native,
      redirect_port: 8_080
    }

    assert {:ok, {:pre_registered, selected}} =
             RegistrationPolicy.select(cimd_and_dcr_metadata(), config)

    assert selected.client_id == "client-1"
    assert selected.client_secret == "secret"
    assert selected.registration_method == :pre_registered

    assert {:error, :client_secret_resolver_failed} =
             RegistrationPolicy.select(%{}, %{
               client_registration: {:pre_registered, "client-1", fn -> raise "secret" end}
             })
  end

  test "selects only a configured CIMD URL when the server advertises support" do
    assert {:ok, {:cimd, selected}} =
             RegistrationPolicy.select(cimd_and_dcr_metadata(), %{
               client_registration: {:cimd, @cimd_url},
               redirect_port: 8_080
             })

    assert selected.client_id == @cimd_url

    assert {:error, :cimd_not_supported} =
             RegistrationPolicy.select(%{}, %{
               client_registration: {:cimd, @cimd_url},
               redirect_port: 8_080
             })

    assert {:error, :https_client_id_required} =
             RegistrationPolicy.select(cimd_and_dcr_metadata(), %{
               client_registration: {:cimd, "http://client.example/metadata.json"},
               redirect_port: 8_080
             })
  end

  test "auto falls back to DCR only when advertised and explicitly configured" do
    assert {:ok, {:dynamic, selected}} =
             RegistrationPolicy.select(cimd_and_dcr_metadata(), %{
               client_registration: :auto,
               application_type: :native,
               redirect_port: 8_080
             })

    assert selected.application_type == :native

    assert {:error, :application_type_required} =
             RegistrationPolicy.select(cimd_and_dcr_metadata(), %{
               client_registration: :auto,
               redirect_port: 8_080
             })

    assert {:error, :redirect_port_required} =
             RegistrationPolicy.select(cimd_and_dcr_metadata(), %{
               client_registration: :auto,
               application_type: :native
             })
  end

  test "auto never invents a CIMD URL" do
    assert {:error, {:client_registration_required, :configure_cimd_url}} =
             RegistrationPolicy.select(
               %{"client_id_metadata_document_supported" => true},
               %{client_registration: :auto, application_type: :native, redirect_port: 8_080}
             )
  end

  test "legacy client keys remain compatibility aliases" do
    assert {:ok, {:pre_registered, selected}} =
             RegistrationPolicy.select(cimd_and_dcr_metadata(), %{
               client_id: "legacy-client",
               client_secret: "legacy-secret"
             })

    assert selected.client_id == "legacy-client"
    assert selected.client_secret == "legacy-secret"

    assert {:ok, {:pre_registered, selected}} =
             RegistrationPolicy.select(cimd_and_dcr_metadata(), %{
               client_registration: :auto,
               client_id: "legacy-client",
               client_secret: "legacy-secret",
               client_metadata_url: @cimd_url,
               application_type: :native,
               redirect_port: 8_080
             })

    assert selected.client_id == "legacy-client"

    assert {:ok, {:cimd, selected}} =
             RegistrationPolicy.select(cimd_and_dcr_metadata(), %{
               client_metadata_url: @cimd_url,
               redirect_port: 8_080
             })

    assert selected.client_id == @cimd_url

    assert {:ok, {:dynamic, selected}} =
             RegistrationPolicy.select(
               %{"registration_endpoint" => "https://auth.example/register"},
               %{
                 client_metadata_url: @cimd_url,
                 application_type: :native,
                 redirect_port: 8_080
               }
             )

    assert selected.registration_method == :dynamic
  end

  defp cimd_and_dcr_metadata do
    %{
      "client_id_metadata_document_supported" => true,
      "registration_endpoint" => "https://auth.example/register"
    }
  end
end
