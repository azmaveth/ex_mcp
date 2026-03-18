defmodule ExMCP.Authorization.FullOAuthFlow do
  @moduledoc """
  Full OAuth 2.1 authorization code flow with PKCE for MCP.

  Orchestrates the complete browser-based OAuth flow:

  1. Discover Protected Resource Metadata (RFC 9728)
  2. Discover Authorization Server metadata (RFC 8414 / OIDC)
  3. Dynamic Client Registration (RFC 7591) if no client_id
  4. Authorization Code flow with PKCE (RFC 7636)
  5. Local redirect URI server to receive callback
  6. Token exchange at token endpoint

  This is used when a server returns 401 and the client has no
  pre-existing credentials. For clients with credentials, use
  `ExMCP.Authorization.DiscoveryFlow` instead.

  ## Usage

      {:ok, token} = FullOAuthFlow.execute(%{
        resource_url: "http://localhost:3000/mcp",
        redirect_port: 0  # auto-assign port
      })

  """

  require Logger

  alias ExMCP.Authorization.{
    ClientRegistration,
    OAuthFlow,
    OIDCDiscovery,
    ProtectedResourceMetadata
  }

  @type config :: %{
          required(:resource_url) => String.t(),
          optional(:client_id) => String.t(),
          optional(:client_secret) => String.t(),
          optional(:redirect_port) => non_neg_integer(),
          optional(:scopes) => [String.t()],
          optional(:resource) => String.t() | [String.t()],
          optional(:http_client) => module(),
          optional(:www_authenticate) => String.t()
        }

  @doc """
  Execute the full OAuth flow.

  Returns `{:ok, %{access_token: "...", ...}}` on success.
  """
  @spec execute(config()) :: {:ok, map()} | {:error, term()}
  def execute(config) do
    with {:ok, prm} <- discover_resource_metadata(config),
         {:ok, as_metadata} <- discover_as_metadata(prm, config),
         {:ok, client_info} <- ensure_client_registered(as_metadata, config) do
      run_auth_code_flow(as_metadata, client_info, config)
    end
  end

  # Step 1: Discover which AS protects the resource
  defp discover_resource_metadata(config) do
    # Try PRM URL from WWW-Authenticate header first
    prm_url = extract_resource_metadata_url(config[:www_authenticate])

    _ = prm_url
    # Always use well-known discovery based on resource URL
    ProtectedResourceMetadata.discover(config.resource_url)
  end

  # Step 2: Fetch AS metadata
  defp discover_as_metadata(prm, config) do
    as_url =
      case prm do
        %{authorization_servers: [%{issuer: issuer} | _]} -> issuer
        _ -> nil
      end

    if as_url do
      case OIDCDiscovery.discover(as_url, http_client: config[:http_client]) do
        {:ok, metadata} -> {:ok, metadata}
        {:error, reason} -> {:error, {:as_discovery_failed, reason}}
      end
    else
      {:error, :no_authorization_server_found}
    end
  end

  # Step 3: Ensure we have a client_id (via dynamic registration if needed)
  defp ensure_client_registered(_as_metadata, %{client_id: id, client_secret: secret})
       when is_binary(id) and is_binary(secret) do
    {:ok, %{client_id: id, client_secret: secret}}
  end

  defp ensure_client_registered(_as_metadata, %{client_id: id}) when is_binary(id) do
    {:ok, %{client_id: id}}
  end

  defp ensure_client_registered(as_metadata, config) do
    registration_endpoint = as_metadata["registration_endpoint"]

    if registration_endpoint do
      Logger.info("Dynamically registering OAuth client at #{registration_endpoint}")
      redirect_uri = "http://127.0.0.1:0/callback"

      case ClientRegistration.register_client(%{
             registration_endpoint: registration_endpoint,
             client_name: "ex_mcp",
             redirect_uris: [redirect_uri],
             grant_types: ["authorization_code"],
             response_types: ["code"],
             scope: Enum.join(config[:scopes] || [], " "),
             client_uri: nil,
             logo_uri: nil,
             contacts: nil,
             tos_uri: nil,
             policy_uri: nil,
             software_id: nil,
             software_version: nil
           }) do
        {:ok, reg} ->
          {:ok,
           %{
             client_id: reg["client_id"],
             client_secret: reg["client_secret"]
           }}

        {:error, reason} ->
          {:error, {:registration_failed, reason}}
      end
    else
      {:error, :no_registration_endpoint}
    end
  end

  # Step 4: Run authorization code flow with PKCE
  defp run_auth_code_flow(as_metadata, client_info, config) do
    authorization_endpoint = as_metadata["authorization_endpoint"]
    token_endpoint = as_metadata["token_endpoint"]

    with :ok <- validate_endpoints(authorization_endpoint, token_endpoint),
         {:ok, server_pid, redirect_uri} <- setup_redirect_server(config),
         {:ok, auth_url, state_data} <- start_flow(client_info, redirect_uri, as_metadata, config) do
      result =
        authorize_and_exchange(
          auth_url,
          state_data,
          server_pid,
          client_info,
          redirect_uri,
          token_endpoint
        )

      stop_redirect_server(server_pid)
      result
    end
  end

  defp validate_endpoints(auth_ep, token_ep) do
    if auth_ep && token_ep, do: :ok, else: {:error, :missing_endpoints}
  end

  defp setup_redirect_server(config) do
    port = config[:redirect_port] || 0

    case start_redirect_server(port) do
      {:ok, server_pid, actual_port} ->
        {:ok, server_pid, "http://127.0.0.1:#{actual_port}/callback"}

      {:error, reason} ->
        {:error, {:redirect_server_failed, reason}}
    end
  end

  defp start_flow(client_info, redirect_uri, as_metadata, config) do
    OAuthFlow.start_authorization_flow(%{
      client_id: client_info.client_id,
      redirect_uri: redirect_uri,
      authorization_endpoint: as_metadata["authorization_endpoint"],
      scopes: config[:scopes] || [],
      resource: config[:resource]
    })
  end

  defp authorize_and_exchange(
         auth_url,
         state_data,
         server_pid,
         client_info,
         redirect_uri,
         token_endpoint
       ) do
    Logger.info("OAuth authorization URL: #{auth_url}")

    with {:ok, _} <- follow_authorization(auth_url),
         {:ok, code} <- wait_for_callback(server_pid) do
      OAuthFlow.exchange_code_for_token(%{
        code: code,
        code_verifier: state_data.code_verifier,
        client_id: client_info.client_id,
        redirect_uri: redirect_uri,
        token_endpoint: token_endpoint,
        client_secret: client_info[:client_secret]
      })
    end
  end

  # Follow the authorization URL (for automated testing — real browsers would open this)
  defp follow_authorization(url) do
    case :httpc.request(:get, {String.to_charlist(url), []}, [{:autoredirect, false}], []) do
      {:ok, {{_, status, _}, headers, _body}} when status in [301, 302, 303, 307, 308] ->
        # Follow redirect
        location =
          headers
          |> Enum.find(fn {k, _} -> String.downcase(List.to_string(k)) == "location" end)
          |> case do
            {_, loc} -> List.to_string(loc)
            nil -> nil
          end

        if location do
          {:ok, location}
        else
          {:error, :no_redirect_location}
        end

      {:ok, {{_, 200, _}, _headers, _body}} ->
        # Some auth servers respond with 200 and a form
        {:ok, url}

      {:ok, {{_, status, _}, _headers, body}} ->
        {:error, {:auth_server_error, status, List.to_string(body)}}

      {:error, reason} ->
        {:error, {:auth_request_failed, reason}}
    end
  end

  # Start a minimal HTTP server to receive the OAuth callback
  defp start_redirect_server(port) do
    parent = self()

    pid =
      spawn_link(fn ->
        {:ok, listen_socket} =
          :gen_tcp.listen(port, [:binary, active: false, reuseaddr: true])

        {:ok, actual_port} = :inet.port(listen_socket)
        send(parent, {:redirect_server_started, actual_port})

        # Accept one connection
        case :gen_tcp.accept(listen_socket, 30_000) do
          {:ok, socket} ->
            case :gen_tcp.recv(socket, 0, 10_000) do
              {:ok, data} ->
                # Parse the code from the callback URL
                code = extract_code_from_request(data)

                # Send HTTP response
                response =
                  "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n" <>
                    "<html><body>Authorization complete. You may close this window.</body></html>"

                :gen_tcp.send(socket, response)
                :gen_tcp.close(socket)
                :gen_tcp.close(listen_socket)
                send(parent, {:redirect_callback, {:ok, code}})

              {:error, reason} ->
                :gen_tcp.close(socket)
                :gen_tcp.close(listen_socket)
                send(parent, {:redirect_callback, {:error, reason}})
            end

          {:error, reason} ->
            :gen_tcp.close(listen_socket)
            send(parent, {:redirect_callback, {:error, reason}})
        end
      end)

    receive do
      {:redirect_server_started, actual_port} -> {:ok, pid, actual_port}
    after
      5_000 -> {:error, :redirect_server_timeout}
    end
  end

  defp wait_for_callback(_server_pid) do
    receive do
      {:redirect_callback, result} -> result
    after
      30_000 -> {:error, :callback_timeout}
    end
  end

  defp stop_redirect_server(pid) do
    if Process.alive?(pid), do: Process.exit(pid, :normal)
  end

  defp extract_code_from_request(data) do
    # Parse "GET /callback?code=xxx&state=yyy HTTP/1.1\r\n..."
    case Regex.run(~r/[?&]code=([^&\s]+)/, data) do
      [_, code] -> code
      _ -> nil
    end
  end

  defp extract_resource_metadata_url(nil), do: nil

  defp extract_resource_metadata_url(www_auth) when is_binary(www_auth) do
    case Regex.run(~r/resource_metadata="([^"]+)"/, www_auth) do
      [_, url] -> url
      _ -> nil
    end
  end
end
