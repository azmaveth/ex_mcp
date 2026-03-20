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
    HTTPClient,
    OAuthFlow,
    OIDCDiscovery
  }

  @type config :: %{
          required(:resource_url) => String.t(),
          optional(:client_id) => String.t(),
          optional(:client_secret) => String.t(),
          optional(:redirect_port) => non_neg_integer(),
          optional(:scopes) => [String.t()],
          optional(:resource) => String.t() | [String.t()],
          optional(:http_client) => module(),
          optional(:www_authenticate) => String.t(),
          optional(:protocol_version) => String.t()
        }

  @doc """
  Execute the full OAuth flow.

  Returns `{:ok, %{access_token: "...", ...}}` on success.
  """
  @spec execute(config()) :: {:ok, map()} | {:error, term()}
  def execute(config) do
    :telemetry.execute(
      [:ex_mcp, :auth, :flow, :started],
      %{system_time: System.system_time()},
      %{resource_url: config[:resource_url]}
    )

    # Check if caller provided pre-existing credentials (not from dynamic registration)
    has_preexisting_creds = is_binary(config[:client_id]) and is_binary(config[:client_secret])

    result =
      with {:ok, prm} <- discover_resource_metadata(config),
           :ok <- validate_prm_resource(prm, config),
           {:ok, as_metadata} <- discover_as_metadata(prm, config),
           {:ok, client_info} <- ensure_client_registered(as_metadata, config) do
        # Merge PRM scopes_supported into config for scope negotiation
        config =
          case prm[:scopes_supported] do
            scopes when is_list(scopes) and scopes != [] ->
              Map.put_new(config, :prm_scopes, scopes)

            _ ->
              config
          end

        select_and_run_grant_flow(as_metadata, client_info, config, has_preexisting_creds)
      end

    case result do
      {:ok, _} = ok ->
        :telemetry.execute(
          [:ex_mcp, :auth, :flow, :completed],
          %{system_time: System.system_time()},
          %{resource_url: config[:resource_url]}
        )

        ok

      {:error, reason} = err ->
        :telemetry.execute(
          [:ex_mcp, :auth, :flow, :failed],
          %{system_time: System.system_time()},
          %{resource_url: config[:resource_url], reason: reason}
        )

        err
    end
  end

  # Step 1: Discover which AS protects the resource
  # Select grant flow based on AS metadata's grant_types_supported.
  # If the server only supports client_credentials, use that regardless.
  # Otherwise, use auth code if no pre-existing creds, client_credentials if we have them.
  defp select_and_run_grant_flow(as_metadata, client_info, config, has_preexisting_creds) do
    grant_types = as_metadata["grant_types_supported"] || []
    supports_auth_code = "authorization_code" in grant_types or grant_types == []
    supports_client_creds = "client_credentials" in grant_types

    cond do
      supports_client_creds and not supports_auth_code ->
        # Server only supports client_credentials — must use it
        run_client_credentials_flow(as_metadata, client_info, config)

      has_preexisting_creds and supports_client_creds ->
        # Have credentials and server supports client_credentials
        run_client_credentials_flow(as_metadata, client_info, config)

      true ->
        # Default to authorization code flow
        run_auth_code_flow(as_metadata, client_info, config)
    end
  end

  defp discover_resource_metadata(config) do
    # Try PRM URL from WWW-Authenticate header first
    prm_url = extract_resource_metadata_url(config[:www_authenticate])

    prm_result =
      case prm_url do
        nil ->
          discover_prm_with_fallback(config.resource_url)

        url ->
          case fetch_prm_directly(url) do
            {:ok, _} = ok -> ok
            {:error, _} -> discover_prm_with_fallback(config.resource_url)
          end
      end

    case prm_result do
      {:ok, _} = ok ->
        ok

      {:error, _} ->
        # PRM not available — fall back to direct AS metadata discovery.
        # This is required for 2025-03-26 backcompat where PRM didn't exist,
        # and is a reasonable fallback for any version when PRM is unavailable.
        Logger.info("PRM not available, falling back to direct AS discovery")
        discover_as_from_www_authenticate(config)
    end
  end

  # Fallback when PRM is unavailable: extract AS URL from WWW-Authenticate header
  # or discover AS metadata directly from the resource origin.
  # Returns a synthetic PRM with pre-fetched AS metadata to avoid double discovery.
  defp discover_as_from_www_authenticate(config) do
    www_auth = config[:www_authenticate] || ""

    # Try to extract AS URL from WWW-Authenticate header
    as_uri = extract_as_uri_from_www_auth(www_auth)

    if as_uri do
      {:ok, %{authorization_servers: [%{issuer: as_uri}]}}
    else
      # No AS URL in header — try well-known discovery on the resource origin
      uri = URI.parse(config[:resource_url])
      base = "#{uri.scheme}://#{uri.host}#{if uri.port, do: ":#{uri.port}", else: ""}"

      case OIDCDiscovery.discover(base) do
        {:ok, metadata} ->
          issuer = metadata["issuer"] || base
          # Store the already-fetched metadata to avoid re-discovery
          {:ok,
           %{
             authorization_servers: [%{issuer: issuer}],
             _prefetched_as_metadata: metadata
           }}

        {:error, _} ->
          # Last resort: construct endpoint URLs from the resource origin.
          # Per MCP 2025-03-26, when no metadata discovery works, assume
          # standard OAuth endpoints at the resource origin.
          Logger.info("No AS metadata found, constructing endpoints from origin: #{base}")

          synthetic_metadata = %{
            "issuer" => base,
            "authorization_endpoint" => "#{base}/authorize",
            "token_endpoint" => "#{base}/token",
            "registration_endpoint" => "#{base}/register",
            "response_types_supported" => ["code"],
            "grant_types_supported" => ["authorization_code"],
            "code_challenge_methods_supported" => ["S256"]
          }

          {:ok,
           %{
             authorization_servers: [%{issuer: base}],
             _prefetched_as_metadata: synthetic_metadata
           }}
      end
    end
  end

  defp extract_as_uri_from_www_auth(www_auth) when is_binary(www_auth) do
    case Regex.run(~r/as_uri="([^"]+)"/, www_auth) do
      [_, uri] -> uri
      _ -> nil
    end
  end

  defp extract_as_uri_from_www_auth(_), do: nil

  # Validate that PRM resource field matches our server URL (RFC 8707)
  defp validate_prm_resource(%{resource: prm_resource}, config) when is_binary(prm_resource) do
    server_url = config[:resource_url] || ""

    if urls_match?(prm_resource, server_url) do
      :ok
    else
      Logger.warning("PRM resource mismatch: #{prm_resource} != #{server_url}")
      {:error, {:resource_mismatch, prm_resource, server_url}}
    end
  end

  defp validate_prm_resource(_, _), do: :ok

  defp urls_match?(prm_resource, server_url) do
    # The PRM resource can be the base origin (protects entire origin)
    # or a specific path. Check if server URL starts with PRM resource.
    norm_prm = normalize_url(prm_resource)
    norm_server = normalize_url(server_url)
    norm_server == norm_prm or String.starts_with?(norm_server, norm_prm <> "/")
  end

  defp normalize_url(url) when is_binary(url) do
    uri = URI.parse(url)
    path = (uri.path || "/") |> String.trim_trailing("/")
    "#{uri.scheme}://#{uri.host}:#{uri.port || default_port(uri.scheme)}#{path}"
  end

  defp normalize_url(_), do: ""

  defp default_port("https"), do: 443
  defp default_port("http"), do: 80
  defp default_port(_), do: 80

  # Try path-based PRM discovery first, then fall back to root well-known.
  # Per MCP spec, path-based is /.well-known/oauth-protected-resource/mcp
  # and root is /.well-known/oauth-protected-resource
  defp discover_prm_with_fallback(resource_url) do
    uri = URI.parse(resource_url)
    base = "#{uri.scheme}://#{uri.host}#{if uri.port, do: ":#{uri.port}", else: ""}"
    path = uri.path || ""

    # Try path-based first (e.g., /.well-known/oauth-protected-resource/mcp)
    path_based_url = "#{base}/.well-known/oauth-protected-resource#{path}"

    case fetch_prm_directly(path_based_url) do
      {:ok, _} = ok ->
        ok

      {:error, _} ->
        # Fall back to root (e.g., /.well-known/oauth-protected-resource)
        root_url = "#{base}/.well-known/oauth-protected-resource"
        fetch_prm_directly(root_url)
    end
  end

  # Fetch PRM from an explicit URL (from WWW-Authenticate header)
  defp fetch_prm_directly(url) do
    case :httpc.request(:get, {String.to_charlist(url), []}, [], []) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        body_str = if is_list(body), do: List.to_string(body), else: body

        case Jason.decode(body_str) do
          {:ok, data} ->
            as_list =
              (data["authorization_servers"] || [])
              |> Enum.map(fn issuer -> %{issuer: issuer} end)

            result = %{authorization_servers: as_list}
            # Include resource and scopes_supported for validation and scope negotiation
            result =
              if data["resource"], do: Map.put(result, :resource, data["resource"]), else: result

            result =
              if data["scopes_supported"],
                do: Map.put(result, :scopes_supported, data["scopes_supported"]),
                else: result

            {:ok, result}

          {:error, reason} ->
            {:error, {:prm_parse_error, reason}}
        end

      {:ok, {{_, status, _}, _headers, _body}} ->
        {:error, {:prm_fetch_error, status}}

      {:error, reason} ->
        {:error, {:prm_request_failed, reason}}
    end
  end

  # Step 2: Fetch AS metadata
  defp discover_as_metadata(prm, config) do
    # Check for prefetched AS metadata (from PRM fallback path)
    case prm do
      %{_prefetched_as_metadata: metadata} when is_map(metadata) ->
        :telemetry.execute(
          [:ex_mcp, :auth, :discovery, :completed],
          %{system_time: System.system_time()},
          %{issuer: metadata["issuer"]}
        )

        {:ok, metadata}

      %{authorization_servers: [%{issuer: issuer} | _]} ->
        case OIDCDiscovery.discover(issuer, http_client: config[:http_client]) do
          {:ok, metadata} ->
            :telemetry.execute(
              [:ex_mcp, :auth, :discovery, :completed],
              %{system_time: System.system_time()},
              %{issuer: issuer}
            )

            {:ok, metadata}

          {:error, reason} ->
            {:error, {:as_discovery_failed, reason}}
        end

      _ ->
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
    # Check if server supports CIMD (Client ID Metadata Document)
    # If so, use a URL as client_id instead of dynamic registration
    if as_metadata["client_id_metadata_document_supported"] do
      cimd_url =
        config[:client_metadata_url] ||
          "https://conformance-test.local/client-metadata.json"

      Logger.info("Server supports CIMD, using URL-based client_id: #{cimd_url}")
      {:ok, %{client_id: cimd_url}}
    else
      registration_endpoint = as_metadata["registration_endpoint"]

      if registration_endpoint do
        do_register_client(registration_endpoint, as_metadata, config)
      else
        {:error, :no_registration_endpoint}
      end
    end
  end

  defp do_register_client(registration_endpoint, as_metadata, config) do
    Logger.info("Dynamically registering OAuth client at #{registration_endpoint}")
    redirect_uri = "http://127.0.0.1:0/callback"
    supported = as_metadata["token_endpoint_auth_methods_supported"] || []
    auth_method = select_registration_auth_method(supported)

    case ClientRegistration.register_client(%{
           registration_endpoint: registration_endpoint,
           client_name: "ex_mcp",
           redirect_uris: [redirect_uri],
           grant_types: ["authorization_code"],
           response_types: ["code"],
           token_endpoint_auth_method: auth_method,
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
        client_id = reg[:client_id] || reg["client_id"]

        :telemetry.execute(
          [:ex_mcp, :auth, :registration, :completed],
          %{system_time: System.system_time()},
          %{client_id: client_id}
        )

        {:ok,
         %{
           client_id: client_id,
           client_secret: reg[:client_secret] || reg["client_secret"]
         }}

      {:error, reason} ->
        {:error, {:registration_failed, reason}}
    end
  end

  defp select_registration_auth_method(supported) do
    cond do
      "none" in supported -> "none"
      "client_secret_basic" in supported -> "client_secret_basic"
      "client_secret_post" in supported -> "client_secret_post"
      true -> "none"
    end
  end

  # Step 4a: Client credentials flow (when we have pre-existing credentials)
  defp run_client_credentials_flow(as_metadata, client_info, config) do
    token_endpoint = as_metadata["token_endpoint"]

    if is_nil(token_endpoint) do
      {:error, :missing_token_endpoint}
    else
      supported_methods =
        as_metadata["token_endpoint_auth_methods_supported"] || ["client_secret_post"]

      token_auth_method = select_token_auth_method(supported_methods)

      Logger.info("Using client_credentials flow with #{token_auth_method} auth")

      # Pass token_endpoint and issuer into config for JWT audience.
      # Per MCP ext-auth, the JWT aud claim should be the issuer URL.
      config =
        config
        |> Map.put(:token_endpoint, as_metadata["issuer"] || token_endpoint)

      body = build_client_credentials_body(client_info, config, token_auth_method)

      result = HTTPClient.make_token_request(token_endpoint, body, auth_method: token_auth_method)

      case result do
        {:ok, token_data} ->
          :telemetry.execute(
            [:ex_mcp, :auth, :token, :obtained],
            %{system_time: System.system_time()},
            %{token_type: token_data[:token_type] || token_data["token_type"]}
          )

          {:ok, token_data}

        error ->
          error
      end
    end
  end

  defp build_client_credentials_body(client_info, config, :private_key_jwt) do
    # JWT-based client authentication for client_credentials grant
    token_endpoint = config[:token_endpoint] || ""
    private_key = config[:private_key] || client_info[:private_key]
    alg = config[:signing_algorithm] || "ES256"

    case ExMCP.Authorization.ClientAssertion.build_assertion_params(
           client_id: client_info.client_id,
           token_endpoint: token_endpoint,
           private_key: private_key,
           alg: alg
         ) do
      {:ok, assertion_params} ->
        resource = config[:resource] || config[:resource_url] || ""

        [{"grant_type", "client_credentials"}, {"resource", resource}]
        |> Enum.concat(assertion_params)
        |> Enum.reject(fn {_, v} -> is_nil(v) or v == "" end)

      {:error, reason} ->
        Logger.warning("JWT assertion build failed: #{inspect(reason)}, falling back")

        [
          grant_type: "client_credentials",
          client_id: client_info.client_id,
          resource: config[:resource] || config[:resource_url]
        ]
        |> Enum.reject(fn {_, v} -> is_nil(v) end)
    end
  end

  defp build_client_credentials_body(client_info, config, _auth_method) do
    [
      grant_type: "client_credentials",
      client_id: client_info.client_id,
      client_secret: Map.get(client_info, :client_secret),
      resource: config[:resource] || config[:resource_url]
    ]
    |> Enum.reject(fn {_, v} -> is_nil(v) end)
  end

  # Step 4b: Run authorization code flow with PKCE
  defp run_auth_code_flow(as_metadata, client_info, config) do
    authorization_endpoint = as_metadata["authorization_endpoint"]
    token_endpoint = as_metadata["token_endpoint"]

    # Determine token endpoint auth method from AS metadata
    supported_methods =
      as_metadata["token_endpoint_auth_methods_supported"] || ["client_secret_post"]

    token_auth_method = select_token_auth_method(supported_methods)

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
          token_endpoint,
          config,
          token_auth_method
        )

      stop_redirect_server(server_pid)

      case result do
        {:ok, token_data} ->
          :telemetry.execute(
            [:ex_mcp, :auth, :token, :obtained],
            %{system_time: System.system_time()},
            %{token_type: token_data[:token_type] || token_data["token_type"]}
          )

          {:ok, token_data}

        error ->
          error
      end
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
    # Use scopes from: 1) WWW-Authenticate header, 2) PRM scopes_supported,
    # 3) AS metadata scopes_supported, 4) empty
    scopes =
      case config[:scopes] do
        s when is_list(s) and s != [] ->
          s

        _ ->
          config[:prm_scopes] || as_metadata["scopes_supported"] || []
      end

    OAuthFlow.start_authorization_flow(%{
      client_id: client_info.client_id,
      redirect_uri: redirect_uri,
      authorization_endpoint: as_metadata["authorization_endpoint"],
      scopes: scopes,
      resource: config[:resource] || config[:resource_url]
    })
  end

  defp authorize_and_exchange(
         auth_url,
         state_data,
         server_pid,
         client_info,
         redirect_uri,
         token_endpoint,
         config,
         token_auth_method
       ) do
    Logger.info("OAuth authorization URL: #{auth_url}")
    Logger.info("Token endpoint auth method: #{token_auth_method}")

    with {:ok, _} <- follow_authorization(auth_url),
         {:ok, code} <- wait_for_callback(server_pid, state_data.state_param) do
      HTTPClient.make_token_request(
        token_endpoint,
        [
          grant_type: "authorization_code",
          code: code,
          redirect_uri: redirect_uri,
          client_id: client_info.client_id,
          code_verifier: state_data.code_verifier,
          client_secret: client_info[:client_secret],
          resource: config[:resource] || config[:resource_url]
        ]
        |> Enum.reject(fn {_, v} -> is_nil(v) end),
        auth_method: token_auth_method
      )
    end
  end

  defp select_token_auth_method(supported) when is_list(supported) do
    cond do
      "private_key_jwt" in supported -> :private_key_jwt
      "none" in supported -> :none
      "client_secret_basic" in supported -> :client_secret_basic
      "client_secret_post" in supported -> :client_secret_post
      true -> :client_secret_post
    end
  end

  defp select_token_auth_method(_), do: :client_secret_post

  # Follow the authorization URL and its redirects (for automated testing).
  # The conformance test server auto-approves and redirects to our callback.
  # We follow redirects until we hit our callback URL (127.0.0.1).
  defp follow_authorization(url) do
    case :httpc.request(:get, {String.to_charlist(url), []}, [{:autoredirect, false}], []) do
      {:ok, {{_, status, _}, headers, _body}} when status in [301, 302, 303, 307, 308] ->
        location =
          headers
          |> Enum.find(fn {k, _} -> String.downcase(List.to_string(k)) == "location" end)
          |> case do
            {_, loc} -> List.to_string(loc)
            nil -> nil
          end

        if location do
          if String.contains?(location, "127.0.0.1") do
            # This redirect goes to our callback server — follow it so the
            # callback server receives the code
            Logger.info("Following OAuth redirect to callback: #{location}")
            :httpc.request(:get, {String.to_charlist(location), []}, [], [])
            {:ok, location}
          else
            # Intermediate redirect — follow it
            follow_authorization(location)
          end
        else
          {:error, :no_redirect_location}
        end

      {:ok, {{_, 200, _}, _headers, _body}} ->
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
                # Validate that the request targets the expected callback path
                request_path = extract_request_path(data)

                if request_path == "/callback" do
                  # Parse code and state from the callback URL
                  code = extract_code_from_request(data)
                  callback_state = extract_state_from_request(data)

                  # Send HTTP response
                  response =
                    "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n" <>
                      "<html><body>Authorization complete. You may close this window.</body></html>"

                  :gen_tcp.send(socket, response)
                  :gen_tcp.close(socket)
                  :gen_tcp.close(listen_socket)
                  send(parent, {:redirect_callback, {:ok, code, callback_state}})
                else
                  # Reject requests to unexpected paths (open redirect protection)
                  response =
                    "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\n\r\nInvalid callback path"

                  :gen_tcp.send(socket, response)
                  :gen_tcp.close(socket)
                  :gen_tcp.close(listen_socket)
                  send(parent, {:redirect_callback, {:error, :invalid_callback_path}})
                end

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

  defp wait_for_callback(_server_pid, expected_state) do
    receive do
      {:redirect_callback, {:ok, code, callback_state}} ->
        # Validate the state parameter to prevent CSRF attacks
        if callback_state == expected_state do
          {:ok, code}
        else
          Logger.warning(
            "OAuth state mismatch: expected #{inspect(expected_state)}, got #{inspect(callback_state)}"
          )

          {:error, :state_mismatch}
        end

      {:redirect_callback, {:error, _reason} = error} ->
        error
    after
      30_000 -> {:error, :callback_timeout}
    end
  end

  defp stop_redirect_server(pid) do
    if Process.alive?(pid), do: Process.exit(pid, :normal)
  end

  defp extract_request_path(data) do
    # Parse "GET /callback?code=xxx&state=yyy HTTP/1.1\r\n..."
    case Regex.run(~r/^(?:GET|POST)\s+([^\s?]+)/, data) do
      [_, path] -> path
      _ -> nil
    end
  end

  defp extract_code_from_request(data) do
    # Parse "GET /callback?code=xxx&state=yyy HTTP/1.1\r\n..."
    case Regex.run(~r/[?&]code=([^&\s]+)/, data) do
      [_, code] -> code
      _ -> nil
    end
  end

  defp extract_state_from_request(data) do
    case Regex.run(~r/[?&]state=([^&\s]+)/, data) do
      [_, state] -> URI.decode_www_form(state)
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
