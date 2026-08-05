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
        client_registration: :auto,
        application_type: :native,
        redirect_port: 8080
      })

  """

  require Logger

  @max_callback_request_bytes 16_384
  @max_callback_params 16
  @max_callback_param_bytes 4_096

  alias ExMCP.Authorization.{
    ClientAssertion,
    ClientRegistration,
    CredentialStore,
    HTTPClient,
    Issuer,
    OAuthFlow,
    OIDCDiscovery,
    RegistrationPolicy
  }

  @type config :: %{
          required(:resource_url) => String.t(),
          optional(:client_id) => String.t(),
          optional(:client_secret) => String.t(),
          optional(:client_registration) => RegistrationPolicy.configured_strategy(),
          optional(:credential_issuer) => String.t(),
          optional(:credential_store) => CredentialStore.store(),
          optional(:credential_context) => term(),
          optional(:client_metadata_url) => String.t(),
          optional(:application_type) => RegistrationPolicy.application_type(),
          optional(:redirect_port) => non_neg_integer(),
          optional(:private_key) => JOSE.JWK.t(),
          optional(:signing_algorithm) => String.t(),
          optional(:key_id) => String.t(),
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

        run_and_persist_token(as_metadata, client_info, config)
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
  @jwt_bearer_grant "urn:ietf:params:oauth:grant-type:jwt-bearer"

  defp run_and_persist_token(as_metadata, client_info, config) do
    with {:ok, token_data} <- select_and_run_grant_flow(as_metadata, client_info, config),
         :ok <- persist_token(token_data, as_metadata, client_info, config) do
      {:ok, token_data}
    end
  end

  defp select_and_run_grant_flow(as_metadata, client_info, config) do
    grant_types = as_metadata["grant_types_supported"] || []
    supports_auth_code = "authorization_code" in grant_types or grant_types == []
    supports_client_creds = "client_credentials" in grant_types
    supports_jwt_bearer = @jwt_bearer_grant in grant_types

    has_preexisting_creds =
      client_info[:registration_method] == :pre_registered and
        is_binary(client_info[:client_secret])

    cond do
      supports_jwt_bearer and config[:idp_id_token] ->
        # Cross-app access: exchange IdP token for ID-JAG, then JWT bearer grant
        run_cross_app_flow(as_metadata, client_info, config)

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
          with :ok <- Issuer.compare(base, metadata["issuer"]) do
            # Store the already-fetched metadata to avoid re-discovery
            {:ok,
             %{
               authorization_servers: [%{issuer: base}],
               _prefetched_as_metadata: metadata
             }}
          end

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
      %{
        _prefetched_as_metadata: metadata,
        authorization_servers: [%{issuer: issuer} | _]
      }
      when is_map(metadata) ->
        with :ok <- Issuer.compare(issuer, metadata["issuer"]) do
          :telemetry.execute(
            [:ex_mcp, :auth, :discovery, :completed],
            %{system_time: System.system_time()},
            %{issuer: issuer}
          )

          {:ok, metadata}
        end

      %{authorization_servers: [%{issuer: issuer} | _]} ->
        case OIDCDiscovery.discover(issuer, http_client: config[:http_client]) do
          {:ok, metadata} ->
            with :ok <- Issuer.compare(issuer, metadata["issuer"]) do
              :telemetry.execute(
                [:ex_mcp, :auth, :discovery, :completed],
                %{system_time: System.system_time()},
                %{issuer: issuer}
              )

              {:ok, metadata}
            end

          {:error, reason} ->
            {:error, {:as_discovery_failed, reason}}
        end

      _ ->
        {:error, :no_authorization_server_found}
    end
  end

  # Step 3: Select pre-registration, configured CIMD, or deprecated DCR.
  defp ensure_client_registered(as_metadata, config) do
    case RegistrationPolicy.select(as_metadata, config) do
      {:ok, {:pre_registered, client_info}} ->
        {:ok, client_info}

      {:ok, {:cimd, client_info}} ->
        Logger.info("Using configured Client ID Metadata Document")
        {:ok, client_info}

      {:ok, {:dynamic, selection}} ->
        case load_persisted_registration(as_metadata, config) do
          {:ok, registration} ->
            Logger.info("Using persisted issuer-bound OAuth client registration")
            {:ok, Map.from_struct(registration)}

          :not_found ->
            do_register_client(selection, as_metadata, config)

          {:error, _reason} = error ->
            error
        end

      {:error, _reason} = error ->
        error
    end
  end

  defp do_register_client(selection, as_metadata, config) do
    registration_endpoint = selection.registration_endpoint
    Logger.info("Dynamically registering OAuth client at #{registration_endpoint}")
    redirect_uri = "http://127.0.0.1:#{config.redirect_port}/callback"
    supported = as_metadata["token_endpoint_auth_methods_supported"] || []
    auth_method = select_registration_auth_method(supported)

    case ClientRegistration.register_client(%{
           registration_endpoint: registration_endpoint,
           client_name: "ex_mcp",
           application_type: Atom.to_string(selection.application_type),
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

        client_info = %{
          issuer: as_metadata["issuer"],
          client_id: client_id,
          client_secret: reg[:client_secret] || reg["client_secret"],
          registration_method: :dynamic
        }

        with :ok <- persist_registration(client_info, config) do
          :telemetry.execute(
            [:ex_mcp, :auth, :registration, :completed],
            %{system_time: System.system_time()},
            %{client_id: client_id, issuer: as_metadata["issuer"]}
          )

          {:ok, client_info}
        end

      {:error, {:registration_error, status, %{"error" => "invalid_redirect_uri"} = response}} ->
        {:error,
         {:redirect_uri_rejected,
          application_type: selection.application_type,
          redirect_uri: redirect_uri,
          status: status,
          response: response}}

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

  defp load_persisted_registration(as_metadata, %{credential_store: store} = config) do
    CredentialStore.fetch_registration(
      store,
      credential_context(config),
      as_metadata["issuer"]
    )
  end

  defp load_persisted_registration(_as_metadata, _config), do: :not_found

  defp persist_registration(_client_info, config) when not is_map_key(config, :credential_store),
    do: :ok

  defp persist_registration(client_info, config) do
    CredentialStore.put_registration(
      config.credential_store,
      credential_context(config),
      client_info
    )
  end

  defp persist_token(_token_data, _as_metadata, _client_info, config)
       when not is_map_key(config, :credential_store),
       do: :ok

  defp persist_token(token_data, as_metadata, client_info, config) do
    expires_at =
      case token_field(token_data, :expires_in) do
        seconds when is_integer(seconds) and seconds >= 0 -> System.system_time(:second) + seconds
        _other -> nil
      end

    token = %{
      issuer: as_metadata["issuer"],
      client_id: client_info.client_id,
      resource: config[:resource] || config[:resource_url],
      audience: config[:audience],
      subject: config[:subject],
      client_identity: config[:client_identity] || client_info.client_id,
      granted_scopes:
        token_field(token_data, :scope) || config[:scopes] || config[:prm_scopes] || [],
      access_token: token_field(token_data, :access_token),
      refresh_token: token_field(token_data, :refresh_token),
      token_type: token_field(token_data, :token_type) || "Bearer",
      expires_at: expires_at
    }

    CredentialStore.put_token(config.credential_store, token)
  end

  defp credential_context(config),
    do: config[:credential_context] || config[:resource_url]

  defp token_field(token_data, field) do
    Map.get(token_data, field) || Map.get(token_data, Atom.to_string(field))
  end

  # Step 4a: Client credentials flow (when we have pre-existing credentials)
  defp run_client_credentials_flow(as_metadata, client_info, config) do
    token_endpoint = as_metadata["token_endpoint"]

    if is_nil(token_endpoint) do
      {:error, :missing_token_endpoint}
    else
      supported_methods =
        as_metadata["token_endpoint_auth_methods_supported"] || ["client_secret_post"]

      # Pass token_endpoint and issuer into config for JWT audience.
      # Per MCP ext-auth, the JWT aud claim should be the issuer URL.
      config =
        config
        |> Map.put(:token_endpoint, as_metadata["issuer"] || token_endpoint)

      result =
        with {:ok, token_auth_method} <-
               select_token_auth_method(supported_methods, client_info, config),
             :ok <- log_token_auth_method("client_credentials", token_auth_method),
             {:ok, body} <-
               build_client_credentials_body(client_info, config, token_auth_method) do
          HTTPClient.make_token_request(token_endpoint, body, auth_method: token_auth_method)
        end

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

  # Step 4c: Cross-app access flow (RFC 8693 token exchange + RFC 7523 JWT bearer)
  # 1. Exchange IdP ID token for an ID-JAG at the IdP's token endpoint
  # 2. Present the ID-JAG to the AS via JWT bearer grant
  defp run_cross_app_flow(as_metadata, client_info, config) do
    token_endpoint = as_metadata["token_endpoint"]
    idp_token_endpoint = config[:idp_token_endpoint]
    id_token = config[:idp_id_token]
    resource = config[:resource] || config[:resource_url]

    Logger.info("Running cross-app access flow (token exchange → JWT bearer)")

    # Step 1: Exchange ID token for ID-JAG at IdP
    exchange_body = [
      {"grant_type", "urn:ietf:params:oauth:grant-type:token-exchange"},
      {"subject_token", id_token},
      {"subject_token_type", "urn:ietf:params:oauth:token-type:id_token"},
      {"requested_token_type", "urn:ietf:params:oauth:token-type:id-jag"},
      {"audience", as_metadata["issuer"] || token_endpoint},
      {"resource", resource}
    ]

    # Add client auth if we have IdP client credentials
    exchange_body =
      if config[:idp_client_id] do
        exchange_body ++ [{"client_id", config[:idp_client_id]}]
      else
        exchange_body
      end

    case HTTPClient.make_token_request(idp_token_endpoint, exchange_body, auth_method: :none) do
      {:ok, exchange_result} ->
        id_jag = exchange_result[:access_token] || exchange_result["access_token"]

        Logger.info("ID-JAG obtained via token exchange, presenting to AS")

        # Step 2: Present ID-JAG to AS via JWT bearer grant
        # Use client_secret_basic auth if we have a secret
        bearer_body = [
          {"grant_type", @jwt_bearer_grant},
          {"assertion", id_jag},
          {"client_id", client_info.client_id},
          {"client_secret", client_info[:client_secret] || ""},
          {"resource", resource}
        ]

        auth_method =
          if client_info[:client_secret], do: :client_secret_basic, else: :none

        case HTTPClient.make_token_request(token_endpoint, bearer_body, auth_method: auth_method) do
          {:ok, token_data} ->
            :telemetry.execute(
              [:ex_mcp, :auth, :token, :obtained],
              %{system_time: System.system_time()},
              %{token_type: "cross_app_access"}
            )

            {:ok, token_data}

          {:error, reason} ->
            {:error, {:jwt_bearer_failed, reason}}
        end

      {:error, reason} ->
        {:error, {:token_exchange_failed, reason}}
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

        body =
          [{"grant_type", "client_credentials"}, {"resource", resource}]
          |> Enum.concat(assertion_params)
          |> Enum.reject(fn {_, v} -> is_nil(v) or v == "" end)

        {:ok, body}

      {:error, reason} ->
        {:error, {:client_assertion_failed, reason}}
    end
  end

  defp build_client_credentials_body(client_info, config, _auth_method) do
    body =
      [
        grant_type: "client_credentials",
        client_id: client_info.client_id,
        client_secret: Map.get(client_info, :client_secret),
        resource: config[:resource] || config[:resource_url]
      ]
      |> Enum.reject(fn {_, v} -> is_nil(v) end)

    {:ok, body}
  end

  # Step 4b: Run authorization code flow with PKCE
  defp run_auth_code_flow(as_metadata, client_info, config) do
    authorization_endpoint = as_metadata["authorization_endpoint"]
    token_endpoint = as_metadata["token_endpoint"]

    # Determine token endpoint auth method from AS metadata
    supported_methods =
      as_metadata["token_endpoint_auth_methods_supported"] || ["client_secret_post"]

    with :ok <- validate_endpoints(authorization_endpoint, token_endpoint),
         {:ok, token_auth_method} <-
           select_token_auth_method(supported_methods, client_info, config),
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
      issuer: as_metadata["issuer"],
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
    Logger.info("Starting OAuth authorization request")
    Logger.info("Token endpoint auth method: #{token_auth_method}")

    with {:ok, _} <- follow_authorization(auth_url),
         {:ok, code} <- wait_for_callback(server_pid, state_data),
         {:ok, body} <-
           authorization_code_token_body(
             code,
             state_data,
             client_info,
             redirect_uri,
             token_endpoint,
             config,
             token_auth_method
           ) do
      HTTPClient.make_token_request(
        token_endpoint,
        body,
        auth_method: token_auth_method
      )
    end
  end

  defp authorization_code_token_body(
         code,
         state_data,
         client_info,
         redirect_uri,
         token_endpoint,
         config,
         token_auth_method
       ) do
    base =
      [
        grant_type: "authorization_code",
        code: code,
        redirect_uri: redirect_uri,
        client_id: client_info.client_id,
        code_verifier: state_data.code_verifier,
        client_secret: client_info[:client_secret],
        resource: config[:resource] || config[:resource_url]
      ]
      |> Enum.reject(fn {_, value} -> is_nil(value) end)

    maybe_add_client_assertion(base, client_info, token_endpoint, config, token_auth_method)
  end

  defp maybe_add_client_assertion(base, client_info, token_endpoint, config, :private_key_jwt) do
    private_key = config[:private_key] || client_info[:private_key]

    if is_nil(private_key) do
      {:error, :private_key_required}
    else
      case ClientAssertion.build_assertion_params(
             client_id: client_info.client_id,
             token_endpoint: token_endpoint,
             private_key: private_key,
             alg: config[:signing_algorithm] || client_info[:signing_algorithm] || "ES256",
             kid: config[:key_id] || client_info[:key_id]
           ) do
        {:ok, assertion_params} ->
          base = Enum.reject(base, fn {key, _value} -> key in [:client_id, "client_id"] end)
          {:ok, base ++ assertion_params}

        {:error, reason} ->
          {:error, {:client_assertion_failed, reason}}
      end
    end
  end

  defp maybe_add_client_assertion(base, _client_info, _token_endpoint, _config, _auth_method),
    do: {:ok, base}

  defp select_token_auth_method(supported, client_info, config) when is_list(supported) do
    private_key = config[:private_key] || client_info[:private_key]
    client_secret = client_info[:client_secret]

    cond do
      "private_key_jwt" in supported and not is_nil(private_key) ->
        {:ok, :private_key_jwt}

      "client_secret_basic" in supported and is_binary(client_secret) ->
        {:ok, :client_secret_basic}

      "client_secret_post" in supported and is_binary(client_secret) ->
        {:ok, :client_secret_post}

      "none" in supported ->
        {:ok, :none}

      true ->
        {:error, {:no_usable_token_auth_method, supported}}
    end
  end

  defp select_token_auth_method(_supported, _client_info, _config),
    do: {:error, :invalid_token_auth_methods}

  defp log_token_auth_method(flow, token_auth_method) do
    Logger.info("Using #{flow} flow with #{token_auth_method} auth")
    :ok
  end

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
            Logger.info("Following OAuth redirect to callback")
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
            receive_callback(socket, listen_socket, parent)

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

  defp receive_callback(socket, listen_socket, parent) do
    case :gen_tcp.recv(socket, 0, 10_000) do
      {:ok, data} ->
        {response, result} = callback_response(data)
        :gen_tcp.send(socket, response)
        close_callback_sockets(socket, listen_socket)
        send(parent, {:redirect_callback, result})

      {:error, reason} ->
        close_callback_sockets(socket, listen_socket)
        send(parent, {:redirect_callback, {:error, reason}})
    end
  end

  defp callback_response(data) do
    case extract_request_path(data) do
      "/callback" -> callback_query_response(data)
      _other -> {bad_callback_response("Invalid callback path"), {:error, :invalid_callback_path}}
    end
  end

  defp callback_query_response(data) do
    case extract_callback_params(data) do
      {:ok, callback_params} ->
        response =
          "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n" <>
            "<html><body>Authorization complete. You may close this window.</body></html>"

        {response, {:ok, callback_params}}

      {:error, reason} ->
        {bad_callback_response("Invalid callback query"), {:error, reason}}
    end
  end

  defp bad_callback_response(message) do
    "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\n\r\n#{message}"
  end

  defp close_callback_sockets(socket, listen_socket) do
    :gen_tcp.close(socket)
    :gen_tcp.close(listen_socket)
  end

  defp wait_for_callback(_server_pid, transaction) do
    receive do
      {:redirect_callback, {:ok, callback_params}} ->
        case OAuthFlow.validate_authorization_response(callback_params, transaction) do
          {:ok, code} ->
            {:ok, code}

          {:error, :state_mismatch} = error ->
            Logger.warning("OAuth callback state mismatch")
            error

          {:error, {:issuer_mismatch, _details}} = error ->
            Logger.warning("OAuth callback issuer mismatch")
            error

          {:error, _reason} = error ->
            error
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

  defp extract_callback_params(data) when byte_size(data) <= @max_callback_request_bytes do
    with [_, request_target] <- Regex.run(~r/^(?:GET|POST)\s+(\S+)/, data),
         query when is_binary(query) <- URI.parse(request_target).query,
         {:ok, pairs} <- decode_callback_query(query) do
      {:ok, Map.new(pairs)}
    else
      _invalid -> {:error, :invalid_callback_query}
    end
  rescue
    ArgumentError -> {:error, :invalid_callback_query}
  end

  defp extract_callback_params(_data), do: {:error, :invalid_callback_query}

  defp decode_callback_query(query) do
    pairs = query |> URI.query_decoder() |> Enum.take(@max_callback_params + 1)

    valid? =
      length(pairs) <= @max_callback_params and
        Enum.all?(pairs, fn {key, value} ->
          byte_size(key) <= @max_callback_param_bytes and
            byte_size(value) <= @max_callback_param_bytes
        end) and
        map_size(Map.new(pairs)) == length(pairs)

    if valid?, do: {:ok, pairs}, else: {:error, :invalid_callback_query}
  end

  defp extract_resource_metadata_url(nil), do: nil

  defp extract_resource_metadata_url(www_auth) when is_binary(www_auth) do
    case Regex.run(~r/resource_metadata="([^"]+)"/, www_auth) do
      [_, url] -> url
      _ -> nil
    end
  end
end
