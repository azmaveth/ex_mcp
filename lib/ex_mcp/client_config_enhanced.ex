defmodule ExMCP.ClientConfigEnhanced do
  @moduledoc """
  Enhanced configuration builder for ExMCP clients with reduced code duplication.

  This module provides the same comprehensive functionality as ClientConfig but with:
  - Macro-generated configuration setters to eliminate duplication
  - Environment-based configuration loading
  - Enhanced validation with custom rules
  - Configuration composition and templates
  - Performance optimizations

  ## Usage

      # Same API as original ClientConfig
      config = ExMCP.ClientConfigEnhanced.new()
               |> ExMCP.ClientConfigEnhanced.put_transport(:http)
               |> ExMCP.ClientConfigEnhanced.put_url("https://api.example.com")
      
      # New environment loading capability
      config = ExMCP.ClientConfigEnhanced.from_env(:my_app, :mcp_config)
      
      # Quick configuration helpers
      config = ExMCP.ClientConfigEnhanced.quick_http("https://api.example.com")
      config = ExMCP.ClientConfigEnhanced.quick_stdio(["python", "server.py"])

  ## Environment Variables

  The enhanced module supports loading configuration from environment variables:

      EXMCP_TIMEOUT_CONNECT=5000
      EXMCP_RETRY_MAX_ATTEMPTS=3
      EXMCP_AUTH_TOKEN=secret-token
      EXMCP_TRANSPORT_URL=https://api.example.com
  """

  # Import the original module's types and default functions
  alias ExMCP.ClientConfig

  # Import configuration macros
  import ExMCP.ClientConfig.Macros

  # Import logging for error reporting
  require Logger

  # Re-export all original types
  @type profile :: ClientConfig.profile()
  @type transport_type :: ClientConfig.transport_type()
  @type auth_type :: ClientConfig.auth_type()
  @type log_level :: ClientConfig.log_level()
  @type transport_config :: ClientConfig.transport_config()
  @type retry_policy :: ClientConfig.retry_policy()
  @type timeout_config :: ClientConfig.timeout_config()
  @type auth_config :: ClientConfig.auth_config()
  @type pool_config :: ClientConfig.pool_config()
  @type observability_config :: ClientConfig.observability_config()
  @type t :: ClientConfig.t()

  # Configuration merge strategies
  @type merge_strategy :: :shallow | :deep | :override | :preserve_existing

  # Environment variable mapping
  @type env_mapping :: %{String.t() => atom() | {atom(), atom()}}

  # Validation rule function type
  @type validation_rule :: (any() -> :ok | {:error, String.t()})

  # Configuration template type
  @type template_name :: :production_api | :development_local | :test_fast | :custom

  # ===== MACRO FOUNDATION: Eliminate Code Duplication =====

  # Generate enhanced configuration setters using the macro
  defconfig_setter(:retry_policy, merge_strategy: :shallow, validation: [:positive_integers])
  defconfig_setter(:pool, merge_strategy: :shallow, validation: [:positive_integers])

  # Example of using other merge strategies (for documentation purposes)
  # defconfig_setter(:deep_config, merge_strategy: :deep)
  # defconfig_setter(:override_config, merge_strategy: :override)  
  # defconfig_setter(:preserve_config, merge_strategy: :preserve_existing)

  # Custom implementation for client_info with user_agent generation
  @doc """
  Configures client information with automatic user_agent generation.
  """
  @spec put_client_info(t(), keyword() | map()) :: t()
  def put_client_info(config, opts) when is_list(opts) do
    put_client_info(config, Map.new(opts))
  end

  def put_client_info(config, opts) when is_map(opts) do
    validated_opts = apply_validation_rules(opts, [:string_fields])

    # Get current client_info
    current_info = Map.get(config, :client_info, %{})

    # Merge options
    updated_info = Map.merge(current_info, validated_opts)

    # Generate user_agent if name and version are present
    updated_info =
      if Map.has_key?(updated_info, :name) and Map.has_key?(updated_info, :version) do
        user_agent =
          "#{updated_info.name}/#{updated_info.version} ExMCP/#{updated_info[:exmcp_version] || "0.6.0"}"

        Map.put(updated_info, :user_agent, user_agent)
      else
        updated_info
      end

    %{config | client_info: updated_info}
  end

  # Custom implementation for complex observability merging
  @doc """
  Configures observability settings with enhanced deep merging.
  """
  @spec put_observability(t(), keyword()) :: t()
  def put_observability(config, opts) do
    observability_config = enhanced_deep_merge_observability(config.observability, opts)
    %{config | observability: observability_config}
  end

  # ===== ENVIRONMENT CONFIGURATION LOADING =====

  @doc """
  Creates a configuration from Application environment settings.

  ## Examples

      # Load from config/config.exs: config :my_app, mcp_config: [...]
      config = ExMCP.ClientConfigEnhanced.from_env(:my_app, :mcp_config)
      
      # With fallback to default profile
      config = ExMCP.ClientConfigEnhanced.from_env(:my_app, :mcp_config, default: :production)
  """
  @spec from_env(atom(), atom(), keyword()) :: t()
  def from_env(app_name, config_key, opts \\ []) do
    default_profile = Keyword.get(opts, :default, :development)

    case Application.get_env(app_name, config_key) do
      nil ->
        # No configuration found, use default profile
        ClientConfig.new(default_profile)

      env_config when is_list(env_config) ->
        # Found configuration, merge with profile
        base_config = ClientConfig.new(default_profile)
        apply_env_config(base_config, env_config)

      env_config when is_map(env_config) ->
        base_config = ClientConfig.new(default_profile)
        apply_env_config(base_config, Map.to_list(env_config))
    end
  end

  @doc """
  Creates a configuration from system environment variables.

  ## Examples

      # Use default variable mapping
      config = ExMCP.ClientConfigEnhanced.from_env_vars()
      
      # Use custom mapping
      mapping = %{
        "API_URL" => :url,
        "API_TIMEOUT" => {:timeouts, :request},
        "RETRY_COUNT" => {:retry_policy, :max_attempts}
      }
      config = ExMCP.ClientConfigEnhanced.from_env_vars(mapping)
  """
  @spec from_env_vars(env_mapping() | nil) :: t()
  def from_env_vars(mapping \\ nil) do
    env_mapping = mapping || default_env_mapping()

    base_config = ClientConfig.new()

    Enum.reduce(env_mapping, base_config, fn {env_var, config_path}, acc ->
      case System.get_env(env_var) do
        nil -> acc
        value -> apply_env_value(acc, config_path, value)
      end
    end)
  end

  @doc """
  Merges environment overrides into an existing configuration.

  ## Examples

      base_config = ExMCP.ClientConfigEnhanced.new(:production)
      config = ExMCP.ClientConfigEnhanced.merge_env_overrides(base_config, %{
        "OVERRIDE_URL" => :url,
        "OVERRIDE_TIMEOUT" => {:timeouts, :request}
      })
  """
  @spec merge_env_overrides(t(), env_mapping()) :: t()
  def merge_env_overrides(config, env_mapping) do
    Enum.reduce(env_mapping, config, fn {env_var, config_path}, acc ->
      case System.get_env(env_var) do
        nil -> acc
        value -> apply_env_value(acc, config_path, value)
      end
    end)
  end

  # ===== ENHANCED VALIDATION SYSTEM =====

  @doc """
  Adds a custom validation rule for a specific field.

  ## Examples

      config = ExMCP.ClientConfigEnhanced.new()
      |> ExMCP.ClientConfigEnhanced.add_validation_rule(
        :timeout,
        &validate_timeout_range/1,
        "Timeout must be between 1000 and 300000 ms"
      )
  """
  @spec add_validation_rule(t(), atom(), validation_rule(), String.t()) :: t()
  def add_validation_rule(config, field, rule_fun, error_message) do
    custom_options = Map.get(config, :custom_options, %{})
    validation_rules = Map.get(custom_options, :validation_rules, %{})

    field_rules = Map.get(validation_rules, field, [])
    updated_rules = field_rules ++ [{rule_fun, error_message}]

    updated_validation_rules = Map.put(validation_rules, field, updated_rules)
    updated_custom_options = Map.put(custom_options, :validation_rules, updated_validation_rules)

    %{config | custom_options: updated_custom_options}
  end

  @doc """
  Validates configuration with enhanced error reporting and custom rules.

  ## Examples

      case ExMCP.ClientConfigEnhanced.validate_with_rules(config) do
        :ok -> {:ok, client} = ExMCP.connect(config)
        {:error, detailed_errors} -> handle_validation_errors(detailed_errors)
      end
  """
  @spec validate_with_rules(t(), keyword()) :: :ok | {:error, [{atom(), String.t()}]}
  def validate_with_rules(config, custom_rules \\ []) do
    # Start with original validation
    case ClientConfig.validate(config) do
      :ok ->
        # Apply enhanced validation
        enhanced_validation_errors = validate_enhanced_rules(config, custom_rules)

        case enhanced_validation_errors do
          [] -> :ok
          errors -> {:error, errors}
        end

      {:error, basic_errors} ->
        # Convert basic errors to enhanced format
        enhanced_errors = Enum.map(basic_errors, fn error -> {:general, error} end)
        {:error, enhanced_errors}
    end
  end

  # ===== CONFIGURATION COMPOSITION & TEMPLATES =====

  @doc """
  Merges two configurations with specified strategy.

  ## Examples

      # Deep merge (default)
      merged = ExMCP.ClientConfigEnhanced.merge_configs(base, override)
      
      # Override strategy
      merged = ExMCP.ClientConfigEnhanced.merge_configs(base, override, :override)
  """
  @spec merge_configs(t(), t(), merge_strategy()) :: t()
  def merge_configs(base_config, override_config, strategy \\ :deep) do
    apply_merge_strategy(strategy, base_config, override_config)
  end

  @doc """
  Creates configuration from a template with customizations.

  ## Examples

      # Production API template
      config = ExMCP.ClientConfigEnhanced.from_template(:production_api, 
        url: "https://api.production.com",
        auth: [type: :bearer, token: "secret"]
      )
      
      # Development local template
      config = ExMCP.ClientConfigEnhanced.from_template(:development_local,
        command: ["python", "dev_server.py"]
      )
  """
  @spec from_template(template_name(), keyword()) :: t()
  def from_template(template_name, customizations \\ []) do
    base_template = get_template(template_name)
    apply_customizations(base_template, customizations)
  end

  # ===== DEVELOPER EXPERIENCE ENHANCEMENTS =====

  @doc """
  Quick HTTP configuration for common scenarios.

  ## Examples

      config = ExMCP.ClientConfigEnhanced.quick_http("https://api.example.com")
      config = ExMCP.ClientConfigEnhanced.quick_http("https://api.example.com", 
        auth: [type: :bearer, token: "secret"],
        timeout: 30_000
      )
  """
  @spec quick_http(String.t(), keyword()) :: t()
  def quick_http(url, opts \\ []) do
    ClientConfig.new(:http, [{:url, url} | opts])
  end

  @doc """
  Quick stdio configuration for common scenarios.

  ## Examples

      config = ExMCP.ClientConfigEnhanced.quick_stdio(["python", "server.py"])
      config = ExMCP.ClientConfigEnhanced.quick_stdio("mcp-server", 
        env: [{"DEBUG", "1"}],
        timeout: 15_000
      )
  """
  @spec quick_stdio(String.t() | [String.t()], keyword()) :: t()
  def quick_stdio(command, opts \\ []) do
    ClientConfig.new(:stdio, [{:command, command} | opts])
  end

  @doc """
  Pretty-prints configuration for debugging.

  ## Examples

      ExMCP.ClientConfigEnhanced.inspect_config(config)
      ExMCP.ClientConfigEnhanced.inspect_config(config, sections: [:transport, :auth])
  """
  @spec inspect_config(t(), keyword()) :: String.t()
  def inspect_config(config, opts \\ []) do
    sections = Keyword.get(opts, :sections, [:all])
    format = Keyword.get(opts, :format, :pretty)

    case format do
      :pretty -> format_config_pretty(config, sections)
      :compact -> format_config_compact(config, sections)
      :json -> format_config_json(config, sections)
    end
  end

  @doc """
  Shows differences between two configurations.

  ## Examples

      diff = ExMCP.ClientConfigEnhanced.diff_configs(old_config, new_config)
      IO.puts(diff)
  """
  @spec diff_configs(t(), t()) :: String.t()
  def diff_configs(config1, config2) do
    diff_maps(config1, config2, "")
  end

  # ===== DELEGATE REMAINING FUNCTIONS TO ORIGINAL MODULE =====

  # Delegate all functions not redefined above to the original ClientConfig
  defdelegate new(), to: ClientConfig
  defdelegate new(profile_or_transport), to: ClientConfig
  defdelegate new(transport_type, opts), to: ClientConfig
  defdelegate put_transport(config, transport_type, opts \\ []), to: ClientConfig
  defdelegate add_fallback(config, transport_type, opts \\ []), to: ClientConfig
  defdelegate put_timeout(config, opts), to: ClientConfig
  defdelegate put_auth(config, auth_type, opts \\ []), to: ClientConfig
  defdelegate put_custom(config, key, value), to: ClientConfig
  defdelegate validate(config), to: ClientConfig
  defdelegate to_client_opts(config), to: ClientConfig
  defdelegate get_all_transports(config), to: ClientConfig

  # ===== PRIVATE IMPLEMENTATION =====

  # Apply merge strategy helper
  defp apply_merge_strategy(:shallow, current_value, new_value) when is_map(current_value) do
    Map.merge(current_value || %{}, new_value)
  end

  defp apply_merge_strategy(:shallow, base_config, override_config) do
    Map.merge(base_config, override_config)
  end

  defp apply_merge_strategy(:deep, current_value, new_value)
       when is_map(current_value) and is_map(new_value) do
    deep_merge(current_value || %{}, new_value)
  end

  defp apply_merge_strategy(:deep, base_config, override_config) do
    deep_merge_configs(base_config, override_config)
  end

  defp apply_merge_strategy(:override, _current_value, new_value) do
    new_value
  end

  defp apply_merge_strategy(:preserve_existing, current_value, new_value)
       when is_map(current_value) do
    Map.merge(new_value, current_value || %{})
  end

  defp apply_merge_strategy(:preserve_existing, base_config, override_config) do
    Map.merge(override_config, base_config)
  end

  # Validation rule application
  defp apply_validation_rules(opts, rules) do
    # Apply built-in validation rules
    result =
      Enum.reduce_while(rules, {:ok, opts}, fn rule, {:ok, acc} ->
        apply_single_validation_rule(rule, acc)
      end)

    case result do
      {:ok, validated_opts} ->
        validated_opts

      {:error, reason} ->
        # Log the validation error but don't crash the process
        _ = Logger.warning("Validation failed: #{reason}")
        # Return original opts if validation fails
        opts
    end
  end

  defp apply_single_validation_rule(rule, opts) do
    validator = get_validator_for_rule(rule)

    case validator.(opts) do
      {:ok, validated_opts} -> {:cont, {:ok, validated_opts}}
      {:error, _} = error -> {:halt, error}
    end
  end

  defp get_validator_for_rule(:positive_integers), do: &validate_positive_integers/1
  defp get_validator_for_rule(:string_fields), do: &validate_string_fields/1
  defp get_validator_for_rule(:url_format), do: &validate_url_format/1
  defp get_validator_for_rule(_), do: fn opts -> {:ok, opts} end

  defp validate_positive_integers(opts) do
    Enum.reduce_while(opts, {:ok, opts}, fn {key, value}, {:ok, acc} ->
      if is_integer(value) and value <= 0 do
        {:halt, {:error, "#{key} must be a positive integer, got: #{value}"}}
      else
        {:cont, {:ok, acc}}
      end
    end)
  end

  defp validate_string_fields(opts) do
    string_fields = [:name, :version, :user_agent, :username, :password]

    Enum.reduce_while(opts, {:ok, opts}, fn {key, value}, {:ok, acc} ->
      if key in string_fields and not is_binary(value) do
        {:halt, {:error, "#{key} must be a string, got: #{inspect(value)}"}}
      else
        {:cont, {:ok, acc}}
      end
    end)
  end

  defp validate_url_format(opts) do
    url_fields = [:url, :authorization_endpoint, :token_endpoint]

    Enum.reduce_while(opts, {:ok, opts}, fn {key, value}, {:ok, acc} ->
      if key in url_fields and is_binary(value) do
        case URI.parse(value) do
          %URI{scheme: scheme} when scheme in ["http", "https"] ->
            {:cont, {:ok, acc}}

          _ ->
            {:halt, {:error, "#{key} must be a valid HTTP/HTTPS URL, got: #{value}"}}
        end
      else
        {:cont, {:ok, acc}}
      end
    end)
  end

  # Enhanced deep merge for observability config
  defp enhanced_deep_merge_observability(current, updates) do
    Enum.reduce(updates, current, fn {key, value}, acc ->
      case {Map.get(acc, key), value} do
        {existing, new_value} when is_map(existing) and is_list(new_value) ->
          Map.put(acc, key, Map.merge(existing, Map.new(new_value)))

        {existing, new_value} when is_map(existing) and is_map(new_value) ->
          Map.put(acc, key, Map.merge(existing, new_value))

        {_existing, new_value} ->
          Map.put(acc, key, new_value)
      end
    end)
  end

  # Environment configuration helpers
  defp apply_env_config(config, env_config) do
    Enum.reduce(env_config, config, fn {key, value}, acc ->
      case key do
        :transport -> ClientConfig.put_transport(acc, value[:type] || :http, value)
        :retry_policy -> put_retry_policy(acc, value)
        :timeout -> ClientConfig.put_timeout(acc, value)
        :auth -> ClientConfig.put_auth(acc, value[:type] || :none, value)
        :pool -> put_pool(acc, value)
        :observability -> put_observability(acc, value)
        :client_info -> put_client_info(acc, value)
        _ -> ClientConfig.put_custom(acc, key, value)
      end
    end)
  end

  defp default_env_mapping do
    %{
      "EXMCP_TRANSPORT_URL" => :url,
      "EXMCP_TIMEOUT_CONNECT" => {:timeouts, :connect},
      "EXMCP_TIMEOUT_REQUEST" => {:timeouts, :request},
      "EXMCP_RETRY_MAX_ATTEMPTS" => {:retry_policy, :max_attempts},
      "EXMCP_AUTH_TOKEN" => {:auth, :token},
      "EXMCP_AUTH_TYPE" => {:auth, :type},
      "EXMCP_POOL_SIZE" => {:pool, :size},
      "EXMCP_LOG_LEVEL" => {:observability, :logging, :level}
    }
  end

  defp apply_env_value(config, config_path, value) do
    parsed_value = parse_env_value(value)

    case config_path do
      :url ->
        # Special handling for URL - set on transport
        transport_type = (config.transport && config.transport.type) || :http
        ClientConfig.put_transport(config, transport_type, url: parsed_value)

      atom when is_atom(atom) ->
        # Simple field update
        Map.put(config, atom, parsed_value)

      {field, subfield} ->
        # Nested field update
        current_value = Map.get(config, field, %{})
        updated_value = Map.put(current_value, subfield, parsed_value)
        Map.put(config, field, updated_value)

      {field, subfield, subsubfield} ->
        # Deeply nested field update
        current_value = Map.get(config, field, %{})
        current_sub_value = Map.get(current_value, subfield, %{})
        updated_sub_value = Map.put(current_sub_value, subsubfield, parsed_value)
        updated_value = Map.put(current_value, subfield, updated_sub_value)
        Map.put(config, field, updated_value)
    end
  end

  defp parse_env_value(value) when is_binary(value) do
    cond do
      value =~ ~r/^\d+$/ -> String.to_integer(value)
      value in ["true", "false"] -> value == "true"
      value =~ ~r/^https?:\/\// -> value
      true -> value
    end
  end

  # Configuration templates
  defp get_template(template_name) do
    case template_name do
      :production_api ->
        ClientConfig.new(:production)
        |> ClientConfig.put_transport(:http, url: "https://api.production.com")
        |> ClientConfig.put_timeout(total: 600_000, connect: 10_000, request: 30_000)
        |> put_retry_policy(enabled: true, max_attempts: 5, backoff_type: :exponential)

      :development_local ->
        ClientConfig.new(:development)
        |> ClientConfig.put_transport(:stdio, command: "mcp-server")
        |> ClientConfig.put_timeout(total: 60_000, connect: 5_000, request: 15_000)

      :test_fast ->
        ClientConfig.new(:test)
        |> ClientConfig.put_transport(:http, url: "http://localhost:8080")
        |> ClientConfig.put_timeout(total: 15_000, connect: 1_000, request: 5_000)
        |> put_retry_policy(enabled: false)

      :custom ->
        ClientConfig.new()

      _ ->
        # Fallback for unknown templates
        ClientConfig.new()
    end
  end

  defp apply_customizations(config, customizations) do
    Enum.reduce(customizations, config, fn {key, value}, acc ->
      case key do
        :url ->
          # Extract current transport type
          transport_type = (acc.transport && acc.transport.type) || :http
          ClientConfig.put_transport(acc, transport_type, url: value)

        :transport ->
          ClientConfig.put_transport(acc, value[:type] || :http, value)

        :auth ->
          ClientConfig.put_auth(acc, value[:type] || :none, value)

        :timeout ->
          ClientConfig.put_timeout(acc, value)

        _ ->
          ClientConfig.put_custom(acc, key, value)
      end
    end)
  end

  # Enhanced validation
  defp validate_enhanced_rules(config, custom_rules) do
    validation_rules = get_validation_rules(config)
    all_rules = validation_rules ++ custom_rules

    Enum.reduce(all_rules, [], fn {field, rule_fun, error_message}, errors ->
      field_value = Map.get(config, field)

      case rule_fun.(field_value) do
        :ok -> errors
        {:error, _} -> [{field, error_message} | errors]
      end
    end)
  end

  defp get_validation_rules(config) do
    custom_options = Map.get(config, :custom_options, %{})

    Map.get(custom_options, :validation_rules, %{})
    |> Enum.flat_map(fn {field, rules} ->
      Enum.map(rules, fn {rule_fun, error_message} ->
        {field, rule_fun, error_message}
      end)
    end)
  end

  # Deep merge utilities
  defp deep_merge(map1, map2) when is_map(map1) and is_map(map2) do
    Map.merge(map1, map2, fn _key, v1, v2 ->
      if is_map(v1) and is_map(v2) do
        deep_merge(v1, v2)
      else
        v2
      end
    end)
  end

  defp deep_merge(_, value), do: value

  defp deep_merge_configs(config1, config2) do
    Map.merge(config1, config2, fn _key, v1, v2 ->
      if is_map(v1) and is_map(v2) do
        deep_merge(v1, v2)
      else
        v2
      end
    end)
  end

  # Configuration formatting for debugging
  defp format_config_pretty(config, sections) do
    sections_to_show = if :all in sections, do: Map.keys(config), else: sections

    sections_to_show
    |> Enum.filter(&Map.has_key?(config, &1))
    |> Enum.map_join("\n\n", fn section ->
      value = Map.get(config, section)
      "#{section}:\n#{format_value(value, "  ")}"
    end)
  end

  defp format_config_compact(config, _sections) do
    inspect(config, pretty: true, limit: :infinity)
  end

  defp format_config_json(config, _sections) do
    # Convert atoms to strings for JSON serialization
    json_ready = convert_atoms_to_strings(config)
    Jason.encode!(json_ready, pretty: true)
  end

  defp format_value(value, indent) when is_map(value) do
    Enum.map_join(value, "\n", fn {k, v} ->
      "#{indent}#{k}: #{format_value(v, indent <> "  ")}"
    end)
  end

  defp format_value(value, _indent), do: inspect(value)

  defp convert_atoms_to_strings(term) when is_map(term) do
    term
    |> Enum.map(fn {k, v} -> {to_string(k), convert_atoms_to_strings(v)} end)
    |> Enum.into(%{})
  end

  defp convert_atoms_to_strings(term) when is_list(term) do
    Enum.map(term, &convert_atoms_to_strings/1)
  end

  defp convert_atoms_to_strings(term) when is_atom(term) do
    to_string(term)
  end

  defp convert_atoms_to_strings(term), do: term

  # Configuration diff utility
  defp diff_maps(map1, map2, prefix) do
    all_keys = MapSet.union(MapSet.new(Map.keys(map1)), MapSet.new(Map.keys(map2)))

    Enum.reduce(all_keys, [], fn key, acc ->
      path = if prefix == "", do: to_string(key), else: "#{prefix}.#{key}"

      case {Map.get(map1, key), Map.get(map2, key)} do
        {nil, value} ->
          ["+ #{path}: #{inspect(value)}" | acc]

        {value, nil} ->
          ["- #{path}: #{inspect(value)}" | acc]

        {same, same} ->
          acc

        {old_val, new_val} when is_map(old_val) and is_map(new_val) ->
          [diff_maps(old_val, new_val, path) | acc]

        {old_val, new_val} ->
          ["~ #{path}: #{inspect(old_val)} -> #{inspect(new_val)}" | acc]
      end
    end)
    |> List.flatten()
    |> Enum.reverse()
    |> Enum.join("\n")
  end
end
