defmodule ExMCP.ClientConfigEnhancedTest do
  use ExUnit.Case, async: true

  alias ExMCP.ClientConfig
  alias ExMCP.ClientConfigEnhanced

  describe "macro-generated configuration setters" do
    test "put_retry_policy/2 uses enhanced macro-generated setter" do
      config = ClientConfigEnhanced.new()

      updated_config =
        ClientConfigEnhanced.put_retry_policy(config,
          max_attempts: 5,
          base_interval: 2000,
          backoff_type: :exponential
        )

      assert updated_config.retry_policy.max_attempts == 5
      assert updated_config.retry_policy.base_interval == 2000
      assert updated_config.retry_policy.backoff_type == :exponential
      # Should preserve existing fields
      assert updated_config.retry_policy.enabled == true
    end

    test "put_pool/2 uses enhanced macro-generated setter" do
      config = ClientConfigEnhanced.new()

      updated_config =
        ClientConfigEnhanced.put_pool(config,
          enabled: true,
          size: 15,
          max_overflow: 8
        )

      assert updated_config.pool.enabled == true
      assert updated_config.pool.size == 15
      assert updated_config.pool.max_overflow == 8
      # Should preserve existing fields
      assert updated_config.pool.checkout_timeout == 5_000
    end

    test "put_client_info/2 uses enhanced macro-generated setter" do
      config = ClientConfigEnhanced.new()

      updated_config =
        ClientConfigEnhanced.put_client_info(config,
          name: "Test Client",
          version: "2.0.0"
        )

      assert updated_config.client_info.name == "Test Client"
      assert updated_config.client_info.version == "2.0.0"
      # Should generate updated user_agent
      assert updated_config.client_info.user_agent =~ "Test Client/2.0.0"
    end

    test "macro-generated setters accept both keyword lists and maps" do
      config = ClientConfigEnhanced.new()

      # Test with keyword list
      config1 = ClientConfigEnhanced.put_retry_policy(config, max_attempts: 3)
      assert config1.retry_policy.max_attempts == 3

      # Test with map
      config2 = ClientConfigEnhanced.put_retry_policy(config, %{max_attempts: 4})
      assert config2.retry_policy.max_attempts == 4
    end

    test "enhanced observability configuration with deep merging" do
      config = ClientConfigEnhanced.new()

      updated_config =
        ClientConfigEnhanced.put_observability(config,
          logging: [level: :warn, format: :json],
          telemetry: [enabled: true, prefix: [:custom]]
        )

      assert updated_config.observability.logging.level == :warn
      assert updated_config.observability.logging.format == :json
      # Should preserve existing logging fields
      assert updated_config.observability.logging.enabled == true

      assert updated_config.observability.telemetry.enabled == true
      assert updated_config.observability.telemetry.prefix == [:custom]
    end
  end

  describe "environment configuration loading" do
    test "from_env/3 loads configuration from Application environment" do
      # Set up test application config
      Application.put_env(:test_app, :mcp_config,
        transport: [type: :http, url: "https://test.example.com"],
        retry_policy: [max_attempts: 10],
        auth: [type: :bearer, token: "test-token"]
      )

      config = ClientConfigEnhanced.from_env(:test_app, :mcp_config)

      assert config.transport.url == "https://test.example.com"
      assert config.retry_policy.max_attempts == 10
      assert config.auth.type == :bearer
      assert config.auth.token == "test-token"

      # Cleanup
      Application.delete_env(:test_app, :mcp_config)
    end

    test "from_env/3 uses default profile when no config found" do
      config = ClientConfigEnhanced.from_env(:nonexistent_app, :nonexistent_key)

      # Should have development profile defaults
      assert config.profile == :development
      assert config.timeouts.connect == 5_000
    end

    test "from_env/3 with custom default profile" do
      config =
        ClientConfigEnhanced.from_env(:nonexistent_app, :nonexistent_key, default: :production)

      assert config.profile == :production
      assert config.timeouts.connect == 10_000
    end

    test "from_env_vars/1 loads from system environment variables" do
      # Set up test environment variables
      System.put_env("TEST_EXMCP_TRANSPORT_URL", "https://env.example.com")
      System.put_env("TEST_EXMCP_TIMEOUT_CONNECT", "8000")
      System.put_env("TEST_EXMCP_RETRY_MAX_ATTEMPTS", "7")

      mapping = %{
        "TEST_EXMCP_TRANSPORT_URL" => :url,
        "TEST_EXMCP_TIMEOUT_CONNECT" => {:timeouts, :connect},
        "TEST_EXMCP_RETRY_MAX_ATTEMPTS" => {:retry_policy, :max_attempts}
      }

      config = ClientConfigEnhanced.from_env_vars(mapping)

      assert config.transport.url == "https://env.example.com"
      assert config.timeouts.connect == 8000
      assert config.retry_policy.max_attempts == 7

      # Cleanup
      System.delete_env("TEST_EXMCP_TRANSPORT_URL")
      System.delete_env("TEST_EXMCP_TIMEOUT_CONNECT")
      System.delete_env("TEST_EXMCP_RETRY_MAX_ATTEMPTS")
    end

    test "from_env_vars/0 uses default environment mapping" do
      # Set up some default environment variables
      System.put_env("EXMCP_TRANSPORT_URL", "https://default.example.com")
      System.put_env("EXMCP_AUTH_TOKEN", "default-token")

      config = ClientConfigEnhanced.from_env_vars()

      assert config.transport.url == "https://default.example.com"
      assert config.auth.token == "default-token"

      # Cleanup
      System.delete_env("EXMCP_TRANSPORT_URL")
      System.delete_env("EXMCP_AUTH_TOKEN")
    end

    test "merge_env_overrides/2 applies environment overrides" do
      base_config = ClientConfigEnhanced.new(:production)

      # Set up test environment variables
      System.put_env("OVERRIDE_URL", "https://override.example.com")
      System.put_env("OVERRIDE_TIMEOUT", "25000")

      overrides = %{
        "OVERRIDE_URL" => :url,
        "OVERRIDE_TIMEOUT" => {:timeouts, :request}
      }

      config = ClientConfigEnhanced.merge_env_overrides(base_config, overrides)

      assert config.transport.url == "https://override.example.com"
      assert config.timeouts.request == 25000
      # Should preserve other production settings
      assert config.retry_policy.max_attempts == 5

      # Cleanup
      System.delete_env("OVERRIDE_URL")
      System.delete_env("OVERRIDE_TIMEOUT")
    end
  end

  describe "enhanced validation system" do
    test "add_validation_rule/4 adds custom validation rules" do
      timeout_validator = fn timeout ->
        if is_integer(timeout) and timeout >= 1000 and timeout <= 300_000 do
          :ok
        else
          {:error, "invalid timeout"}
        end
      end

      config =
        ClientConfigEnhanced.new()
        |> ClientConfigEnhanced.add_validation_rule(
          :timeout,
          timeout_validator,
          "Timeout must be between 1000 and 300000 ms"
        )

      # Check that validation rule was added
      validation_rules = get_in(config.custom_options, [:validation_rules, :timeout])
      assert length(validation_rules) == 1
      assert {_rule_fun, error_msg} = hd(validation_rules)
      assert error_msg == "Timeout must be between 1000 and 300000 ms"
    end

    test "validate_with_rules/2 applies enhanced validation" do
      config =
        ClientConfigEnhanced.new()
        |> ClientConfig.put_transport(:http, url: "https://example.com")

      # Should pass basic validation
      assert ClientConfigEnhanced.validate_with_rules(config) == :ok

      # Test with invalid configuration
      invalid_config = %{config | retry_policy: %{config.retry_policy | max_attempts: -1}}
      assert {:error, errors} = ClientConfigEnhanced.validate_with_rules(invalid_config)
      assert length(errors) > 0
    end

    test "validate_with_rules/2 provides detailed error information" do
      timeout_validator = fn _timeout -> {:error, "custom error"} end

      config =
        ClientConfigEnhanced.new()
        |> ClientConfig.put_transport(:http, url: "https://example.com")
        |> ClientConfigEnhanced.add_validation_rule(
          :timeout,
          timeout_validator,
          "Custom timeout error"
        )

      custom_rules = [{:timeout, timeout_validator, "Custom timeout error"}]

      case ClientConfigEnhanced.validate_with_rules(config, custom_rules) do
        {:error, errors} ->
          assert Enum.any?(errors, fn {field, message} ->
                   field == :timeout and message == "Custom timeout error"
                 end)

        :ok ->
          flunk("Expected validation to fail")
      end
    end
  end

  describe "configuration composition and templates" do
    test "merge_configs/3 with different strategies" do
      base_config = ClientConfigEnhanced.new(:development)
      override_config = ClientConfigEnhanced.new(:production)

      # Test deep merge (default)
      deep_merged = ClientConfigEnhanced.merge_configs(base_config, override_config)
      assert deep_merged.profile == :production
      # From production
      assert deep_merged.timeouts.connect == 10_000

      # Test override strategy
      override_merged =
        ClientConfigEnhanced.merge_configs(base_config, override_config, :override)

      assert override_merged == override_config

      # Test preserve existing strategy
      preserve_merged =
        ClientConfigEnhanced.merge_configs(base_config, override_config, :preserve_existing)

      # Preserved from base
      assert preserve_merged.profile == :development
    end

    test "from_template/2 creates configuration from templates" do
      # Test production API template
      prod_config =
        ClientConfigEnhanced.from_template(:production_api,
          url: "https://my-api.com",
          auth: [type: :bearer, token: "secret"]
        )

      assert prod_config.profile == :production
      assert prod_config.transport.url == "https://my-api.com"
      assert prod_config.auth.type == :bearer
      assert prod_config.auth.token == "secret"
      # From production profile
      assert prod_config.retry_policy.max_attempts == 5

      # Test development local template
      dev_config =
        ClientConfigEnhanced.from_template(:development_local,
          command: ["node", "server.js"]
        )

      assert dev_config.profile == :development
      assert dev_config.transport.type == :stdio
      assert dev_config.transport.command == ["node", "server.js"]

      # Test test fast template
      test_config = ClientConfigEnhanced.from_template(:test_fast)

      assert test_config.profile == :test
      assert test_config.transport.url == "http://localhost:8080"
      assert test_config.retry_policy.enabled == false
    end
  end

  describe "developer experience enhancements" do
    test "quick_http/2 creates HTTP configuration quickly" do
      config = ClientConfigEnhanced.quick_http("https://api.example.com")

      assert config.transport.type == :http
      assert config.transport.url == "https://api.example.com"

      # With additional options
      config_with_opts =
        ClientConfigEnhanced.quick_http("https://api.example.com",
          auth: [type: :bearer, token: "secret"],
          timeout: 30_000
        )

      assert config_with_opts.transport.url == "https://api.example.com"
      # Note: Additional options are handled by the underlying ClientConfig.new/2
    end

    test "quick_stdio/2 creates stdio configuration quickly" do
      config = ClientConfigEnhanced.quick_stdio(["python", "server.py"])

      assert config.transport.type == :stdio
      assert config.transport.command == ["python", "server.py"]

      # With string command
      config_string = ClientConfigEnhanced.quick_stdio("mcp-server")

      assert config_string.transport.type == :stdio
      assert config_string.transport.command == "mcp-server"
    end

    test "inspect_config/2 formats configuration for debugging" do
      config = ClientConfigEnhanced.new()

      # Test pretty format (default)
      pretty_output = ClientConfigEnhanced.inspect_config(config)
      assert is_binary(pretty_output)
      assert pretty_output =~ "transport:"
      assert pretty_output =~ "retry_policy:"

      # Test compact format
      compact_output = ClientConfigEnhanced.inspect_config(config, format: :compact)
      assert is_binary(compact_output)
      assert compact_output =~ "%ExMCP.ClientConfig{"

      # Test with specific sections
      transport_only = ClientConfigEnhanced.inspect_config(config, sections: [:transport])
      assert transport_only =~ "transport:"
      refute transport_only =~ "retry_policy:"
    end

    test "diff_configs/2 shows differences between configurations" do
      config1 = ClientConfigEnhanced.new(:development)
      config2 = ClientConfigEnhanced.new(:production)

      diff = ClientConfigEnhanced.diff_configs(config1, config2)

      assert is_binary(diff)
      assert diff =~ "profile:"
      assert diff =~ ":development"
      assert diff =~ ":production"
    end
  end

  describe "backward compatibility" do
    test "delegates to original ClientConfig for unchanged functions" do
      # Test that original functions still work
      config = ClientConfigEnhanced.new()

      # Should delegate to original ClientConfig.put_transport/3
      updated_config = ClientConfigEnhanced.put_transport(config, :http, url: "https://test.com")
      assert updated_config.transport.url == "https://test.com"

      # Should delegate to original ClientConfig.put_timeout/2
      timeout_config = ClientConfigEnhanced.put_timeout(config, connect: 15_000)
      assert timeout_config.timeouts.connect == 15_000

      # Should delegate to original ClientConfig.validate/1 - use config with valid URL
      assert ClientConfigEnhanced.validate(updated_config) == :ok

      # Should delegate to original ClientConfig.to_client_opts/1
      opts = ClientConfigEnhanced.to_client_opts(config)
      assert is_list(opts)
      assert Keyword.has_key?(opts, :transport)
    end

    test "maintains full API compatibility with original ClientConfig" do
      # Create configs with both modules using same parameters
      original_config =
        ClientConfig.new(:production)
        |> ClientConfig.put_transport(:http, url: "https://api.example.com")
        |> ClientConfig.put_timeout(connect: 8000, request: 20000)
        |> ClientConfig.put_auth(:bearer, token: "test-token")

      enhanced_config =
        ClientConfigEnhanced.new(:production)
        |> ClientConfigEnhanced.put_transport(:http, url: "https://api.example.com")
        |> ClientConfigEnhanced.put_timeout(connect: 8000, request: 20000)
        |> ClientConfigEnhanced.put_auth(:bearer, token: "test-token")

      # Should have equivalent core fields
      assert original_config.profile == enhanced_config.profile
      assert original_config.transport == enhanced_config.transport
      assert original_config.timeouts == enhanced_config.timeouts
      assert original_config.auth == enhanced_config.auth

      # Should produce equivalent client options
      original_opts = ClientConfig.to_client_opts(original_config)
      enhanced_opts = ClientConfigEnhanced.to_client_opts(enhanced_config)

      # Filter out any enhanced-specific options for comparison
      core_keys = [:transport, :url, :timeout, :request_timeout, :headers]
      original_core = Keyword.take(original_opts, core_keys)
      enhanced_core = Keyword.take(enhanced_opts, core_keys)

      assert original_core == enhanced_core
    end
  end

  describe "error handling and edge cases" do
    test "handles invalid environment variable values gracefully" do
      # Set up invalid environment variables
      System.put_env("INVALID_TIMEOUT", "not-a-number")
      System.put_env("INVALID_BOOLEAN", "maybe")

      mapping = %{
        "INVALID_TIMEOUT" => {:timeouts, :connect},
        "INVALID_BOOLEAN" => {:retry_policy, :enabled}
      }

      # Should not crash, but may have unexpected values
      config = ClientConfigEnhanced.from_env_vars(mapping)
      assert %ClientConfig{} = config

      # Cleanup
      System.delete_env("INVALID_TIMEOUT")
      System.delete_env("INVALID_BOOLEAN")
    end

    test "handles missing template gracefully" do
      # Should fall back to custom template (basic config)
      config = ClientConfigEnhanced.from_template(:nonexistent_template)
      assert %ClientConfig{} = config
    end

    test "validates input types for macro-generated setters" do
      config = ClientConfigEnhanced.new()

      # Test invalid input types trigger appropriate errors
      assert_raise ArgumentError, fn ->
        ClientConfigEnhanced.put_retry_policy(config, max_attempts: -1)
      end

      assert_raise ArgumentError, fn ->
        ClientConfigEnhanced.put_client_info(config, name: 123)
      end
    end
  end
end
