defmodule ExMCP.ClientConfig.Macros do
  @moduledoc """
  Macros for generating configuration setter functions to eliminate code duplication.
  """

  @doc """
  Macro to generate configuration setter functions with common patterns.

  This eliminates the repetitive Map.merge patterns found throughout the original module.
  """
  defmacro defconfig_setter(field_name, options \\ []) do
    function_name = :"put_#{field_name}"
    default_merge_strategy = Keyword.get(options, :merge_strategy, :shallow)
    validation_rules = Keyword.get(options, :validation, [])

    quote do
      @doc """
      Sets the #{unquote(field_name)} configuration using enhanced macro-generated setter.

      Automatically handles merging and validation based on field requirements.
      """
      @spec unquote(function_name)(t(), keyword() | map()) :: t()
      def unquote(function_name)(config, opts) when is_list(opts) do
        unquote(function_name)(config, Map.new(opts))
      end

      def unquote(function_name)(config, opts) when is_map(opts) do
        # Apply validation rules if specified
        validated_opts = apply_validation_rules(opts, unquote(validation_rules))

        # Get current field value
        current_value = Map.get(config, unquote(field_name))

        # Merge with appropriate strategy
        merged_value =
          apply_merge_strategy(
            unquote(default_merge_strategy),
            current_value,
            validated_opts
          )

        # Update config
        Map.put(config, unquote(field_name), merged_value)
      end
    end
  end
end
