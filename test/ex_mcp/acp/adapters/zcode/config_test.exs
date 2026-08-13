defmodule ExMCP.ACP.Adapters.ZCode.ConfigTest do
  use ExUnit.Case, async: true

  alias ExMCP.ACP.Adapters.ZCode.Config

  describe "modes" do
    test "returns all five ZCode modes" do
      modes = Config.modes()
      ids = Enum.map(modes, & &1["id"])

      assert "plan" in ids
      assert "build" in ids
      assert "edit" in ids
      assert "auto" in ids
      assert "yolo" in ids
    end

    test "each mode has name and description" do
      for mode <- Config.modes() do
        assert is_binary(mode["name"])
        assert is_binary(mode["description"])
      end
    end
  end

  describe "default_mode/0" do
    test "returns build" do
      assert Config.default_mode() == "build"
    end
  end

  describe "normalize_mode_id/1" do
    test "nil returns default" do
      assert Config.normalize_mode_id(nil) == "build"
    end

    test "string passes through" do
      assert Config.normalize_mode_id("plan") == "plan"
    end

    test "atom is converted to string" do
      assert Config.normalize_mode_id(:yolo) == "yolo"
    end
  end

  describe "normalize_requested_mode/1" do
    test "valid mode returns ok" do
      assert {:ok, "plan"} = Config.normalize_requested_mode("plan")
      assert {:ok, "build"} = Config.normalize_requested_mode(nil)
    end

    test "invalid mode returns error" do
      assert {:error, _} = Config.normalize_requested_mode("bogus")
    end
  end

  describe "config_options/1" do
    test "includes mode option" do
      options = Config.config_options(%{mode_id: "build", models: []})
      mode = Enum.find(options, &(&1["id"] == "mode"))
      assert mode != nil
      assert mode["category"] == "mode"
      assert mode["currentValue"] == "build"
    end

    test "includes thought_level option" do
      options = Config.config_options(%{mode_id: "build", models: []})
      thought = Enum.find(options, &(&1["id"] == "thought_level"))
      assert thought != nil
      assert thought["category"] == "thought_level"
      assert thought["currentValue"] == "medium"
    end

    test "includes model option when models available" do
      models = [
        %{
          "ref" => %{"providerId" => "anthropic", "modelId" => "claude-sonnet"},
          "label" => "Sonnet"
        }
      ]

      options =
        Config.config_options(%{
          mode_id: "build",
          models: models,
          current_model: %{"providerId" => "anthropic", "modelId" => "claude-sonnet"}
        })

      model = Enum.find(options, &(&1["id"] == "model"))
      assert model != nil
      assert model["currentValue"] == "anthropic/claude-sonnet"
    end

    test "omits model option when no models" do
      options = Config.config_options(%{mode_id: "build", models: []})
      model = Enum.find(options, &(&1["id"] == "model"))
      assert model == nil
    end
  end
end
