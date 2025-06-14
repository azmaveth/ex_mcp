defmodule ExMCP.Transport.Beam.ZeroCopyTest do
  @moduledoc """
  Tests for zero-copy optimization in BEAM transport.

  These tests verify that large payloads are efficiently handled using
  references instead of copying, and that the system maintains proper
  cleanup and error handling.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Transport.Beam.ZeroCopy

  describe "reference identification" do
    test "identifies zero-copy references correctly" do
      ref = make_ref()

      assert ZeroCopy.is_zero_copy_ref?({:zero_copy_ref, ref})
      refute ZeroCopy.is_zero_copy_ref?(ref)
      refute ZeroCopy.is_zero_copy_ref?("not a ref")
      refute ZeroCopy.is_zero_copy_ref?(%{data: "map"})
    end
  end

  describe "payload storage and retrieval (integration)" do
    setup do
      # Start a test-specific ZeroCopy process
      opts = [threshold: 1024, max_refs: 10, ttl_ms: 5000]
      {:ok, pid} = start_supervised({ZeroCopy, opts})
      %{zero_copy: pid}
    end

    test "handles small payloads without storage", %{zero_copy: _pid} do
      small_data = "small payload"
      assert {:passthrough, ^small_data} = ZeroCopy.store_payload(small_data)
    end

    test "stores and retrieves large payloads", %{zero_copy: _pid} do
      large_data = :crypto.strong_rand_bytes(2048)

      assert {:ok, ref} = ZeroCopy.store_payload(large_data)
      assert is_reference(ref)

      # Should be able to fetch the same data
      assert {:ok, fetched_data} = ZeroCopy.fetch_payload(ref)
      assert fetched_data == large_data
    end

    test "returns error for non-existent references", %{zero_copy: _pid} do
      fake_ref = make_ref()
      assert {:error, :ref_not_found} = ZeroCopy.fetch_payload(fake_ref)
    end

    test "handles invalid payload types", %{zero_copy: _pid} do
      assert {:error, :invalid_payload_type} = ZeroCopy.store_payload(%{not: "binary"})
      assert {:error, :invalid_payload_type} = ZeroCopy.store_payload(123)
    end

    test "handles invalid references gracefully", %{zero_copy: _pid} do
      assert {:error, :invalid_reference} = ZeroCopy.fetch_payload("not_a_ref")
      assert {:error, :invalid_reference} = ZeroCopy.fetch_payload(123)
    end
  end

  describe "message processing (integration)" do
    setup do
      opts = [threshold: 1024, max_refs: 10, ttl_ms: 5000]
      {:ok, pid} = start_supervised({ZeroCopy, opts})
      %{zero_copy: pid}
    end

    test "processes messages with zero-copy references", %{zero_copy: _pid} do
      large_data = :crypto.strong_rand_bytes(2048)
      {:ok, ref} = ZeroCopy.store_payload(large_data)

      message = %{
        "method" => "test",
        "params" => %{
          "data" => {:zero_copy_ref, ref},
          "other" => "normal data"
        }
      }

      assert {:ok, processed} = ZeroCopy.process_message(message)
      assert processed["params"]["data"] == large_data
      assert processed["params"]["other"] == "normal data"
      assert processed["method"] == "test"
    end

    test "handles messages without zero-copy references", %{zero_copy: _pid} do
      message = %{
        "method" => "normal",
        "params" => %{"data" => "regular data"}
      }

      assert {:ok, ^message} = ZeroCopy.process_message(message)
    end

    test "prepares messages by storing large payloads as references", %{zero_copy: _pid} do
      large_data = :crypto.strong_rand_bytes(2048)
      small_data = "small"

      message = %{
        "large_field" => large_data,
        "small_field" => small_data
      }

      assert {:ok, prepared} = ZeroCopy.prepare_message(message)

      # Large field should be converted to reference
      assert {:zero_copy_ref, ref} = prepared["large_field"]
      assert is_reference(ref)

      # Small field should remain unchanged
      assert prepared["small_field"] == small_data

      # Should be able to fetch the original data
      assert {:ok, fetched} = ZeroCopy.fetch_payload(ref)
      assert fetched == large_data
    end
  end

  describe "threshold configuration (integration)" do
    setup do
      opts = [threshold: 5000, max_refs: 10, ttl_ms: 5000]
      {:ok, pid} = start_supervised({ZeroCopy, opts})
      %{zero_copy: pid}
    end

    test "respects custom thresholds", %{zero_copy: _pid} do
      # Data below threshold should pass through
      medium_data = :crypto.strong_rand_bytes(3000)
      assert {:passthrough, ^medium_data} = ZeroCopy.store_payload(medium_data)

      # Data above threshold should be stored
      large_data = :crypto.strong_rand_bytes(8000)
      assert {:ok, ref} = ZeroCopy.store_payload(large_data)
      assert is_reference(ref)
    end
  end

  describe "error handling" do
    test "handles invalid message types for processing" do
      assert {:error, :invalid_message_type} = ZeroCopy.process_message("not a map")
      assert {:error, :invalid_message_type} = ZeroCopy.process_message(123)
    end

    test "handles invalid message types for preparation" do
      assert {:error, :invalid_message_type} = ZeroCopy.prepare_message("not a map")
      assert {:error, :invalid_message_type} = ZeroCopy.prepare_message(123)
    end
  end

  describe "round-trip processing (integration)" do
    setup do
      opts = [threshold: 1024, max_refs: 20, ttl_ms: 10000]
      {:ok, pid} = start_supervised({ZeroCopy, opts})
      %{zero_copy: pid}
    end

    test "complete message processing workflow", %{zero_copy: _pid} do
      # Create a complex message with mixed payload sizes
      original_message = %{
        "method" => "complex_operation",
        "params" => %{
          "small_config" => %{"setting" => "value"},
          "large_dataset" => :crypto.strong_rand_bytes(5000),
          # Below threshold
          "medium_payload" => :crypto.strong_rand_bytes(800),
          "nested" => %{
            "another_large" => :crypto.strong_rand_bytes(3000),
            "small" => "text"
          }
        }
      }

      # Prepare for transmission
      {:ok, prepared} = ZeroCopy.prepare_message(original_message)

      # Verify large payloads became references
      assert {:zero_copy_ref, _} = prepared["params"]["large_dataset"]
      assert {:zero_copy_ref, _} = prepared["params"]["nested"]["another_large"]

      # Verify small payloads remained unchanged
      assert prepared["params"]["medium_payload"] == original_message["params"]["medium_payload"]
      assert prepared["params"]["small_config"] == original_message["params"]["small_config"]

      assert prepared["params"]["nested"]["small"] ==
               original_message["params"]["nested"]["small"]

      # Process on receiving end
      {:ok, processed} = ZeroCopy.process_message(prepared)

      # Should match original
      assert processed["method"] == original_message["method"]
      assert processed["params"]["large_dataset"] == original_message["params"]["large_dataset"]

      assert processed["params"]["nested"]["another_large"] ==
               original_message["params"]["nested"]["another_large"]

      assert processed["params"]["small_config"] == original_message["params"]["small_config"]
    end
  end
end
