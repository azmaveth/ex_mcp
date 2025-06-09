defmodule ExMCP.OAuthPKCETest do
  @moduledoc """
  Comprehensive test suite for PKCE (Proof Key for Code Exchange) implementation.

  Tests the PKCE implementation according to RFC 7636 and MCP specification requirements.
  PKCE is mandatory for all OAuth 2.1 authorization code flows to prevent authorization
  code interception attacks.

  This test suite is designed to initially fail until the complete OAuth implementation
  is in place, following TDD principles.
  """

  use ExUnit.Case, async: true

  alias ExMCP.Authorization.PKCE

  describe "PKCE Challenge Generation" do
    test "generates code verifier and challenge successfully" do
      assert {:ok, verifier, challenge} = PKCE.generate_challenge()

      # Verify verifier properties
      assert is_binary(verifier)
      assert String.length(verifier) >= 43
      assert String.length(verifier) <= 128

      # Verify challenge properties  
      assert is_binary(challenge)
      assert String.length(challenge) > 0

      # Verify challenge is base64url encoded (no padding)
      refute String.contains?(challenge, "=")
      refute String.contains?(challenge, "+")
      refute String.contains?(challenge, "/")

      # Verify verifier contains only unreserved characters
      assert Regex.match?(~r/^[A-Za-z0-9\-._~]+$/, verifier)
    end

    test "generates unique challenges on each call" do
      {:ok, verifier1, challenge1} = PKCE.generate_challenge()
      {:ok, verifier2, challenge2} = PKCE.generate_challenge()
      {:ok, verifier3, challenge3} = PKCE.generate_challenge()

      # All verifiers should be unique
      assert verifier1 != verifier2
      assert verifier2 != verifier3
      assert verifier1 != verifier3

      # All challenges should be unique
      assert challenge1 != challenge2
      assert challenge2 != challenge3
      assert challenge1 != challenge3
    end

    test "generates cryptographically secure verifiers" do
      # Generate multiple verifiers and verify they have good entropy
      verifiers =
        for _i <- 1..100 do
          {:ok, verifier, _challenge} = PKCE.generate_challenge()
          verifier
        end

      # All should be unique (extremely high probability with proper RNG)
      unique_verifiers = Enum.uniq(verifiers)
      assert length(unique_verifiers) == 100

      # Verify length distribution is consistent
      lengths = Enum.map(verifiers, &String.length/1)
      assert Enum.all?(lengths, fn len -> len >= 43 and len <= 128 end)
    end

    test "verifier meets RFC 7636 length requirements" do
      {:ok, verifier, _challenge} = PKCE.generate_challenge()

      length = String.length(verifier)
      assert length >= 43, "Verifier too short: #{length} characters"
      assert length <= 128, "Verifier too long: #{length} characters"
    end

    test "verifier contains only unreserved URI characters" do
      {:ok, verifier, _challenge} = PKCE.generate_challenge()

      # RFC 7636: unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
      assert Regex.match?(~r/^[A-Za-z0-9\-._~]+$/, verifier)

      # Should not contain reserved characters
      refute String.contains?(verifier, " ")
      refute String.contains?(verifier, "/")
      refute String.contains?(verifier, "?")
      refute String.contains?(verifier, "#")
      refute String.contains?(verifier, "[")
      refute String.contains?(verifier, "]")
      refute String.contains?(verifier, "@")
      refute String.contains?(verifier, "!")
      refute String.contains?(verifier, "$")
      refute String.contains?(verifier, "&")
      refute String.contains?(verifier, "'")
      refute String.contains?(verifier, "(")
      refute String.contains?(verifier, ")")
      refute String.contains?(verifier, "*")
      refute String.contains?(verifier, "+")
      refute String.contains?(verifier, ",")
      refute String.contains?(verifier, ";")
      refute String.contains?(verifier, "=")
    end

    test "challenge is base64url encoded without padding" do
      {:ok, _verifier, challenge} = PKCE.generate_challenge()

      # Should be base64url encoded (no padding)
      refute String.contains?(challenge, "=")
      refute String.contains?(challenge, "+")
      refute String.contains?(challenge, "/")

      # Should contain only base64url characters
      assert Regex.match?(~r/^[A-Za-z0-9\-_]+$/, challenge)

      # Should be 43 characters (256 bits encoded in base64url)
      assert String.length(challenge) == 43
    end

    test "challenge is SHA256 hash of verifier" do
      {:ok, verifier, challenge} = PKCE.generate_challenge()

      # Manually compute expected challenge
      expected_challenge =
        :crypto.hash(:sha256, verifier)
        |> Base.url_encode64(padding: false)

      assert challenge == expected_challenge
    end

    test "handles edge cases gracefully" do
      # Test that function doesn't crash under various conditions
      # Multiple rapid calls
      results =
        for _i <- 1..10 do
          PKCE.generate_challenge()
        end

      assert Enum.all?(results, fn result ->
               match?({:ok, _verifier, _challenge}, result)
             end)
    end
  end

  describe "PKCE Challenge Verification" do
    test "verifies correct challenge successfully" do
      {:ok, verifier, challenge} = PKCE.generate_challenge()

      assert :ok = PKCE.verify_challenge(verifier, challenge)
    end

    test "rejects incorrect challenge" do
      {:ok, verifier, _challenge} = PKCE.generate_challenge()
      wrong_challenge = "incorrect_challenge_value"

      assert {:error, :invalid_challenge} = PKCE.verify_challenge(verifier, wrong_challenge)
    end

    test "rejects challenge from different verifier" do
      {:ok, verifier1, _challenge1} = PKCE.generate_challenge()
      {:ok, _verifier2, challenge2} = PKCE.generate_challenge()

      # verifier1 should not match challenge2
      assert {:error, :invalid_challenge} = PKCE.verify_challenge(verifier1, challenge2)
    end

    test "handles malformed challenge gracefully" do
      {:ok, verifier, _challenge} = PKCE.generate_challenge()

      # Various malformed challenges
      malformed_challenges = [
        "",
        "invalid characters!@#$%",
        "too_short",
        # too long
        String.duplicate("a", 200),
        "contains spaces and symbols!",
        nil
      ]

      Enum.each(malformed_challenges, fn malformed ->
        result = PKCE.verify_challenge(verifier, malformed)
        assert match?({:error, :invalid_challenge}, result)
      end)
    end

    test "handles malformed verifier gracefully" do
      {:ok, _verifier, challenge} = PKCE.generate_challenge()

      # Various malformed verifiers
      malformed_verifiers = [
        "",
        "invalid characters!@#$%",
        "too_short",
        # too long
        String.duplicate("a", 200),
        "contains spaces and symbols!",
        nil
      ]

      Enum.each(malformed_verifiers, fn malformed ->
        result = PKCE.verify_challenge(malformed, challenge)
        assert match?({:error, :invalid_challenge}, result)
      end)
    end

    test "is case sensitive" do
      {:ok, verifier, challenge} = PKCE.generate_challenge()

      # Uppercase challenge should not match
      uppercase_challenge = String.upcase(challenge)

      if uppercase_challenge != challenge do
        assert {:error, :invalid_challenge} = PKCE.verify_challenge(verifier, uppercase_challenge)
      end

      # Uppercase verifier should not match (if it contains lowercase)
      uppercase_verifier = String.upcase(verifier)

      if uppercase_verifier != verifier do
        assert {:error, :invalid_challenge} = PKCE.verify_challenge(uppercase_verifier, challenge)
      end
    end

    test "verifies multiple different challenges" do
      # Generate and verify multiple challenge/verifier pairs
      pairs =
        for _i <- 1..20 do
          {:ok, verifier, challenge} = PKCE.generate_challenge()
          {verifier, challenge}
        end

      # All should verify correctly
      Enum.each(pairs, fn {verifier, challenge} ->
        assert :ok = PKCE.verify_challenge(verifier, challenge)
      end)

      # Cross-verification should fail
      [{v1, c1}, {v2, c2} | _] = pairs
      assert {:error, :invalid_challenge} = PKCE.verify_challenge(v1, c2)
      assert {:error, :invalid_challenge} = PKCE.verify_challenge(v2, c1)
    end
  end

  describe "PKCE Code Verifier Validation" do
    test "validates correct code verifier" do
      {:ok, verifier, _challenge} = PKCE.generate_challenge()

      assert :ok = PKCE.validate_code_verifier(verifier)
    end

    test "validates minimum length requirement" do
      # Too short (42 characters)
      too_short = String.duplicate("a", 42)
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier(too_short)

      # Exactly minimum (43 characters)
      min_length = String.duplicate("a", 43)
      assert :ok = PKCE.validate_code_verifier(min_length)
    end

    test "validates maximum length requirement" do
      # Exactly maximum (128 characters)
      max_length = String.duplicate("a", 128)
      assert :ok = PKCE.validate_code_verifier(max_length)

      # Too long (129 characters)
      too_long = String.duplicate("a", 129)
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier(too_long)
    end

    test "validates unreserved character requirement" do
      # Valid unreserved characters
      valid_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"
      assert :ok = PKCE.validate_code_verifier(valid_chars)

      # Invalid characters
      invalid_verifiers = [
        # Contains !
        String.duplicate("a", 43) <> "!",
        # Contains @
        String.duplicate("a", 43) <> "@",
        # Contains #
        String.duplicate("a", 43) <> "#",
        # Contains $
        String.duplicate("a", 43) <> "$",
        # Contains %
        String.duplicate("a", 43) <> "%",
        # Contains ^
        String.duplicate("a", 43) <> "^",
        # Contains &
        String.duplicate("a", 43) <> "&",
        # Contains *
        String.duplicate("a", 43) <> "*",
        # Contains (
        String.duplicate("a", 43) <> "(",
        # Contains )
        String.duplicate("a", 43) <> ")",
        # Contains +
        String.duplicate("a", 43) <> "+",
        # Contains =
        String.duplicate("a", 43) <> "=",
        # Contains [
        String.duplicate("a", 43) <> "[",
        # Contains ]
        String.duplicate("a", 43) <> "]",
        # Contains {
        String.duplicate("a", 43) <> "{",
        # Contains }
        String.duplicate("a", 43) <> "}",
        # Contains \
        String.duplicate("a", 43) <> "\\",
        # Contains |
        String.duplicate("a", 43) <> "|",
        # Contains ;
        String.duplicate("a", 43) <> ";",
        # Contains :
        String.duplicate("a", 43) <> ":",
        # Contains '
        String.duplicate("a", 43) <> "'",
        # Contains "
        String.duplicate("a", 43) <> "\"",
        # Contains ,
        String.duplicate("a", 43) <> ",",
        # Contains <
        String.duplicate("a", 43) <> "<",
        # Contains >
        String.duplicate("a", 43) <> ">",
        # Contains /
        String.duplicate("a", 43) <> "/",
        # Contains ?
        String.duplicate("a", 43) <> "?",
        # Contains space
        String.duplicate("a", 43) <> " "
      ]

      Enum.each(invalid_verifiers, fn invalid ->
        assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier(invalid)
      end)
    end

    test "validates non-string input" do
      invalid_inputs = [
        nil,
        123,
        [],
        %{},
        :atom,
        {:tuple, "value"}
      ]

      Enum.each(invalid_inputs, fn invalid ->
        assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier(invalid)
      end)
    end

    test "validates empty string" do
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier("")
    end

    test "validates Unicode characters" do
      # Unicode characters should be rejected
      unicode_verifier = String.duplicate("a", 43) <> "é"
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier(unicode_verifier)

      emoji_verifier = String.duplicate("a", 43) <> "🔐"
      assert {:error, :invalid_code_verifier} = PKCE.validate_code_verifier(emoji_verifier)
    end

    test "validates all allowed unreserved characters" do
      # Test each allowed character individually
      # 42 + 1 = 43 (minimum length)
      base = String.duplicate("a", 42)

      allowed_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"

      Enum.each(String.graphemes(allowed_chars), fn char ->
        verifier = base <> char

        assert :ok = PKCE.validate_code_verifier(verifier),
               "Character '#{char}' should be allowed"
      end)
    end
  end

  describe "PKCE Security Properties" do
    test "verifier has sufficient entropy" do
      # Generate many verifiers and check for patterns
      verifiers =
        for _i <- 1..1000 do
          {:ok, verifier, _challenge} = PKCE.generate_challenge()
          verifier
        end

      # All should be unique (extremely high probability)
      unique_verifiers = Enum.uniq(verifiers)
      assert length(unique_verifiers) == 1000

      # Check character distribution is reasonably uniform
      all_chars = Enum.join(verifiers, "")

      char_counts =
        all_chars
        |> String.graphemes()
        |> Enum.frequencies()

      # Should have good character distribution
      # Uses variety of characters
      assert map_size(char_counts) > 10
    end

    test "challenge cannot be reverse engineered" do
      {:ok, verifier, challenge} = PKCE.generate_challenge()

      # Challenge should not contain any part of the verifier
      verifier_parts =
        for i <- 0..(String.length(verifier) - 4) do
          String.slice(verifier, i, 4)
        end

      Enum.each(verifier_parts, fn part ->
        refute String.contains?(challenge, part),
               "Challenge contains verifier substring: #{part}"
      end)
    end

    test "timing attack resistance" do
      {:ok, verifier, challenge} = PKCE.generate_challenge()

      # Verification time should be consistent regardless of how wrong the challenge is
      wrong_challenges = [
        "completely_wrong_challenge_that_is_totally_different",
        # Almost correct
        String.slice(challenge, 1..-1//1) <> "x",
        # Empty
        "",
        # Too long
        challenge <> "extra"
      ]

      # All should fail quickly and consistently
      Enum.each(wrong_challenges, fn wrong ->
        start_time = System.monotonic_time(:microsecond)
        result = PKCE.verify_challenge(verifier, wrong)
        end_time = System.monotonic_time(:microsecond)

        assert match?({:error, :invalid_challenge}, result)

        # Should complete quickly (< 1ms for crypto operations)
        duration = end_time - start_time
        # 1000 microseconds = 1ms
        assert duration < 1000
      end)
    end

    test "concurrent usage safety" do
      # Test that PKCE functions are safe for concurrent use
      tasks =
        for _i <- 1..50 do
          Task.async(fn ->
            {:ok, verifier, challenge} = PKCE.generate_challenge()
            verification_result = PKCE.verify_challenge(verifier, challenge)
            validation_result = PKCE.validate_code_verifier(verifier)
            {verifier, challenge, verification_result, validation_result}
          end)
        end

      results = Task.await_many(tasks, 5000)

      # All should succeed
      assert length(results) == 50

      Enum.each(results, fn {verifier, challenge, verification, validation} ->
        assert is_binary(verifier)
        assert is_binary(challenge)
        assert verification == :ok
        assert validation == :ok
      end)

      # All verifiers should be unique
      verifiers = Enum.map(results, fn {verifier, _, _, _} -> verifier end)
      unique_verifiers = Enum.uniq(verifiers)
      assert length(unique_verifiers) == 50
    end

    test "memory usage is reasonable" do
      # Generate many challenges and ensure memory doesn't grow excessively
      initial_memory = :erlang.memory(:total)

      for _i <- 1..1000 do
        {:ok, _verifier, _challenge} = PKCE.generate_challenge()
      end

      final_memory = :erlang.memory(:total)
      memory_growth = final_memory - initial_memory

      # Memory growth should be reasonable (< 10MB for 1000 challenges)
      assert memory_growth < 10_000_000
    end
  end

  describe "PKCE Integration with Authorization Flow" do
    test "generated challenge works with authorization flow" do
      {:ok, verifier, challenge} = PKCE.generate_challenge()

      # Simulate what happens in authorization flow
      auth_params = %{
        code_challenge: challenge,
        code_challenge_method: "S256"
      }

      # Verify the parameters are well-formed
      assert is_binary(auth_params.code_challenge)
      assert auth_params.code_challenge_method == "S256"
      assert String.length(auth_params.code_challenge) == 43

      # Later, in token exchange, verifier should validate
      assert :ok = PKCE.verify_challenge(verifier, auth_params.code_challenge)
    end

    test "supports only S256 method" do
      {:ok, verifier, challenge} = PKCE.generate_challenge()

      # Verify this is S256 method (SHA256 + base64url)
      expected_challenge =
        :crypto.hash(:sha256, verifier)
        |> Base.url_encode64(padding: false)

      assert challenge == expected_challenge

      # Verify it's NOT plain method (which would just be the verifier)
      assert challenge != verifier
    end

    test "maintains security across full OAuth flow simulation" do
      # Simulate complete flow

      # 1. Client generates PKCE challenge
      {:ok, code_verifier, code_challenge} = PKCE.generate_challenge()

      # 2. Client includes challenge in authorization request
      auth_url_params = %{
        code_challenge: code_challenge,
        code_challenge_method: "S256"
      }

      # 3. Server stores code_challenge for later verification
      stored_challenge = auth_url_params.code_challenge

      # 4. Client receives authorization code (simulated)
      authorization_code = "mock_auth_code_123"

      # 5. Client includes verifier in token request
      token_request_params = %{
        code: authorization_code,
        code_verifier: code_verifier
      }

      # 6. Server verifies challenge matches verifier
      verification_result =
        PKCE.verify_challenge(
          token_request_params.code_verifier,
          stored_challenge
        )

      assert verification_result == :ok

      # 7. Verify security: wrong verifier should fail
      {:ok, wrong_verifier, _} = PKCE.generate_challenge()
      wrong_verification = PKCE.verify_challenge(wrong_verifier, stored_challenge)
      assert match?({:error, :invalid_challenge}, wrong_verification)
    end
  end
end
