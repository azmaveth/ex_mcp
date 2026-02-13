defmodule ExMCP.Authorization.JWTTest do
  use ExUnit.Case, async: true

  alias ExMCP.Authorization.JWT

  setup do
    rsa_key = JWT.generate_rsa_key(size: 2048)
    ec_key = JWT.generate_ec_key(curve: "P-256")
    %{rsa_key: rsa_key, ec_key: ec_key}
  end

  describe "load_key/1" do
    test "loads a JWK from a map with string keys", %{rsa_key: rsa_key} do
      jwk_map = JWT.to_map(rsa_key)
      assert {:ok, _jwk} = JWT.load_key(jwk_map)
    end

    test "loads a JWK from a map with atom keys", %{rsa_key: rsa_key} do
      jwk_map = JWT.to_map(rsa_key)
      atom_map = Map.new(jwk_map, fn {k, v} -> {String.to_atom(k), v} end)
      assert {:ok, _jwk} = JWT.load_key(atom_map)
    end

    test "loads a JWK from a PEM string", %{rsa_key: rsa_key} do
      {_type, pem} = JOSE.JWK.to_pem(rsa_key)
      assert {:ok, _jwk} = JWT.load_key(pem)
    end

    test "returns error for invalid format" do
      assert {:error, :invalid_key_format} = JWT.load_key(12345)
      assert {:error, :invalid_key_format} = JWT.load_key("not a pem")
    end

    test "returns error for file that doesn't exist" do
      assert {:error, {:file_read_error, :enoent}} = JWT.load_key({:pem_file, "/nonexistent"})
    end
  end

  describe "generate_rsa_key/1" do
    test "generates an RSA key pair" do
      jwk = JWT.generate_rsa_key()
      map = JWT.to_map(jwk)
      assert map["kty"] == "RSA"
    end
  end

  describe "generate_ec_key/1" do
    test "generates an EC key pair with default curve" do
      jwk = JWT.generate_ec_key()
      map = JWT.to_map(jwk)
      assert map["kty"] == "EC"
      assert map["crv"] == "P-256"
    end

    test "generates an EC key pair with specified curve" do
      jwk = JWT.generate_ec_key(curve: "P-384")
      map = JWT.to_map(jwk)
      assert map["crv"] == "P-384"
    end
  end

  describe "to_public_key/1" do
    test "extracts public key from RSA private key", %{rsa_key: rsa_key} do
      pub = JWT.to_public_key(rsa_key)
      pub_map = JWT.to_map(pub)
      assert pub_map["kty"] == "RSA"
      # Public key should not have private components
      refute Map.has_key?(pub_map, "d")
    end

    test "extracts public key from EC private key", %{ec_key: ec_key} do
      pub = JWT.to_public_key(ec_key)
      pub_map = JWT.to_map(pub)
      assert pub_map["kty"] == "EC"
      refute Map.has_key?(pub_map, "d")
    end
  end

  describe "sign/3 and verify/2" do
    test "round-trip with RSA key", %{rsa_key: rsa_key} do
      claims = %{"sub" => "user123", "iss" => "test"}
      assert {:ok, token} = JWT.sign(claims, rsa_key)
      assert {:ok, decoded} = JWT.verify(token, rsa_key)
      assert decoded["sub"] == "user123"
      assert decoded["iss"] == "test"
    end

    test "round-trip with EC key", %{ec_key: ec_key} do
      claims = %{"sub" => "user456"}
      assert {:ok, token} = JWT.sign(claims, ec_key, alg: "ES256")
      assert {:ok, decoded} = JWT.verify(token, ec_key)
      assert decoded["sub"] == "user456"
    end

    test "includes kid in header when provided", %{rsa_key: rsa_key} do
      claims = %{"sub" => "test"}
      assert {:ok, token} = JWT.sign(claims, rsa_key, kid: "key-1")
      assert {:ok, header} = JWT.peek_header(token)
      assert header["kid"] == "key-1"
    end

    test "includes custom typ header", %{rsa_key: rsa_key} do
      claims = %{"sub" => "test"}
      assert {:ok, token} = JWT.sign(claims, rsa_key, typ: "at+jwt")
      assert {:ok, header} = JWT.peek_header(token)
      assert header["typ"] == "at+jwt"
    end

    test "fails with wrong key", %{rsa_key: rsa_key} do
      other_key = JWT.generate_rsa_key(size: 2048)
      claims = %{"sub" => "test"}
      assert {:ok, token} = JWT.sign(claims, rsa_key)
      assert {:error, :verification_failed} = JWT.verify(token, other_key)
    end

    test "verifies against a list of keys (JWKS)", %{rsa_key: rsa_key} do
      other_key = JWT.generate_rsa_key(size: 2048)
      claims = %{"sub" => "test"}
      assert {:ok, token} = JWT.sign(claims, rsa_key)
      # Should succeed when correct key is in the list
      assert {:ok, decoded} = JWT.verify(token, [other_key, rsa_key])
      assert decoded["sub"] == "test"
    end
  end

  describe "validate_claims/2" do
    test "validates exp - valid token" do
      now = System.system_time(:second)
      claims = %{"exp" => now + 300}
      assert {:ok, _} = JWT.validate_claims(claims)
    end

    test "validates exp - expired token" do
      now = System.system_time(:second)
      claims = %{"exp" => now - 60}
      assert {:error, :token_expired} = JWT.validate_claims(claims)
    end

    test "validates iss" do
      claims = %{"iss" => "correct-issuer"}
      assert {:ok, _} = JWT.validate_claims(claims, iss: "correct-issuer")
      assert {:error, {:invalid_issuer, _}} = JWT.validate_claims(claims, iss: "wrong-issuer")
    end

    test "validates aud - string" do
      claims = %{"aud" => "my-audience"}
      assert {:ok, _} = JWT.validate_claims(claims, aud: "my-audience")
      assert {:error, {:invalid_audience, _}} = JWT.validate_claims(claims, aud: "wrong")
    end

    test "validates aud - list in claims" do
      claims = %{"aud" => ["aud1", "aud2"]}
      assert {:ok, _} = JWT.validate_claims(claims, aud: "aud1")
      assert {:ok, _} = JWT.validate_claims(claims, aud: "aud2")
      assert {:error, {:invalid_audience, _}} = JWT.validate_claims(claims, aud: "aud3")
    end

    test "validates sub" do
      claims = %{"sub" => "user123"}
      assert {:ok, _} = JWT.validate_claims(claims, sub: "user123")
      assert {:error, {:invalid_subject, _}} = JWT.validate_claims(claims, sub: "other")
    end

    test "validates max_age" do
      now = System.system_time(:second)
      claims = %{"iat" => now - 10}
      assert {:ok, _} = JWT.validate_claims(claims, max_age: 60)
      assert {:error, :token_too_old} = JWT.validate_claims(claims, max_age: 5)
    end

    test "validates required claims" do
      claims = %{"iss" => "test", "sub" => "user"}
      assert {:ok, _} = JWT.validate_claims(claims, required: ["iss", "sub"])

      assert {:error, {:missing_required_claims, ["aud"]}} =
               JWT.validate_claims(claims, required: ["iss", "aud"])
    end

    test "validates nbf - not yet valid" do
      now = System.system_time(:second)
      claims = %{"nbf" => now + 300}
      assert {:error, :token_not_yet_valid} = JWT.validate_claims(claims)
    end

    test "validates iat - future iat rejected" do
      now = System.system_time(:second)
      claims = %{"iat" => now + 300}
      assert {:error, :invalid_iat} = JWT.validate_claims(claims)
    end
  end

  describe "verify_and_validate/3" do
    test "full round-trip with validation", %{rsa_key: rsa_key} do
      now = System.system_time(:second)

      claims = %{
        "iss" => "test-issuer",
        "sub" => "user123",
        "aud" => "my-audience",
        "exp" => now + 300,
        "iat" => now
      }

      assert {:ok, token} = JWT.sign(claims, rsa_key)

      assert {:ok, decoded} =
               JWT.verify_and_validate(token, rsa_key,
                 iss: "test-issuer",
                 sub: "user123",
                 aud: "my-audience"
               )

      assert decoded["iss"] == "test-issuer"
    end

    test "rejects token with wrong issuer", %{rsa_key: rsa_key} do
      claims = %{"iss" => "wrong"}
      assert {:ok, token} = JWT.sign(claims, rsa_key)

      assert {:error, {:invalid_issuer, "wrong"}} =
               JWT.verify_and_validate(token, rsa_key, iss: "expected")
    end
  end

  describe "generate_jti/0" do
    test "generates unique values" do
      jtis = for _ <- 1..100, do: JWT.generate_jti()
      assert length(Enum.uniq(jtis)) == 100
    end

    test "generates non-empty strings" do
      jti = JWT.generate_jti()
      assert is_binary(jti)
      assert byte_size(jti) > 0
    end
  end

  describe "peek_header/1" do
    test "reads header from token", %{rsa_key: rsa_key} do
      claims = %{"sub" => "test"}
      assert {:ok, token} = JWT.sign(claims, rsa_key, alg: "RS256", kid: "test-kid")
      assert {:ok, header} = JWT.peek_header(token)
      assert header["alg"] == "RS256"
      assert header["kid"] == "test-kid"
      assert header["typ"] == "JWT"
    end

    test "returns error for invalid token" do
      assert {:error, _} = JWT.peek_header("not-a-token")
    end
  end
end
