defmodule ExMCP.Transport.HTTPSslOptionsTest do
  use ExUnit.Case, async: true

  alias ExMCP.Transport.HTTP

  describe "build_ssl_options/1" do
    test "returns a flat ssl option list for map config (no {:ssl, _} wrapper)" do
      opts = HTTP.build_ssl_options(%{cacerts: []})

      assert Keyword.keyword?(opts)
      refute Keyword.has_key?(opts, :ssl)
      assert Keyword.get(opts, :verify) == :verify_peer
      assert Keyword.get(opts, :cacerts) == []
      assert Keyword.get(opts, :versions) == [:"tlsv1.2", :"tlsv1.3"]
    end

    test "returns a flat ssl option list for non-map config" do
      opts = HTTP.build_ssl_options(nil)

      assert Keyword.keyword?(opts)
      refute Keyword.has_key?(opts, :ssl)
      assert Keyword.get(opts, :verify) == :verify_peer
      assert Keyword.get(opts, :versions) == [:"tlsv1.2", :"tlsv1.3"]
      assert Keyword.has_key?(opts, :cacerts)
    end

    test "includes hostname wildcard matching by default" do
      for opts <- [HTTP.build_ssl_options(%{cacerts: []}), HTTP.build_ssl_options(nil)] do
        assert [match_fun: match_fun] = Keyword.get(opts, :customize_hostname_check)
        assert is_function(match_fun)
      end
    end

    test "user tls config overrides the hostname check" do
      custom = [match_fun: fn _reference, _presented -> true end]
      opts = HTTP.build_ssl_options(%{cacerts: [], customize_hostname_check: custom})

      assert Keyword.get(opts, :customize_hostname_check) == custom
    end

    test "honors overrides and optional client cert settings" do
      verify_fun = {fn _cert, _event, state -> {:valid, state} end, nil}

      opts =
        HTTP.build_ssl_options(%{
          verify: :verify_none,
          cacerts: [],
          versions: [:"tlsv1.3"],
          cert: "cert-der",
          key: "key-der",
          ciphers: ["ECDHE-RSA-AES256-GCM-SHA384"],
          verify_fun: verify_fun
        })

      assert Keyword.keyword?(opts)
      refute Keyword.has_key?(opts, :ssl)
      assert Keyword.get(opts, :verify) == :verify_none
      assert Keyword.get(opts, :versions) == [:"tlsv1.3"]
      assert Keyword.get(opts, :cert) == "cert-der"
      assert Keyword.get(opts, :key) == "key-der"
      assert Keyword.get(opts, :ciphers) == ["ECDHE-RSA-AES256-GCM-SHA384"]
      assert Keyword.get(opts, :verify_fun) == verify_fun
    end
  end

  describe "async_state_changes/2" do
    test "reports only durable fields that differ from the snapshot" do
      snapshot = %HTTP{session_id: "old", access_token: nil, headers: []}

      new_state = %{
        snapshot
        | session_id: "rotated",
          access_token: "token-123",
          auth_completed: true,
          headers: [{"Authorization", "Bearer token-123"}]
      }

      changes = HTTP.async_state_changes(snapshot, new_state)

      assert changes == %{
               session_id: "rotated",
               access_token: "token-123",
               auth_completed: true,
               headers: [{"Authorization", "Bearer token-123"}]
             }
    end

    test "returns an empty map when nothing durable changed" do
      snapshot = %HTTP{session_id: "s", headers: []}

      assert HTTP.async_state_changes(snapshot, snapshot) == %{}
    end

    test "ignores transient and process-owned fields" do
      snapshot = %HTTP{session_id: "s", headers: []}

      new_state = %{
        snapshot
        | last_response: %{"jsonrpc" => "2.0"},
          sse_pid: self(),
          sse_deferred_attempted: true
      }

      assert HTTP.async_state_changes(snapshot, new_state) == %{}
    end

    test "reports SSE retry metadata from SSE-formatted POST responses" do
      snapshot = %HTTP{session_id: "s", headers: []}
      new_state = %{snapshot | retry_delay: 2_000, last_event_id: "evt-9"}

      assert HTTP.async_state_changes(snapshot, new_state) == %{
               retry_delay: 2_000,
               last_event_id: "evt-9"
             }
    end
  end
end
