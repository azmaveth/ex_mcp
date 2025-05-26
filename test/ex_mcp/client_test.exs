defmodule ExMCP.ClientTest do
  use ExUnit.Case
  import Mox

  alias ExMCP.Client
  alias ExMCP.Protocol
  alias ExMCP.Transport.Mock, as: MockTransport

  setup :verify_on_exit!

  defp create_mock_client(_context) do
    # Create a test process to handle transport interactions
    test_pid = self()

    {:ok, agent} = Agent.start_link(fn -> %{messages: [], responses: %{}} end)

    # Set the owner to allow any process to use these stubs
    Mox.set_mox_global()

    MockTransport
    |> stub(:connect, fn _opts -> {:ok, agent} end)
    |> stub(:send_message, fn msg, state ->
      Agent.update(state, fn s ->
        %{s | messages: s.messages ++ [msg]}
      end)

      # Check if this is an initialize request
      case Jason.decode(msg) do
        {:ok, %{"method" => "initialize", "id" => id}} ->
          # Queue initialize response
          response = %{
            "jsonrpc" => "2.0",
            "id" => id,
            "result" => %{
              "protocolVersion" => "2025-03-26",
              "serverInfo" => %{"name" => "test-server", "version" => "1.0"},
              "capabilities" => %{"roots" => %{}, "resources" => %{"subscribe" => true}}
            }
          }

          Agent.update(state, fn s ->
            %{s | responses: Map.put(s.responses, "initialize", Jason.encode!(response))}
          end)

        _ ->
          :ok
      end

      {:ok, state}
    end)
    |> stub(:receive_message, fn state ->
      # Check for queued responses
      response =
        Agent.get(state, fn s ->
          case s.responses do
            %{"initialize" => resp} ->
              Agent.update(state, fn s ->
                %{s | responses: Map.delete(s.responses, "initialize")}
              end)

              resp

            _ ->
              nil
          end
        end)

      if response do
        {:ok, response, state}
      else
        # Block briefly then return nil
        Process.sleep(10)
        {:ok, nil, state}
      end
    end)
    |> stub(:close, fn _state -> :ok end)

    {:ok, client} = Client.start_link(transport: MockTransport)

    # Wait for initialization to complete
    Process.sleep(100)

    {:ok, %{client: client, agent: agent, test_pid: test_pid}}
  end

  describe "roots operations" do
    setup [:create_mock_client]

    test "list_roots/2", %{client: client, agent: agent} do
      # Set up response for roots/list
      spawn(fn ->
        Process.sleep(20)

        Agent.update(agent, fn s ->
          response = %{
            "jsonrpc" => "2.0",
            # Assuming initialize was id 1
            "id" => 2,
            "result" => %{
              "roots" => [
                %{"uri" => "file:///home", "name" => "Home"},
                %{"uri" => "file:///work", "name" => "Work"}
              ]
            }
          }

          %{s | responses: Map.put(s.responses, "roots/list", Jason.encode!(response))}
        end)
      end)

      # Queue the response before making the call
      MockTransport
      |> expect(:receive_message, fn state ->
        response =
          Agent.get(state, fn s ->
            Map.get(s.responses, "roots/list")
          end)

        if response do
          Agent.update(state, fn s -> %{s | responses: Map.delete(s.responses, "roots/list")} end)
          {:ok, response, state}
        else
          {:ok, nil, state}
        end
      end)

      assert {:ok, roots} = Client.list_roots(client, 1000)
      assert length(roots) == 2
      assert Enum.any?(roots, fn r -> r.uri == "file:///home" end)
    end
  end

  describe "resource subscriptions" do
    setup [:create_mock_client]

    test "subscribe_resource/3", %{client: client, agent: agent} do
      uri = "file:///test.txt"

      # Set up response
      spawn(fn ->
        Process.sleep(20)

        Agent.update(agent, fn s ->
          response = %{
            "jsonrpc" => "2.0",
            "id" => 2,
            "result" => %{}
          }

          %{s | responses: Map.put(s.responses, "subscribe", Jason.encode!(response))}
        end)
      end)

      MockTransport
      |> expect(:receive_message, fn state ->
        response =
          Agent.get(state, fn s ->
            Map.get(s.responses, "subscribe")
          end)

        if response do
          Agent.update(state, fn s -> %{s | responses: Map.delete(s.responses, "subscribe")} end)
          {:ok, response, state}
        else
          {:ok, nil, state}
        end
      end)

      assert {:ok, _result} = Client.subscribe_resource(client, uri, 1000)

      # Verify the request was sent
      messages = Agent.get(agent, fn s -> s.messages end)

      assert Enum.any?(messages, fn msg ->
               case Jason.decode(msg) do
                 {:ok, %{"method" => "resources/subscribe", "params" => %{"uri" => ^uri}}} -> true
                 _ -> false
               end
             end)
    end

    test "unsubscribe_resource/3", %{client: client, agent: agent} do
      uri = "file:///test.txt"

      # Set up response
      spawn(fn ->
        Process.sleep(20)

        Agent.update(agent, fn s ->
          response = %{
            "jsonrpc" => "2.0",
            "id" => 2,
            "result" => %{}
          }

          %{s | responses: Map.put(s.responses, "unsubscribe", Jason.encode!(response))}
        end)
      end)

      MockTransport
      |> expect(:receive_message, fn state ->
        response =
          Agent.get(state, fn s ->
            Map.get(s.responses, "unsubscribe")
          end)

        if response do
          Agent.update(state, fn s -> %{s | responses: Map.delete(s.responses, "unsubscribe")} end)

          {:ok, response, state}
        else
          {:ok, nil, state}
        end
      end)

      assert {:ok, _result} = Client.unsubscribe_resource(client, uri, 1000)
    end
  end

  describe "sampling" do
    setup [:create_mock_client]

    test "create_message/3", %{client: client, agent: agent} do
      params = %{
        messages: [%{role: "user", content: %{type: "text", text: "Hello"}}],
        max_tokens: 100
      }

      # Set up response
      spawn(fn ->
        Process.sleep(20)

        Agent.update(agent, fn s ->
          response = %{
            "jsonrpc" => "2.0",
            "id" => 2,
            "result" => %{
              "role" => "assistant",
              "content" => %{"type" => "text", "text" => "Hi there!"}
            }
          }

          %{s | responses: Map.put(s.responses, "create_message", Jason.encode!(response))}
        end)
      end)

      MockTransport
      |> expect(:receive_message, fn state ->
        response =
          Agent.get(state, fn s ->
            Map.get(s.responses, "create_message")
          end)

        if response do
          Agent.update(state, fn s ->
            %{s | responses: Map.delete(s.responses, "create_message")}
          end)

          {:ok, response, state}
        else
          {:ok, nil, state}
        end
      end)

      assert {:ok, result} = Client.create_message(client, params, 1000)
      assert result.role == "assistant"
      assert result.content.text == "Hi there!"
    end
  end

  describe "notifications" do
    setup [:create_mock_client]

    test "handles roots/list_changed notification", %{client: client} do
      # Send a notification to the client
      notification = Protocol.encode_notification("notifications/roots/list_changed", %{})
      {:ok, json} = Protocol.encode_to_string(notification)

      # Simulate receiving the notification
      send(client, {:transport_message, json})

      # Give it time to process
      Process.sleep(50)

      # Verify it doesn't crash and continues running
      assert Process.alive?(client)
    end

    test "handles resource/updated notification", %{client: client} do
      notification =
        Protocol.encode_notification("notifications/resource/updated", %{uri: "file:///test.txt"})

      {:ok, json} = Protocol.encode_to_string(notification)

      send(client, {:transport_message, json})
      Process.sleep(50)

      assert Process.alive?(client)
    end
  end
end
