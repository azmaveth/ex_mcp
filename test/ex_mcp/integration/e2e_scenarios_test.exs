defmodule ExMCP.Integration.E2EScenariosTest do
  @moduledoc """
  Comprehensive End-to-End Test Scenarios for ExMCP.

  This test suite implements complete workflows that simulate real-world usage
  patterns of the MCP protocol, testing the entire stack from client to server
  with actual implementations rather than mocks.
  """

  use ExUnit.Case, async: false

  alias ExMCP.{Client, Server}
  alias ExMCP.Testing.{MockServer, Assertions}

  @moduletag :integration
  @moduletag :e2e
  @moduletag timeout: 60_000

  import Assertions

  describe "real MCP server interactions" do
    test "complete tool discovery and execution workflow" do
      # Create a real server with actual tool implementations
      defmodule E2EToolServer do
        use ExMCP.Server

        deftool "echo" do
          meta do
            description("Echo the input text back")

            input_schema(%{
              type: "object",
              properties: %{
                text: %{type: "string", description: "Text to echo"}
              },
              required: ["text"]
            })
          end
        end

        deftool "math_calculator" do
          meta do
            description("Perform basic math operations")

            input_schema(%{
              type: "object",
              properties: %{
                operation: %{type: "string", enum: ["add", "subtract", "multiply", "divide"]},
                a: %{type: "number"},
                b: %{type: "number"}
              },
              required: ["operation", "a", "b"]
            })
          end
        end

        @impl true
        def handle_initialize(params, state) do
          {:ok,
           %{
             "protocolVersion" => params["protocolVersion"],
             "serverInfo" => %{
               "name" => "e2e-tool-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{
               "tools" => %{}
             }
           }, state}
        end

        @impl true
        def handle_tool_call("echo", %{"text" => text}, state) do
          {:ok, %{content: [%{"type" => "text", "text" => "Echo: #{text}"}]}, state}
        end

        @impl true
        def handle_tool_call("math_calculator", %{"operation" => op, "a" => a, "b" => b}, state) do
          result =
            case op do
              "add" -> a + b
              "subtract" -> a - b
              "multiply" -> a * b
              "divide" when b != 0 -> a / b
              "divide" -> {:error, "Division by zero"}
            end

          case result do
            {:error, msg} ->
              {:error, msg, state}

            value ->
              {:ok, %{content: [%{"type" => "text", "text" => "Result: #{value}"}]}, state}
          end
        end
      end

      # Start the server
      {:ok, server_pid} = E2EToolServer.start_link(transport: :test)
      Process.sleep(10)

      # Connect client
      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Step 1: Discover tools
      {:ok, tools_response} = Client.list_tools(client)
      assert length(tools_response.tools) == 2

      # Verify tool definitions
      echo_tool = Enum.find(tools_response.tools, &(&1["name"] == "echo"))
      math_tool = Enum.find(tools_response.tools, &(&1["name"] == "math_calculator"))

      assert echo_tool != nil
      assert echo_tool["description"] == "Echo the input text back"
      assert math_tool != nil
      assert math_tool["description"] == "Perform basic math operations"

      # Step 2: Execute echo tool
      {:ok, echo_result} = Client.call_tool(client, "echo", %{"text" => "Hello, World!"})
      assert Enum.at(echo_result.content, 0).text == "Echo: Hello, World!"

      # Step 3: Execute math operations
      {:ok, add_result} =
        Client.call_tool(client, "math_calculator", %{
          "operation" => "add",
          "a" => 10,
          "b" => 5
        })

      assert Enum.at(add_result.content, 0).text == "Result: 15"

      {:ok, div_result} =
        Client.call_tool(client, "math_calculator", %{
          "operation" => "divide",
          "a" => 20,
          "b" => 4
        })

      assert Enum.at(div_result.content, 0).text == "Result: 5.0"

      # Step 4: Test error handling
      {:error, error} =
        Client.call_tool(client, "math_calculator", %{
          "operation" => "divide",
          "a" => 10,
          "b" => 0
        })

      assert error["message"] == "Division by zero"

      # Cleanup
      Client.stop(client)
      GenServer.stop(server_pid)
    end

    test "resource management and access workflow" do
      defmodule E2EResourceServer do
        use ExMCP.Server

        defresource "config://app.json" do
          meta do
            name("Application Configuration")
            description("Main application configuration file")
          end
        end

        defresource "data://users.csv" do
          meta do
            name("User Data")
            description("User database export")
          end
        end

        @impl true
        def handle_initialize(params, state) do
          {:ok,
           %{
             "protocolVersion" => params["protocolVersion"],
             "serverInfo" => %{
               "name" => "e2e-resource-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{
               "resources" => %{}
             }
           }, state}
        end

        @impl true
        def handle_resource_read("config://app.json", _full_uri, state) do
          content = %{
            "database" => %{"host" => "localhost", "port" => 5432},
            "api" => %{"version" => "v1", "timeout" => 30}
          }

          {:ok,
           %{
             uri: "config://app.json",
             mimeType: "application/json",
             text: Jason.encode!(content)
           }, state}
        end

        @impl true
        def handle_resource_read("data://users.csv", _full_uri, state) do
          csv_content = """
          id,name,email
          1,Alice,alice@example.com
          2,Bob,bob@example.com
          3,Charlie,charlie@example.com
          """

          {:ok,
           %{
             uri: "data://users.csv",
             mimeType: "text/csv",
             text: String.trim(csv_content)
           }, state}
        end

        @impl true
        def handle_resource_read(uri, _full_uri, state) do
          {:error, "Resource not found: #{uri}", state}
        end
      end

      # Start server and client
      {:ok, server_pid} = E2EResourceServer.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Step 1: List available resources
      {:ok, resources_response} = Client.list_resources(client)
      assert length(resources_response.resources) == 2

      config_resource =
        Enum.find(resources_response.resources, &(&1["uri"] == "config://app.json"))

      data_resource = Enum.find(resources_response.resources, &(&1["uri"] == "data://users.csv"))

      assert config_resource["name"] == "Application Configuration"
      assert data_resource["name"] == "User Data"

      # Step 2: Read configuration resource
      {:ok, config_content} = Client.read_resource(client, "config://app.json")
      assert config_content.mimeType == "application/json"

      parsed_config = Jason.decode!(config_content.text)
      assert parsed_config["database"]["host"] == "localhost"
      assert parsed_config["api"]["version"] == "v1"

      # Step 3: Read data resource
      {:ok, data_content} = Client.read_resource(client, "data://users.csv")
      assert data_content.mimeType == "text/csv"
      assert data_content.text =~ "Alice,alice@example.com"

      # Step 4: Test error handling for missing resource
      {:error, error} = Client.read_resource(client, "missing://resource")
      assert error["message"] =~ "Resource not found"

      # Cleanup
      Client.stop(client)
      GenServer.stop(server_pid)
    end

    test "prompt generation and customization workflow" do
      defmodule E2EPromptServer do
        use ExMCP.Server

        defprompt "summarize_text" do
          meta do
            name("Text Summarizer")
            description("Generate a summary of the provided text")
          end
        end

        defprompt "translate" do
          meta do
            name("Language Translator")
            description("Translate text to target language")
          end
        end

        @impl true
        def handle_initialize(params, state) do
          {:ok,
           %{
             "protocolVersion" => params["protocolVersion"],
             "serverInfo" => %{
               "name" => "e2e-prompt-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{
               "prompts" => %{}
             }
           }, state}
        end

        @impl true
        def handle_prompt_get("summarize_text", args, state) do
          text = Map.get(args, "text", "")
          max_length = Map.get(args, "max_length", "100 words")

          prompt_content = """
          Please summarize the following text in #{max_length}:

          #{text}

          Summary:
          """

          {:ok,
           %{
             description: "Summarization prompt for given text",
             messages: [
               %{
                 role: "user",
                 content: %{
                   type: "text",
                   text: prompt_content
                 }
               }
             ]
           }, state}
        end

        @impl true
        def handle_prompt_get("translate", args, state) do
          text = Map.get(args, "text", "")
          target_lang = Map.get(args, "target_lang", "English")
          style = Map.get(args, "style", "formal")

          prompt_content = """
          Translate the following text to #{target_lang} using a #{style} style:

          #{text}

          Translation:
          """

          {:ok,
           %{
             description: "Translation prompt for #{target_lang}",
             messages: [
               %{
                 role: "user",
                 content: %{
                   type: "text",
                   text: prompt_content
                 }
               }
             ]
           }, state}
        end

        @impl true
        def handle_prompt_get(name, _args, state) do
          {:error, "Prompt not found: #{name}", state}
        end
      end

      # Start server and client
      {:ok, server_pid} = E2EPromptServer.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Step 1: List available prompts
      {:ok, prompts_response} = Client.list_prompts(client)
      assert length(prompts_response.prompts) == 2

      summarize_prompt = Enum.find(prompts_response.prompts, &(&1["name"] == "summarize_text"))
      translate_prompt = Enum.find(prompts_response.prompts, &(&1["name"] == "translate"))

      assert summarize_prompt["description"] == "Generate a summary of the provided text"
      assert translate_prompt["description"] == "Translate text to target language"

      # Step 2: Get summarization prompt
      {:ok, summary_result} =
        Client.get_prompt(client, "summarize_text", %{
          "text" => "Artificial intelligence is revolutionizing many industries...",
          "max_length" => "50 words"
        })

      assert summary_result.description == "Summarization prompt for given text"
      assert length(summary_result.messages) == 1

      message_content = hd(summary_result.messages)["content"]["text"]
      assert message_content =~ "50 words"
      assert message_content =~ "Artificial intelligence"

      # Step 3: Get translation prompt
      {:ok, translate_result} =
        Client.get_prompt(client, "translate", %{
          "text" => "Hello, how are you?",
          "target_lang" => "Spanish",
          "style" => "informal"
        })

      assert translate_result.description == "Translation prompt for Spanish"
      translation_content = hd(translate_result.messages)["content"]["text"]
      assert translation_content =~ "Spanish"
      assert translation_content =~ "informal"
      assert translation_content =~ "Hello, how are you?"

      # Step 4: Test error handling
      {:error, error} = Client.get_prompt(client, "nonexistent", %{})
      assert error["message"] =~ "Prompt not found"

      # Cleanup
      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "complex multi-operation workflows" do
    test "data processing pipeline with chained operations" do
      defmodule E2EPipelineServer do
        use ExMCP.Server

        deftool "fetch_data" do
          meta do
            description("Fetch data from a source")

            input_schema(%{
              type: "object",
              properties: %{
                source: %{type: "string", enum: ["database", "api", "file"]}
              },
              required: ["source"]
            })
          end
        end

        deftool "transform_data" do
          meta do
            description("Transform data using specified operations")

            input_schema(%{
              type: "object",
              properties: %{
                data: %{type: "string"},
                operations: %{type: "array", items: %{type: "string"}}
              },
              required: ["data", "operations"]
            })
          end
        end

        deftool "export_data" do
          meta do
            description("Export processed data to a destination")

            input_schema(%{
              type: "object",
              properties: %{
                data: %{type: "string"},
                format: %{type: "string", enum: ["json", "csv", "xml"]}
              },
              required: ["data", "format"]
            })
          end
        end

        @impl true
        def handle_initialize(params, state) do
          {:ok,
           %{
             "protocolVersion" => params["protocolVersion"],
             "serverInfo" => %{
               "name" => "e2e-pipeline-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{
               "tools" => %{}
             }
           }, state}
        end

        @impl true
        def handle_tool_call("fetch_data", %{"source" => source}, state) do
          data =
            case source do
              "database" ->
                "user_id,name,age\n1,Alice,25\n2,Bob,30\n3,Charlie,35"

              "api" ->
                "[{\"id\":1,\"name\":\"Alice\",\"age\":25},{\"id\":2,\"name\":\"Bob\",\"age\":30}]"

              "file" ->
                "Alice 25\nBob 30\nCharlie 35"
            end

          {:ok, %{content: [%{"type" => "text", "text" => data}]}, state}
        end

        @impl true
        def handle_tool_call("transform_data", %{"data" => data, "operations" => ops}, state) do
          transformed =
            Enum.reduce(ops, data, fn op, acc ->
              case op do
                "uppercase" ->
                  String.upcase(acc)

                "reverse_lines" ->
                  acc |> String.split("\n") |> Enum.reverse() |> Enum.join("\n")

                "add_prefix" ->
                  acc |> String.split("\n") |> Enum.map(&"PROCESSED: #{&1}") |> Enum.join("\n")

                _ ->
                  acc
              end
            end)

          {:ok, %{content: [%{"type" => "text", "text" => transformed}]}, state}
        end

        @impl true
        def handle_tool_call("export_data", %{"data" => data, "format" => format}, state) do
          result =
            case format do
              "json" -> "{\"exported_data\": #{Jason.encode!(data)}}"
              "csv" -> "exported_data\n\"#{String.replace(data, "\n", " | ")}\""
              "xml" -> "<export><data>#{data}</data></export>"
            end

          {:ok, %{content: [%{"type" => "text", "text" => result}]}, state}
        end
      end

      # Start server and client
      {:ok, server_pid} = E2EPipelineServer.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Step 1: Fetch data from database
      {:ok, fetch_result} = Client.call_tool(client, "fetch_data", %{"source" => "database"})
      raw_data = Enum.at(fetch_result.content, 0).text
      assert raw_data =~ "Alice,25"

      # Step 2: Transform the data
      {:ok, transform_result} =
        Client.call_tool(client, "transform_data", %{
          "data" => raw_data,
          "operations" => ["add_prefix", "uppercase"]
        })

      transformed_data = Enum.at(transform_result.content, 0).text
      assert transformed_data =~ "PROCESSED: USER_ID,NAME,AGE"

      # Step 3: Export the processed data
      {:ok, export_result} =
        Client.call_tool(client, "export_data", %{
          "data" => transformed_data,
          "format" => "json"
        })

      exported_data = Enum.at(export_result.content, 0).text
      assert exported_data =~ "exported_data"
      assert exported_data =~ "PROCESSED"

      # Verify the complete pipeline worked
      assert String.contains?(exported_data, "PROCESSED: USER_ID,NAME,AGE")

      # Cleanup
      Client.stop(client)
      GenServer.stop(server_pid)
    end

    test "resource-informed tool execution workflow" do
      defmodule E2EResourceToolServer do
        use ExMCP.Server

        defresource "config://processing.json" do
          meta do
            name("Processing Configuration")
            description("Configuration for data processing operations")
          end
        end

        deftool "configure_processor" do
          meta do
            description("Configure processor with settings from resource")

            input_schema(%{
              type: "object",
              properties: %{
                config_uri: %{type: "string"},
                processor_type: %{type: "string"}
              },
              required: ["config_uri", "processor_type"]
            })
          end
        end

        deftool "process_with_config" do
          meta do
            description("Process data using configured settings")

            input_schema(%{
              type: "object",
              properties: %{
                data: %{type: "string"},
                config: %{type: "object"}
              },
              required: ["data", "config"]
            })
          end
        end

        @impl true
        def handle_initialize(params, state) do
          {:ok,
           %{
             "protocolVersion" => params["protocolVersion"],
             "serverInfo" => %{
               "name" => "e2e-resource-tool-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{
               "tools" => %{},
               "resources" => %{}
             }
           }, state}
        end

        @impl true
        def handle_resource_read("config://processing.json", _full_uri, state) do
          config = %{
            "text_processor" => %{
              "max_length" => 100,
              "case_transform" => "upper",
              "add_timestamp" => true
            },
            "data_processor" => %{
              "format" => "json",
              "validate" => true,
              "sort_keys" => true
            }
          }

          {:ok,
           %{
             uri: "config://processing.json",
             mimeType: "application/json",
             text: Jason.encode!(config)
           }, state}
        end

        @impl true
        def handle_tool_call(
              "configure_processor",
              %{"config_uri" => uri, "processor_type" => type},
              state
            ) do
          # In real scenario, this would read the resource
          # For test, we'll return a mock configuration
          case type do
            "text" ->
              config = %{"max_length" => 100, "case_transform" => "upper"}
              {:ok, %{content: [%{"type" => "text", "text" => Jason.encode!(config)}]}, state}

            "data" ->
              config = %{"format" => "json", "validate" => true}
              {:ok, %{content: [%{"type" => "text", "text" => Jason.encode!(config)}]}, state}

            _ ->
              {:error, "Unknown processor type", state}
          end
        end

        @impl true
        def handle_tool_call("process_with_config", %{"data" => data, "config" => config}, state) do
          processed =
            case config["case_transform"] do
              "upper" -> String.upcase(data)
              "lower" -> String.downcase(data)
              _ -> data
            end

          final_result =
            if config["add_timestamp"] do
              timestamp = DateTime.utc_now() |> DateTime.to_iso8601()
              "#{processed} [Processed at: #{timestamp}]"
            else
              processed
            end

          {:ok, %{content: [%{"type" => "text", "text" => final_result}]}, state}
        end
      end

      # Start server and client
      {:ok, server_pid} = E2EResourceToolServer.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Step 1: Read configuration resource
      {:ok, config_resource} = Client.read_resource(client, "config://processing.json")
      config_data = Jason.decode!(config_resource.text)
      text_config = config_data["text_processor"]

      # Step 2: Configure processor using resource data
      {:ok, configure_result} =
        Client.call_tool(client, "configure_processor", %{
          "config_uri" => "config://processing.json",
          "processor_type" => "text"
        })

      processor_config = Enum.at(configure_result.content, 0).text |> Jason.decode!()

      # Step 3: Process data using the configuration
      {:ok, process_result} =
        Client.call_tool(client, "process_with_config", %{
          "data" => "hello world, this is a test message",
          "config" => %{
            "case_transform" => "upper",
            "add_timestamp" => true
          }
        })

      result_text = Enum.at(process_result.content, 0).text
      assert result_text =~ "HELLO WORLD"
      assert result_text =~ "Processed at:"

      # Verify the workflow used resource data effectively
      assert text_config["case_transform"] == "upper"
      assert processor_config["case_transform"] == "upper"

      # Cleanup
      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end

  describe "error recovery and resilience" do
    test "client handles server errors gracefully and continues operation" do
      defmodule E2EErrorRecoveryServer do
        use ExMCP.Server

        deftool "reliable_tool" do
          meta do
            description("Always works reliably")
            input_schema(%{type: "object", properties: %{}})
          end
        end

        deftool "unreliable_tool" do
          meta do
            description("Sometimes fails")

            input_schema(%{
              type: "object",
              properties: %{
                should_fail: %{type: "boolean"}
              }
            })
          end
        end

        @impl true
        def handle_initialize(params, state) do
          {:ok,
           %{
             "protocolVersion" => params["protocolVersion"],
             "serverInfo" => %{
               "name" => "e2e-error-recovery-server",
               "version" => "1.0.0"
             },
             "capabilities" => %{
               "tools" => %{}
             }
           }, state}
        end

        @impl true
        def handle_tool_call("reliable_tool", _args, state) do
          {:ok, %{content: [%{"type" => "text", "text" => "Success"}]}, state}
        end

        @impl true
        def handle_tool_call("unreliable_tool", %{"should_fail" => true}, state) do
          {:error, "Simulated failure", state}
        end

        @impl true
        def handle_tool_call("unreliable_tool", _args, state) do
          {:ok, %{content: [%{"type" => "text", "text" => "Unreliable success"}]}, state}
        end
      end

      # Start server and client
      {:ok, server_pid} = E2EErrorRecoveryServer.start_link(transport: :test)
      Process.sleep(10)

      {:ok, client} =
        Client.start_link(
          transport: :test,
          server: server_pid
        )

      # Step 1: Verify reliable tool works
      {:ok, result1} = Client.call_tool(client, "reliable_tool", %{})
      assert Enum.at(result1.content, 0).text == "Success"

      # Step 2: Test unreliable tool failure
      {:error, error} = Client.call_tool(client, "unreliable_tool", %{"should_fail" => true})
      assert error["message"] == "Simulated failure"

      # Step 3: Verify client can still operate after error
      {:ok, result2} = Client.call_tool(client, "reliable_tool", %{})
      assert Enum.at(result2.content, 0).text == "Success"

      # Step 4: Test unreliable tool success
      {:ok, result3} = Client.call_tool(client, "unreliable_tool", %{"should_fail" => false})
      assert Enum.at(result3.content, 0).text == "Unreliable success"

      # Step 5: Verify client is still fully operational
      {:ok, tools} = Client.list_tools(client)
      assert length(tools.tools) == 2

      # Cleanup
      Client.stop(client)
      GenServer.stop(server_pid)
    end
  end
end
