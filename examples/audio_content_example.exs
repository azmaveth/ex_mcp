#!/usr/bin/env elixir

# Audio Content Example
# Demonstrates how to work with audio content in MCP

defmodule AudioContentExample do
  @moduledoc """
  Example MCP server that demonstrates audio content handling.
  
  This server provides tools for:
  - Transcribing audio files
  - Generating audio from text (TTS)
  - Audio format conversion
  - Audio analysis
  """
  
  use ExMCP.Server.Handler
  alias ExMCP.Content
  
  @impl true
  def init(_args) do
    {:ok, %{processed_audio: %{}}}
  end
  
  @impl true
  def handle_initialize(_params, state) do
    result = %{
      protocolVersion: "2025-03-26",
      serverInfo: %{
        name: "audio-content-server",
        version: "1.0.0",
        description: "MCP server demonstrating audio content handling"
      },
      capabilities: %{
        tools: %{},
        resources: %{}
      }
    }
    
    {:ok, result, state}
  end
  
  @impl true
  def handle_list_tools(_cursor, state) do
    tools = [
      %{
        name: "transcribe_audio",
        description: "Transcribe audio content to text",
        inputSchema: %{
          type: "object",
          properties: %{
            audio: %{
              type: "object",
              properties: %{
                type: %{type: "string", const: "audio"},
                data: %{type: "string", description: "Base64-encoded audio data"},
                mimeType: %{type: "string", description: "Audio MIME type"}
              },
              required: ["type", "data", "mimeType"]
            },
            language: %{
              type: "string",
              description: "Language code (e.g., 'en', 'es', 'fr')",
              default: "en"
            }
          },
          required: ["audio"]
        }
      },
      %{
        name: "text_to_speech",
        description: "Convert text to audio speech",
        inputSchema: %{
          type: "object",
          properties: %{
            text: %{type: "string", description: "Text to convert to speech"},
            voice: %{
              type: "string",
              enum: ["male", "female", "neutral"],
              default: "neutral"
            },
            language: %{type: "string", default: "en"},
            format: %{
              type: "string",
              enum: ["mp3", "wav", "ogg"],
              default: "mp3"
            }
          },
          required: ["text"]
        }
      },
      %{
        name: "analyze_audio",
        description: "Analyze audio properties",
        inputSchema: %{
          type: "object",
          properties: %{
            audio: %{
              type: "object",
              properties: %{
                type: %{type: "string", const: "audio"},
                data: %{type: "string"},
                mimeType: %{type: "string"}
              },
              required: ["type", "data", "mimeType"]
            }
          },
          required: ["audio"]
        }
      }
    ]
    
    {:ok, tools, nil, state}
  end
  
  @impl true
  def handle_call_tool("transcribe_audio", arguments, state) do
    with {:ok, audio_content} <- validate_audio_content(arguments["audio"]),
         {:ok, transcript} <- transcribe_audio(audio_content, arguments["language"] || "en") do
      
      # Store for resource access
      id = generate_id()
      state = put_in(state.processed_audio[id], %{
        type: :transcript,
        content: transcript,
        source: audio_content,
        timestamp: DateTime.utc_now()
      })
      
      result = %{
        transcript: transcript,
        language: arguments["language"] || "en",
        resourceUri: "transcript://#{id}"
      }
      
      {:ok, result, state}
    else
      {:error, reason} ->
        {:error, reason, state}
    end
  end
  
  @impl true
  def handle_call_tool("text_to_speech", arguments, state) do
    text = arguments["text"]
    voice = arguments["voice"] || "neutral"
    language = arguments["language"] || "en"
    format = arguments["format"] || "mp3"
    
    # Simulate TTS generation
    audio_data = simulate_tts(text, voice, language, format)
    mime_type = get_mime_type(format)
    
    # Create audio content
    audio_content = Content.audio(audio_data, mime_type)
    
    # Store for resource access
    id = generate_id()
    state = put_in(state.processed_audio[id], %{
      type: :generated_audio,
      content: audio_content,
      text: text,
      settings: %{voice: voice, language: language, format: format},
      timestamp: DateTime.utc_now()
    })
    
    result = %{
      audio: audio_content,
      resourceUri: "audio://#{id}"
    }
    
    {:ok, result, state}
  end
  
  @impl true
  def handle_call_tool("analyze_audio", arguments, state) do
    with {:ok, audio_content} <- validate_audio_content(arguments["audio"]) do
      # Simulate audio analysis
      analysis = analyze_audio_properties(audio_content)
      
      result = %{
        mimeType: audio_content.mimeType,
        analysis: analysis
      }
      
      {:ok, result, state}
    else
      {:error, reason} ->
        {:error, reason, state}
    end
  end
  
  @impl true
  def handle_call_tool(_name, _arguments, state) do
    {:error, "Unknown tool", state}
  end
  
  @impl true
  def handle_list_resources(_cursor, state) do
    resources = 
      state.processed_audio
      |> Enum.map(fn {id, data} ->
        case data.type do
          :transcript ->
            %{
              uri: "transcript://#{id}",
              name: "Transcript #{id}",
              description: "Transcribed audio from #{DateTime.to_string(data.timestamp)}",
              mimeType: "text/plain"
            }
          
          :generated_audio ->
            %{
              uri: "audio://#{id}",
              name: "Generated Audio #{id}",
              description: "TTS audio: '#{String.slice(data.text, 0, 50)}...'",
              mimeType: data.content.mimeType
            }
        end
      end)
    
    {:ok, resources, nil, state}
  end
  
  @impl true
  def handle_read_resource(uri, state) do
    case String.split(uri, "://") do
      ["transcript", id] ->
        case Map.get(state.processed_audio, id) do
          %{type: :transcript, content: content} ->
            {:ok, %{contents: [Content.text(content)]}, state}
          _ ->
            {:error, "Transcript not found", state}
        end
      
      ["audio", id] ->
        case Map.get(state.processed_audio, id) do
          %{type: :generated_audio, content: audio_content} ->
            {:ok, %{contents: [audio_content]}, state}
          _ ->
            {:error, "Audio not found", state}
        end
      
      _ ->
        {:error, "Unknown resource type", state}
    end
  end
  
  @impl true
  def handle_list_prompts(_cursor, state) do
    {:ok, [], nil, state}
  end
  
  @impl true
  def handle_get_prompt(_name, _arguments, state) do
    {:error, "Prompt not found", state}
  end
  
  @impl true
  def handle_complete(_ref, _argument, state) do
    {:ok, %{completion: []}, state}
  end
  
  @impl true
  def handle_list_resource_templates(_cursor, state) do
    {:ok, [], nil, state}
  end
  
  # Private helpers
  
  defp validate_audio_content(%{"type" => "audio", "data" => data, "mimeType" => mime_type} = content) do
    # Convert string keys to atoms for validation
    atom_content = %{
      type: :audio,
      data: data,
      mimeType: mime_type
    }
    
    # Add annotations if present
    atom_content = 
      case Map.get(content, "annotations") do
        nil -> atom_content
        annotations -> Map.put(atom_content, :annotations, annotations)
      end
    
    Content.validate(atom_content)
  end
  
  defp validate_audio_content(_), do: {:error, "Invalid audio content"}
  
  defp transcribe_audio(audio_content, language) do
    # In a real implementation, you would:
    # 1. Decode the base64 audio data
    # 2. Send to a transcription service (Whisper, Google Speech-to-Text, etc.)
    # 3. Return the transcript
    
    # For demo purposes, simulate transcription
    {:ok, "This is a simulated transcript of the audio in #{language}. " <>
          "In a real implementation, this would use a service like OpenAI Whisper " <>
          "or Google Speech-to-Text to transcribe the actual audio content."}
  end
  
  defp simulate_tts(text, voice, language, format) do
    # In a real implementation, you would:
    # 1. Send text to a TTS service
    # 2. Get back audio data
    # 3. Encode as base64
    
    # For demo, return dummy base64 data
    Base.encode64("DUMMY_AUDIO_DATA_#{text}_#{voice}_#{language}_#{format}")
  end
  
  defp analyze_audio_properties(_audio_content) do
    # Simulate audio analysis
    %{
      duration: "2:34",
      bitrate: "128 kbps",
      sampleRate: "44.1 kHz",
      channels: "stereo",
      format: "MP3",
      estimatedSize: "2.3 MB"
    }
  end
  
  defp get_mime_type("mp3"), do: "audio/mp3"
  defp get_mime_type("wav"), do: "audio/wav"
  defp get_mime_type("ogg"), do: "audio/ogg"
  defp get_mime_type(_), do: "audio/mpeg"
  
  defp generate_id do
    :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
  end
end

# Example client usage
defmodule AudioContentClient do
  def demo do
    # Start the server
    {:ok, server} = ExMCP.Server.start_link(
      handler: AudioContentExample,
      transport: :beam,
      name: :audio_server
    )
    
    # Start a client
    {:ok, client} = ExMCP.Client.start_link(
      transport: :beam,
      server: :audio_server
    )
    
    # Wait for initialization
    Process.sleep(100)
    
    # List available tools
    {:ok, result} = ExMCP.Client.list_tools(client)
    tools = Map.get(result, "tools") || Map.get(result, :tools, [])
    IO.puts("\nAvailable audio tools:")
    Enum.each(tools, fn tool ->
      name = Map.get(tool, "name") || Map.get(tool, :name)
      desc = Map.get(tool, "description") || Map.get(tool, :description)
      IO.puts("  - #{name}: #{desc}")
    end)
    
    # Example 1: Text to Speech
    IO.puts("\n1. Converting text to speech...")
    {:ok, tts_result} = ExMCP.Client.call_tool(client, "text_to_speech", %{
      "text" => "Hello! This is a test of the MCP audio content feature.",
      "voice" => "female",
      "language" => "en",
      "format" => "mp3"
    })
    
    IO.puts("Generated audio:")
    IO.inspect(tts_result["audio"], pretty: true)
    IO.puts("Resource URI: #{tts_result["resourceUri"]}")
    
    # Example 2: Transcribe the generated audio
    IO.puts("\n2. Transcribing audio...")
    {:ok, transcript_result} = ExMCP.Client.call_tool(client, "transcribe_audio", %{
      "audio" => tts_result["audio"],
      "language" => "en"
    })
    
    IO.puts("Transcript: #{transcript_result["transcript"]}")
    
    # Example 3: Analyze audio
    IO.puts("\n3. Analyzing audio properties...")
    {:ok, analysis_result} = ExMCP.Client.call_tool(client, "analyze_audio", %{
      "audio" => tts_result["audio"]
    })
    
    IO.puts("Audio analysis:")
    IO.inspect(analysis_result["analysis"], pretty: true)
    
    # Example 4: List resources
    IO.puts("\n4. Listing generated resources...")
    {:ok, %{"resources" => resources}} = ExMCP.Client.list_resources(client)
    Enum.each(resources, fn resource ->
      IO.puts("  - #{resource["name"]} (#{resource["uri"]})")
    end)
    
    # Example 5: Read a resource
    if resource_uri = tts_result["resourceUri"] do
      IO.puts("\n5. Reading audio resource...")
      {:ok, %{"contents" => contents}} = ExMCP.Client.read_resource(client, resource_uri)
      IO.puts("Retrieved #{length(contents)} content item(s)")
    end
    
    # Cleanup
    GenServer.stop(client)
    GenServer.stop(server)
  end
end

# Run the demo
AudioContentClient.demo()