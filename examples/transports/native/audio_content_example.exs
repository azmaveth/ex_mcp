#!/usr/bin/env elixir

# Audio Content Example with Native Service Dispatcher
# Demonstrates how to work with audio content using ExMCP.Service

defmodule AudioContentService do
  @moduledoc """
  Example MCP service demonstrating audio content handling with Native Service Dispatcher.
  
  This service provides tools for:
  - Transcribing audio files
  - Generating audio from text (TTS)
  - Audio format conversion
  - Audio analysis
  """
  
  use ExMCP.Service, name: :audio_service
  alias ExMCP.Content
  
  @impl true
  def init(_args) do
    {:ok, %{processed_audio: %{}}}
  end
  
  @impl true
  def handle_mcp_request("list_tools", _params, state) do
    tools = [
      %{
        "name" => "transcribe_audio",
        "description" => "Transcribe audio content to text",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "audio_data" => %{
              "type" => "string",
              "description" => "Base64 encoded audio data"
            },
            "format" => %{
              "type" => "string",
              "enum" => ["mp3", "wav", "flac", "ogg"],
              "description" => "Audio format"
            },
            "language" => %{
              "type" => "string",
              "default" => "en",
              "description" => "Language code for transcription"
            }
          },
          "required" => ["audio_data", "format"]
        }
      },
      %{
        "name" => "text_to_speech",
        "description" => "Convert text to speech audio",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "text" => %{
              "type" => "string",
              "description" => "Text to convert to speech"
            },
            "voice" => %{
              "type" => "string",
              "enum" => ["male", "female", "child"],
              "default" => "female",
              "description" => "Voice type"
            },
            "format" => %{
              "type" => "string", 
              "enum" => ["mp3", "wav"],
              "default" => "mp3",
              "description" => "Output audio format"
            },
            "speed" => %{
              "type" => "number",
              "minimum" => 0.5,
              "maximum" => 2.0,
              "default" => 1.0,
              "description" => "Speech speed multiplier"
            }
          },
          "required" => ["text"]
        }
      },
      %{
        "name" => "convert_audio",
        "description" => "Convert audio between formats",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "audio_data" => %{
              "type" => "string",
              "description" => "Base64 encoded audio data"
            },
            "from_format" => %{
              "type" => "string",
              "enum" => ["mp3", "wav", "flac", "ogg"]
            },
            "to_format" => %{
              "type" => "string",
              "enum" => ["mp3", "wav", "flac", "ogg"]
            }
          },
          "required" => ["audio_data", "from_format", "to_format"]
        }
      },
      %{
        "name" => "analyze_audio",
        "description" => "Analyze audio properties",
        "inputSchema" => %{
          "type" => "object",
          "properties" => %{
            "audio_data" => %{
              "type" => "string",
              "description" => "Base64 encoded audio data"
            },
            "format" => %{
              "type" => "string",
              "enum" => ["mp3", "wav", "flac", "ogg"]
            }
          },
          "required" => ["audio_data", "format"]
        }
      }
    ]
    
    {:ok, %{"tools" => tools}, state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "transcribe_audio", "arguments" => args}, state) do
    %{"audio_data" => audio_data, "format" => format} = args
    language = Map.get(args, "language", "en")
    
    # Simulate transcription (in real implementation, use actual speech-to-text service)
    transcript = simulate_transcription(audio_data, format, language)
    
    # Store processed audio
    audio_id = "transcription_#{System.unique_integer()}"
    new_state = put_in(state.processed_audio[audio_id], %{
      type: "transcription",
      original_format: format,
      language: language,
      transcript: transcript
    })
    
    content = [
      Content.text("Transcription completed for #{format} audio in #{language}"),
      Content.text("Transcript: #{transcript}")
    ]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "text_to_speech", "arguments" => args}, state) do
    %{"text" => text} = args
    voice = Map.get(args, "voice", "female")
    format = Map.get(args, "format", "mp3")
    speed = Map.get(args, "speed", 1.0)
    
    # Simulate TTS generation
    audio_data = simulate_tts(text, voice, format, speed)
    
    # Store generated audio
    audio_id = "tts_#{System.unique_integer()}"
    new_state = put_in(state.processed_audio[audio_id], %{
      type: "generated",
      text: text,
      voice: voice,
      format: format,
      speed: speed
    })
    
    content = [
      Content.text("Generated #{format} audio with #{voice} voice at #{speed}x speed"),
      # In a real implementation, you'd return the actual audio data
      Content.text("Audio data: #{String.slice(audio_data, 0, 50)}... (truncated)")
    ]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "convert_audio", "arguments" => args}, state) do
    %{"audio_data" => audio_data, "from_format" => from_format, "to_format" => to_format} = args
    
    # Simulate format conversion
    converted_data = simulate_conversion(audio_data, from_format, to_format)
    
    # Store conversion result
    audio_id = "conversion_#{System.unique_integer()}"
    new_state = put_in(state.processed_audio[audio_id], %{
      type: "conversion",
      from_format: from_format,
      to_format: to_format,
      original_size: byte_size(audio_data),
      converted_size: byte_size(converted_data)
    })
    
    content = [
      Content.text("Converted audio from #{from_format} to #{to_format}"),
      Content.text("Original size: #{byte_size(audio_data)} bytes"),
      Content.text("Converted size: #{byte_size(converted_data)} bytes"),
      Content.text("Converted data: #{String.slice(converted_data, 0, 50)}... (truncated)")
    ]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("tools/call", %{"name" => "analyze_audio", "arguments" => args}, state) do
    %{"audio_data" => audio_data, "format" => format} = args
    
    # Simulate audio analysis
    analysis = simulate_analysis(audio_data, format)
    
    # Store analysis result
    audio_id = "analysis_#{System.unique_integer()}"
    new_state = put_in(state.processed_audio[audio_id], %{
      type: "analysis",
      format: format,
      analysis: analysis
    })
    
    content = [
      Content.text("Audio Analysis Results:"),
      Content.text("Format: #{format}"),
      Content.text("Duration: #{analysis.duration} seconds"),
      Content.text("Sample Rate: #{analysis.sample_rate} Hz"),
      Content.text("Channels: #{analysis.channels}"),
      Content.text("Bitrate: #{analysis.bitrate} kbps"),
      Content.text("Peak Volume: #{analysis.peak_volume} dB")
    ]
    
    {:ok, %{"content" => content}, new_state}
  end
  
  def handle_mcp_request("list_resources", _params, state) do
    # Provide access to processed audio as resources
    resources = state.processed_audio
    |> Enum.map(fn {id, audio} ->
      %{
        "uri" => "audio://processed/#{id}",
        "name" => "#{String.capitalize(audio.type)} #{id}",
        "description" => case audio.type do
          "transcription" -> "Transcribed #{audio.original_format} audio"
          "generated" -> "Generated #{audio.format} audio from text"
          "conversion" -> "Converted audio from #{audio.from_format} to #{audio.to_format}"
          "analysis" -> "Analysis of #{audio.format} audio"
        end,
        "mimeType" => "application/json"
      }
    end)
    
    {:ok, %{"resources" => resources}, state}
  end
  
  def handle_mcp_request("resources/read", %{"uri" => "audio://processed/" <> id}, state) do
    case Map.get(state.processed_audio, id) do
      nil ->
        {:error, %{"code" => -32602, "message" => "Audio resource not found: #{id}"}, state}
      
      audio_data ->
        content = %{
          "uri" => "audio://processed/#{id}",
          "mimeType" => "application/json",
          "text" => Jason.encode!(audio_data, pretty: true)
        }
        {:ok, %{"contents" => [content]}, state}
    end
  end
  
  def handle_mcp_request("list_prompts", _params, state) do
    prompts = [
      %{
        "name" => "audio_processing_workflow",
        "description" => "Template for audio processing workflows",
        "arguments" => [
          %{
            "name" => "task_type",
            "description" => "Type of audio processing task",
            "required" => true
          }
        ]
      }
    ]
    
    {:ok, %{"prompts" => prompts}, state}
  end
  
  def handle_mcp_request("prompts/get", %{"name" => "audio_processing_workflow", "arguments" => args}, state) do
    task_type = Map.get(args, "task_type", "general")
    
    messages = [
      %{
        "role" => "user",
        "content" => %{
          "type" => "text",
          "text" => """
          I need help with audio processing for: #{task_type}

          Available tools:
          - transcribe_audio: Convert audio to text
          - text_to_speech: Convert text to audio 
          - convert_audio: Change audio formats
          - analyze_audio: Get audio properties

          Please suggest the best approach for this task.
          """
        }
      }
    ]
    
    {:ok, %{"messages" => messages}, state}
  end
  
  def handle_mcp_request(method, _params, state) do
    {:error, %{"code" => -32601, "message" => "Method not found: #{method}"}, state}
  end
  
  # Simulation functions (replace with real implementations)
  
  defp simulate_transcription(audio_data, format, language) do
    # Simulate transcription based on audio length and language
    data_size = byte_size(audio_data)
    words_count = div(data_size, 1000) + 5  # Rough estimate
    
    case language do
      "en" -> "This is a simulated English transcription with approximately #{words_count} words from #{format} audio."
      "es" -> "Esta es una transcripción simulada en español con aproximadamente #{words_count} palabras del audio #{format}."
      "fr" -> "Ceci est une transcription simulée en français avec environ #{words_count} mots de l'audio #{format}."
      _ -> "Simulated transcription in #{language} with #{words_count} words from #{format} audio."
    end
  end
  
  defp simulate_tts(text, voice, format, speed) do
    # Simulate TTS generation - create dummy audio data
    text_length = String.length(text)
    audio_duration = text_length / (150 * speed)  # ~150 chars per second at 1x speed
    
    # Generate pseudo-audio data (would be actual audio bytes in real implementation)
    Base.encode64("SIMULATED_#{String.upcase(format)}_AUDIO_#{voice}_#{speed}x_#{audio_duration}s_#{text_length}chars")
  end
  
  defp simulate_conversion(audio_data, from_format, to_format) do
    # Simulate format conversion with different compression ratios
    compression_ratios = %{
      "mp3" => 0.1,
      "wav" => 1.0,
      "flac" => 0.6,
      "ogg" => 0.15
    }
    
    from_ratio = compression_ratios[from_format] || 1.0
    to_ratio = compression_ratios[to_format] || 1.0
    
    # Simulate size change based on format
    original_size = byte_size(audio_data)
    new_size = round(original_size * (to_ratio / from_ratio))
    
    # Generate converted data
    converted_data = String.duplicate("X", new_size)
    Base.encode64("CONVERTED_#{String.upcase(to_format)}_FROM_#{String.upcase(from_format)}_#{converted_data}")
  end
  
  defp simulate_analysis(audio_data, format) do
    # Simulate audio analysis
    data_size = byte_size(audio_data)
    
    %{
      duration: Float.round(data_size / 44100 * 2, 2),  # Rough estimate
      sample_rate: case format do
        "wav" -> 44100
        "mp3" -> 44100
        "flac" -> 96000
        "ogg" -> 44100
      end,
      channels: 2,
      bitrate: case format do
        "wav" -> 1411
        "mp3" -> 320
        "flac" -> 2000
        "ogg" -> 256
      end,
      peak_volume: -6.2
    }
  end
end

# Demo client
defmodule AudioContentDemo do
  require Logger
  
  def run do
    Logger.info("Starting Audio Content Service Demo with Native Service Dispatcher")
    Logger.info("=" |> String.duplicate(70))
    
    # Start the service
    {:ok, _} = AudioContentService.start_link()
    
    # Wait for registration
    Process.sleep(100)
    
    # Demo 1: List available tools
    Logger.info("\n=== Available Audio Tools ===")
    {:ok, %{"tools" => tools}} = ExMCP.Native.call(:audio_service, "list_tools", %{})
    Enum.each(tools, fn tool ->
      IO.puts("• #{tool["name"]}: #{tool["description"]}")
    end)
    
    # Demo 2: Transcribe audio
    Logger.info("\n=== Audio Transcription ===")
    sample_audio = Base.encode64("FAKE_AUDIO_DATA_FOR_DEMO")
    
    start_time = System.monotonic_time(:microsecond)
    {:ok, %{"content" => content}} = ExMCP.Native.call(:audio_service, "tools/call", %{
      "name" => "transcribe_audio",
      "arguments" => %{
        "audio_data" => sample_audio,
        "format" => "wav",
        "language" => "en"
      }
    })
    transcribe_time = System.monotonic_time(:microsecond) - start_time
    
    Enum.each(content, fn item ->
      IO.puts("  #{item["text"]}")
    end)
    IO.puts("  ⚡ Transcription completed in #{transcribe_time}μs")
    
    # Demo 3: Text to speech
    Logger.info("\n=== Text to Speech ===")
    
    start_time = System.monotonic_time(:microsecond)
    {:ok, %{"content" => content}} = ExMCP.Native.call(:audio_service, "tools/call", %{
      "name" => "text_to_speech",
      "arguments" => %{
        "text" => "Hello from ExMCP Native Service Dispatcher!",
        "voice" => "female",
        "format" => "mp3",
        "speed" => 1.2
      }
    })
    tts_time = System.monotonic_time(:microsecond) - start_time
    
    Enum.each(content, fn item ->
      IO.puts("  #{item["text"]}")
    end)
    IO.puts("  ⚡ TTS generation completed in #{tts_time}μs")
    
    # Demo 4: Audio conversion
    Logger.info("\n=== Audio Format Conversion ===")
    
    start_time = System.monotonic_time(:microsecond)
    {:ok, %{"content" => content}} = ExMCP.Native.call(:audio_service, "tools/call", %{
      "name" => "convert_audio",
      "arguments" => %{
        "audio_data" => sample_audio,
        "from_format" => "wav",
        "to_format" => "mp3"
      }
    })
    convert_time = System.monotonic_time(:microsecond) - start_time
    
    Enum.each(content, fn item ->
      IO.puts("  #{item["text"]}")
    end)
    IO.puts("  ⚡ Conversion completed in #{convert_time}μs")
    
    # Demo 5: Audio analysis
    Logger.info("\n=== Audio Analysis ===")
    
    start_time = System.monotonic_time(:microsecond)
    {:ok, %{"content" => content}} = ExMCP.Native.call(:audio_service, "tools/call", %{
      "name" => "analyze_audio",
      "arguments" => %{
        "audio_data" => sample_audio,
        "format" => "wav"
      }
    })
    analysis_time = System.monotonic_time(:microsecond) - start_time
    
    Enum.each(content, fn item ->
      IO.puts("  #{item["text"]}")
    end)
    IO.puts("  ⚡ Analysis completed in #{analysis_time}μs")
    
    # Demo 6: List processed audio resources
    Logger.info("\n=== Processed Audio Resources ===")
    {:ok, %{"resources" => resources}} = ExMCP.Native.call(:audio_service, "list_resources", %{})
    if length(resources) > 0 do
      Enum.each(resources, fn resource ->
        IO.puts("  • #{resource["name"]}: #{resource["description"]}")
      end)
      
      # Read one resource
      first_resource = List.first(resources)
      {:ok, %{"contents" => [content]}} = ExMCP.Native.call(:audio_service, "resources/read", %{
        "uri" => first_resource["uri"]
      })
      IO.puts("  Resource content preview:")
      IO.puts("  #{String.slice(content["text"], 0, 100)}...")
    else
      IO.puts("  No processed audio resources yet")
    end
    
    # Performance summary
    total_time = transcribe_time + tts_time + convert_time + analysis_time
    avg_time = div(total_time, 4)
    
    Logger.info("\n=== Performance Summary ===")
    IO.puts("Total processing time: #{total_time}μs")
    IO.puts("Average operation time: #{avg_time}μs")
    IO.puts("Native Service Dispatcher provides:")
    IO.puts("  ✓ Sub-millisecond audio tool calls")
    IO.puts("  ✓ Zero serialization for large audio data")
    IO.puts("  ✓ Direct Elixir term passing")
    IO.puts("  ✓ Perfect for real-time audio processing")
    
    :ok
  end
end

# Run the demo
AudioContentDemo.run()