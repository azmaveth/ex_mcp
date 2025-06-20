#!/usr/bin/env elixir

# File Manager MCP Server
# 
# This example shows how to build a file management service
# using ExMCP's DSL. It demonstrates working with resources,
# subscriptions, and file-based operations.

Mix.install([
  {:ex_mcp, path: Path.expand("..", __DIR__)}
])

defmodule FileManager do
  use ExMCP.Server
  
  @impl true
  def init(args) do
    # SECURITY: Never default to current working directory in production
    # Always explicitly set a secure, sandboxed root_dir
    root_dir = case Keyword.get(args, :root_dir) do
      nil -> 
        # Create a secure temporary directory for this session
        tmp_dir = Path.join(System.tmp_dir!(), "file_manager_#{:rand.uniform(999999)}")
        File.mkdir_p!(tmp_dir)
        IO.puts(:stderr, "WARNING: No :root_dir specified. Using temporary directory: #{tmp_dir}")
        IO.puts(:stderr, "WARNING: In production, always explicitly set a secure :root_dir option")
        tmp_dir
      dir -> dir
    end
    
    {:ok, %{
      root_dir: root_dir,
      watchers: %{},
      file_locks: %{}
    }}
  end
  
  # File Operations Tools
  
  deftool "list_files" do
    description "List files in a directory"
    
    args do
      field :path, :string, 
        default: ".",
        description: "Directory path (relative to root)"
        
      field :pattern, :string,
        description: "Filter pattern (e.g., '*.txt')"
        
      field :recursive, :boolean,
        default: false,
        description: "Include subdirectories"
        
      field :include_hidden, :boolean,
        default: false,
        description: "Include hidden files"
    end
  end
  
  deftool "read_file" do
    description "Read file contents"
    
    args do
      field :path, :string, required: true
      field :encoding, :string,
        enum: ["utf8", "binary", "base64"],
        default: "utf8"
      field :lines, :object do
        field :from, :integer, minimum: 1
        field :to, :integer, minimum: 1
      end
    end
  end
  
  deftool "write_file" do
    description "Write content to a file"
    
    args do
      field :path, :string, required: true
      field :content, :string, required: true
      field :mode, :string,
        enum: ["write", "append", "create"],
        default: "write",
        description: "Write mode"
      field :encoding, :string,
        enum: ["utf8", "binary", "base64"],
        default: "utf8"
    end
  end
  
  deftool "file_info" do
    description "Get detailed file information"
    
    args do
      field :path, :string, required: true
      field :include_checksum, :boolean, default: false
    end
  end
  
  deftool "search_files" do
    description "Search for files by content"
    
    args do
      field :query, :string, required: true
      field :path, :string, default: "."
      field :file_pattern, :string, default: "*"
      field :case_sensitive, :boolean, default: false
      field :max_results, :integer, default: 50, minimum: 1, maximum: 1000
    end
  end
  
  # File Resources
  
  defresource "file://*" do
    name "File System"
    description "Access files in the managed directory"
    mime_type "application/octet-stream"
    list_pattern true
    subscribable true
  end
  
  defresource "file://metadata/*" do
    name "File Metadata"
    description "File metadata and properties"
    mime_type "application/json"
    list_pattern true
  end
  
  # File Management Prompts
  
  defprompt "organize_files" do
    name "File Organization Assistant"
    description "Helps organize and categorize files"
    
    arguments do
      arg :directory, required: true, description: "Directory to organize"
      arg :strategy, description: "Organization strategy (by type, date, etc.)"
      arg :rules, description: "Custom organization rules"
    end
  end
  
  defprompt "file_analyzer" do
    name "File Content Analyzer"
    description "Analyzes file contents and suggests actions"
    
    arguments do
      arg :file_path, required: true
      arg :analysis_type, description: "Type of analysis needed"
    end
  end
  
  # Handler Implementations
  
  @impl true
  def handle_tool_call("list_files", args, state) do
    path = Map.get(args, "path", ".")
    pattern = Map.get(args, "pattern", "*")
    recursive = Map.get(args, "recursive", false)
    include_hidden = Map.get(args, "include_hidden", false)
    
    full_path = Path.join(state.root_dir, path)
    
    files = list_directory(full_path, pattern, recursive, include_hidden)
    
    content = [
      text("Found #{length(files)} files"),
      json(%{
        path: path,
        files: files,
        total: length(files)
      })
    ]
    
    {:ok, %{content: content}, state}
  end
  
  @impl true
  def handle_tool_call("read_file", args, state) do
    path = args["path"]
    encoding = Map.get(args, "encoding", "utf8")
    lines = Map.get(args, "lines")
    
    case validate_path_security(state.root_dir, path) do
      {:ok, full_path} ->
        case read_file_safe(full_path, encoding, lines) do
          {:ok, content} ->
            {:ok, %{content: [text(content)]}, state}
          {:error, reason} ->
            {:error, "Failed to read file: #{reason}", state}
        end
      {:error, reason} ->
        {:error, reason, state}
    end
  end
  
  @impl true
  def handle_tool_call("write_file", args, state) do
    path = args["path"]
    content = args["content"]
    mode = Map.get(args, "mode", "write")
    encoding = Map.get(args, "encoding", "utf8")
    
    case validate_path_security(state.root_dir, path) do
      {:ok, full_path} ->
        # Check if file is locked
        if Map.get(state.file_locks, path) do
          {:error, "File is locked: #{path}", state}
        else
          case write_file_safe(full_path, content, mode, encoding) do
            :ok ->
              # Notify watchers
              notify_watchers(path, :modified, state)
              {:ok, %{content: [text("File written successfully")]}, state}
            {:error, reason} ->
              {:error, "Failed to write file: #{reason}", state}
          end
        end
      {:error, reason} ->
        {:error, reason, state}
    end
  end
  
  @impl true
  def handle_tool_call("file_info", args, state) do
    path = args["path"]
    include_checksum = Map.get(args, "include_checksum", false)
    
    case validate_path_security(state.root_dir, path) do
      {:ok, full_path} ->
        case get_file_info(full_path, include_checksum) do
          {:ok, info} ->
            content = [
              text("File information for: #{path}"),
              json(info)
            ]
            {:ok, %{content: content}, state}
          {:error, reason} ->
            {:error, "Failed to get file info: #{reason}", state}
        end
      {:error, reason} ->
        {:error, reason, state}
    end
  end
  
  @impl true
  def handle_tool_call("search_files", args, state) do
    query = args["query"]
    path = Map.get(args, "path", ".")
    pattern = Map.get(args, "file_pattern", "*")
    case_sensitive = Map.get(args, "case_sensitive", false)
    max_results = Map.get(args, "max_results", 50)
    
    case validate_path_security(state.root_dir, path) do
      {:ok, full_path} ->
        results = search_in_files(
          full_path,
          query,
          pattern,
          case_sensitive,
          max_results
        )
        
        content = [
          text("Search results for '#{query}': #{length(results)} matches"),
          json(%{query: query, results: results, count: length(results)})
        ]
        
        {:ok, %{content: content}, state}
      {:error, reason} ->
        {:error, reason, state}
    end
  end
  
  @impl true
  def handle_resource_read("file://" <> path, _uri, state) do
    case validate_path_security(state.root_dir, path) do
      {:ok, full_path} ->
        case File.read(full_path) do
          {:ok, content} ->
            mime_type = get_mime_type(path)
            if String.starts_with?(mime_type, "text/") do
              {:ok, [text(content)], state}
            else
              {:ok, [blob(Base.encode64(content), mime_type)], state}
            end
          {:error, reason} ->
            {:error, "Cannot read file: #{reason}", state}
        end
      {:error, reason} ->
        {:error, reason, state}
    end
  end
  
  @impl true
  def handle_resource_read("file://metadata/" <> path, _uri, state) do
    case validate_path_security(state.root_dir, path) do
      {:ok, full_path} ->
        case get_file_info(full_path, true) do
          {:ok, metadata} ->
            {:ok, [json(metadata)], state}
          {:error, reason} ->
            {:error, "Cannot get metadata: #{reason}", state}
        end
      {:error, reason} ->
        {:error, reason, state}
    end
  end
  
  @impl true
  def handle_resource_subscribe("file://" <> path, state) do
    # Set up file watching
    watchers = Map.put(state.watchers, path, true)
    new_state = %{state | watchers: watchers}
    
    # In production, use file system events
    IO.puts("Watching for changes: #{path}")
    
    {:ok, new_state}
  end
  
  @impl true
  def handle_prompt_get("organize_files", args, state) do
    directory = args["directory"]
    strategy = Map.get(args, "strategy", "by file type")
    rules = Map.get(args, "rules", "standard organization")
    
    messages = [
      system("You are a file organization expert. Suggest efficient file organization strategies."),
      user("""
      Please help me organize files in #{directory}.
      Strategy: #{strategy}
      Rules: #{rules}
      
      What's the best way to organize these files?
      """),
      assistant("I'll help you organize the files in #{directory} using a #{strategy} approach. Let me analyze the current structure and suggest an organization plan.")
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  @impl true
  def handle_prompt_get("file_analyzer", args, state) do
    file_path = args["file_path"]
    analysis_type = Map.get(args, "analysis_type", "general content analysis")
    
    messages = [
      system("You are a file content analyst. Provide insights about file contents and suggest appropriate actions."),
      user("Analyze this file: #{file_path} for #{analysis_type}"),
      assistant("I'll analyze #{file_path} focusing on #{analysis_type}. Let me examine the content and provide insights.")
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  # Helper Functions
  
  defp list_directory(path, pattern, recursive, include_hidden) do
    wildcard = if recursive, do: "**/" <> pattern, else: pattern
    
    Path.wildcard(Path.join(path, wildcard))
    |> Enum.map(&Path.relative_to(&1, path))
    |> Enum.reject(fn file ->
      not include_hidden and String.starts_with?(Path.basename(file), ".")
    end)
    |> Enum.map(fn file ->
      full = Path.join(path, file)
      stat = File.stat!(full)
      
      %{
        name: file,
        type: if(stat.type == :directory, do: "directory", else: "file"),
        size: stat.size,
        modified: stat.mtime |> elem(0) |> DateTime.from_unix!() |> DateTime.to_iso8601()
      }
    end)
  end
  
  defp read_file_safe(path, encoding, lines) do
    with {:ok, content} <- File.read(path) do
      content = case encoding do
        "base64" -> Base.encode64(content)
        "binary" -> content
        _ -> content
      end
      
      if lines do
        lines_list = String.split(content, "\n")
        from = Map.get(lines, "from", 1) - 1
        to = Map.get(lines, "to", length(lines_list))
        selected = Enum.slice(lines_list, from, to - from)
        {:ok, Enum.join(selected, "\n")}
      else
        {:ok, content}
      end
    end
  end
  
  defp write_file_safe(path, content, mode, encoding) do
    content = case encoding do
      "base64" -> Base.decode64!(content)
      _ -> content
    end
    
    case mode do
      "append" -> File.write(path, content, [:append])
      "create" -> 
        if File.exists?(path) do
          {:error, "File already exists"}
        else
          File.write(path, content)
        end
      _ -> File.write(path, content)
    end
  end
  
  defp get_file_info(path, include_checksum) do
    with {:ok, stat} <- File.stat(path) do
      info = %{
        name: Path.basename(path),
        path: path,
        size: stat.size,
        type: stat.type,
        permissions: stat.mode,
        created: stat.ctime |> elem(0) |> DateTime.from_unix!() |> DateTime.to_iso8601(),
        modified: stat.mtime |> elem(0) |> DateTime.from_unix!() |> DateTime.to_iso8601(),
        accessed: stat.atime |> elem(0) |> DateTime.from_unix!() |> DateTime.to_iso8601()
      }
      
      info = if include_checksum and stat.type == :regular do
        {:ok, content} = File.read(path)
        checksum = :crypto.hash(:sha256, content) |> Base.encode16(case: :lower)
        Map.put(info, :checksum, checksum)
      else
        info
      end
      
      {:ok, info}
    end
  end
  
  defp search_in_files(path, query, pattern, case_sensitive, max_results) do
    query = if case_sensitive, do: query, else: String.downcase(query)
    
    Path.wildcard(Path.join(path, pattern))
    |> Enum.take(max_results)
    |> Enum.reduce([], fn file, acc ->
      # Only search in files smaller than 10MB to prevent memory issues
      case File.stat(file) do
        {:ok, %{size: size}} when size > 10_000_000 ->
          IO.puts(:stderr, "Skipping large file: #{file} (#{div(size, 1_000_000)}MB)")
          acc
        {:ok, _} ->
          case File.read(file) do
            {:ok, content} ->
              content = if case_sensitive, do: content, else: String.downcase(content)
              if String.contains?(content, query) do
                [%{
                  file: Path.relative_to(file, path),
                  matches: count_matches(content, query)
                } | acc]
              else
                acc
              end
            _ -> acc
          end
        _ -> acc
      end
    end)
    |> Enum.reverse()
  end
  
  defp count_matches(content, query) do
    # More efficient counting using regular expressions
    # This avoids creating large intermediate lists
    case Regex.compile(Regex.escape(query)) do
      {:ok, regex} -> 
        Regex.scan(regex, content) |> length()
      {:error, _} -> 
        # Fallback for edge cases
        max(0, length(String.split(content, query)) - 1)
    end
  end
  
  defp get_mime_type(path) do
    case Path.extname(path) do
      ".txt" -> "text/plain"
      ".json" -> "application/json"
      ".xml" -> "application/xml"
      ".html" -> "text/html"
      ".css" -> "text/css"
      ".js" -> "text/javascript"
      ".ex" -> "text/x-elixir"
      ".exs" -> "text/x-elixir"
      ".png" -> "image/png"
      ".jpg" -> "image/jpeg"
      ".jpeg" -> "image/jpeg"
      ".gif" -> "image/gif"
      ".pdf" -> "application/pdf"
      _ -> "application/octet-stream"
    end
  end
  
  defp notify_watchers(path, event, state) do
    Enum.each(state.watchers, fn {watched_path, _} ->
      if String.starts_with?(path, watched_path) do
        IO.puts("File event: #{event} on #{path}")
      end
    end)
  end
  
  defp blob(data, mime_type) do
    %{
      type: "blob",
      data: data,
      mimeType: mime_type
    }
  end
  
  # SECURITY: Validate that the requested path stays within the root directory
  # This prevents directory traversal attacks like "../../../../etc/passwd"
  defp validate_path_security(root_dir, requested_path) do
    # Normalize both paths to resolve any .. sequences
    root_canonical = Path.expand(root_dir)
    target_path_raw = Path.join(root_dir, requested_path)
    target_path_canonical = Path.expand(target_path_raw)
    
    # Ensure the canonical target path starts with the canonical root path
    if String.starts_with?(target_path_canonical, root_canonical <> "/") or 
       target_path_canonical == root_canonical do
      {:ok, target_path_canonical}
    else
      {:error, "Access denied: Path '#{requested_path}' is outside the allowed root directory"}
    end
  end
end

# Server Runner
defmodule FileManagerRunner do
  def run do
    IO.puts("📁 Starting File Manager MCP Server...")
    IO.puts("=" <> String.duplicate("=", 50))
    
    # Create a safe sandbox directory
    sandbox = Path.join(System.tmp_dir!(), "mcp_file_manager_example")
    File.mkdir_p!(sandbox)
    
    # Create some example files
    File.write!(Path.join(sandbox, "readme.txt"), "Welcome to File Manager!")
    File.write!(Path.join(sandbox, "data.json"), Jason.encode!(%{example: true}))
    File.mkdir_p!(Path.join(sandbox, "documents"))
    File.write!(Path.join(sandbox, "documents/note.txt"), "Example note")
    
    {:ok, _server} = FileManager.start_link(
      transport: :stdio,
      name: :file_manager,
      root_dir: sandbox
    )
    
    IO.puts("\nFile Manager is running!")
    IO.puts("Root directory: #{sandbox}")
    
    IO.puts("\nAvailable Tools:")
    IO.puts("  • list_files - Browse directory contents")
    IO.puts("  • read_file - Read file contents")
    IO.puts("  • write_file - Write or append to files")
    IO.puts("  • file_info - Get detailed file information")
    IO.puts("  • search_files - Search files by content")
    
    IO.puts("\nAvailable Resources:")
    IO.puts("  • file://* - Direct file access")
    IO.puts("  • file://metadata/* - File metadata")
    
    IO.puts("\nAvailable Prompts:")
    IO.puts("  • organize_files - File organization help")
    IO.puts("  • file_analyzer - Content analysis")
    
    IO.puts("\nServer is ready for connections!")
    
    Process.sleep(:infinity)
  end
end

# Run if executed directly
if System.get_env("MIX_ENV") != "test" do
  FileManagerRunner.run()
end