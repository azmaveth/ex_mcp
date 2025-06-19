#!/usr/bin/env elixir

# Stateful Server Example
# Demonstrates stateful operations with defhandler macro

Mix.install([
  {:ex_mcp, path: Path.expand("../../..", __DIR__)}
])

defmodule StatefulTodoServer do
  use ExMCP.ServerV2
  
  # Server state management
  @initial_state %{
    todos: [],
    counter: 0,
    last_error: nil
  }
  
  # Tool with complex validation and error handling
  deftool "create_todo" do
    meta do
      name "Create Todo"
      description "Creates a new todo item with validation"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        title: %{
          type: "string",
          minLength: 3,
          maxLength: 100,
          description: "Todo title"
        },
        priority: %{
          type: "string",
          enum: ["low", "medium", "high"],
          default: "medium"
        },
        due_date: %{
          type: "string",
          format: "date",
          description: "Due date in YYYY-MM-DD format"
        },
        tags: %{
          type: "array",
          items: %{type: "string"},
          maxItems: 5
        }
      },
      required: ["title"]
    }
    
  end
  
  # Tool demonstrating async operations
  deftool "analyze_todos" do
    meta do
      name "Analyze Todos"
      description "Analyzes todos and generates statistics"
    end
    
    args do
      field :include_completed, :boolean, default: false
      field :group_by, :string, enum: ["priority", "date", "tags"]
    end
    
  end
  
  # Tool with streaming/progress support (simulated)
  deftool "export_todos" do
    meta do
      name "Export Todos"
      description "Exports todos with progress updates"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        format: %{type: "string", enum: ["json", "csv", "markdown"]},
        _progressToken: %{type: "string", description: "Progress token for updates"}
      },
      required: ["format"]
    }
    
  end
  
  # Resource with dynamic content
  defresource "todos://list" do
    meta do
      name "Todo List"
      description "Current todo items"
    end
    mime_type "application/json"
    
  end
  
  # Resource with URI parameters
  defresource "todos://item/*" do
    meta do
      name "Todo Item"
      description "Individual todo item by ID"
    end
    mime_type "application/json"
    
  end
  
  # Prompt with dynamic state-based content
  defprompt "todo_assistant" do
    meta do
      name "Todo Assistant"
      description "Helps manage and prioritize todos"
    end
    
    arguments [
      %{name: "action", type: "string", required: true, 
        enum: ["prioritize", "suggest_next", "analyze_workload"]},
      %{name: "context", type: "string", description: "Additional context"}
    ]
    
  end
  
  # Tool handlers with state management
  defhandler :tool, "create_todo", args, state do
    title = args["title"]
    priority = args["priority"] || "medium"
    due_date = args["due_date"]
    tags = args["tags"] || []
    
    # Validate due date if provided
    if due_date do
      case Date.from_iso8601(due_date) do
        {:ok, date} ->
          if Date.compare(date, Date.utc_today()) == :lt do
            {:error, "Due date cannot be in the past", state}
          else
            create_todo_item(title, priority, date, tags, state)
          end
        {:error, _} ->
          {:error, "Invalid date format. Use YYYY-MM-DD", state}
      end
    else
      create_todo_item(title, priority, nil, tags, state)
    end
  end
  
  defp create_todo_item(title, priority, due_date, tags, state) do
    todo = %{
      id: "todo_#{state.counter + 1}",
      title: title,
      priority: priority,
      due_date: due_date,
      tags: tags,
      completed: false,
      created_at: DateTime.utc_now()
    }
    
    new_state = %{state | 
      todos: [todo | state.todos],
      counter: state.counter + 1
    }
    
    response = json(%{
      id: todo.id,
      message: "Todo created successfully"
    })
    
    {:ok, [response], new_state}
  end
  
  defhandler :tool, "analyze_todos", args, state do
    include_completed = args["include_completed"] || false
    group_by = args["group_by"]
    
    todos = if include_completed do
      state.todos
    else
      Enum.filter(state.todos, fn todo -> !todo.completed end)
    end
    
    analysis = case group_by do
      "priority" ->
        todos
        |> Enum.group_by(& &1.priority)
        |> Enum.map(fn {priority, items} -> {priority, length(items)} end)
        |> Map.new()
        
      "date" ->
        todos
        |> Enum.group_by(& &1.due_date)
        |> Enum.map(fn {date, items} -> 
          {if(date, do: Date.to_string(date), else: "no_date"), length(items)}
        end)
        |> Map.new()
        
      "tags" ->
        todos
        |> Enum.flat_map(fn todo -> todo.tags end)
        |> Enum.frequencies()
        
      _ ->
        %{
          total: length(todos),
          by_priority: todos |> Enum.group_by(& &1.priority) |> Enum.map(fn {k, v} -> {k, length(v)} end) |> Map.new()
        }
    end
    
    response = json(%{
      analysis: analysis,
      total_todos: length(todos),
      group_by: group_by || "overview"
    })
    
    {:ok, [response], state}
  end
  
  defhandler :tool, "export_todos", args, state do
    format = args["format"]
    _progress_token = args["_progressToken"]
    
    # In a real implementation, we'd send progress updates via SSE
    # For this example, we'll just format the todos
    
    todos = Enum.reject(state.todos, & &1.completed)
    
    content = case format do
      "json" ->
        Jason.encode!(todos, pretty: true)
        
      "csv" ->
        headers = "ID,Title,Priority,Due Date,Tags\n"
        rows = Enum.map(todos, fn todo ->
          [
            todo.id,
            todo.title,
            todo.priority,
            todo.due_date || "",
            Enum.join(todo.tags, ";")
          ]
          |> Enum.join(",")
        end)
        headers <> Enum.join(rows, "\n")
        
      "markdown" ->
        lines = Enum.map(todos, fn todo ->
          date_str = if todo.due_date, do: " (due: #{todo.due_date})", else: ""
          tags_str = if todo.tags != [], do: " [#{Enum.join(todo.tags, ", ")}]", else: ""
          "- [ ] **#{todo.title}**#{date_str} _#{todo.priority}_#{tags_str}"
        end)
        "# Todo List\n\n" <> Enum.join(lines, "\n")
    end
    
    response = text(content)
    {:ok, [response], state}
  end
  
  # Resource handlers
  defhandler :resource, "todos://list", _uri, state do
    active_todos = Enum.reject(state.todos, & &1.completed)
    content = json(%{
      todos: active_todos,
      total: length(active_todos)
    })
    {:ok, [content], state}
  end
  
  defhandler :resource, "todos://item/" <> todo_id, _uri, state do
    case Enum.find(state.todos, fn todo -> todo.id == todo_id end) do
      nil ->
        {:error, "Todo not found: #{todo_id}", state}
      todo ->
        content = json(todo)
        {:ok, [content], state}
    end
  end
  
  # Prompt handler
  defhandler :prompt, "todo_assistant", args, state do
    action = args["action"]
    context = args["context"] || ""
    
    active_todos = Enum.reject(state.todos, & &1.completed)
    
    system_msg = case action do
      "prioritize" ->
        "You are a task prioritization expert. Analyze the todos and suggest the best order."
        
      "suggest_next" ->
        "You are a productivity coach. Based on the todos, suggest what should be done next."
        
      "analyze_workload" ->
        "You are a workload analyst. Assess if the todo list is manageable."
    end
    
    todo_summary = Enum.map(active_todos, fn todo ->
      "- #{todo.title} (#{todo.priority}#{if todo.due_date, do: ", due: #{todo.due_date}", else: ""})"
    end) |> Enum.join("\n")
    
    user_msg = "Current todos:\n#{todo_summary}\n\nContext: #{context}"
    
    messages = [
      system(system_msg),
      user(user_msg)
    ]
    
    {:ok, %{messages: messages}, state}
  end
  
  # Initialize with state
  @impl true
  def init(_args) do
    {:ok, @initial_state}
  end
  
  # Custom error handler
  @impl true
  def handle_error(error, state) do
    IO.puts("[Server] Error occurred: #{inspect(error)}")
    {:ok, %{state | last_error: error}}
  end
end

# Start the server
IO.puts("""
==========================================
ExMCP v2 Stateful Server Example
==========================================

This demonstrates:
- State management with defhandler
- Complex input validation
- Stateful todo operations
- Dynamic resource handlers
- Context-aware prompts

Features:
- Create todos with validation
- Analyze todo statistics
- Export in multiple formats
- Access individual todos by ID
- AI-powered todo assistant

Starting server...
""")

{:ok, _server} = ExMCP.Server.start_link(
  handler: StatefulTodoServer,
  transport: :stdio
)

Process.sleep(:infinity)