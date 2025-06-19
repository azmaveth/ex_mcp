#!/usr/bin/env elixir

# Error Handling Server Example
# Shows comprehensive error handling with defhandler macro

Mix.install([
  {:ex_mcp, path: Path.expand("../../..", __DIR__)}
])

defmodule ErrorHandlingServer do
  use ExMCP.ServerV2
  
  # Tool demonstrating various error scenarios
  deftool "validate_data" do
    meta do
      name "Data Validator"
      description "Validates data and demonstrates error handling"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        data: %{type: "object"},
        rules: %{
          type: "object",
          properties: %{
            require_name: %{type: "boolean"},
            min_age: %{type: "integer"},
            max_items: %{type: "integer"}
          }
        }
      },
      required: ["data"]
    }
    
  end
  
  # Tool that can throw exceptions
  deftool "risky_operation" do
    meta do
      name "Risky Operation"
      description "Performs operations that might fail"
    end
    
    args do
      field :operation, :string, 
        required: true,
        enum: ["divide", "parse", "fetch", "timeout"]
      field :value1, :any
      field :value2, :any
    end
    
  end
  
  # Tool with custom error types
  deftool "process_payment" do
    meta do
      name "Payment Processor"
      description "Simulates payment processing with domain-specific errors"
    end
    
    input_schema %{
      type: "object",
      properties: %{
        amount: %{type: "number", minimum: 0.01},
        currency: %{type: "string", enum: ["USD", "EUR", "GBP"]},
        card_number: %{type: "string", pattern: "^[0-9]{16}$"}
      },
      required: ["amount", "currency", "card_number"]
    }
    
  end
  
  # Resource that can fail
  defresource "config://database" do
    meta do
      name "Database Configuration"
      description "Database connection settings"
    end
    mime_type "application/json"
    
  end
  
  # Prompt with error handling
  defprompt "debug_assistant" do
    meta do
      name "Debug Assistant"
      description "Helps debug errors"
    end
    
    arguments [
      %{name: "error_message", type: "string", required: true},
      %{name: "context", type: "object"}
    ]
    
  end
  
  # Tool handlers demonstrating error scenarios
  defhandler :tool, "validate_data", args, state do
    data = args["data"] || %{}
    rules = args["rules"] || %{}
    
    errors = []
    
    # Check required name
    if rules["require_name"] && !data["name"] do
      errors = ["Missing required field: name" | errors]
    end
    
    # Check minimum age
    if min_age = rules["min_age"] do
      age = data["age"] || 0
      if age < min_age do
        errors = ["Age #{age} is below minimum #{min_age}" | errors]
      end
    end
    
    # Check max items
    if max_items = rules["max_items"] do
      items = data["items"] || []
      if length(items) > max_items do
        errors = ["Too many items: #{length(items)} (max: #{max_items})" | errors]
      end
    end
    
    if errors == [] do
      response = text("Validation passed!")
      {:ok, [response], state}
    else
      # Return validation error with details
      {:error, %{
        code: "VALIDATION_FAILED",
        message: "Data validation failed",
        details: %{errors: Enum.reverse(errors)}
      }, state}
    end
  end
  
  defhandler :tool, "risky_operation", args, state do
    operation = args["operation"]
    value1 = args["value1"]
    value2 = args["value2"]
    
    try do
      result = case operation do
        "divide" ->
          # This might raise ArithmeticError
          value1 / value2
          
        "parse" ->
          # This might raise ArgumentError
          String.to_integer(value1)
          
        "fetch" ->
          # This might raise KeyError
          Map.fetch!(value1, value2)
          
        "timeout" ->
          # Simulate timeout
          Process.sleep(1000)
          raise "Operation timed out"
          
        _ ->
          raise ArgumentError, "Unknown operation: #{operation}"
      end
      
      response = text("Result: #{inspect(result)}")
      {:ok, [response], state}
    rescue
      ArithmeticError ->
        {:error, "Division by zero", state}
        
      ArgumentError ->
        {:error, %{
          code: "INVALID_ARGUMENT",
          message: "Invalid argument provided",
          details: %{operation: operation, value: value1}
        }, state}
        
      KeyError ->
        {:error, %{
          code: "KEY_NOT_FOUND",
          message: "Key not found in map",
          details: %{key: value2}
        }, state}
        
      error ->
        # Generic error handler
        {:error, Exception.message(error), state}
    end
  end
  
  defhandler :tool, "process_payment", args, state do
    amount = args["amount"]
    currency = args["currency"]
    card_number = args["card_number"]
    
    # Simulate various payment failures
    cond do
      amount > 10000 ->
        {:error, %{
          code: "PAYMENT_LIMIT_EXCEEDED",
          message: "Transaction amount exceeds daily limit",
          details: %{
            amount: amount,
            currency: currency,
            limit: 10000
          }
        }, state}
        
      String.starts_with?(card_number, "4111") ->
        {:error, %{
          code: "CARD_DECLINED",
          message: "Card was declined by the issuer",
          details: %{
            last_four: String.slice(card_number, -4..-1),
            reason: "insufficient_funds"
          }
        }, state}
        
      currency == "GBP" && amount < 1 ->
        {:error, %{
          code: "MINIMUM_AMOUNT",
          message: "Amount below minimum for currency",
          details: %{
            currency: currency,
            minimum: 1.00
          }
        }, state}
        
      true ->
        # Success case
        transaction_id = :crypto.strong_rand_bytes(8) |> Base.encode16()
        response = json(%{
          success: true,
          transaction_id: transaction_id,
          amount: amount,
          currency: currency
        })
        {:ok, [response], state}
    end
  end
  
  # Resource handler that can fail
  defhandler :resource, "config://database", _uri, state do
    if state.db_available do
      config = json(%{
        host: "localhost",
        port: 5432,
        database: "app_db"
      })
      {:ok, [config], state}
    else
      {:error, %{
        code: "RESOURCE_UNAVAILABLE",
        message: "Database configuration not available",
        details: %{
          reason: "Database service is down",
          retry_after: 30
        }
      }, state}
    end
  end
  
  # Prompt handler with validation
  defhandler :prompt, "debug_assistant", args, state do
    error_message = args["error_message"]
    context = args["context"] || %{}
    
    if String.length(error_message) < 10 do
      {:error, %{
        code: "INVALID_PROMPT_ARGS",
        message: "Error message too short to analyze",
        details: %{
          min_length: 10,
          actual_length: String.length(error_message)
        }
      }, state}
    else
      messages = [
        system("You are a debugging assistant. Help analyze and fix errors."),
        user("Error: #{error_message}\nContext: #{inspect(context)}")
      ]
      
      {:ok, %{messages: messages}, state}
    end
  end
  
  @impl true
  def init(_args) do
    {:ok, %{errors_count: 0, db_available: false}}
  end
  
  # Track errors
  @impl true
  def handle_error(error, state) do
    IO.puts("[Error Handler] #{inspect(error)}")
    {:ok, %{state | errors_count: state.errors_count + 1}}
  end
end

# Demo runner
defmodule ErrorDemo do
  def run_demo do
    {:ok, server} = ExMCP.Server.start_link(
      handler: ErrorHandlingServer,
      transport: :stdio
    )
    
    IO.puts("""
    ==========================================
    ExMCP v2 Error Handling Demo
    ==========================================
    
    This server demonstrates:
    1. Simple error responses using defhandler
    2. Structured errors with code and details
    3. Exception handling with try/rescue
    4. Domain-specific error scenarios
    5. Resource availability errors
    6. Prompt argument validation
    
    Available tools:
    - validate_data: Data validation with rules
    - risky_operation: Operations that can fail
    - process_payment: Payment processing simulation
    
    The server is running. Connect a client to test error scenarios.
    """)
    
    server
  end
end

# Start the demo
ErrorDemo.run_demo()
Process.sleep(:infinity)