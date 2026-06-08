defmodule ExMCP.Server.DSL do
  @moduledoc """
  Modern DSL for defining MCP server primitives next to their handlers.

  Use this module with `ExMCP.Server.Handler`:

      defmodule MyServer do
        use ExMCP.Server.Handler
        use ExMCP.Server.DSL

        tool "echo", "Echo back the input" do
          param :message, :string, required: true

          run fn %{message: message}, _state ->
            {:ok, %{text: message}}
          end
        end
      end
  """

  alias ExMCP.Server.DSL.Builder

  defmacro __using__(opts) do
    quote do
      import ExMCP.Server.DSL
      alias ExMCP.Server.DSL.Result, as: ToolResult

      @ex_mcp_dsl_opts unquote(Macro.escape(opts))
      Module.register_attribute(__MODULE__, :ex_mcp_dsl_tools, accumulate: true)
      Module.register_attribute(__MODULE__, :ex_mcp_dsl_resources, accumulate: true)
      Module.register_attribute(__MODULE__, :ex_mcp_dsl_resource_templates, accumulate: true)
      Module.register_attribute(__MODULE__, :ex_mcp_dsl_prompts, accumulate: true)

      @doc false
      def child_spec(opts) do
        %{
          id: Keyword.get(opts, :id, __MODULE__),
          start: {__MODULE__, :start_link, [opts]},
          type: :worker,
          restart: :permanent,
          shutdown: 500
        }
      end

      defoverridable child_spec: 1

      @before_compile ExMCP.Server.DSL
    end
  end

  defmacro __before_compile__(env) do
    opts = Module.get_attribute(env.module, :ex_mcp_dsl_opts, [])
    tools = env.module |> Module.get_attribute(:ex_mcp_dsl_tools, []) |> Enum.reverse()
    resources = env.module |> Module.get_attribute(:ex_mcp_dsl_resources, []) |> Enum.reverse()

    resource_templates =
      env.module
      |> Module.get_attribute(:ex_mcp_dsl_resource_templates, [])
      |> Enum.reverse()

    prompts = env.module |> Module.get_attribute(:ex_mcp_dsl_prompts, []) |> Enum.reverse()

    quote do
      unquote(
        generate_server_callbacks(env.module, opts, tools, resources, resource_templates, prompts)
      )

      unquote_splicing(generate_tool_handlers(tools))
      unquote(generate_tool_callbacks(tools))

      unquote_splicing(generate_resource_handlers(resources, :resource))
      unquote_splicing(generate_resource_handlers(resource_templates, :resource_template))
      unquote(generate_resource_callbacks(resources, resource_templates))

      unquote_splicing(generate_prompt_handlers(prompts))
      unquote(generate_prompt_callbacks(prompts))
    end
  end

  @doc """
  Defines a tool with a co-located handler.
  """
  defmacro tool(name, description \\ nil, do: block) do
    entry = build_entry(__CALLER__, :tool, name, description, block)

    quote do
      @ex_mcp_dsl_tools unquote(Macro.escape(entry))
    end
  end

  @doc """
  Defines a static resource with a co-located read handler.
  """
  defmacro resource(uri, description \\ nil, do: block) do
    entry = build_entry(__CALLER__, :resource, uri, description, block)

    quote do
      @ex_mcp_dsl_resources unquote(Macro.escape(entry))
    end
  end

  @doc """
  Defines a resource template with a co-located read handler.
  """
  defmacro resource_template(uri_template, description \\ nil, do: block) do
    entry = build_entry(__CALLER__, :resource_template, uri_template, description, block)

    quote do
      @ex_mcp_dsl_resource_templates unquote(Macro.escape(entry))
    end
  end

  @doc """
  Defines a prompt with a co-located renderer.
  """
  defmacro prompt(name, description \\ nil, do: block) do
    entry = build_entry(__CALLER__, :prompt, name, description, block)

    quote do
      @ex_mcp_dsl_prompts unquote(Macro.escape(entry))
    end
  end

  defmacro param(_name, _type, _opts \\ []), do: :ok
  defmacro arg(_name, _opts \\ []), do: :ok
  defmacro run(_handler), do: :ok
  defmacro handle(_handler), do: :ok
  defmacro read(_handler), do: :ok
  defmacro render(_handler), do: :ok
  defmacro title(_title), do: :ok
  defmacro name(_name), do: :ok
  defmacro description(_description), do: :ok
  defmacro annotations(_annotations), do: :ok
  defmacro icons(_icons), do: :ok
  defmacro meta(_meta), do: :ok
  defmacro execution(_execution), do: :ok
  defmacro input_schema(_schema), do: :ok
  defmacro output_schema(_schema), do: :ok
  defmacro mime_type(_mime_type), do: :ok
  defmacro size(_size), do: :ok

  @doc false
  def validate_tool_response(response, nil), do: {:ok, response}

  def validate_tool_response(response, output_schema) do
    structured_content =
      Map.get(response, :structuredContent) || Map.get(response, "structuredContent")

    if is_nil(structured_content) do
      {:ok, response}
    else
      data = atom_keys_to_strings(structured_content)

      case validate_with_schema(data, output_schema) do
        :ok ->
          {:ok, response}

        {:error, errors} ->
          {:error, "Output validation failed: #{format_validation_errors(errors)}"}
      end
    end
  end

  defp build_entry(env, kind, id_ast, description_ast, block) do
    id = eval_ast!(id_ast, env)
    description = eval_ast!(description_ast, env)
    instructions = parse_instructions(block, env)

    case kind do
      :tool ->
        handler = required_handler!(instructions, [:run, :handle], "tool")
        params = Map.get(instructions, :params, [])

        definition =
          Builder.tool(id, Map.get(instructions, :description, description),
            params: params,
            input_schema: Map.get(instructions, :input_schema),
            output_schema: Map.get(instructions, :output_schema),
            annotations: Map.get(instructions, :annotations),
            title: Map.get(instructions, :title),
            icons: Map.get(instructions, :icons),
            execution: Map.get(instructions, :execution),
            meta: Map.get(instructions, :meta)
          )

        {definition, handler, params}

      :resource ->
        handler = required_handler!(instructions, [:read], "resource")

        definition =
          Builder.resource(id, Map.get(instructions, :description, description),
            name: Map.get(instructions, :name),
            title: Map.get(instructions, :title),
            mime_type: Map.get(instructions, :mime_type),
            annotations: Map.get(instructions, :annotations),
            size: Map.get(instructions, :size),
            icons: Map.get(instructions, :icons),
            meta: Map.get(instructions, :meta)
          )

        {definition, handler, []}

      :resource_template ->
        handler = required_handler!(instructions, [:read], "resource template")
        params = Map.get(instructions, :params, [])

        definition =
          Builder.resource_template(id, Map.get(instructions, :description, description),
            name: Map.get(instructions, :name),
            title: Map.get(instructions, :title),
            mime_type: Map.get(instructions, :mime_type),
            annotations: Map.get(instructions, :annotations),
            icons: Map.get(instructions, :icons),
            meta: Map.get(instructions, :meta)
          )

        {definition, handler, params}

      :prompt ->
        handler = required_handler!(instructions, [:render], "prompt")
        args = Map.get(instructions, :args, [])

        definition =
          Builder.prompt(id, Map.get(instructions, :description, description),
            title: Map.get(instructions, :title),
            args: args,
            icons: Map.get(instructions, :icons),
            meta: Map.get(instructions, :meta)
          )

        {definition, handler, args}
    end
  end

  defp parse_instructions(block, env) do
    block
    |> block_statements()
    |> Enum.reduce(%{params: [], args: []}, &parse_instruction(&1, &2, env))
    |> Map.update!(:params, &Enum.reverse/1)
    |> Map.update!(:args, &Enum.reverse/1)
  end

  defp block_statements({:__block__, _meta, statements}), do: statements
  defp block_statements(statement), do: [statement]

  defp parse_instruction({:param, _meta, args}, acc, env) do
    param = parse_param(args, env)
    Map.update!(acc, :params, &[param | &1])
  end

  defp parse_instruction({:arg, _meta, args}, acc, env) do
    arg = parse_prompt_arg(args, env)
    Map.update!(acc, :args, &[arg | &1])
  end

  defp parse_instruction({kind, _meta, [handler]}, acc, _env)
       when kind in [:run, :handle, :read, :render] do
    Map.put(acc, kind, handler)
  end

  defp parse_instruction({:title, _meta, [title]}, acc, env),
    do: Map.put(acc, :title, eval_ast!(title, env))

  defp parse_instruction({:name, _meta, [name]}, acc, env),
    do: Map.put(acc, :name, eval_ast!(name, env))

  defp parse_instruction({:description, _meta, [description]}, acc, env),
    do: Map.put(acc, :description, eval_ast!(description, env))

  defp parse_instruction({:annotations, _meta, [annotations]}, acc, env),
    do: Map.put(acc, :annotations, eval_ast!(annotations, env))

  defp parse_instruction({:icons, _meta, [icons]}, acc, env),
    do: Map.put(acc, :icons, eval_ast!(icons, env))

  defp parse_instruction({:meta, _meta, [meta]}, acc, env),
    do: Map.put(acc, :meta, eval_ast!(meta, env))

  defp parse_instruction({:execution, _meta, [execution]}, acc, env),
    do: Map.put(acc, :execution, eval_ast!(execution, env))

  defp parse_instruction({:input_schema, _meta, [schema]}, acc, env),
    do: Map.put(acc, :input_schema, eval_ast!(schema, env))

  defp parse_instruction({:output_schema, _meta, [schema]}, acc, env),
    do: Map.put(acc, :output_schema, eval_ast!(schema, env))

  defp parse_instruction({:mime_type, _meta, [mime_type]}, acc, env),
    do: Map.put(acc, :mime_type, eval_ast!(mime_type, env))

  defp parse_instruction({:size, _meta, [size]}, acc, env),
    do: Map.put(acc, :size, eval_ast!(size, env))

  defp parse_instruction({unknown, _meta, _args}, _acc, _env) when is_atom(unknown) do
    raise CompileError, description: "Unknown ExMCP.Server.DSL instruction: #{unknown}"
  end

  defp parse_param([name, type], env), do: parse_param([name, type, []], env)

  defp parse_param([name, type, opts], env) do
    Builder.param(
      eval_name!(name, env),
      eval_ast!(type, env),
      eval_ast!(opts, env)
    )
  end

  defp parse_prompt_arg([name], env), do: parse_prompt_arg([name, []], env)

  defp parse_prompt_arg([name, opts], env) do
    Builder.param(
      eval_name!(name, env),
      :string,
      eval_ast!(opts, env)
    )
  end

  defp required_handler!(instructions, keys, label) do
    Enum.find_value(keys, &Map.get(instructions, &1)) ||
      raise CompileError, description: "#{label} must define #{Enum.join(keys, " or ")}/1"
  end

  defp eval_name!(ast, env) do
    case eval_ast!(ast, env) do
      name when is_atom(name) ->
        name

      name when is_binary(name) ->
        String.to_atom(name)

      other ->
        raise CompileError, description: "Expected atom or string name, got: #{inspect(other)}"
    end
  end

  defp eval_ast!(ast, env) do
    ast
    |> Macro.expand(env)
    |> literal_value!()
  end

  defp literal_value!({:%{}, _meta, pairs}) do
    Map.new(pairs, fn {key, value} -> {literal_value!(key), literal_value!(value)} end)
  end

  defp literal_value!({:{}, _meta, values}),
    do: List.to_tuple(Enum.map(values, &literal_value!/1))

  defp literal_value!(tuple) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> Enum.map(&literal_value!/1) |> List.to_tuple()

  defp literal_value!(list) when is_list(list), do: Enum.map(list, &literal_value!/1)
  defp literal_value!(value) when is_binary(value), do: value
  defp literal_value!(value) when is_atom(value), do: value
  defp literal_value!(value) when is_integer(value), do: value
  defp literal_value!(value) when is_float(value), do: value

  defp literal_value!(other) do
    raise CompileError,
      description: "Expected a literal value in ExMCP.Server.DSL, got: #{Macro.to_string(other)}"
  end

  defp generate_tool_handlers(tools) do
    tools
    |> Enum.with_index()
    |> Enum.map(fn {{_tool, handler, _params}, index} ->
      name = :"__ex_mcp_dsl_tool_#{index}__"

      quote do
        @doc false
        def unquote(name)(args, state), do: unquote(handler).(args, state)
      end
    end)
  end

  defp generate_server_callbacks(module, opts, tools, resources, resource_templates, prompts) do
    server_info = server_info(module, opts)
    capabilities = capabilities(tools, resources, resource_templates, prompts)
    start_link_callback = generate_start_link_callback(server_info)
    initialize_callback = generate_initialize_callback(server_info, capabilities)

    quote do
      unquote(start_link_callback)
      unquote(initialize_callback)
    end
  end

  defp generate_start_link_callback(server_info) do
    quote do
      alias ExMCP.Server.{HandlerServer, Transport}

      @doc """
      Starts this MCP handler using the requested transport.
      """
      def start_link(opts \\ []) do
        case Keyword.get(opts, :transport, :beam) do
          :test ->
            opts
            |> Keyword.put_new(:handler, __MODULE__)
            |> HandlerServer.start_link()

          :beam ->
            opts
            |> Keyword.put(:transport, :test)
            |> Keyword.put_new(:handler, __MODULE__)
            |> HandlerServer.start_link()

          transport when transport in [:http, :stdio] ->
            Transport.start_server(
              __MODULE__,
              unquote(Macro.escape(server_info)),
              [],
              opts
            )

          transport ->
            {:error, {:unsupported_transport, transport}}
        end
      end
    end
  end

  defp generate_initialize_callback(server_info, capabilities) do
    quote do
      alias ExMCP.Internal.VersionRegistry

      @impl ExMCP.Server.Handler
      def handle_initialize(params, state) do
        client_version =
          Map.get(params, "protocolVersion") ||
            Map.get(params, :protocolVersion) ||
            VersionRegistry.latest_version()

        protocol_version =
          case VersionRegistry.negotiate_version(
                 client_version,
                 VersionRegistry.supported_versions()
               ) do
            {:ok, version} -> version
            {:error, _reason} -> VersionRegistry.latest_version()
          end

        result = %{
          "protocolVersion" => protocol_version,
          "serverInfo" => unquote(Macro.escape(server_info)),
          "capabilities" => unquote(Macro.escape(capabilities))
        }

        {:ok, result, Map.put(state, :protocol_version, protocol_version)}
      end
    end
  end

  defp generate_tool_callbacks([]), do: nil

  defp generate_tool_callbacks(tools) do
    definitions = Enum.map(tools, fn {definition, _handler, _params} -> definition end)

    mapping =
      tools
      |> Enum.with_index()
      |> Map.new(fn {{definition, _handler, params}, index} ->
        output_schema = compile_output_schema(definition[:outputSchema])
        {definition.name, {:"__ex_mcp_dsl_tool_#{index}__", output_schema, params}}
      end)

    quote do
      alias ExMCP.Server.DSL.{Builder, Result}

      @impl ExMCP.Server.Handler
      def handle_list_tools(_cursor, state) do
        {:ok, unquote(Macro.escape(definitions)), nil, state}
      end

      @impl ExMCP.Server.Handler
      def handle_call_tool(name, arguments, state) do
        mapping = unquote(Macro.escape(mapping))

        case Map.get(mapping, name) do
          {handler, output_schema, params} ->
            arguments = Builder.normalize_arguments(arguments, params)
            result = apply(__MODULE__, handler, [arguments, state])

            case Result.normalize_tool(result, state) do
              {:ok, response, new_state} ->
                validation =
                  response
                  |> ExMCP.Server.DSL.validate_tool_response(output_schema)
                  |> __ex_mcp_dsl_widen_validation__()

                case validation do
                  {:ok, response} -> {:ok, response, new_state}
                  {:error, reason} -> {:ok, Result.error(reason), new_state}
                end
            end

          nil ->
            {:ok, Result.error("Unknown tool: #{name}"), state}
        end
      end

      defp __ex_mcp_dsl_widen_validation__(validation), do: validation
    end
  end

  defp generate_resource_handlers(entries, kind) do
    entries
    |> Enum.with_index()
    |> Enum.map(fn {{_definition, handler, _params}, index} ->
      name = :"__ex_mcp_dsl_#{kind}_#{index}__"

      quote do
        @doc false
        def unquote(name)(params, state), do: unquote(handler).(params, state)
      end
    end)
  end

  defp generate_resource_callbacks([], []), do: nil

  defp generate_resource_callbacks(resources, resource_templates) do
    resource_definitions =
      Enum.map(resources, fn {definition, _handler, _params} -> definition end)

    template_definitions =
      Enum.map(resource_templates, fn {definition, _handler, _params} -> definition end)

    resource_mapping =
      resources
      |> Enum.with_index()
      |> Map.new(fn {{definition, _handler, _params}, index} ->
        {definition.uri, {:"__ex_mcp_dsl_resource_#{index}__", definition[:mimeType]}}
      end)

    template_mapping =
      resource_templates
      |> Enum.with_index()
      |> Enum.map(fn {{definition, _handler, params}, index} ->
        {definition.uriTemplate, :"__ex_mcp_dsl_resource_template_#{index}__",
         definition[:mimeType], params}
      end)

    quote do
      alias ExMCP.Server.DSL.{Builder, Matcher, Result}

      unquote(resource_overridables(resources, resource_templates))
      unquote(generate_list_resources(resource_definitions))
      unquote(generate_list_resource_templates(template_definitions))

      @impl ExMCP.Server.Handler
      def handle_read_resource(uri, state) do
        resource_mapping = unquote(Macro.escape(resource_mapping))
        template_mapping = unquote(Macro.escape(template_mapping))

        case Map.get(resource_mapping, uri) do
          {handler, mime_type} ->
            params = %{uri: uri}
            result = apply(__MODULE__, handler, [params, state])
            Result.normalize_resource(result, uri, mime_type, state)

          nil ->
            read_resource_template(uri, template_mapping, state)
        end
      end

      defp read_resource_template(uri, template_mapping, state) do
        Enum.find_value(template_mapping, fn {template, handler, mime_type, params} ->
          case Matcher.match_uri_template(uri, template) do
            {:ok, variables} ->
              variables =
                variables
                |> Builder.normalize_template_variables()
                |> Map.put(:uri, uri)
                |> Builder.normalize_arguments(params)

              result = apply(__MODULE__, handler, [variables, state])
              Result.normalize_resource(result, uri, mime_type, state)

            :error ->
              nil
          end
        end) || {:error, "Resource not found: #{uri}", state}
      end
    end
  end

  defp resource_overridables([], []) do
    nil
  end

  defp resource_overridables(resources, resource_templates) do
    callbacks =
      []
      |> maybe_add_callback(resources != [], {:handle_list_resources, 2})
      |> maybe_add_callback(resource_templates != [], {:handle_list_resource_templates, 2})
      |> maybe_add_callback(
        resources != [] or resource_templates != [],
        {:handle_read_resource, 2}
      )

    quote do
      defoverridable unquote(callbacks)
    end
  end

  defp generate_list_resources([]), do: nil

  defp generate_list_resources(resource_definitions) do
    quote do
      @impl ExMCP.Server.Handler
      def handle_list_resources(_cursor, state) do
        {:ok, unquote(Macro.escape(resource_definitions)), nil, state}
      end
    end
  end

  defp generate_list_resource_templates([]), do: nil

  defp generate_list_resource_templates(template_definitions) do
    quote do
      @impl ExMCP.Server.Handler
      def handle_list_resource_templates(_cursor, state) do
        {:ok, unquote(Macro.escape(template_definitions)), nil, state}
      end
    end
  end

  defp generate_prompt_handlers(prompts) do
    prompts
    |> Enum.with_index()
    |> Enum.map(fn {{_prompt, handler, _args}, index} ->
      name = :"__ex_mcp_dsl_prompt_#{index}__"

      quote do
        @doc false
        def unquote(name)(args, state), do: unquote(handler).(args, state)
      end
    end)
  end

  defp generate_prompt_callbacks([]), do: nil

  defp generate_prompt_callbacks(prompts) do
    definitions = Enum.map(prompts, fn {definition, _handler, _args} -> definition end)

    mapping =
      prompts
      |> Enum.with_index()
      |> Map.new(fn {{definition, _handler, args}, index} ->
        {definition.name, {:"__ex_mcp_dsl_prompt_#{index}__", args}}
      end)

    quote do
      alias ExMCP.Server.DSL.{Builder, Result}

      defoverridable handle_list_prompts: 2, handle_get_prompt: 3

      @impl ExMCP.Server.Handler
      def handle_list_prompts(_cursor, state) do
        {:ok, unquote(Macro.escape(definitions)), nil, state}
      end

      @impl ExMCP.Server.Handler
      def handle_get_prompt(name, arguments, state) do
        mapping = unquote(Macro.escape(mapping))

        case Map.get(mapping, name) do
          {handler, args} ->
            arguments = Builder.normalize_arguments(arguments, args)
            result = apply(__MODULE__, handler, [arguments, state])
            Result.normalize_prompt(result, state)

          nil ->
            {:error, "Prompt not found: #{name}", state}
        end
      end
    end
  end

  defp compile_output_schema(nil), do: nil

  defp compile_output_schema(schema) do
    if Code.ensure_loaded?(ExJsonSchema) do
      schema
      |> atom_keys_to_strings()
      |> ExJsonSchema.Schema.resolve()
    else
      schema
    end
  end

  defp validate_with_schema(data, schema) do
    if Code.ensure_loaded?(ExJsonSchema) and schema do
      case ExJsonSchema.Validator.validate(schema, data) do
        :ok -> :ok
        {:error, errors} -> {:error, errors}
      end
    else
      :ok
    end
  end

  defp atom_keys_to_strings(map) when is_map(map) do
    Map.new(map, fn
      {key, value} when is_atom(key) -> {Atom.to_string(key), atom_keys_to_strings(value)}
      {key, value} -> {key, atom_keys_to_strings(value)}
    end)
  end

  defp atom_keys_to_strings(list) when is_list(list), do: Enum.map(list, &atom_keys_to_strings/1)
  defp atom_keys_to_strings(value), do: value

  defp format_validation_errors(errors) when is_list(errors),
    do: Enum.map_join(errors, ", ", &inspect/1)

  defp maybe_add_callback(callbacks, true, callback), do: [callback | callbacks]
  defp maybe_add_callback(callbacks, false, _callback), do: callbacks

  defp server_info(module, opts) do
    case Keyword.get(opts, :server_info) do
      nil ->
        %{
          "name" => to_string(Keyword.get(opts, :name, module)),
          "version" => to_string(Keyword.get(opts, :version, "1.0.0"))
        }

      server_info ->
        server_info
        |> Map.new(fn {key, value} -> {to_string(key), value} end)
        |> Map.update("name", to_string(module), &to_string/1)
        |> Map.update("version", "1.0.0", &to_string/1)
    end
  end

  defp capabilities(tools, resources, resource_templates, prompts) do
    %{}
    |> maybe_put_capability(tools != [], "tools", %{})
    |> maybe_put_capability(resources != [] or resource_templates != [], "resources", %{})
    |> maybe_put_capability(prompts != [], "prompts", %{})
  end

  defp maybe_put_capability(capabilities, true, key, value),
    do: Map.put(capabilities, key, value)

  defp maybe_put_capability(capabilities, false, _key, _value), do: capabilities
end
