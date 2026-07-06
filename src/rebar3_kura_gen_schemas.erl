-module(rebar3_kura_gen_schemas).
-moduledoc """
`rebar3 kura gen_schemas` - bootstrap kura schema modules from an existing
PostgreSQL database.

One-shot: it reads the app's repo config, connects through the app's
already-fetched kura backend, introspects the `public` schema, and writes
one `-behaviour(kura_schema)` module per table under `src/schemas/`. It
emits schema modules only - never migrations, never a sync/pull mode - so
schemas stay the single source of truth that `kura compile` derives
migrations from.
""".

-export([init/1, do/1, format_error/1]).

-ifdef(TEST).
-export([schema_source/2, column_row/1, constraint_row/1]).
-endif.

-define(PROVIDER, gen_schemas).
-define(NAMESPACE, kura).
-define(DEPS, [{default, app_discovery}]).

-doc false.
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 kura gen_schemas"},
        {opts, [
            {repo, $r, "repo", string, "Repo name to read config for (default: the sole repo)"},
            {force, $f, "force", boolean, "Overwrite existing schema files"},
            {strict, $s, "strict", boolean, "Abort on any column of an unsupported type"}
        ]},
        {short_desc, "Generate kura schemas from an existing PostgreSQL database"},
        {desc,
            "Introspects the public schema of a live PostgreSQL database and\n"
            "generates one kura_schema module per table under src/schemas/.\n\n"
            "Emits schema modules only, never migrations. Existing files are\n"
            "skipped unless --force. Columns of a type kura does not model are\n"
            "skipped with a warning (--strict aborts instead)."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-doc false.
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar3_kura_compile:ensure_kura_on_path(State),
    App = current_app(State),
    AppName = binary_to_list(rebar_app_info:name(App)),
    AppDir = rebar_app_info:dir(App),
    {Args, _} = rebar_state:command_parsed_args(State),
    Config = resolve_repo_config(State, Args),
    ensure_backend_on_path(State, Config),
    Tables = introspect(Config),
    Strict = proplists:get_bool(strict, Args),
    Force = proplists:get_bool(force, Args),
    ok = report_skipped(Tables, Strict),
    lists:foreach(
        fun(Table) -> generate_schema(AppName, AppDir, Table, Force) end,
        Tables
    ),
    print_next_steps(),
    {ok, State}.

-doc false.
-spec format_error(term()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

current_app(State) ->
    case rebar_state:current_app(State) of
        undefined ->
            case rebar_state:project_apps(State) of
                [A | _] -> A;
                [] -> rebar_api:abort("No application found", [])
            end;
        A ->
            A
    end.

%%----------------------------------------------------------------------
%% Config + connection
%%----------------------------------------------------------------------

resolve_repo_config(State, Args) ->
    Repos = repos_env(State),
    case proplists:get_value(repo, Args) of
        undefined ->
            case maps:to_list(Repos) of
                [{_Name, Cfg}] -> Cfg;
                [] -> rebar_api:abort("No {repos, #{...}} config found under the kura app env", []);
                _ -> rebar_api:abort("Multiple repos configured; pass --repo Name", [])
            end;
        Name ->
            case repo_by_name(Name, Repos) of
                undefined -> rebar_api:abort("Repo ~s not found in config", [Name]);
                Cfg -> Cfg
            end
    end.

repo_by_name(Name, Repos) ->
    try list_to_existing_atom(Name) of
        Atom -> maps:get(Atom, Repos, undefined)
    catch
        error:badarg -> undefined
    end.

repos_env(State) ->
    App = current_app(State),
    ConfigFile = filename:join([rebar_app_info:dir(App), "config", "sys.config"]),
    case file:consult(ConfigFile) of
        {ok, [Config]} ->
            KuraEnv = proplists:get_value(kura, Config, []),
            proplists:get_value(repos, KuraEnv, #{});
        _ ->
            #{}
    end.

ensure_backend_on_path(State, Config) ->
    Backend = maps:get(backend, Config, kura_backend_postgres),
    case code:ensure_loaded(Backend) of
        {module, _} ->
            ok;
        {error, _} ->
            add_dep_to_path(State, atom_to_binary(dep_name_for(Backend), utf8))
    end.

dep_name_for(kura_backend_postgres) -> kura_postgres;
dep_name_for(kura_backend_sqlite) -> kura_sqlite;
dep_name_for(Other) -> Other.

add_dep_to_path(State, DepName) ->
    AllDeps = rebar_state:all_deps(State),
    case [D || D <- AllDeps, rebar_app_info:name(D) =:= DepName] of
        [Dep] -> code:add_pathz(filename:join(rebar_app_info:dir(Dep), "ebin"));
        [] -> rebar_api:abort("backend dependency ~s not found", [DepName])
    end.

introspect(Config) ->
    Backend = maps:get(backend, Config, kura_backend_postgres),
    PoolMod = Backend:pool_module(),
    DriverMod = Backend:driver_module(),
    Pool = kura_gen_schemas_pool,
    PgoConfig = pgo_config(Config, Pool),
    _ = PoolMod:start_pool(Pool, PgoConfig),
    ColRows = run_query(
        DriverMod, PoolMod, Pool, rebar3_kura_introspect_pg:columns_query(), fun column_row/1
    ),
    ConRows = run_query(
        DriverMod,
        PoolMod,
        Pool,
        rebar3_kura_introspect_pg:constraints_query(),
        fun constraint_row/1
    ),
    rebar3_kura_introspect_pg:build_tables(ColRows, ConRows).

pgo_config(Config, Pool) ->
    #{
        pool => Pool,
        host => to_list(maps:get(host, Config, maps:get(hostname, Config, "localhost"))),
        port => maps:get(port, Config, 5432),
        database => to_list(maps:get(database, Config)),
        user => to_list(maps:get(user, Config, maps:get(username, Config, "postgres"))),
        password => to_list(maps:get(password, Config, "")),
        pool_size => 2,
        decode_opts => [return_rows_as_maps, column_name_as_atom]
    }.

run_query(DriverMod, PoolMod, Pool, SQL, RowFun) ->
    Result = DriverMod:query(PoolMod, Pool, SQL, [], #{}),
    [RowFun(Row) || Row <- rows_of(Result)].

rows_of(#{rows := Rows}) -> Rows;
rows_of({ok, #{rows := Rows}}) -> Rows;
rows_of({error, Reason}) -> rebar_api:abort("Database query failed: ~p", [Reason]);
rows_of(Other) -> rebar_api:abort("Unexpected query result shape: ~p", [Other]).

%% Extract by explicit key so the tuple order can't drift from the SELECT.
column_row(#{
    table_name := T,
    column_name := C,
    udt_name := U,
    character_maximum_length := L,
    is_nullable := N,
    column_default := D
}) ->
    {T, C, U, L, N, D}.

constraint_row(#{
    table_name := T, column_name := C, constraint_type := Ct, foreign_table := F
}) ->
    {T, C, Ct, F}.

%%----------------------------------------------------------------------
%% Codegen
%%----------------------------------------------------------------------

report_skipped(Tables, Strict) ->
    Skipped = lists:flatten([maps:get(skipped, T, []) || T <- Tables]),
    lists:foreach(
        fun({Table, Column, TypeName}) ->
            rebar_api:warn(
                "Skipping ~s.~s: kura does not model PG type ~s", [Table, Column, TypeName]
            )
        end,
        Skipped
    ),
    case Strict andalso Skipped =/= [] of
        true -> rebar_api:abort("Aborting (--strict): ~p unsupported columns", [length(Skipped)]);
        false -> ok
    end.

generate_schema(App, AppDir, #{table := Table} = TableInfo, Force) ->
    Mod = App ++ "_" ++ binary_to_list(Table),
    FileName = filename:join([AppDir, "src", "schemas", Mod ++ ".erl"]),
    Content = schema_source(App, TableInfo),
    write_file(FileName, Content, Force).

-doc "Render a kura_schema module source for a table description. Pure.".
-spec schema_source(string(), map()) -> iolist().
schema_source(App, #{table := Table, fields := Fields} = TableInfo) ->
    Mod = App ++ "_" ++ binary_to_list(Table),
    Assocs = maps:get(assocs, TableInfo, []),
    [
        "-module(",
        Mod,
        ").\n",
        "-behaviour(kura_schema).\n",
        "-include_lib(\"kura/include/kura.hrl\").\n\n",
        "-export([table/0, fields/0",
        case Assocs of
            [] -> "";
            _ -> ", associations/0"
        end,
        "]).\n\n",
        "table() -> <<\"",
        Table,
        "\">>.\n\n",
        "fields() ->\n    [\n",
        lists:join(",\n", [render_field(F) || F <- Fields]),
        "\n    ].\n",
        render_associations(App, Assocs)
    ].

render_field(#{name := Name, type := Type, primary_key := Pk, nullable := Nullable}) ->
    Parts =
        [
            io_lib:format("name = ~s", [Name]),
            io_lib:format("type = ~w", [Type])
        ] ++
            [io_lib:format("primary_key = ~w", [Pk]) || Pk] ++
            [io_lib:format("nullable = ~w", [Nullable]) || not Nullable],
    ["        #kura_field{", lists:join(", ", Parts), "}"].

render_associations(_App, []) ->
    "";
render_associations(App, Assocs) ->
    [
        "\nassociations() ->\n    [\n",
        lists:join(",\n", [render_assoc(App, A) || A <- Assocs]),
        "\n    ].\n"
    ].

render_assoc(App, {belongs_to, Name, FkTable, Column}) ->
    Schema = App ++ "_" ++ binary_to_list(FkTable),
    io_lib:format(
        "        #kura_assoc{name = ~s, type = belongs_to, schema = ~s, foreign_key = ~s}",
        [Name, Schema, Column]
    ).

write_file(Path, Content, Force) ->
    case filelib:is_regular(Path) andalso not Force of
        true ->
            rebar_api:info("File already exists, skipping (use --force): ~s", [Path]);
        false ->
            ok = filelib:ensure_dir(Path),
            ok = file:write_file(Path, Content),
            rebar_api:info("Created ~s", [Path])
    end.

print_next_steps() ->
    rebar_api:info(
        "Generated schemas under src/schemas/. Review them, then note: the "
        "tables already exist, so a subsequent `rebar3 kura compile` will emit "
        "create_table migrations for them. Apply those against a fresh database "
        "only - do not run them against the introspected one.",
        []
    ).

to_list(V) when is_list(V) -> V;
to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_atom(V) -> atom_to_list(V).
