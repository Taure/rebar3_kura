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
-export([schema_source/2, column_row/1, constraint_row/1, ensure_backend_on_path/2]).
-endif.

-define(PROVIDER, gen_schemas).
-define(NAMESPACE, kura).
-define(DEPS, [{default, install_deps}]).

%% Written quoted even where the running OTP takes them bare: `maybe` and
%% `else` are reserved by a feature, `cond` and `let` for the future.
-define(RESERVED_WORDS, [
    "after",
    "and",
    "andalso",
    "band",
    "begin",
    "bnot",
    "bor",
    "bsl",
    "bsr",
    "bxor",
    "case",
    "catch",
    "cond",
    "div",
    "else",
    "end",
    "fun",
    "if",
    "let",
    "maybe",
    "not",
    "of",
    "or",
    "orelse",
    "receive",
    "rem",
    "try",
    "when",
    "xor"
]).

-doc false.
-spec init(term()) -> {ok, term()}.
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
            {strict, $s, "strict", boolean, "Abort on any skipped column or table"}
        ]},
        {short_desc, "Generate kura schemas from an existing PostgreSQL database"},
        {desc,
            "Introspects the public schema of a live PostgreSQL database and\n"
            "generates one kura_schema module per table under src/schemas/.\n\n"
            "Emits schema modules only, never migrations. Existing files are\n"
            "skipped unless --force. A column of a type kura does not model, and\n"
            "a table or column whose name is not a lowercase ASCII identifier,\n"
            "is skipped with a warning (--strict aborts instead)."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-doc false.
-spec do(term()) -> {ok, term()} | {error, string()}.
do(State) ->
    rebar3_kura_compile:ensure_kura_on_path(State),
    App = current_app(State),
    AppName = binary_to_list(rebar_app_info:name(App)),
    AppDir = rebar_app_info:dir(App),
    {Args, _} = rebar_state:command_parsed_args(State),
    Config = resolve_repo_config(State, Args),
    ensure_backend_on_path(State, Config),
    {Tables, UnsafeTables} = introspect(Config),
    Strict = proplists:get_bool(strict, Args),
    Force = proplists:get_bool(force, Args),
    ok = report_skipped(Tables, UnsafeTables, Strict),
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
    DepName = atom_to_binary(dep_name_for(Backend), utf8),
    AllDeps = rebar_state:all_deps(State),
    case [D || D <- AllDeps, rebar_app_info:name(D) =:= DepName] of
        [] -> rebar_api:abort("backend dependency ~s not found", [DepName]);
        [_] -> ok
    end,
    %% The backend's own driver (minato, esqlite) is a transitive dep, and
    %% start_pool/2 starts it as an application - so its .app has to be
    %% reachable too, not just the backend's ebin.
    _ = [code:add_pathz(dep_ebin(D)) || D <- AllDeps],
    ok.

dep_ebin(App) ->
    unicode:characters_to_list(filename:join(rebar_app_info:dir(App), "ebin")).

dep_name_for(kura_backend_postgres) -> kura_postgres;
dep_name_for(kura_backend_sqlite) -> kura_sqlite;
dep_name_for(Other) -> Other.

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
    {
        rebar3_kura_introspect_pg:build_tables(ColRows, ConRows),
        rebar3_kura_introspect_pg:unsafe_tables(ColRows)
    }.

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
    column_default := D,
    atttypmod := M
}) ->
    {T, C, U, L, N, D, M}.

constraint_row(#{
    table_name := T, column_name := C, constraint_type := Ct, foreign_table := F
}) ->
    {T, C, Ct, F}.

%%----------------------------------------------------------------------
%% Codegen
%%----------------------------------------------------------------------

%% A name or type the catalog holds is printed as a term, so one with a
%% newline or a terminal escape in it cannot write lines of its own.
report_skipped(Tables, UnsafeTables, Strict) ->
    Skipped = lists:append([maps:get(skipped, T, []) || T <- Tables]),
    _ = [
        rebar_api:warn("Skipping table ~0tp: its name is not a safe identifier", [Table])
     || Table <- UnsafeTables
    ],
    _ = [warn_skipped(Skip) || Skip <- Skipped],
    case Strict andalso {Skipped, UnsafeTables} =/= {[], []} of
        true ->
            rebar_api:abort(
                "Aborting (--strict): ~p skipped columns, ~p skipped tables",
                [length(Skipped), length(UnsafeTables)]
            );
        false ->
            ok
    end.

warn_skipped({Table, Column, ~"unsafe identifier"}) ->
    rebar_api:warn(
        "Skipping ~s.~0tp: its name is not a safe identifier", [Table, Column]
    );
warn_skipped({Table, Column, TypeName}) ->
    rebar_api:warn(
        "Skipping ~s.~s: kura does not model PG type ~0tp", [Table, Column, TypeName]
    ).

generate_schema(App, AppDir, #{table := Table} = TableInfo, Force) ->
    Mod = module_name(App, Table),
    FileName = filename:join([AppDir, "src", "schemas", Mod ++ ".erl"]),
    Content = schema_source(App, TableInfo),
    write_file(FileName, Content, Force).

-doc """
Render a kura_schema module source for a table description. Pure.

A name that is not a plain atom is written quoted, so a column called `end`
or `_id` is the atom of that name, and no name can end the term it is in. No
atom is made of a name to do it.
""".
-spec schema_source(string(), map()) -> unicode:chardata().
schema_source(App, #{table := Table, fields := Fields} = TableInfo) ->
    Mod = module_name(App, Table),
    Assocs = maps:get(assocs, TableInfo, []),
    [
        "-module(",
        quoted_atom(Mod),
        ").\n",
        "-behaviour(kura_schema).\n",
        "-include_lib(\"kura/include/kura.hrl\").\n\n",
        "-export([table/0, fields/0",
        case Assocs of
            [] -> "";
            _ -> ", associations/0"
        end,
        "]).\n\n",
        "table() -> ",
        table_literal(Table),
        ".\n\n",
        "fields() ->\n    [\n",
        lists:join(",\n", [render_field(F) || F <- Fields]),
        "\n    ].\n",
        render_associations(App, Assocs)
    ].

%% A plain identifier is written with the sigil. Any other name is written
%% as the term it is, which holds whatever bytes it has.
-spec table_literal(binary()) -> iodata().
table_literal(Table) ->
    case rebar3_kura_introspect_pg:valid_ident(Table) of
        true -> [$~, $", Table, $"];
        false -> io_lib:format("~0tp", [Table])
    end.

render_field(#{name := Name, type := Type, primary_key := Pk, nullable := Nullable}) ->
    Parts =
        [
            io_lib:format("name = ~ts", [quoted_atom(Name)]),
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
    Schema = module_name(App, FkTable),
    io_lib:format(
        "        #kura_assoc{name = ~ts, type = belongs_to, schema = ~ts, foreign_key = ~ts}",
        [quoted_atom(Name), quoted_atom(Schema), quoted_atom(Column)]
    ).

-spec module_name(string(), binary()) -> string().
module_name(App, Table) ->
    App ++ "_" ++ characters(Table).

-spec characters(binary()) -> string().
characters(Name) ->
    case unicode:characters_to_list(Name) of
        Characters when is_list(Characters) -> Characters;
        _NotUtf8 -> binary_to_list(Name)
    end.

-spec quoted_atom(binary() | string()) -> string().
quoted_atom(Name) when is_binary(Name) ->
    quoted_atom(characters(Name));
quoted_atom(Name) when is_list(Name) ->
    case plain_atom(Name) of
        true -> Name;
        false -> lists:flatten(io_lib:write_string(Name, $'))
    end.

-spec plain_atom(string()) -> boolean().
plain_atom([First | Rest] = Name) when First >= $a, First =< $z ->
    [] =:= [C || C <- Rest, not plain_atom_character(C)] andalso
        not lists:member(Name, ?RESERVED_WORDS);
plain_atom(_Name) ->
    false.

-spec plain_atom_character(char()) -> boolean().
plain_atom_character(C) ->
    (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) orelse C =:= $_.

write_file(Path, Content, Force) ->
    case filelib:is_regular(Path) andalso not Force of
        true ->
            rebar_api:info("File already exists, skipping (use --force): ~s", [Path]);
        false ->
            ok = filelib:ensure_dir(Path),
            ok = file:write_file(Path, utf8(Content)),
            rebar_api:info("Created ~s", [Path])
    end.

-spec utf8(unicode:chardata()) -> binary().
utf8(Content) ->
    case unicode:characters_to_binary(Content) of
        Source when is_binary(Source) -> Source
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
