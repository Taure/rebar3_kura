-module(rebar3_kura_compile).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-include_lib("kura/include/kura.hrl").

-define(PROVIDER, compile).
-define(NAMESPACE, kura).
-define(DEPS, [{default, app_discovery}]).

init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 kura compile"},
        {opts, []},
        {short_desc, "Auto-generate Kura migrations from schema changes"},
        {desc,
            "Diffs kura_schema modules against existing migrations and generates new migration files."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Apps =
        case rebar_state:current_app(State) of
            undefined -> rebar_state:project_apps(State);
            AppInfo -> [AppInfo]
        end,
    ensure_kura_on_path(State),
    lists:foreach(fun(AppInfo) -> process_app(AppInfo) end, Apps),
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal
%%====================================================================

ensure_kura_on_path(State) ->
    case code:lib_dir(kura) of
        {error, bad_name} ->
            AllDeps = rebar_state:all_deps(State),
            case [D || D <- AllDeps, rebar_app_info:name(D) =:= <<"kura">>] of
                [KuraDep] ->
                    Dir = rebar_app_info:dir(KuraDep),
                    code:add_pathz(filename:join(Dir, "ebin")),
                    ok;
                [] ->
                    rebar_api:abort("kura dependency not found", [])
            end;
        _ ->
            ok
    end.

process_app(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    SrcDir = filename:join(AppDir, "src"),
    MigDir = filename:join(SrcDir, "migrations"),

    SchemaFiles = find_schema_files(SrcDir, MigDir),
    MigFiles = find_migration_files(MigDir),

    CompileOpts = compile_opts(AppInfo),
    {SchemaMods, MigMods} = compile_all(SchemaFiles, MigFiles, CompileOpts),
    AllLoaded = SchemaMods ++ MigMods,

    try
        DbState = kura_schema_diff:build_db_state(MigMods),
        DesiredState = kura_schema_diff:build_desired_state(SchemaMods),
        case kura_schema_diff:diff(DbState, DesiredState) of
            {[], []} ->
                rebar_api:info("kura: schemas up to date", []);
            {UpOps, DownOps} ->
                filelib:ensure_dir(filename:join(MigDir, ".")),
                generate_migration(MigDir, UpOps, DownOps),
                rebar_api:info("kura: migration generated", [])
        end
    after
        cleanup(AllLoaded)
    end.

find_schema_files(SrcDir, MigDir) ->
    AllErl = filelib:wildcard(filename:join([SrcDir, "**", "*.erl"])),
    [
        F
     || F <- AllErl,
        not lists:prefix(MigDir, F),
        is_schema_file(F)
    ].

is_schema_file(File) ->
    {ok, Bin} = file:read_file(File),
    re:run(Bin, "-behaviou?r\\(kura_schema\\)") =/= nomatch.

find_migration_files(MigDir) ->
    case filelib:is_dir(MigDir) of
        true ->
            filelib:wildcard(filename:join(MigDir, "m*.erl"));
        false ->
            []
    end.

compile_opts(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    [
        binary,
        return_errors,
        {i, filename:join(AppDir, "include")},
        {i, filename:join(AppDir, "src")}
    ].

compile_all(SchemaFiles, MigFiles, Opts) ->
    SchemaMods = [compile_and_load(F, Opts) || F <- SchemaFiles],
    MigMods = [compile_and_load(F, Opts) || F <- lists:sort(MigFiles)],
    {SchemaMods, MigMods}.

compile_and_load(File, Opts) ->
    case compile:file(File, Opts) of
        {ok, Mod, Bin} ->
            {module, Mod} = code:load_binary(Mod, File, Bin),
            Mod;
        {ok, Mod, Bin, _Warnings} ->
            {module, Mod} = code:load_binary(Mod, File, Bin),
            Mod;
        {error, Errors, _Warnings} ->
            rebar_api:abort("Failed to compile ~s: ~p", [File, Errors])
    end.

cleanup(Modules) ->
    lists:foreach(
        fun(Mod) ->
            code:purge(Mod),
            code:delete(Mod)
        end,
        Modules
    ).

%%====================================================================
%% Migration generation
%%====================================================================

generate_migration(MigDir, UpOps, DownOps) ->
    Timestamp = make_timestamp(MigDir),
    Desc = derive_description(UpOps),
    ModName = lists:flatten(io_lib:format("m~s_~s", [Timestamp, Desc])),
    FileName = filename:join(MigDir, ModName ++ ".erl"),

    Content = render_migration(ModName, UpOps, DownOps),
    ok = file:write_file(FileName, Content),
    rebar_api:info("kura: generated ~s", [FileName]).

make_timestamp(MigDir) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    Ts = lists:flatten(
        io_lib:format(
            "~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B",
            [Y, Mo, D, H, Mi, S]
        )
    ),
    %% Check for collision
    Pattern = filename:join(MigDir, "m" ++ Ts ++ "_*.erl"),
    case filelib:wildcard(Pattern) of
        [] ->
            Ts;
        _ ->
            %% Bump by 1 second
            Secs = calendar:datetime_to_gregorian_seconds({{Y, Mo, D}, {H, Mi, S}}) + 1,
            {{Y2, Mo2, D2}, {H2, Mi2, S2}} = calendar:gregorian_seconds_to_datetime(Secs),
            lists:flatten(
                io_lib:format(
                    "~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B",
                    [Y2, Mo2, D2, H2, Mi2, S2]
                )
            )
    end.

derive_description(Ops) ->
    case Ops of
        [{create_table, Table, _}] ->
            "create_" ++ binary_to_list(Table);
        [{alter_table, Table, _}] ->
            "alter_" ++ binary_to_list(Table);
        [{drop_table, Table}] ->
            "drop_" ++ binary_to_list(Table);
        _ ->
            "update_schema"
    end.

render_migration(ModName, UpOps, DownOps) ->
    io_lib:format(
        "-module(~s).~n"
        "-behaviour(kura_migration).~n"
        "-include_lib(\"kura/include/kura.hrl\").~n"
        "-export([up/0, down/0]).~n"
        "~n"
        "up() ->~n"
        "    ~s.~n"
        "~n"
        "down() ->~n"
        "    ~s.~n",
        [ModName, render_ops(UpOps), render_ops(DownOps)]
    ).

render_ops(Ops) ->
    Inner = lists:join(",\n     ", [render_op(Op) || Op <- Ops]),
    io_lib:format("[~s]", [Inner]).

render_op({create_table, Table, Cols}) ->
    ColStrs = lists:join(",\n        ", [render_column(C) || C <- Cols]),
    io_lib:format("{create_table, <<\"~s\">>, [~n        ~s~n    ]}", [Table, ColStrs]);
render_op({drop_table, Table}) ->
    io_lib:format("{drop_table, <<\"~s\">>}", [Table]);
render_op({alter_table, Table, AlterOps}) ->
    OpStrs = lists:join(",\n        ", [render_alter_op(Op) || Op <- AlterOps]),
    io_lib:format("{alter_table, <<\"~s\">>, [~n        ~s~n    ]}", [Table, OpStrs]).

render_alter_op({add_column, Col}) ->
    io_lib:format("{add_column, ~s}", [render_column(Col)]);
render_alter_op({drop_column, Name}) ->
    io_lib:format("{drop_column, ~p}", [Name]);
render_alter_op({modify_column, Name, Type}) ->
    io_lib:format("{modify_column, ~p, ~p}", [Name, Type]);
render_alter_op({rename_column, Old, New}) ->
    io_lib:format("{rename_column, ~p, ~p}", [Old, New]).

render_column(#kura_column{name = N, type = T, nullable = Null, default = Def, primary_key = PK}) ->
    Parts = [io_lib:format("name = ~p", [N]), io_lib:format("type = ~p", [T])],
    Parts2 =
        case PK of
            true -> Parts ++ ["primary_key = true"];
            false -> Parts
        end,
    Parts3 =
        case Null of
            false -> Parts2 ++ ["nullable = false"];
            true -> Parts2
        end,
    Parts4 =
        case Def of
            undefined -> Parts3;
            _ -> Parts3 ++ [io_lib:format("default = ~p", [Def])]
        end,
    io_lib:format("#kura_column{~s}", [lists:join(", ", Parts4)]).
