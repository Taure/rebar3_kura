-module(rebar3_kura_check).
-moduledoc """
Compile-time gate that fails the build when declared schemas have
drifted from the migration history.

The runtime sibling, `kura_schema_verify:verify/1,2`, catches drift
in production. This provider catches it earlier — at PR review time
in CI — by computing the same diff `kura compile` would compute and
exiting non-zero if anything is missing.

```sh
rebar3 kura check       # exit 1 with a drift report, or 0 if clean
```

Use it as a CI step right after build. Anybody who edits a schema
without regenerating a migration (or any tool that generates an
incomplete diff) gets a red build instead of a silent landmine.
""".

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, check).
-define(NAMESPACE, kura).
-define(DEPS, [{default, app_discovery}]).

init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 kura check"},
        {opts, []},
        {short_desc, "Fail if schemas have drifted from migrations"},
        {desc,
            "Compares declared kura_schema modules against existing migrations, "
            "without generating new files. Exits non-zero if drift is detected. "
            "Intended as a CI gate, paired with `rebar3 kura compile` for the "
            "actual fix."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Apps =
        case rebar_state:current_app(State) of
            undefined -> rebar_state:project_apps(State);
            AppInfo -> [AppInfo]
        end,
    rebar3_kura_compile:ensure_kura_on_path(State),
    case check_apps(Apps, []) of
        [] ->
            rebar_api:info("kura: no schema drift", []),
            {ok, State};
        Drift ->
            report_drift(Drift),
            {error, format_error(schema_drift)}
    end.

format_error(schema_drift) ->
    "schema drift detected — run `rebar3 kura compile` to generate the "
    "missing migration, then commit the result";
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal
%%====================================================================

check_apps([], Acc) ->
    Acc;
check_apps([AppInfo | Rest], Acc) ->
    AppName = rebar_app_info:name(AppInfo),
    case check_app(AppInfo) of
        none -> check_apps(Rest, Acc);
        {drift, UpOps, DownOps} -> check_apps(Rest, [{AppName, UpOps, DownOps} | Acc])
    end.

check_app(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    SrcDir = filename:join(AppDir, "src"),
    MigDir = filename:join(SrcDir, "migrations"),
    SchemaFiles = rebar3_kura_compile:find_schema_files(SrcDir, MigDir),
    MigFiles = rebar3_kura_compile:find_migration_files(MigDir),
    CompileOpts = rebar3_kura_compile:compile_opts(AppInfo),
    {SchemaMods, MigMods} = rebar3_kura_compile:compile_all(
        SchemaFiles, MigFiles, CompileOpts
    ),
    AllLoaded = SchemaMods ++ MigMods,
    try
        DbState = kura_schema_diff:build_db_state(MigMods),
        DesiredState = kura_schema_diff:build_desired_state(SchemaMods),
        case kura_schema_diff:diff(DbState, DesiredState) of
            {[], []} -> none;
            {UpOps, DownOps} -> {drift, UpOps, DownOps}
        end
    after
        rebar3_kura_compile:cleanup(AllLoaded)
    end.

report_drift(Drift) ->
    rebar_api:info("kura: schema drift detected — see below", []),
    report_drift_loop(Drift).

report_drift_loop([]) ->
    ok;
report_drift_loop([{AppName, UpOps, _DownOps} | Rest]) ->
    rebar_api:info("kura: ~s has ~p missing migration op(s):", [AppName, length(UpOps)]),
    report_ops(UpOps),
    report_drift_loop(Rest).

report_ops([]) ->
    ok;
report_ops([Op | Rest]) ->
    rebar_api:info("  - ~s", [describe_op(Op)]),
    report_ops(Rest).

describe_op({create_table, Table, _Cols}) ->
    io_lib:format("missing CREATE TABLE \"~s\"", [Table]);
describe_op({drop_table, Table}) ->
    io_lib:format("schema removed table \"~s\" but no DROP TABLE migration", [Table]);
describe_op({alter_table, Table, AlterOps}) ->
    io_lib:format("missing ALTER TABLE \"~s\" (~p ops)", [Table, length(AlterOps)]);
describe_op({create_index, Table, Cols, Opts}) ->
    UniqueStr =
        case maps:get(unique, Opts, false) of
            true -> "UNIQUE ";
            false -> ""
        end,
    io_lib:format(
        "missing CREATE ~sINDEX on \"~s\" (~s)",
        [UniqueStr, Table, format_cols(Cols)]
    );
describe_op({drop_index, Name}) ->
    io_lib:format("schema removed index \"~s\" but no DROP INDEX migration", [Name]);
describe_op({execute, SQL}) ->
    io_lib:format("missing execute: ~s", [SQL]).

format_cols(Cols) ->
    Names = [atom_to_list(C) || C <- Cols],
    string:join(Names, ", ").
