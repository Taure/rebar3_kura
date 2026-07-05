-module(rebar3_kura_lint_migrations).
-moduledoc """
Compile-time gate that flags unsafe migration operations before they
reach a running database.

Some DDL is a foot-gun during a rolling deploy: dropping a table or
column that old code still reads, renaming a column out from under
running nodes, retyping a column in place, or adding a `NOT NULL`
column with no default (which rewrites and locks the whole table).
`kura` already warns about these at apply time via
`kura_migrator:check_unsafe_operations/2`; this provider runs the same
check statically, in CI, so an unsafe migration fails review instead of
a deploy.

```sh
rebar3 kura lint_migrations          # exit 1 with a report, or 0 if clean
rebar3 kura lint_migrations --warn   # report only, always exit 0
```

An operation the author has judged safe (say the column was already
dead) is opted out per-op through the migration's `safe/0` callback -
the same list `kura_migrator` honours at apply time.
""".

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-ifdef(TEST).
-export([lint_module/1, safe_entries/1, outcome/2]).
-endif.

-define(PROVIDER, lint_migrations).
-define(NAMESPACE, kura).
-define(DEPS, [{default, app_discovery}]).

init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 kura lint_migrations"},
        {opts, [
            {warn, $w, "warn", boolean, "Report unsafe operations but do not fail the build"}
        ]},
        {short_desc, "Flag unsafe (destructive or blocking) migration operations"},
        {desc,
            "Statically runs kura's unsafe-operation check over every migration's "
            "up/0, flagging drops, renames, in-place retypes, and NOT NULL columns "
            "without a default. Exits non-zero on a finding unless --warn is given. "
            "Honours each migration's safe/0 opt-out. Intended as a CI gate."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Apps =
        case rebar_state:current_app(State) of
            undefined -> rebar_state:project_apps(State);
            AppInfo -> [AppInfo]
        end,
    rebar3_kura_compile:ensure_kura_on_path(State),
    Findings = lint_apps(Apps, []),
    {Args, _} = rebar_state:command_parsed_args(State),
    case outcome(Findings, proplists:get_bool(warn, Args)) of
        clean ->
            rebar_api:info("kura: no unsafe migration operations", []),
            {ok, State};
        warn ->
            report(Findings),
            rebar_api:warn(
                "kura: ~b unsafe migration operation(s) - not failing (--warn)",
                [length(Findings)]
            ),
            {ok, State};
        fail ->
            report(Findings),
            {error, format_error(unsafe_migrations)}
    end.

%% The CI-gate contract: findings fail the build unless --warn downgrades them.
outcome([], _Warn) -> clean;
outcome(_Findings, true) -> warn;
outcome(_Findings, false) -> fail.

format_error(unsafe_migrations) ->
    "unsafe migration operations detected - make each one backwards-compatible "
    "with the currently deployed code, or mark it safe via the migration's "
    "safe/0 callback; re-run with --warn to report without failing";
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Internal
%%====================================================================

lint_apps([], Acc) ->
    lists:reverse(Acc);
lint_apps([AppInfo | Rest], Acc) ->
    lint_apps(Rest, lists:reverse(lint_app(AppInfo), Acc)).

lint_app(AppInfo) ->
    AppName = rebar_app_info:name(AppInfo),
    AppDir = rebar_app_info:dir(AppInfo),
    MigDir = filename:join([AppDir, "src", "migrations"]),
    MigFiles = rebar3_kura_compile:find_migration_files(MigDir),
    CompileOpts = rebar3_kura_compile:compile_opts(AppInfo),
    {_SchemaMods, MigMods} = rebar3_kura_compile:compile_all([], MigFiles, CompileOpts),
    try
        [{AppName, Mod, W} || Mod <- MigMods, W <- lint_module(Mod)]
    after
        rebar3_kura_compile:cleanup(MigMods)
    end.

lint_module(Mod) ->
    kura_migrator:check_unsafe_operations(Mod:up(), safe_entries(Mod)).

safe_entries(Mod) ->
    case erlang:function_exported(Mod, safe, 0) of
        true -> Mod:safe();
        false -> []
    end.

report(Findings) ->
    rebar_api:warn("kura: unsafe migration operation(s) detected:", []),
    lists:foreach(fun report_one/1, Findings).

report_one({AppName, Mod, #{op := Op, target := Target, risk := Risk, safe_alt := Alt} = Finding}) ->
    rebar_api:warn("  ~s / ~p: ~s ~p~s", [AppName, Mod, Op, Target, table_suffix(Finding)]),
    rebar_api:warn("      risk: ~s", [Risk]),
    rebar_api:warn("      safe: ~s", [Alt]).

table_suffix(#{table := Table}) -> [" on table ", Table];
table_suffix(_) -> "".
