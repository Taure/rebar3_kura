-module(rebar3_kura_setup).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).
-export([render_repo/2]).

-define(PROVIDER, setup).
-define(NAMESPACE, kura).
-define(DEPS, [{default, app_discovery}]).

init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 kura setup --name my_app_db"},
        {opts, [
            {name, $n, "name", string, "Repo module name (default: APPNAME_repo)"}
        ]},
        {short_desc, "Set up Kura in your project"},
        {desc,
            "Checks prerequisites, generates a repo module, creates the migrations directory,\n"
            "and prints remaining manual steps."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    App =
        case rebar_state:current_app(State) of
            undefined ->
                case rebar_state:project_apps(State) of
                    [A | _] -> A;
                    [] -> rebar_api:abort("No application found", [])
                end;
            A ->
                A
        end,
    AppDir = rebar_app_info:dir(App),
    AppName = binary_to_list(rebar_app_info:name(App)),
    ModName =
        case proplists:get_value(name, Args) of
            undefined -> AppName ++ "_repo";
            N -> N
        end,

    rebar_api:info("~n==> Setting up Kura for ~s~n", [AppName]),

    %% Step 1: Check kura dependency
    check_kura_dep(State, AppDir),

    %% Step 2: Generate repo module
    generate_repo(AppDir, ModName, AppName),

    %% Step 3: Create migrations directory
    create_migrations_dir(AppDir),

    %% Step 4: Check provider hook
    check_provider_hook(AppDir),

    %% Step 5: Print remaining manual steps
    print_next_steps(AppName, ModName),

    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%====================================================================
%% Setup steps
%%====================================================================

check_kura_dep(State, AppDir) ->
    %% all_deps only sees fetched deps; also check rebar.config directly
    AllDeps = rebar_state:all_deps(State),
    case [D || D <- AllDeps, rebar_app_info:name(D) =:= <<"kura">>] of
        [_] ->
            rebar_api:info("[ok] kura dependency found", []);
        [] ->
            case has_kura_in_config(AppDir) of
                true ->
                    rebar_api:info("[ok] kura dependency declared in rebar.config", []);
                false ->
                    rebar_api:info(
                        "[!!] kura not found in deps. Add to rebar.config:~n"
                        "     {deps, [{kura, \"~~> 1.2\"}]}.",
                        []
                    )
            end
    end.

has_kura_in_config(AppDir) ->
    ConfigFile = filename:join(AppDir, "rebar.config"),
    case file:consult(ConfigFile) of
        {ok, Terms} ->
            Deps = proplists:get_value(deps, Terms, []),
            lists:any(fun is_kura_dep/1, Deps);
        _ ->
            false
    end.

is_kura_dep(kura) -> true;
is_kura_dep({kura, _}) -> true;
is_kura_dep({kura, _, _}) -> true;
is_kura_dep(_) -> false.

generate_repo(AppDir, ModName, AppName) ->
    FileName = filename:join([AppDir, "src", ModName ++ ".erl"]),
    case filelib:is_regular(FileName) of
        true ->
            rebar_api:info("[ok] ~s already exists, skipping", [ModName]);
        false ->
            Content = render_repo(ModName, AppName),
            ok = file:write_file(FileName, Content),
            rebar_api:info("[ok] generated ~s", [FileName])
    end.

create_migrations_dir(AppDir) ->
    MigDir = filename:join([AppDir, "src", "migrations"]),
    case filelib:is_dir(MigDir) of
        true ->
            rebar_api:info("[ok] src/migrations/ already exists", []);
        false ->
            ok = filelib:ensure_dir(filename:join(MigDir, ".")),
            ok = file:make_dir(MigDir),
            rebar_api:info("[ok] created src/migrations/", [])
    end.

check_provider_hook(AppDir) ->
    ConfigFile = filename:join(AppDir, "rebar.config"),
    case file:consult(ConfigFile) of
        {ok, Terms} ->
            Hooks = proplists:get_value(provider_hooks, Terms, []),
            PreHooks = proplists:get_value(pre, Hooks, []),
            case lists:member({compile, {kura, compile}}, PreHooks) of
                true ->
                    rebar_api:info("[ok] provider hook configured", []);
                false ->
                    rebar_api:info(
                        "[!!] provider hook missing. Add to rebar.config:~n"
                        "     {provider_hooks, [{pre, [{compile, {kura, compile}}]}]}.",
                        []
                    )
            end;
        _ ->
            rebar_api:info(
                "[!!] could not read rebar.config to check provider hooks", []
            )
    end.

print_next_steps(AppName, ModName) ->
    rebar_api:info(
        "~n"
        "--- Next steps ---~n"
        "~n"
        "1. Add the provider hook to rebar.config:~n"
        "~n"
        "   {provider_hooks, [~n"
        "       {pre, [{compile, {kura, compile}}]}~n"
        "   ]}.~n"
        "~n"
        "2. Start the repo and run migrations in ~s_app:start/2:~n"
        "~n"
        "   ~s:start(),~n"
        "   kura_migrator:migrate(~s),~n"
        "~n"
        "3. Create a schema in src/schemas/:~n"
        "~n"
        "   -module(my_schema).~n"
        "   -behaviour(kura_schema).~n"
        "   -include_lib(\"kura/include/kura.hrl\").~n"
        "   -export([table/0, primary_key/0, fields/0]).~n"
        "~n"
        "   table() -> <<\"my_table\">>.~n"
        "   primary_key() -> id.~n"
        "   fields() -> [~n"
        "       #kura_field{name = id, type = id},~n"
        "       #kura_field{name = name, type = string}~n"
        "   ].~n"
        "~n"
        "4. Run `rebar3 compile` — migrations will be auto-generated.~n",
        [AppName, ModName, ModName]
    ).

%%====================================================================
%% Repo rendering
%%====================================================================

render_repo(ModName, AppName) ->
    io_lib:format(
        "-module(~s).~n"
        "-behaviour(kura_repo).~n"
        "~n"
        "-export([~n"
        "    config/0,~n"
        "    start/0,~n"
        "    all/1,~n"
        "    get/2,~n"
        "    get_by/2,~n"
        "    one/1,~n"
        "    insert/1,~n"
        "    insert/2,~n"
        "    update/1,~n"
        "    delete/1,~n"
        "    insert_all/2,~n"
        "    update_all/2,~n"
        "    delete_all/1,~n"
        "    preload/3,~n"
        "    transaction/1,~n"
        "    multi/1,~n"
        "    query/2,~n"
        "    exists/1,~n"
        "    reload/2,~n"
        "    insert_all/3~n"
        "]).~n"
        "~n"
        "config() ->~n"
        "    Database = application:get_env(~s, database, <<\"~s_dev\">>),~n"
        "    #{~n"
        "        pool => ?MODULE,~n"
        "        database => Database,~n"
        "        hostname => <<\"localhost\">>,~n"
        "        port => 5432,~n"
        "        username => <<\"postgres\">>,~n"
        "        password => <<\"postgres\">>,~n"
        "        pool_size => 10~n"
        "    }.~n"
        "~n"
        "start() -> kura_repo_worker:start(?MODULE).~n"
        "all(Q) -> kura_repo_worker:all(?MODULE, Q).~n"
        "get(Schema, Id) -> kura_repo_worker:get(?MODULE, Schema, Id).~n"
        "get_by(Schema, Clauses) -> kura_repo_worker:get_by(?MODULE, Schema, Clauses).~n"
        "one(Q) -> kura_repo_worker:one(?MODULE, Q).~n"
        "insert(CS) -> kura_repo_worker:insert(?MODULE, CS).~n"
        "insert(CS, Opts) -> kura_repo_worker:insert(?MODULE, CS, Opts).~n"
        "update(CS) -> kura_repo_worker:update(?MODULE, CS).~n"
        "delete(CS) -> kura_repo_worker:delete(?MODULE, CS).~n"
        "insert_all(Schema, Entries) -> kura_repo_worker:insert_all(?MODULE, Schema, Entries).~n"
        "update_all(Q, Updates) -> kura_repo_worker:update_all(?MODULE, Q, Updates).~n"
        "delete_all(Q) -> kura_repo_worker:delete_all(?MODULE, Q).~n"
        "preload(Schema, Records, Assocs) -> kura_repo_worker:preload(?MODULE, Schema, Records, Assocs).~n"
        "transaction(Fun) -> kura_repo_worker:transaction(?MODULE, Fun).~n"
        "multi(Multi) -> kura_repo_worker:multi(?MODULE, Multi).~n"
        "query(SQL, Params) -> kura_repo_worker:query(?MODULE, SQL, Params).~n"
        "exists(Q) -> kura_repo_worker:exists(?MODULE, Q).~n"
        "reload(Schema, Record) -> kura_repo_worker:reload(?MODULE, Schema, Record).~n"
        "insert_all(Schema, Entries, Opts) -> kura_repo_worker:insert_all(?MODULE, Schema, Entries, Opts).~n",
        [ModName, AppName, AppName]
    ).
