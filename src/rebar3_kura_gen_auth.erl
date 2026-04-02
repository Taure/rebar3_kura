-module(rebar3_kura_gen_auth).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, gen_auth).
-define(NAMESPACE, kura).
-define(DEPS, [{default, app_discovery}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 kura gen_auth"},
        {opts, []},
        {short_desc, "Generate email/password authentication schemas"},
        {desc,
            "Generates Kura schemas, migration, and accounts context module\n"
            "for email/password authentication.\n\n"
            "Generated files:\n"
            "  src/migrations/m<timestamp>_create_auth_tables.erl\n"
            "  src/schemas/<app>_user.erl\n"
            "  src/schemas/<app>_user_token.erl\n"
            "  src/<app>_accounts.erl\n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
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
    generate_migration(AppDir),
    generate_user_schema(AppName, AppDir),
    generate_user_token_schema(AppName, AppDir),
    generate_accounts(AppName, AppDir),
    print_instructions(AppName),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%======================================================================
%% Internal: timestamp for migration filename
%%======================================================================

timestamp() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    lists:flatten(
        io_lib:format(
            "~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B",
            [Y, Mo, D, H, Mi, S]
        )
    ).

%%======================================================================
%% Migration
%%======================================================================

generate_migration(AppDir) ->
    TS = timestamp(),
    Mod = "m" ++ TS ++ "_create_auth_tables",
    FileName = filename:join([AppDir, "src", "migrations", Mod ++ ".erl"]),
    Content = [
        "-module(",
        Mod,
        ").\n"
        "-moduledoc false.\n"
        "-behaviour(kura_migration).\n"
        "-include_lib(\"kura/include/kura.hrl\").\n\n"
        "-export([up/0, down/0]).\n\n"
        "-spec up() -> [kura_migration:operation()].\n"
        "up() ->\n"
        "    [{execute, ~\"CREATE EXTENSION IF NOT EXISTS citext\"},\n"
        "     {create_table, ~\"users\", [\n"
        "         #kura_column{name = id, type = id, primary_key = true},\n"
        "         #kura_column{name = email, type = string, nullable = false},\n"
        "         #kura_column{name = hashed_password, type = string, nullable = false},\n"
        "         #kura_column{name = confirmed_at, type = utc_datetime},\n"
        "         #kura_column{name = inserted_at, type = utc_datetime},\n"
        "         #kura_column{name = updated_at, type = utc_datetime}\n"
        "     ]},\n"
        "     {create_index, ~\"users_email_index\", ~\"users\", [email], [unique]},\n"
        "     {create_table, ~\"user_tokens\", [\n"
        "         #kura_column{name = id, type = id, primary_key = true},\n"
        "         #kura_column{name = user_id, type = integer, nullable = false},\n"
        "         #kura_column{name = token, type = string, nullable = false},\n"
        "         #kura_column{name = context, type = string, nullable = false},\n"
        "         #kura_column{name = inserted_at, type = utc_datetime}\n"
        "     ]},\n"
        "     {create_index, ~\"user_tokens_user_id_index\", ~\"user_tokens\", [user_id], []},\n"
        "     {create_index, ~\"user_tokens_context_token_index\", ~\"user_tokens\",\n"
        "         [context, token], [unique]},\n"
        "     {execute, ~\"ALTER TABLE user_tokens ADD CONSTRAINT user_tokens_user_id_fkey \"\n"
        "              ~\"FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE\"}\n"
        "    ].\n\n"
        "-spec down() -> [kura_migration:operation()].\n"
        "down() ->\n"
        "    [{drop_table, ~\"user_tokens\"},\n"
        "     {drop_table, ~\"users\"}].\n"
    ],
    write_file_if_not_exists(FileName, Content).

%%======================================================================
%% User schema
%%======================================================================

generate_user_schema(App, AppDir) ->
    Mod = App ++ "_user",
    FileName = filename:join([AppDir, "src", "schemas", Mod ++ ".erl"]),
    Content = [
        "-module(",
        Mod,
        ").\n"
        "-behaviour(kura_schema).\n"
        "-include_lib(\"kura/include/kura.hrl\").\n\n"
        "-export([table/0, fields/0]).\n"
        "-export([registration_changeset/2, password_changeset/2, email_changeset/2]).\n\n"
        "table() -> <<\"users\">>.\n\n"
        "fields() ->\n"
        "    [#kura_field{name = id, type = id, primary_key = true, nullable = false},\n"
        "     #kura_field{name = email, type = string, nullable = false},\n"
        "     #kura_field{name = hashed_password, type = string, nullable = false},\n"
        "     #kura_field{name = confirmed_at, type = utc_datetime},\n"
        "     #kura_field{name = inserted_at, type = utc_datetime},\n"
        "     #kura_field{name = updated_at, type = utc_datetime},\n"
        "     #kura_field{name = password, type = string, virtual = true},\n"
        "     #kura_field{name = password_confirmation, type = string, virtual = true}].\n\n"
        "registration_changeset(Data, Params) ->\n"
        "    CS = kura_changeset:cast(",
        Mod,
        ", Data, Params,\n"
        "             [email, password, password_confirmation]),\n"
        "    CS1 = kura_changeset:validate_required(CS, [email, password, password_confirmation]),\n"
        "    CS2 = kura_changeset:validate_format(CS1, email, <<\"^[^@\\\\s]+@[^@\\\\s]+$\">>),\n"
        "    CS3 = kura_changeset:validate_length(CS2, email, [{max, 160}]),\n"
        "    CS4 = kura_changeset:validate_length(CS3, password, [{min, 12}, {max, 72}]),\n"
        "    CS5 = validate_password_confirmation(CS4),\n"
        "    CS6 = maybe_hash_password(CS5),\n"
        "    kura_changeset:unique_constraint(CS6, email).\n\n"
        "password_changeset(Data, Params) ->\n"
        "    CS = kura_changeset:cast(",
        Mod,
        ", Data, Params,\n"
        "             [password, password_confirmation]),\n"
        "    CS1 = kura_changeset:validate_required(CS, [password, password_confirmation]),\n"
        "    CS2 = kura_changeset:validate_length(CS1, password, [{min, 12}, {max, 72}]),\n"
        "    CS3 = validate_password_confirmation(CS2),\n"
        "    maybe_hash_password(CS3).\n\n"
        "email_changeset(Data, Params) ->\n"
        "    CS = kura_changeset:cast(",
        Mod,
        ", Data, Params, [email]),\n"
        "    CS1 = kura_changeset:validate_required(CS, [email]),\n"
        "    CS2 = kura_changeset:validate_format(CS1, email, <<\"^[^@\\\\s]+@[^@\\\\s]+$\">>),\n"
        "    CS3 = kura_changeset:validate_length(CS2, email, [{max, 160}]),\n"
        "    kura_changeset:unique_constraint(CS3, email).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Internal\n"
        "%%----------------------------------------------------------------------\n\n"
        "validate_password_confirmation(CS) ->\n"
        "    case {kura_changeset:get_change(CS, password),\n"
        "          kura_changeset:get_change(CS, password_confirmation)} of\n"
        "        {Pass, Pass} when Pass =/= undefined -> CS;\n"
        "        {undefined, _} -> CS;\n"
        "        _ -> kura_changeset:add_error(CS, password_confirmation,\n"
        "                 <<\"does not match password\">>)\n"
        "    end.\n\n"
        "maybe_hash_password(#kura_changeset{valid = true} = CS) ->\n"
        "    case kura_changeset:get_change(CS, password) of\n"
        "        undefined -> CS;\n"
        "        Password ->\n"
        "            Hashed = list_to_binary(\n"
        "                bcrypt:hashpw(binary_to_list(Password), bcrypt:gen_salt())),\n"
        "            kura_changeset:put_change(CS, hashed_password, Hashed)\n"
        "    end;\n"
        "maybe_hash_password(CS) ->\n"
        "    CS.\n"
    ],
    write_file_if_not_exists(FileName, Content).

%%======================================================================
%% User token schema
%%======================================================================

generate_user_token_schema(App, AppDir) ->
    Mod = App ++ "_user_token",
    UserMod = App ++ "_user",
    FileName = filename:join([AppDir, "src", "schemas", Mod ++ ".erl"]),
    Content = [
        "-module(",
        Mod,
        ").\n"
        "-behaviour(kura_schema).\n"
        "-include_lib(\"kura/include/kura.hrl\").\n\n"
        "-export([table/0, fields/0, associations/0]).\n\n"
        "table() -> <<\"user_tokens\">>.\n\n"
        "fields() ->\n"
        "    [#kura_field{name = id, type = id, primary_key = true, nullable = false},\n"
        "     #kura_field{name = user_id, type = integer, nullable = false},\n"
        "     #kura_field{name = token, type = string, nullable = false},\n"
        "     #kura_field{name = context, type = string, nullable = false},\n"
        "     #kura_field{name = inserted_at, type = utc_datetime}].\n\n"
        "associations() ->\n"
        "    [#kura_assoc{name = user, type = belongs_to, schema = ",
        UserMod,
        ",\n"
        "                 foreign_key = user_id}].\n"
    ],
    write_file_if_not_exists(FileName, Content).

%%======================================================================
%% Accounts context
%%======================================================================

generate_accounts(App, AppDir) ->
    Mod = App ++ "_accounts",
    Repo = App ++ "_repo",
    UserMod = App ++ "_user",
    TokenMod = App ++ "_user_token",
    FileName = filename:join([AppDir, "src", Mod ++ ".erl"]),
    Content = [
        "-module(",
        Mod,
        ").\n"
        "-include_lib(\"kura/include/kura.hrl\").\n\n"
        "-export([\n"
        "    register_user/1,\n"
        "    get_user_by_email_and_password/2,\n"
        "    get_user_by_id/1,\n"
        "    generate_session_token/1,\n"
        "    get_user_by_session_token/1,\n"
        "    delete_session_token/1,\n"
        "    delete_all_user_tokens/1,\n"
        "    change_user_password/3,\n"
        "    change_user_email/3,\n"
        "    user_to_json/1,\n"
        "    format_errors/1\n"
        "]).\n\n"
        "-define(SESSION_VALIDITY_DAYS, 14).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Registration\n"
        "%%----------------------------------------------------------------------\n\n"
        "register_user(Params) ->\n"
        "    Now = calendar:universal_time(),\n"
        "    CS = ",
        UserMod,
        ":registration_changeset(#{}, Params),\n"
        "    CS1 = kura_changeset:put_change(CS, inserted_at, Now),\n"
        "    CS2 = kura_changeset:put_change(CS1, updated_at, Now),\n"
        "    ",
        Repo,
        ":insert(CS2).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Authentication\n"
        "%%----------------------------------------------------------------------\n\n"
        "get_user_by_email_and_password(Email, Password) ->\n"
        "    Q = kura_query:where(kura_query:from(",
        UserMod,
        "), {email, Email}),\n"
        "    case ",
        Repo,
        ":all(Q) of\n"
        "        {ok, [User]} ->\n"
        "            case verify_password(Password, maps:get(hashed_password, User)) of\n"
        "                true -> {ok, User};\n"
        "                false -> {error, invalid_credentials}\n"
        "            end;\n"
        "        _ ->\n"
        "            dummy_verify(),\n"
        "            {error, invalid_credentials}\n"
        "    end.\n\n"
        "get_user_by_id(Id) ->\n"
        "    case ",
        Repo,
        ":get(",
        UserMod,
        ", Id) of\n"
        "        {ok, User} -> {ok, User};\n"
        "        _ -> {error, not_found}\n"
        "    end.\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Session tokens\n"
        "%%----------------------------------------------------------------------\n\n"
        "generate_session_token(User) ->\n"
        "    Raw = crypto:strong_rand_bytes(32),\n"
        "    SessionToken = base64:encode(Raw),\n"
        "    HashedToken = base64:encode(crypto:hash(sha256, Raw)),\n"
        "    Now = calendar:universal_time(),\n"
        "    CS = kura_changeset:cast(",
        TokenMod,
        ", #{}, #{user_id => maps:get(id, User),\n"
        "        token => HashedToken, context => <<\"session\">>,\n"
        "        inserted_at => Now}, [user_id, token, context, inserted_at]),\n"
        "    case ",
        Repo,
        ":insert(CS) of\n"
        "        {ok, _} -> {ok, SessionToken};\n"
        "        {error, _} = Err -> Err\n"
        "    end.\n\n"
        "get_user_by_session_token(SessionToken) ->\n"
        "    try\n"
        "        Raw = base64:decode(SessionToken),\n"
        "        HashedToken = base64:encode(crypto:hash(sha256, Raw)),\n"
        "        Q = kura_query:where(\n"
        "                kura_query:where(kura_query:from(",
        TokenMod,
        "),\n"
        "                    {token, HashedToken}),\n"
        "                {context, <<\"session\">>}),\n"
        "        case ",
        Repo,
        ":all(Q) of\n"
        "            {ok, [Token]} ->\n"
        "                case token_valid(maps:get(inserted_at, Token)) of\n"
        "                    true -> get_user_by_id(maps:get(user_id, Token));\n"
        "                    false -> {error, token_expired}\n"
        "                end;\n"
        "            _ -> {error, not_found}\n"
        "        end\n"
        "    catch\n"
        "        _:_ -> {error, invalid_token}\n"
        "    end.\n\n"
        "delete_session_token(SessionToken) ->\n"
        "    try\n"
        "        Raw = base64:decode(SessionToken),\n"
        "        HashedToken = base64:encode(crypto:hash(sha256, Raw)),\n"
        "        Q = kura_query:where(\n"
        "                kura_query:where(kura_query:from(",
        TokenMod,
        "),\n"
        "                    {token, HashedToken}),\n"
        "                {context, <<\"session\">>}),\n"
        "        ",
        Repo,
        ":delete_all(Q),\n"
        "        ok\n"
        "    catch\n"
        "        _:_ -> ok\n"
        "    end.\n\n"
        "delete_all_user_tokens(UserId) ->\n"
        "    Q = kura_query:where(kura_query:from(",
        TokenMod,
        "), {user_id, UserId}),\n"
        "    ",
        Repo,
        ":delete_all(Q),\n"
        "    ok.\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Password & email changes\n"
        "%%----------------------------------------------------------------------\n\n"
        "change_user_password(User, CurrentPassword, NewParams) ->\n"
        "    case verify_password(CurrentPassword, maps:get(hashed_password, User)) of\n"
        "        true ->\n"
        "            Now = calendar:universal_time(),\n"
        "            CS = ",
        UserMod,
        ":password_changeset(User, NewParams),\n"
        "            CS1 = kura_changeset:put_change(CS, updated_at, Now),\n"
        "            case ",
        Repo,
        ":update(CS1) of\n"
        "                {ok, UpdatedUser} ->\n"
        "                    delete_all_user_tokens(maps:get(id, User)),\n"
        "                    {ok, UpdatedUser};\n"
        "                {error, _} = Err -> Err\n"
        "            end;\n"
        "        false ->\n"
        "            {error, invalid_password}\n"
        "    end.\n\n"
        "change_user_email(User, CurrentPassword, NewParams) ->\n"
        "    case verify_password(CurrentPassword, maps:get(hashed_password, User)) of\n"
        "        true ->\n"
        "            Now = calendar:universal_time(),\n"
        "            CS = ",
        UserMod,
        ":email_changeset(User, NewParams),\n"
        "            CS1 = kura_changeset:put_change(CS, updated_at, Now),\n"
        "            case ",
        Repo,
        ":update(CS1) of\n"
        "                {ok, UpdatedUser} ->\n"
        "                    delete_all_user_tokens(maps:get(id, User)),\n"
        "                    {ok, UpdatedUser};\n"
        "                {error, _} = Err -> Err\n"
        "            end;\n"
        "        false ->\n"
        "            {error, invalid_password}\n"
        "    end.\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% JSON helpers\n"
        "%%----------------------------------------------------------------------\n\n"
        "user_to_json(User) ->\n"
        "    #{<<\"id\">> => maps:get(id, User),\n"
        "      <<\"email\">> => maps:get(email, User)}.\n\n"
        "format_errors(#kura_changeset{errors = Errors}) ->\n"
        "    maps:from_list([{atom_to_binary(F), M} || {F, M} <- Errors]).\n\n"
        "%%----------------------------------------------------------------------\n"
        "%% Internal\n"
        "%%----------------------------------------------------------------------\n\n"
        "verify_password(Password, HashedPassword) ->\n"
        "    Hash = list_to_binary(\n"
        "        bcrypt:hashpw(binary_to_list(Password), binary_to_list(HashedPassword))),\n"
        "    crypto:hash_equals(Hash, HashedPassword).\n\n"
        "dummy_verify() ->\n"
        "    bcrypt:hashpw(\"dummy\", bcrypt:gen_salt()),\n"
        "    false.\n\n"
        "token_valid(InsertedAt) ->\n"
        "    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),\n"
        "    TokenTime = calendar:datetime_to_gregorian_seconds(InsertedAt),\n"
        "    (Now - TokenTime) < (?SESSION_VALIDITY_DAYS * 24 * 60 * 60).\n"
    ],
    write_file_if_not_exists(FileName, Content).

%%======================================================================
%% Print instructions
%%======================================================================

print_instructions(App) ->
    rebar_api:info("~n==> Kura auth files generated successfully!~n", []),
    rebar_api:info("Generated:~n", []),
    rebar_api:info("  src/migrations/m*_create_auth_tables.erl~n", []),
    rebar_api:info("  src/schemas/~s_user.erl~n", [App]),
    rebar_api:info("  src/schemas/~s_user_token.erl~n", [App]),
    rebar_api:info("  src/~s_accounts.erl~n~n", [App]),
    rebar_api:info("Next steps:~n", []),
    rebar_api:info("1. Add bcrypt to your deps in rebar.config:~n", []),
    rebar_api:info("   {deps, [..., bcrypt]}.~n~n", []),
    rebar_api:info("2. Run the migration:~n", []),
    rebar_api:info("   rebar3 kura migrate~n~n", []),
    rebar_api:info(
        "If using Nova, run 'rebar3 nova gen_auth' to generate~n"
        "controllers and security modules.~n",
        []
    ).

%%======================================================================
%% Helpers
%%======================================================================

write_file_if_not_exists(Path, Content) ->
    case filelib:is_regular(Path) of
        true ->
            rebar_api:info("File already exists, skipping: ~s", [Path]),
            skipped;
        false ->
            ok = filelib:ensure_dir(Path),
            ok = file:write_file(Path, Content),
            rebar_api:info("Created ~s", [Path]),
            ok
    end.
