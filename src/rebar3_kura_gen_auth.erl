-module(rebar3_kura_gen_auth).

-export([init/1, do/1, format_error/1]).
-export([render_user/1, render_user_token/1, render_auth_config/1, instructions/1]).

-define(PROVIDER, gen_auth).
-define(NAMESPACE, kura).
-define(DEPS, [{default, app_discovery}]).

-doc false.
-spec init(term()) -> {ok, term()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 kura gen_auth"},
        {opts, []},
        {short_desc, "Generate nova_auth user and token schemas"},
        {desc,
            "Generates the Kura user and user token schemas and the nova_auth\n"
            "config module for email/password authentication with nova_auth.\n"
            "Run `rebar3 kura compile` to generate the migration from the schemas.\n\n"
            "Generated files:\n"
            "  src/schemas/<app>_user.erl\n"
            "  src/schemas/<app>_user_token.erl\n"
            "  src/<app>_auth.erl\n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-doc false.
-spec do(term()) -> {ok, term()} | {error, string()}.
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
    FmtOpts = rebar3_kura_compile:erlfmt_opts(State),
    Files = [
        {filename:join([AppDir, "src", "schemas", AppName ++ "_user.erl"]), render_user(AppName)},
        {
            filename:join([AppDir, "src", "schemas", AppName ++ "_user_token.erl"]),
            render_user_token(AppName)
        },
        {filename:join([AppDir, "src", AppName ++ "_auth.erl"]), render_auth_config(AppName)}
    ],
    _ = [
        write_file_if_not_exists(Path, rebar3_kura_compile:format_source(Content, FmtOpts))
     || {Path, Content} <- Files
    ],
    print_instructions(AppName),
    {ok, State}.

-doc false.
-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%======================================================================
%% User schema
%%======================================================================

-spec render_user(string()) -> iolist().
render_user(App) ->
    [
        "-module(",
        App,
        "_user).\n"
        "-behaviour(kura_schema).\n"
        "-include_lib(\"kura/include/kura.hrl\").\n"
        "\n"
        "-export([table/0, fields/0, indexes/0]).\n"
        "-export([registration_changeset/2, password_changeset/2, email_changeset/2, normalize_email/1]).\n"
        "\n"
        "table() -> ~\"users\".\n"
        "\n"
        "fields() ->\n"
        "    [\n"
        "        #kura_field{name = id, type = id, primary_key = true, nullable = false},\n"
        "        #kura_field{name = email, type = string, nullable = false},\n"
        "        #kura_field{name = hashed_password, type = string, nullable = false},\n"
        "        #kura_field{name = confirmed_at, type = utc_datetime},\n"
        "        #kura_field{name = inserted_at, type = utc_datetime},\n"
        "        #kura_field{name = updated_at, type = utc_datetime},\n"
        "        #kura_field{name = password, type = string, virtual = true},\n"
        "        #kura_field{name = password_confirmation, type = string, virtual = true}\n"
        "    ].\n"
        "\n"
        "indexes() ->\n"
        "    [{[email], #{unique => true}}].\n"
        "\n"
        "registration_changeset(Data, Params) ->\n"
        "    CS = kura_changeset:cast(?MODULE, Data, Params, [email, password, password_confirmation]),\n"
        "    CS1 = kura_changeset:validate_required(CS, [email, password]),\n"
        "    scrub_password(hash_password(validate_password(validate_email(CS1)))).\n"
        "\n"
        "password_changeset(Data, Params) ->\n"
        "    CS = kura_changeset:cast(?MODULE, Data, Params, [password, password_confirmation]),\n"
        "    CS1 = kura_changeset:validate_required(CS, [password]),\n"
        "    scrub_password(hash_password(validate_password(CS1))).\n"
        "\n"
        "%% A new address is unconfirmed until nova_auth_confirm confirms it again.\n"
        "email_changeset(Data, Params) ->\n"
        "    CS = kura_changeset:cast(?MODULE, Data, Params, [email]),\n"
        "    CS1 = validate_email(kura_changeset:validate_required(CS, [email])),\n"
        "    case kura_changeset:get_change(CS1, email) of\n"
        "        undefined -> CS1;\n"
        "        _ -> kura_changeset:put_change(CS1, confirmed_at, undefined)\n"
        "    end.\n"
        "\n"
        "%% Emails are stored normalised, so the unique index sees one address\n"
        "%% whatever its case. Normalise a login email the same way before\n"
        "%% nova_auth_accounts:authenticate/3.\n"
        "-spec normalize_email(term()) -> {ok, binary()} | error.\n"
        "normalize_email(Email) when is_binary(Email) ->\n"
        "    case unicode:characters_to_nfc_binary(Email) of\n"
        "        Nfc when is_binary(Nfc) -> {ok, string:lowercase(Nfc)};\n"
        "        _ -> error\n"
        "    end;\n"
        "normalize_email(_) ->\n"
        "    error.\n"
        "\n"
        "validate_email(CS) ->\n"
        "    case kura_changeset:get_change(CS, email) of\n"
        "        undefined ->\n"
        "            CS;\n"
        "        Email ->\n"
        "            case normalize_email(Email) of\n"
        "                {ok, Normal} ->\n"
        "                    check_email(drop_unchanged_email(kura_changeset:put_change(CS, email, Normal)));\n"
        "                error ->\n"
        "                    kura_changeset:add_error(CS, email, ~\"is not valid UTF-8\")\n"
        "            end\n"
        "    end.\n"
        "\n"
        "check_email(CS) ->\n"
        "    CS1 = kura_changeset:validate_format(CS, email, ~\"^[^@\\\\s]+@[^@\\\\s]+\\\\z\"),\n"
        "    CS2 = validate_chars(CS1, email, 3, 160),\n"
        "    kura_changeset:unique_constraint(CS2, email).\n"
        "\n"
        "drop_unchanged_email(\n"
        "    #kura_changeset{data = #{email := Email}, changes = #{email := Email} = Changes} = CS\n"
        ") ->\n"
        "    CS#kura_changeset{changes = maps:remove(email, Changes)};\n"
        "drop_unchanged_email(CS) ->\n"
        "    CS.\n"
        "\n"
        "validate_password(CS) ->\n"
        "    CS1 = validate_chars(CS, password, 12, 1024),\n"
        "    kura_changeset:validate_confirmation(CS1, password).\n"
        "\n"
        "%% Counted in characters: kura_changeset:validate_length/3 counts bytes.\n"
        "validate_chars(CS, Field, Min, Max) ->\n"
        "    case kura_changeset:get_change(CS, Field) of\n"
        "        Value when is_binary(Value) ->\n"
        "            case unicode:characters_to_list(Value) of\n"
        "                Chars when is_list(Chars) -> check_chars(CS, Field, string:length(Value), Min, Max);\n"
        "                _ -> kura_changeset:add_error(CS, Field, ~\"is not valid UTF-8\")\n"
        "            end;\n"
        "        _ ->\n"
        "            CS\n"
        "    end.\n"
        "\n"
        "check_chars(CS, Field, Length, Min, _Max) when Length < Min ->\n"
        "    kura_changeset:add_error(CS, Field, chars_message(\"should be at least ~B character(s)\", Min));\n"
        "check_chars(CS, Field, Length, _Min, Max) when Length > Max ->\n"
        "    kura_changeset:add_error(CS, Field, chars_message(\"should be at most ~B character(s)\", Max));\n"
        "check_chars(CS, _Field, _Length, _Min, _Max) ->\n"
        "    CS.\n"
        "\n"
        "chars_message(Format, N) ->\n"
        "    iolist_to_binary(io_lib:format(Format, [N])).\n"
        "\n"
        "hash_password(#kura_changeset{valid = true} = CS) ->\n"
        "    case kura_changeset:get_change(CS, password) of\n"
        "        undefined ->\n"
        "            CS;\n"
        "        Password ->\n"
        "            kura_changeset:put_change(CS, hashed_password, nova_auth_password:hash(Password))\n"
        "    end;\n"
        "hash_password(CS) ->\n"
        "    CS.\n"
        "\n"
        "%% A changeset comes back in {error, CS} and can end up in a log or a crash\n"
        "%% report, so the plaintext does not outlive hashing, valid or not.\n"
        "scrub_password(#kura_changeset{changes = Changes, params = Params} = CS) ->\n"
        "    Secret = [password, password_confirmation, ~\"password\", ~\"password_confirmation\"],\n"
        "    CS#kura_changeset{\n"
        "        changes = maps:without(Secret, Changes), params = maps:without(Secret, Params)\n"
        "    }.\n"
    ].

%%======================================================================
%% User token schema
%%======================================================================

%% family_id and used_at are what nova_auth_refresh needs on top of the
%% session token fields.
-spec render_user_token(string()) -> iolist().
render_user_token(App) ->
    [
        "-module(",
        App,
        "_user_token).\n"
        "-behaviour(kura_schema).\n"
        "-include_lib(\"kura/include/kura.hrl\").\n"
        "\n"
        "-export([table/0, fields/0, indexes/0, associations/0]).\n"
        "\n"
        "table() -> ~\"user_tokens\".\n"
        "\n"
        "fields() ->\n"
        "    [\n"
        "        #kura_field{name = id, type = id, primary_key = true, nullable = false},\n"
        "        #kura_field{name = user_id, type = bigint, nullable = false},\n"
        "        #kura_field{name = token, type = string, nullable = false},\n"
        "        #kura_field{name = context, type = string, nullable = false},\n"
        "        #kura_field{name = family_id, type = string},\n"
        "        #kura_field{name = used_at, type = utc_datetime},\n"
        "        #kura_field{name = inserted_at, type = utc_datetime}\n"
        "    ].\n"
        "\n"
        "indexes() ->\n"
        "    [\n"
        "        {[context, token], #{unique => true}},\n"
        "        {[user_id], #{}}\n"
        "    ].\n"
        "\n"
        "associations() ->\n"
        "    [\n"
        "        #kura_assoc{\n"
        "            name = user,\n"
        "            type = belongs_to,\n"
        "            schema = ",
        App,
        "_user,\n"
        "            foreign_key = user_id,\n"
        "            on_delete = cascade\n"
        "        }\n"
        "    ].\n"
    ].

%%======================================================================
%% nova_auth config
%%======================================================================

-spec render_auth_config(string()) -> iolist().
render_auth_config(App) ->
    [
        "-module(",
        App,
        "_auth).\n"
        "-behaviour(nova_auth).\n"
        "\n"
        "-export([config/0]).\n"
        "\n"
        "config() ->\n"
        "    #{\n"
        "        repo => ",
        App,
        "_repo,\n"
        "        user_schema => ",
        App,
        "_user,\n"
        "        token_schema => ",
        App,
        "_user_token\n"
        "    }.\n"
    ].

%%======================================================================
%% Print instructions
%%======================================================================

print_instructions(App) ->
    rebar_api:info("~ts", [instructions(App)]).

-spec instructions(string()) -> string().
instructions(App) ->
    Template =
        "\n==> Kura auth files generated\n"
        "\n"
        "  src/schemas/APP_user.erl\n"
        "  src/schemas/APP_user_token.erl\n"
        "  src/APP_auth.erl\n"
        "\n"
        "Next steps:\n"
        "\n"
        "1. Add nova_auth to your deps, and to `applications` in your .app.src:\n"
        "\n"
        "   {deps, [..., {nova_auth, \"~> 0.2\"}]}.\n"
        "\n"
        "2. Run `rebar3 kura compile` to generate the migration for the users\n"
        "   and user_tokens tables from the schemas (with the pre-compile hook,\n"
        "   `rebar3 compile` does it too). If either table already exists, read\n"
        "   the generated migration before running it.\n"
        "\n"
        "3. APP_auth assumes your repo module is APP_repo; edit it if not.\n"
        "   Register, confirm the address, then log in through nova_auth:\n"
        "\n"
        "   {ok, User} = nova_auth_accounts:register(\n"
        "       APP_auth, fun APP_user:registration_changeset/2, Params),\n"
        "   {ok, Token} = nova_auth_confirm:generate_confirm_token(APP_auth, User),\n"
        "   %% send Token to the address; when it comes back:\n"
        "   {ok, _} = nova_auth_confirm:confirm_user(APP_auth, Token),\n"
        "\n"
        "   %% Emails are stored normalised, so normalise the login one too:\n"
        "   {ok, Email} = APP_user:normalize_email(RawEmail),\n"
        "   {ok, User} = nova_auth_accounts:authenticate(APP_auth, Email, Password),\n"
        "   %% Issue a session only when maps:get(confirmed_at, User) =/= undefined.\n"
        "\n"
        "   APP_user:email_changeset/2 resets confirmed_at when the address\n"
        "   changes: send a fresh nova_auth_confirm:generate_confirm_token/2 for\n"
        "   the new address.\n",
    lists:flatten(string:replace(Template, "APP", App, all)).

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
