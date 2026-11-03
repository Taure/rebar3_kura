-module(rebar3_kura_gen_auth_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

rendered() ->
    [
        {"ga_user", rebar3_kura_gen_auth:render_user("ga")},
        {"ga_user_token", rebar3_kura_gen_auth:render_user_token("ga")},
        {"ga_auth", rebar3_kura_gen_auth:render_auth_config("ga")}
    ].

generated_modules_are_erlfmt_clean_test_() ->
    [
        {Name, ?_assertEqual({ok, Chars, []}, erlfmt:format_string(Chars, []))}
     || {Name, Content} <- rendered(),
        Chars <- [binary_to_list(iolist_to_binary(Content))]
    ].

generated_modules_use_nova_auth_not_bcrypt_or_thoas_test_() ->
    [
        {Name, [
            ?_assertEqual(nomatch, binary:match(iolist_to_binary(Content), ~"bcrypt")),
            ?_assertEqual(nomatch, binary:match(iolist_to_binary(Content), ~"thoas"))
        ]}
     || {Name, Content} <- rendered()
    ].

auth_config_names_the_generated_schemas_test() ->
    {ok, Mod} = compile_and_load("ga_auth", rebar3_kura_gen_auth:render_auth_config("ga")),
    try
        ?assertEqual(
            #{repo => ga_repo, user_schema => ga_user, token_schema => ga_user_token},
            Mod:config()
        )
    after
        unload(Mod)
    end.

schemas_generate_users_then_user_tokens_test() ->
    {User, Token} = load_schemas(),
    try
        Desired = kura_schema_diff:build_desired_state([Token, User]),
        {Up, Down} = kura_schema_diff:diff(#{}, Desired),
        [{create_table, ~"users", UserCols}, {create_table, ~"user_tokens", TokenCols} | Idx] = Up,
        ?assertEqual(
            [id, email, hashed_password, confirmed_at, inserted_at, updated_at],
            [C#kura_column.name || C <- UserCols]
        ),
        [UserId] = [C || C <- TokenCols, C#kura_column.name =:= user_id],
        ?assertEqual({~"users", id}, UserId#kura_column.references),
        ?assertEqual(cascade, UserId#kura_column.on_delete),
        ?assert(lists:member({create_index, ~"users", [email], #{unique => true}}, Idx)),
        ?assert(
            lists:member({create_index, ~"user_tokens", [context, token], #{unique => true}}, Idx)
        ),
        ?assertEqual([{drop_table, ~"user_tokens"}, {drop_table, ~"users"}], Down)
    after
        unload(User),
        unload(Token)
    end.

registration_changeset_hashes_with_nova_auth_test() ->
    {User, Token} = load_schemas(),
    meck:new(nova_auth_password, [non_strict]),
    meck:expect(nova_auth_password, hash, fun(P) -> <<"hashed:", P/binary>> end),
    Password = ~"correct horse battery",
    try
        CS = User:registration_changeset(#{}, #{
            ~"email" => ~"a@example.com",
            ~"password" => Password,
            ~"password_confirmation" => Password
        }),
        ?assert(CS#kura_changeset.valid),
        ?assertEqual(
            <<"hashed:", Password/binary>>, kura_changeset:get_change(CS, hashed_password)
        ),
        Bad = User:registration_changeset(#{}, #{
            ~"email" => ~"a@example.com",
            ~"password" => Password,
            ~"password_confirmation" => ~"something else"
        }),
        ?assertNot(Bad#kura_changeset.valid),
        ?assertEqual(undefined, kura_changeset:get_change(Bad, hashed_password))
    after
        meck:unload(nova_auth_password),
        unload(User),
        unload(Token)
    end.

do_writes_schemas_and_config_but_no_migration_test() ->
    Dir = filename:join(
        "/tmp", "rebar3_kura_gen_auth_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_dir(filename:join([Dir, "src", "."])),
    App = rebar_app_info:dir(element(2, rebar_app_info:new(~"ga", "0.1.0")), Dir),
    State = rebar_state:project_apps(rebar_state:new(), [App]),
    try
        {ok, _} = rebar3_kura_gen_auth:do(State),
        ?assertEqual(
            ["src/ga_auth.erl", "src/schemas/ga_user.erl", "src/schemas/ga_user_token.erl"],
            lists:sort(filelib:wildcard("src/**/*.erl", Dir))
        ),
        ?assertNot(filelib:is_dir(filename:join([Dir, "src", "migrations"])))
    after
        file:del_dir_r(Dir)
    end.

load_schemas() ->
    {ok, User} = compile_and_load("ga_user", rebar3_kura_gen_auth:render_user("ga")),
    {ok, Token} = compile_and_load("ga_user_token", rebar3_kura_gen_auth:render_user_token("ga")),
    {User, Token}.

compile_and_load(ModName, Source) ->
    Dir = filename:join(
        "/tmp", "rebar3_kura_gen_auth_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_dir(filename:join(Dir, ".")),
    File = filename:join(Dir, ModName ++ ".erl"),
    ok = file:write_file(File, Source),
    Include = filename:join(code:lib_dir(kura), "include"),
    Result = compile:file(File, [binary, return_errors, {i, Include}]),
    ok = file:del_dir_r(Dir),
    case Result of
        {ok, Mod, Bin} when is_atom(Mod), is_binary(Bin) ->
            {module, Mod} = code:load_binary(Mod, File, Bin),
            {ok, Mod};
        {ok, Mod, Bin, _Warnings} when is_atom(Mod), is_binary(Bin) ->
            {module, Mod} = code:load_binary(Mod, File, Bin),
            {ok, Mod};
        {error, Errors, _} ->
            {error, Errors}
    end.

unload(Mod) ->
    ok = kura_schema:forget(Mod),
    code:purge(Mod),
    code:delete(Mod).
