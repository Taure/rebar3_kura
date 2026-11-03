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
    Dir = make_temp_dir(),
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
    Dir = make_temp_dir(),
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

make_temp_dir() ->
    Base = os:getenv("TMPDIR", "/tmp"),
    Dir = filename:join(
        Base, "rebar3_kura_gen_auth_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_dir(filename:join(Dir, ".")),
    Dir.

unload(Mod) ->
    ok = kura_schema:forget(Mod),
    code:purge(Mod),
    code:delete(Mod).

%%====================================================================
%% Generated user changesets: secrets, email identity, password length
%%====================================================================

user_changeset_test_() ->
    {setup,
        fun() ->
            {User, Token} = load_schemas(),
            meck:new(nova_auth_password, [non_strict]),
            meck:expect(nova_auth_password, hash, fun(P) -> <<"hashed:", P/binary>> end),
            {User, Token}
        end,
        fun({User, Token}) ->
            meck:unload(nova_auth_password),
            unload(User),
            unload(Token)
        end,
        fun({User, _Token}) ->
            [
                {"valid registration keeps no plaintext", ?_test(no_plaintext_valid(User))},
                {"invalid registration keeps no plaintext", ?_test(no_plaintext_invalid(User))},
                {"password change keeps no plaintext", ?_test(no_plaintext_password_change(User))},
                {"email case folds to one address", ?_test(email_case_folds(User))},
                {"trailing newline is not an email", ?_test(email_trailing_newline(User))},
                {"invalid UTF-8 email is an error", ?_test(email_invalid_utf8(User))},
                {"email change resets confirmed_at", ?_test(email_change_unconfirms(User))},
                {"same email keeps confirmed_at", ?_test(same_email_keeps_confirmed(User))},
                {"password length counts characters", ?_test(password_counts_characters(User))},
                {"password longer than 1024 is refused", ?_test(password_max(User))},
                {"password counts code points", ?_test(password_counts_code_points(User))},
                {"invalid UTF-8 password is an error", ?_test(password_invalid_utf8(User))}
            ]
        end}.

registration(User, Email, Password, Confirmation) ->
    User:registration_changeset(#{}, #{
        ~"email" => Email, ~"password" => Password, ~"password_confirmation" => Confirmation
    }).

assert_no_plaintext(#kura_changeset{changes = Changes, params = Params}) ->
    Secret = [password, password_confirmation, ~"password", ~"password_confirmation"],
    ?assertEqual([], [K || K <- Secret, maps:is_key(K, Changes)]),
    ?assertEqual([], [K || K <- Secret, maps:is_key(K, Params)]).

no_plaintext_valid(User) ->
    CS = registration(User, ~"a@example.com", ~"correct horse battery", ~"correct horse battery"),
    ?assert(CS#kura_changeset.valid),
    assert_no_plaintext(CS).

no_plaintext_invalid(User) ->
    Mismatch = registration(User, ~"a@example.com", ~"correct horse battery", ~"other"),
    ?assertNot(Mismatch#kura_changeset.valid),
    assert_no_plaintext(Mismatch),
    Short = registration(User, ~"not an email", ~"short", ~"short"),
    ?assertNot(Short#kura_changeset.valid),
    assert_no_plaintext(Short).

no_plaintext_password_change(User) ->
    Data = #{id => 1, email => ~"a@example.com"},
    CS = User:password_changeset(Data, #{
        ~"password" => ~"a new long passphrase",
        ~"password_confirmation" => ~"a new long passphrase"
    }),
    ?assert(CS#kura_changeset.valid),
    ?assertEqual(
        ~"hashed:a new long passphrase", kura_changeset:get_change(CS, hashed_password)
    ),
    assert_no_plaintext(CS).

email_case_folds(User) ->
    Pass = ~"correct horse battery",
    Victim = registration(User, ~"victim@b.com", Pass, Pass),
    Attacker = registration(User, ~"Victim@B.com", Pass, Pass),
    ?assertEqual(~"victim@b.com", kura_changeset:get_change(Attacker, email)),
    ?assertEqual(
        kura_changeset:get_change(Victim, email), kura_changeset:get_change(Attacker, email)
    ),
    ?assertMatch(
        [_ | _], [
            C
         || #kura_constraint{type = unique, field = email} = C <-
                Attacker#kura_changeset.constraints
        ]
    ),
    ?assertEqual({ok, ~"victim@b.com"}, User:normalize_email(~"VICTIM@b.COM")).

email_trailing_newline(User) ->
    Pass = ~"correct horse battery",
    CS = registration(User, ~"victim@b.com\n", Pass, Pass),
    ?assertNot(CS#kura_changeset.valid),
    ?assertMatch([{email, _}], CS#kura_changeset.errors).

email_invalid_utf8(User) ->
    Pass = ~"correct horse battery",
    CS = registration(User, <<255, "@b.com">>, Pass, Pass),
    ?assertNot(CS#kura_changeset.valid),
    ?assertEqual([{email, ~"is not valid UTF-8"}], CS#kura_changeset.errors),
    ?assertEqual(error, User:normalize_email(<<255, "@b.com">>)).

email_change_unconfirms(User) ->
    Data = #{id => 1, email => ~"old@b.com", confirmed_at => {{2026, 1, 1}, {0, 0, 0}}},
    CS = User:email_changeset(Data, #{~"email" => ~"New@B.com"}),
    ?assert(CS#kura_changeset.valid),
    ?assertEqual(~"new@b.com", kura_changeset:get_change(CS, email)),
    ?assertEqual(#{email => ~"new@b.com", confirmed_at => undefined}, CS#kura_changeset.changes).

same_email_keeps_confirmed(User) ->
    Data = #{id => 1, email => ~"old@b.com", confirmed_at => {{2026, 1, 1}, {0, 0, 0}}},
    CS = User:email_changeset(Data, #{~"email" => ~"OLD@b.com"}),
    ?assertEqual(#{}, CS#kura_changeset.changes).

password_counts_characters(User) ->
    Emoji = <<16#1F600/utf8, 16#1F601/utf8, 16#1F602/utf8, 16#1F603/utf8>>,
    ?assertEqual(16, byte_size(Emoji)),
    Short = registration(User, ~"a@b.com", Emoji, Emoji),
    ?assertEqual([{password, ~"should be at least 12 character(s)"}], Short#kura_changeset.errors),
    Japanese = <<<<16#3042/utf8>> || _ <- lists:seq(1, 40)>>,
    Long = registration(User, ~"a@b.com", Japanese, Japanese),
    ?assert(Long#kura_changeset.valid).

password_max(User) ->
    TooLong = binary:copy(~"a", 1025),
    CS = registration(User, ~"a@b.com", TooLong, TooLong),
    ?assertEqual([{password, ~"should be at most 1024 character(s)"}], CS#kura_changeset.errors),
    Max = binary:copy(~"a", 1024),
    ?assert((registration(User, ~"a@b.com", Max, Max))#kura_changeset.valid).

password_counts_code_points(User) ->
    Combining = <<"a", (binary:copy(<<16#301/utf8>>, 2000))/binary, "bcdefghijkl">>,
    CS = registration(User, ~"a@b.com", Combining, Combining),
    ?assertEqual([{password, ~"should be at most 1024 character(s)"}], CS#kura_changeset.errors).

password_invalid_utf8(User) ->
    Bad = <<"correct horse ", 255>>,
    CS = registration(User, ~"a@b.com", Bad, Bad),
    ?assertEqual([{password, ~"is not valid UTF-8"}], CS#kura_changeset.errors).

instructions_cover_confirmation_and_normalised_login_test() ->
    Text = rebar3_kura_gen_auth:instructions("ga"),
    [
        ?assertNotEqual(nomatch, string:find(Text, Needle))
     || Needle <- [
            "rebar3 kura compile",
            "nova_auth_confirm:generate_confirm_token(ga_auth, User)",
            "nova_auth_confirm:confirm_user(ga_auth, Token)",
            "ga_user:normalize_email(RawEmail)",
            "nova_auth_accounts:change_identity(",
            "nova_auth_confirm:generate_confirm_token(ga_auth, Changed)",
            "confirmed_at",
            "{nova_auth, \"~> 0.2\"}"
        ]
    ],
    ?assertEqual(nomatch, string:find(Text, "APP")).
