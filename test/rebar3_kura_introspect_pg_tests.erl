-module(rebar3_kura_introspect_pg_tests).
-include_lib("eunit/include/eunit.hrl").

col(Table, Name, Udt, Len, Nullable, Default) ->
    {Table, Name, Udt, Len, Nullable, Default}.

build_tables_maps_types_and_pk_test() ->
    Cols = [
        col(<<"users">>, <<"id">>, <<"int8">>, null, <<"NO">>, <<"nextval('users_id_seq')">>),
        col(<<"users">>, <<"email">>, <<"varchar">>, 255, <<"NO">>, null),
        col(<<"users">>, <<"bio">>, <<"text">>, null, <<"YES">>, null)
    ],
    Cons = [{<<"users">>, <<"id">>, <<"PRIMARY KEY">>, null}],
    [T] = rebar3_kura_introspect_pg:build_tables(Cols, Cons),
    ?assertEqual(<<"users">>, maps:get(table, T)),
    [Id, Email, Bio] = maps:get(fields, T),
    ?assertEqual(id, maps:get(type, Id)),
    ?assertEqual(true, maps:get(primary_key, Id)),
    ?assertEqual(false, maps:get(nullable, Id)),
    ?assertEqual(string, maps:get(type, Email)),
    ?assertEqual(false, maps:get(primary_key, Email)),
    ?assertEqual(text, maps:get(type, Bio)),
    ?assertEqual(true, maps:get(nullable, Bio)),
    ?assertEqual([], maps:get(skipped, T)).

build_tables_skips_unsupported_test() ->
    Cols = [
        col(<<"docs">>, <<"id">>, <<"int8">>, null, <<"NO">>, <<"nextval('x')">>),
        col(<<"docs">>, <<"search">>, <<"tsvector">>, null, <<"YES">>, null)
    ],
    [T] = rebar3_kura_introspect_pg:build_tables(Cols, []),
    ?assertEqual(1, length(maps:get(fields, T))),
    ?assertEqual([{<<"docs">>, <<"search">>, <<"tsvector">>}], maps:get(skipped, T)).

build_tables_foreign_key_belongs_to_test() ->
    Cols = [
        col(<<"posts">>, <<"id">>, <<"int8">>, null, <<"NO">>, <<"nextval('x')">>),
        col(<<"posts">>, <<"author_id">>, <<"int8">>, null, <<"NO">>, null)
    ],
    Cons = [
        {<<"posts">>, <<"id">>, <<"PRIMARY KEY">>, null},
        {<<"posts">>, <<"author_id">>, <<"FOREIGN KEY">>, <<"users">>}
    ],
    [T] = rebar3_kura_introspect_pg:build_tables(Cols, Cons),
    ?assertEqual(
        [{belongs_to, <<"user">>, <<"users">>, <<"author_id">>}], maps:get(assocs, T)
    ).

build_tables_array_type_test() ->
    Cols = [col(<<"t">>, <<"tags">>, <<"_text">>, null, <<"YES">>, null)],
    [T] = rebar3_kura_introspect_pg:build_tables(Cols, []),
    [F] = maps:get(fields, T),
    ?assertEqual({array, text}, maps:get(type, F)).

queries_are_strings_test() ->
    ?assert(is_list(rebar3_kura_introspect_pg:columns_query())),
    ?assert(is_list(rebar3_kura_introspect_pg:constraints_query())).

valid_ident_test_() ->
    [
        ?_assert(rebar3_kura_introspect_pg:valid_ident(<<"users">>)),
        ?_assert(rebar3_kura_introspect_pg:valid_ident(<<"author_id">>)),
        ?_assert(rebar3_kura_introspect_pg:valid_ident(<<"_hidden">>)),
        ?_assertNot(rebar3_kura_introspect_pg:valid_ident(<<"Users">>)),
        ?_assertNot(rebar3_kura_introspect_pg:valid_ident(<<"a-b">>)),
        ?_assertNot(rebar3_kura_introspect_pg:valid_ident(<<"has space">>)),
        ?_assertNot(rebar3_kura_introspect_pg:valid_ident(<<"../evil">>)),
        ?_assertNot(rebar3_kura_introspect_pg:valid_ident(<<"x\">>.">>))
    ].

build_tables_drops_unsafe_table_test() ->
    Cols = [col(<<"Bad Table">>, <<"id">>, <<"int8">>, null, <<"NO">>, <<"nextval('x')">>)],
    ?assertEqual([], rebar3_kura_introspect_pg:build_tables(Cols, [])).

build_tables_skips_unsafe_column_test() ->
    Cols = [
        col(<<"users">>, <<"id">>, <<"int8">>, null, <<"NO">>, <<"nextval('x')">>),
        col(<<"users">>, <<"Bad Col">>, <<"varchar">>, 255, <<"YES">>, null)
    ],
    [T] = rebar3_kura_introspect_pg:build_tables(Cols, []),
    ?assertEqual(1, length(maps:get(fields, T))),
    ?assertMatch([{<<"users">>, <<"Bad Col">>, _}], maps:get(skipped, T)).
