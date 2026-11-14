-module(rebar3_kura_introspect_pg_tests).
-include_lib("eunit/include/eunit.hrl").

col(Table, Name, Udt, Len, Nullable, Default) ->
    col(Table, Name, Udt, Len, Nullable, Default, -1).

col(Table, Name, Udt, Len, Nullable, Default, TypMod) ->
    {Table, Name, Udt, Len, Nullable, Default, TypMod}.

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

build_tables_vector_dimension_test() ->
    Cols = [
        col(~"items", ~"embedding", ~"vector", null, ~"YES", null, 1536),
        col(~"items", ~"loose", ~"vector", null, ~"YES", null, -1),
        col(~"items", ~"unread", ~"vector", null, ~"YES", null, null),
        col(~"items", ~"width", ~"int4", null, ~"YES", null, 8),
        col(~"items", ~"history", ~"_vector", null, ~"YES", null, 4),
        col(~"items", ~"names", ~"_varchar", null, ~"YES", null, 259),
        col(~"items", ~"one", ~"vector", null, ~"YES", null, 1)
    ],
    [T] = rebar3_kura_introspect_pg:build_tables(Cols, []),
    ?assertEqual(
        [{vector, 1536}, vector, vector, integer, {array, {vector, 4}}, {array, text}, {vector, 1}],
        [maps:get(type, F) || F <- maps:get(fields, T)]
    ).

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

%%----------------------------------------------------------------------
%% Names a catalog can hold
%%----------------------------------------------------------------------

hostile_names() ->
    [
        ~"users\n",
        ~"\nusers",
        ~"us\ners",
        ~"x'). evil() -> ok. %",
        ~"a\"b",
        ~"a'b",
        ~"a)b",
        ~"a;b",
        ~"a b",
        ~"a\tb",
        ~"a\0b",
        ~"a.b",
        ~"användare",
        ~"用户",
        ~"Users",
        ~"1abc",
        ~"",
        ~"../../etc/passwd",
        ~"a/b",
        <<"a", 255>>,
        binary:copy(~"a", 64)
    ].

hostile_names_are_not_valid_test_() ->
    [?_assertNot(rebar3_kura_introspect_pg:valid_ident(Name)) || Name <- hostile_names()].

longest_name_is_valid_test() ->
    ?assert(rebar3_kura_introspect_pg:valid_ident(binary:copy(~"a", 63))).

%% A reserved word is a name PostgreSQL allows quoted, and the generator
%% writes it as a quoted atom.
reserved_words_are_valid_test_() ->
    [
        ?_assert(rebar3_kura_introspect_pg:valid_ident(Name))
     || Name <- [~"end", ~"if", ~"receive", ~"maybe", ~"_"]
    ].

hostile_tables_are_left_out_and_listed_test_() ->
    [
        fun() ->
            Cols = [
                col(Name, ~"id", ~"int8", null, ~"NO", null),
                col(~"users", ~"id", ~"int8", null, ~"NO", null)
            ],
            ?assertMatch([#{table := ~"users"}], rebar3_kura_introspect_pg:build_tables(Cols, [])),
            ?assertEqual([Name], rebar3_kura_introspect_pg:unsafe_tables(Cols))
        end
     || Name <- hostile_names()
    ].

safe_tables_are_not_listed_test() ->
    Cols = [col(~"users", ~"id", ~"int8", null, ~"NO", null)],
    ?assertEqual([], rebar3_kura_introspect_pg:unsafe_tables(Cols)).

hostile_columns_are_skipped_test_() ->
    [
        fun() ->
            Cols = [
                col(~"users", ~"id", ~"int8", null, ~"NO", null),
                col(~"users", Name, ~"text", null, ~"YES", null)
            ],
            [Table] = rebar3_kura_introspect_pg:build_tables(Cols, []),
            ?assertMatch([#{name := ~"id"}], maps:get(fields, Table)),
            ?assertEqual([{~"users", Name, ~"unsafe identifier"}], maps:get(skipped, Table))
        end
     || Name <- hostile_names()
    ].

hostile_foreign_keys_make_no_association_test_() ->
    Cols = [
        col(~"posts", ~"id", ~"int8", null, ~"NO", null),
        col(~"posts", ~"author_id", ~"int8", null, ~"NO", null)
    ],
    [
        fun() ->
            ByTable = [{~"posts", ~"author_id", ~"FOREIGN KEY", Name}],
            ByColumn = [{~"posts", Name, ~"FOREIGN KEY", ~"users"}],
            ?assertMatch(
                [#{assocs := []}], rebar3_kura_introspect_pg:build_tables(Cols, ByTable)
            ),
            ?assertMatch(
                [#{assocs := []}], rebar3_kura_introspect_pg:build_tables(Cols, ByColumn)
            )
        end
     || Name <- hostile_names()
    ].

a_hostile_type_name_is_only_reported_test() ->
    Type = ~"x'). evil() -> ok. %\n",
    Cols = [col(~"users", ~"shape", Type, null, ~"YES", null)],
    ?assertMatch(
        [#{fields := [], skipped := [{~"users", ~"shape", _}]}],
        rebar3_kura_introspect_pg:build_tables(Cols, [])
    ).
