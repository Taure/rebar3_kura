-module(kura_schema_diff_column_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

%%====================================================================
%% A field stored under another column name
%%====================================================================

renamed_test_() ->
    {foreach, fun() -> ok end, fun(_) -> meck:unload() end, [
        fun renamed_field_creates_its_column/0,
        fun renamed_key_field_is_the_key_column/0,
        fun renamed_composite_key_names_its_columns/0,
        fun renamed_foreign_key_field_carries_the_reference/0,
        fun reference_names_the_column_of_a_renamed_target_key/0,
        fun reference_names_the_column_of_a_renamed_ref_target_key/0,
        fun foreign_key_naming_no_field_still_aborts/0,
        fun index_names_the_column/0,
        fun index_naming_the_renamed_field_aborts/0,
        fun renamed_schema_settles_after_replay/0,
        fun renaming_a_column_is_an_add_and_a_drop/0
    ]}.

renamed_field_creates_its_column() ->
    schema(col_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = email, type = string, column = ~"email_address", nullable = false}
    ]),
    ?assertEqual(
        [
            {create_table, ~"users", [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = ~"email_address", type = string, nullable = false}
            ]}
        ],
        up([col_users])
    ).

renamed_key_field_is_the_key_column() ->
    schema(col_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true, column = ~"user_id"},
        #kura_field{name = name, type = string}
    ]),
    ?assertMatch(
        [
            {create_table, ~"users", [
                #kura_column{name = ~"user_id", primary_key = true},
                #kura_column{name = name, primary_key = false}
            ]}
        ],
        up([col_users])
    ).

renamed_composite_key_names_its_columns() ->
    schema(col_members, ~"members", [
        #kura_field{name = team, type = integer, column = ~"team_id"},
        #kura_field{name = user, type = integer, column = ~"user_id"}
    ]),
    meck:expect(col_members, key, fun() -> [team, user] end),
    ?assertMatch(
        [
            {create_table, ~"members",
                [
                    #kura_column{name = ~"team_id", primary_key = false, nullable = false},
                    #kura_column{name = ~"user_id", primary_key = false, nullable = false}
                ],
                [{primary_key, [~"team_id", ~"user_id"]}]}
        ],
        up([col_members])
    ).

renamed_foreign_key_field_carries_the_reference() ->
    schema(col_users, ~"users", [#kura_field{name = id, type = id, primary_key = true}]),
    schema(col_posts, ~"posts", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = author_id, type = integer, column = ~"written_by"}
    ]),
    meck:expect(col_posts, associations, fun() ->
        [
            #kura_assoc{
                name = author,
                type = belongs_to,
                schema = col_users,
                foreign_key = author_id,
                on_delete = cascade
            }
        ]
    end),
    ?assertMatch(
        #kura_column{references = {~"users", id}, on_delete = cascade},
        column(col_posts, ~"posts", ~"written_by")
    ).

reference_names_the_column_of_a_renamed_target_key() ->
    schema(col_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true, column = ~"user_id"}
    ]),
    schema(col_posts, ~"posts", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = author_id, type = integer}
    ]),
    meck:expect(col_posts, associations, fun() ->
        [#kura_assoc{name = author, type = belongs_to, schema = col_users, foreign_key = author_id}]
    end),
    ?assertMatch(
        #kura_column{references = {~"users", ~"user_id"}}, column(col_posts, ~"posts", author_id)
    ).

reference_names_the_column_of_a_renamed_ref_target_key() ->
    schema(col_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = handle, type = string, column = ~"login"}
    ]),
    schema(col_posts, ~"posts", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = author, type = string, column = ~"author_login"}
    ]),
    meck:expect(col_posts, associations, fun() ->
        [
            #kura_assoc{
                name = writer,
                type = belongs_to,
                ref = #kura_ref{target = col_users, fields = [author], target_key = [handle]}
            }
        ]
    end),
    ?assertMatch(
        #kura_column{references = {~"users", ~"login"}},
        column(col_posts, ~"posts", ~"author_login")
    ).

foreign_key_naming_no_field_still_aborts() ->
    schema(col_users, ~"users", [#kura_field{name = id, type = id, primary_key = true}]),
    schema(col_posts, ~"posts", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = author_id, type = integer, column = ~"written_by"}
    ]),
    meck:expect(col_posts, associations, fun() ->
        [
            #kura_assoc{
                name = author, type = belongs_to, schema = col_users, foreign_key = written_by
            }
        ]
    end),
    ?assertError(
        {kura_schema_diff, {assoc_key_not_a_field, col_posts, author, written_by}},
        kura_schema_diff:build_desired_state([col_posts])
    ).

index_names_the_column() ->
    schema(col_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = email, type = string, column = ~"email_address"}
    ]),
    meck:expect(col_users, indexes, fun() -> [{[email_address], #{unique => true}}] end),
    ?assertMatch(
        [{create_table, ~"users", _}, {create_index, ~"users", [~"email_address"], _}],
        up([col_users])
    ).

index_naming_the_renamed_field_aborts() ->
    schema(col_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = email, type = string, column = ~"email_address"}
    ]),
    meck:expect(col_users, indexes, fun() -> [{[email], #{unique => true}}] end),
    ?assertError(
        {kura_schema_diff, {index_unknown_column, col_users, email}},
        kura_schema_diff:build_desired_state([col_users])
    ).

renamed_schema_settles_after_replay() ->
    schema(col_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true, column = ~"user_id"},
        #kura_field{name = email, type = string, column = ~"email_address"}
    ]),
    schema(col_posts, ~"posts", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = author_id, type = integer, column = ~"written_by"}
    ]),
    meck:expect(col_posts, associations, fun() ->
        [#kura_assoc{name = author, type = belongs_to, schema = col_users, foreign_key = author_id}]
    end),
    Desired = kura_schema_diff:build_desired_state([col_users, col_posts]),
    {Up, _Down} = kura_schema_diff:diff(#{}, Desired),
    ?assertEqual({[], []}, kura_schema_diff:diff(replayed(Up), Desired)).

%% The schema does not say that a column kept its data under a new name, so
%% the generator cannot write a rename.
renaming_a_column_is_an_add_and_a_drop() ->
    schema(col_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = email, type = string}
    ]),
    Before = up([col_users]),
    meck:expect(col_users, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = email, type = string, column = ~"email_address"}
        ]
    end),
    ?assertEqual(
        {
            [
                {alter_table, ~"users", [
                    {add_column, #kura_column{name = ~"email_address", type = string}},
                    {drop_column, email}
                ]}
            ],
            [
                {alter_table, ~"users", [
                    {add_column, #kura_column{name = email, type = string}},
                    {drop_column, ~"email_address"}
                ]}
            ]
        },
        kura_schema_diff:diff(replayed(Before), kura_schema_diff:build_desired_state([col_users]))
    ).

%%====================================================================
%% timestamps/0, redact/0 and tenant_scoped/0
%%====================================================================

%% The three callbacks say how kura treats fields at run time. The columns
%% are the ones fields/0 declares, whatever they return.
callbacks_test_() ->
    {foreach, fun() -> ok end, fun(_) -> meck:unload() end, [
        fun custom_timestamps_are_the_declared_fields/0,
        fun no_timestamps_adds_no_column/0,
        fun default_timestamp_fields_are_ordinary_columns/0,
        fun a_timestamp_that_is_not_a_field_adds_no_column/0,
        fun redact_and_tenant_scoped_add_no_column/0
    ]}.

custom_timestamps_are_the_declared_fields() ->
    schema(col_events, ~"events", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = created_at, type = utc_datetime, column = ~"created"},
        #kura_field{name = modified_at, type = utc_datetime}
    ]),
    meck:expect(col_events, timestamps, fun() ->
        [{created_at, utc_datetime}, {modified_at, utc_datetime}]
    end),
    ?assertEqual([id, ~"created", modified_at], column_names(col_events, ~"events")).

no_timestamps_adds_no_column() ->
    schema(col_events, ~"events", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = inserted_at, type = utc_datetime}
    ]),
    meck:expect(col_events, timestamps, fun() -> [] end),
    ?assertEqual([id, inserted_at], column_names(col_events, ~"events")).

default_timestamp_fields_are_ordinary_columns() ->
    schema(col_events, ~"events", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ]),
    ?assertEqual([id, inserted_at, updated_at], column_names(col_events, ~"events")).

%% kura refuses such a schema when it writes a row. The generator derives
%% columns from fields/0 alone, so it has none to write for the entry.
a_timestamp_that_is_not_a_field_adds_no_column() ->
    schema(col_events, ~"events", [#kura_field{name = id, type = id, primary_key = true}]),
    meck:expect(col_events, timestamps, fun() -> [{created_at, utc_datetime}] end),
    ?assertEqual([id], column_names(col_events, ~"events")).

redact_and_tenant_scoped_add_no_column() ->
    schema(col_events, ~"events", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = token, type = string}
    ]),
    Plain = up([col_events]),
    meck:expect(col_events, redact, fun() -> [token] end),
    meck:expect(col_events, tenant_scoped, fun() -> false end),
    ?assertEqual(Plain, up([col_events])).

%%====================================================================
%% Helpers
%%====================================================================

schema(Mod, Table, Fields) ->
    meck:new(Mod, [non_strict]),
    meck:expect(Mod, table, fun() -> Table end),
    meck:expect(Mod, fields, fun() -> Fields end).

up(Mods) ->
    {Up, _Down} = kura_schema_diff:diff(#{}, kura_schema_diff:build_desired_state(Mods)),
    Up.

column(Mod, Table, Name) ->
    [Column] = [C || C <- columns(Mod, Table), C#kura_column.name =:= Name],
    Column.

column_names(Mod, Table) ->
    [C#kura_column.name || C <- columns(Mod, Table)].

columns(Mod, Table) ->
    maps:get(Table, maps:get(columns, kura_schema_diff:build_desired_state([Mod]))).

replayed(Ops) ->
    Mod = list_to_atom(
        "m20260101000000_columns_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    meck:new(Mod, [non_strict]),
    meck:expect(Mod, up, fun() -> Ops end),
    try
        kura_schema_diff:build_db_state([Mod])
    after
        meck:unload(Mod)
    end.
