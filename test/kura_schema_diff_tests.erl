-module(kura_schema_diff_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

cols(State) -> maps:get(columns, State).
idxs(State) -> maps:get(indexes, State).

%%====================================================================
%% build_db_state tests
%%====================================================================

build_db_state_empty_test() ->
    State = kura_schema_diff:build_db_state([]),
    ?assertEqual(#{}, maps:get(columns, State)),
    ?assertEqual(#{}, maps:get(indexes, State)),
    ?assertEqual([], kura_schema_diff:unreplayed_executes(State)).

build_db_state_create_table_test() ->
    meck:new(m_create, [non_strict]),
    meck:expect(m_create, up, fun() ->
        [
            {create_table, <<"pets">>, [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = name, type = string}
            ]}
        ]
    end),
    State = cols(kura_schema_diff:build_db_state([m_create])),
    ?assert(maps:is_key(<<"pets">>, State)),
    ?assertEqual(2, length(maps:get(<<"pets">>, State))),
    meck:unload(m_create).

build_db_state_alter_table_test() ->
    meck:new(m_a_create, [non_strict]),
    meck:new(m_b_alter, [non_strict]),
    meck:expect(m_a_create, up, fun() ->
        [
            {create_table, <<"pets">>, [
                #kura_column{name = id, type = id}
            ]}
        ]
    end),
    meck:expect(m_b_alter, up, fun() ->
        [
            {alter_table, <<"pets">>, [
                {add_column, #kura_column{name = age, type = integer}}
            ]}
        ]
    end),
    State = cols(kura_schema_diff:build_db_state([m_a_create, m_b_alter])),
    Cols = maps:get(<<"pets">>, State),
    ?assertEqual(2, length(Cols)),
    Names = [C#kura_column.name || C <- Cols],
    ?assert(lists:member(age, Names)),
    meck:unload([m_a_create, m_b_alter]).

build_db_state_drop_table_test() ->
    meck:new(m_create3, [non_strict]),
    meck:new(m_drop, [non_strict]),
    meck:expect(m_create3, up, fun() ->
        [{create_table, <<"tmp">>, [#kura_column{name = id, type = id}]}]
    end),
    meck:expect(m_drop, up, fun() ->
        [{drop_table, <<"tmp">>}]
    end),
    State = cols(kura_schema_diff:build_db_state([m_create3, m_drop])),
    ?assertNot(maps:is_key(<<"tmp">>, State)),
    meck:unload([m_create3, m_drop]).

build_db_state_drop_column_test() ->
    meck:new(m_create4, [non_strict]),
    meck:new(m_dropcol, [non_strict]),
    meck:expect(m_create4, up, fun() ->
        [
            {create_table, <<"t">>, [
                #kura_column{name = id, type = id},
                #kura_column{name = old, type = string}
            ]}
        ]
    end),
    meck:expect(m_dropcol, up, fun() ->
        [{alter_table, <<"t">>, [{drop_column, old}]}]
    end),
    State = cols(kura_schema_diff:build_db_state([m_create4, m_dropcol])),
    Cols = maps:get(<<"t">>, State),
    ?assertEqual(1, length(Cols)),
    ?assertEqual(id, (hd(Cols))#kura_column.name),
    meck:unload([m_create4, m_dropcol]).

build_db_state_skips_index_ops_test() ->
    meck:new(m_idx, [non_strict]),
    meck:expect(m_idx, up, fun() ->
        [
            {create_table, <<"t">>, [#kura_column{name = id, type = id}]},
            {create_index, <<"idx_t_id">>, <<"t">>, [id], []},
            {execute, <<"SELECT 1">>}
        ]
    end),
    State = cols(kura_schema_diff:build_db_state([m_idx])),
    ?assertEqual(1, length(maps:get(<<"t">>, State))),
    meck:unload(m_idx).

build_db_state_rename_column_test() ->
    meck:new(m_rc_create, [non_strict]),
    meck:new(m_rc_rename, [non_strict]),
    meck:expect(m_rc_create, up, fun() ->
        [
            {create_table, <<"t">>, [
                #kura_column{name = id, type = id},
                #kura_column{name = old_name, type = string}
            ]}
        ]
    end),
    meck:expect(m_rc_rename, up, fun() ->
        [{alter_table, <<"t">>, [{rename_column, old_name, new_name}]}]
    end),
    State = cols(kura_schema_diff:build_db_state([m_rc_create, m_rc_rename])),
    Cols = maps:get(<<"t">>, State),
    Names = [C#kura_column.name || C <- Cols],
    ?assert(lists:member(new_name, Names)),
    ?assertNot(lists:member(old_name, Names)),
    meck:unload([m_rc_create, m_rc_rename]).

build_db_state_modify_column_test() ->
    meck:new(m_mc_create, [non_strict]),
    meck:new(m_mc_modify, [non_strict]),
    meck:expect(m_mc_create, up, fun() ->
        [
            {create_table, <<"t">>, [
                #kura_column{name = id, type = id},
                #kura_column{name = age, type = integer}
            ]}
        ]
    end),
    meck:expect(m_mc_modify, up, fun() ->
        [{alter_table, <<"t">>, [{modify_column, age, float}]}]
    end),
    State = cols(kura_schema_diff:build_db_state([m_mc_create, m_mc_modify])),
    [_, AgeCol] = maps:get(<<"t">>, State),
    ?assertEqual(float, AgeCol#kura_column.type),
    meck:unload([m_mc_create, m_mc_modify]).

build_db_state_multiple_tables_test() ->
    meck:new(m_multi, [non_strict]),
    meck:expect(m_multi, up, fun() ->
        [
            {create_table, <<"users">>, [#kura_column{name = id, type = id}]},
            {create_table, <<"posts">>, [
                #kura_column{name = id, type = id},
                #kura_column{name = user_id, type = integer}
            ]}
        ]
    end),
    State = cols(kura_schema_diff:build_db_state([m_multi])),
    ?assertEqual(2, maps:size(State)),
    ?assertEqual(1, length(maps:get(<<"users">>, State))),
    ?assertEqual(2, length(maps:get(<<"posts">>, State))),
    meck:unload(m_multi).

build_db_state_multiple_alters_same_table_test() ->
    meck:new(m_maa_create, [non_strict]),
    meck:new(m_mab_alter1, [non_strict]),
    meck:new(m_mac_alter2, [non_strict]),
    meck:expect(m_maa_create, up, fun() ->
        [{create_table, <<"t">>, [#kura_column{name = id, type = id}]}]
    end),
    meck:expect(m_mab_alter1, up, fun() ->
        [{alter_table, <<"t">>, [{add_column, #kura_column{name = a, type = string}}]}]
    end),
    meck:expect(m_mac_alter2, up, fun() ->
        [{alter_table, <<"t">>, [{add_column, #kura_column{name = b, type = integer}}]}]
    end),
    State = cols(kura_schema_diff:build_db_state([m_maa_create, m_mab_alter1, m_mac_alter2])),
    Cols = maps:get(<<"t">>, State),
    ?assertEqual(3, length(Cols)),
    meck:unload([m_maa_create, m_mab_alter1, m_mac_alter2]).

build_db_state_alter_nonexistent_table_test() ->
    %% Altering a table that doesn't exist yet (edge case) — should create empty then add
    meck:new(m_ghost, [non_strict]),
    meck:expect(m_ghost, up, fun() ->
        [{alter_table, <<"ghost">>, [{add_column, #kura_column{name = x, type = string}}]}]
    end),
    State = cols(kura_schema_diff:build_db_state([m_ghost])),
    ?assertEqual([#kura_column{name = x, type = string}], maps:get(<<"ghost">>, State)),
    meck:unload(m_ghost).

%%====================================================================
%% build_desired_state tests
%%====================================================================

build_desired_state_test() ->
    meck:new(test_schema, [non_strict]),
    meck:expect(test_schema, table, fun() -> <<"users">> end),
    meck:expect(test_schema, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true, nullable = false},
            #kura_field{name = name, type = string},
            #kura_field{name = display, type = string, virtual = true}
        ]
    end),
    State = cols(kura_schema_diff:build_desired_state([test_schema])),
    Cols = maps:get(<<"users">>, State),
    %% Virtual field excluded
    ?assertEqual(2, length(Cols)),
    Names = [C#kura_column.name || C <- Cols],
    ?assertNot(lists:member(display, Names)),
    meck:unload(test_schema).

build_desired_state_empty_test() ->
    ?assertEqual(
        #{columns => #{}, indexes => #{}, unsupported => []},
        kura_schema_diff:build_desired_state([])
    ).

build_desired_state_multiple_schemas_test() ->
    meck:new(schema_a, [non_strict]),
    meck:new(schema_b, [non_strict]),
    meck:expect(schema_a, table, fun() -> <<"a">> end),
    meck:expect(schema_a, fields, fun() -> [#kura_field{name = id, type = id}] end),
    meck:expect(schema_b, table, fun() -> <<"b">> end),
    meck:expect(schema_b, fields, fun() ->
        [#kura_field{name = id, type = id}, #kura_field{name = x, type = string}]
    end),
    State = cols(kura_schema_diff:build_desired_state([schema_a, schema_b])),
    ?assertEqual(2, maps:size(State)),
    ?assertEqual(1, length(maps:get(<<"a">>, State))),
    ?assertEqual(2, length(maps:get(<<"b">>, State))),
    meck:unload([schema_a, schema_b]).

build_desired_state_all_virtual_test() ->
    meck:new(schema_virt, [non_strict]),
    meck:expect(schema_virt, table, fun() -> <<"v">> end),
    meck:expect(schema_virt, fields, fun() ->
        [
            #kura_field{name = x, type = string, virtual = true},
            #kura_field{name = y, type = integer, virtual = true}
        ]
    end),
    State = cols(kura_schema_diff:build_desired_state([schema_virt])),
    ?assertEqual([], maps:get(<<"v">>, State)),
    meck:unload(schema_virt).

build_desired_state_preserves_defaults_test() ->
    meck:new(schema_def, [non_strict]),
    meck:expect(schema_def, table, fun() -> <<"t">> end),
    meck:expect(schema_def, fields, fun() ->
        [#kura_field{name = active, type = boolean, default = true}]
    end),
    State = cols(kura_schema_diff:build_desired_state([schema_def])),
    [Col] = maps:get(<<"t">>, State),
    ?assertEqual(true, Col#kura_column.default),
    meck:unload(schema_def).

%%====================================================================
%% diff tests
%%====================================================================

diff_no_changes_test() ->
    Cols = [#kura_column{name = id, type = id}, #kura_column{name = name, type = string}],
    Db = #{<<"t">> => Cols},
    Desired = #{<<"t">> => Cols},
    ?assertEqual({[], []}, kura_schema_diff:diff(Db, Desired)).

diff_both_empty_test() ->
    ?assertEqual({[], []}, kura_schema_diff:diff(#{}, #{})).

diff_new_table_test() ->
    Cols = [#kura_column{name = id, type = id}, #kura_column{name = name, type = string}],
    {Up, Down} = kura_schema_diff:diff(#{}, #{<<"users">> => Cols}),
    ?assertMatch([{create_table, <<"users">>, _}], Up),
    ?assertMatch([{drop_table, <<"users">>}], Down).

diff_new_table_preserves_columns_test() ->
    Cols = [
        #kura_column{name = id, type = id, primary_key = true, nullable = false},
        #kura_column{name = email, type = string, nullable = false}
    ],
    {[{create_table, <<"users">>, CreatedCols}], _} = kura_schema_diff:diff(#{}, #{
        <<"users">> => Cols
    }),
    ?assertEqual(Cols, CreatedCols).

diff_multiple_new_tables_test() ->
    A = [#kura_column{name = id, type = id}],
    B = [#kura_column{name = id, type = id}],
    {Up, Down} = kura_schema_diff:diff(#{}, #{<<"a">> => A, <<"b">> => B}),
    ?assertEqual(2, length(Up)),
    ?assertEqual(2, length(Down)),
    %% Tables sorted alphabetically
    ?assertMatch([{create_table, <<"a">>, _}, {create_table, <<"b">>, _}], Up).

diff_add_column_test() ->
    DbCols = [#kura_column{name = id, type = id}],
    DesiredCols = [#kura_column{name = id, type = id}, #kura_column{name = age, type = integer}],
    {Up, Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assertMatch([{alter_table, <<"t">>, [{add_column, #kura_column{name = age}}]}], Up),
    ?assertMatch([{alter_table, <<"t">>, [{drop_column, age}]}], Down).

diff_add_multiple_columns_test() ->
    DbCols = [#kura_column{name = id, type = id}],
    DesiredCols = [
        #kura_column{name = id, type = id},
        #kura_column{name = age, type = integer},
        #kura_column{name = name, type = string}
    ],
    {[{alter_table, <<"t">>, Ops}], _} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{
        <<"t">> => DesiredCols
    }),
    AddedNames = [N || {add_column, #kura_column{name = N}} <- Ops],
    ?assertEqual([age, name], lists:sort(AddedNames)).

diff_drop_column_test() ->
    DbCols = [#kura_column{name = id, type = id}, #kura_column{name = old, type = string}],
    DesiredCols = [#kura_column{name = id, type = id}],
    {Up, Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assertMatch([{alter_table, <<"t">>, [{drop_column, old}]}], Up),
    ?assertMatch([{alter_table, <<"t">>, [{add_column, #kura_column{name = old}}]}], Down).

diff_type_change_test() ->
    DbCols = [#kura_column{name = id, type = id}, #kura_column{name = age, type = integer}],
    DesiredCols = [#kura_column{name = id, type = id}, #kura_column{name = age, type = float}],
    {Up, Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assertMatch([{alter_table, <<"t">>, [{modify_column, age, float}]}], Up),
    ?assertMatch([{alter_table, <<"t">>, [{modify_column, age, integer}]}], Down).

diff_multiple_type_changes_test() ->
    DbCols = [#kura_column{name = a, type = integer}, #kura_column{name = b, type = string}],
    DesiredCols = [#kura_column{name = a, type = float}, #kura_column{name = b, type = text}],
    {[{alter_table, <<"t">>, Ops}], _} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{
        <<"t">> => DesiredCols
    }),
    ?assertEqual(2, length(Ops)),
    ?assert(lists:member({modify_column, a, float}, Ops)),
    ?assert(lists:member({modify_column, b, text}, Ops)).

diff_combined_add_drop_modify_test() ->
    DbCols = [
        #kura_column{name = id, type = id},
        #kura_column{name = old, type = string},
        #kura_column{name = age, type = integer}
    ],
    DesiredCols = [
        #kura_column{name = id, type = id},
        #kura_column{name = new, type = boolean},
        #kura_column{name = age, type = float}
    ],
    {[{alter_table, <<"t">>, Ops}], [_]} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{
        <<"t">> => DesiredCols
    }),
    ?assert(
        lists:any(
            fun
                ({add_column, #kura_column{name = new}}) -> true;
                (_) -> false
            end,
            Ops
        )
    ),
    ?assert(lists:member({drop_column, old}, Ops)),
    ?assert(lists:member({modify_column, age, float}, Ops)).

diff_multiple_changes_test() ->
    DbCols = [#kura_column{name = id, type = id}, #kura_column{name = old, type = string}],
    DesiredCols = [#kura_column{name = id, type = id}, #kura_column{name = new, type = integer}],
    {Up, _Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assertEqual(1, length(Up)),
    [{alter_table, <<"t">>, AlterOps}] = Up,
    ?assert(
        lists:any(
            fun
                ({add_column, #kura_column{name = new}}) -> true;
                (_) -> false
            end,
            AlterOps
        )
    ),
    ?assert(
        lists:any(
            fun
                ({drop_column, old}) -> true;
                (_) -> false
            end,
            AlterOps
        )
    ).

diff_multi_table_test() ->
    Db = #{<<"a">> => [#kura_column{name = id, type = id}]},
    Desired = #{
        <<"a">> => [#kura_column{name = id, type = id}, #kura_column{name = x, type = string}],
        <<"b">> => [#kura_column{name = id, type = id}]
    },
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    ?assertEqual(2, length(Up)),
    HasCreate = lists:any(
        fun
            ({create_table, <<"b">>, _}) -> true;
            (_) -> false
        end,
        Up
    ),
    HasAlter = lists:any(
        fun
            ({alter_table, <<"a">>, _}) -> true;
            (_) -> false
        end,
        Up
    ),
    ?assert(HasCreate),
    ?assert(HasAlter),
    ?assertEqual(2, length(Down)).

diff_does_not_drop_tables_test() ->
    %% Tables in DB but not in desired should NOT generate drop_table
    Db = #{
        <<"old">> => [#kura_column{name = id, type = id}],
        <<"keep">> => [#kura_column{name = id, type = id}]
    },
    Desired = #{<<"keep">> => [#kura_column{name = id, type = id}]},
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    %% No drop_table for "old"
    ?assertEqual({[], []}, {Up, Down}).

diff_table_in_db_not_in_desired_ignored_test() ->
    Db = #{<<"orphan">> => [#kura_column{name = id, type = id}]},
    ?assertEqual({[], []}, kura_schema_diff:diff(Db, #{})).

diff_array_type_test() ->
    DbCols = [#kura_column{name = tags, type = {array, string}}],
    DesiredCols = [#kura_column{name = tags, type = {array, integer}}],
    {[{alter_table, <<"t">>, [{modify_column, tags, {array, integer}}]}], _} =
        kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}).

diff_nullable_change_test() ->
    DbCols = [
        #kura_column{name = id, type = id},
        #kura_column{name = name, type = string, nullable = true}
    ],
    DesiredCols = [
        #kura_column{name = id, type = id},
        #kura_column{name = name, type = string, nullable = false}
    ],
    {Up, Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assertMatch([{execute, <<"ALTER TABLE \"t\" ALTER COLUMN \"name\" SET NOT NULL">>}], Up),
    ?assertMatch([{execute, <<"ALTER TABLE \"t\" ALTER COLUMN \"name\" DROP NOT NULL">>}], Down).

diff_default_change_test() ->
    DbCols = [#kura_column{name = active, type = boolean, default = undefined}],
    DesiredCols = [#kura_column{name = active, type = boolean, default = true}],
    {Up, Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assertMatch([{execute, <<"ALTER TABLE \"t\" ALTER COLUMN \"active\" SET DEFAULT true">>}], Up),
    ?assertMatch([{execute, <<"ALTER TABLE \"t\" ALTER COLUMN \"active\" DROP DEFAULT">>}], Down).

diff_default_removal_test() ->
    DbCols = [#kura_column{name = active, type = boolean, default = true}],
    DesiredCols = [#kura_column{name = active, type = boolean, default = undefined}],
    {Up, Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assertMatch([{execute, <<"ALTER TABLE \"t\" ALTER COLUMN \"active\" DROP DEFAULT">>}], Up),
    ?assertMatch(
        [{execute, <<"ALTER TABLE \"t\" ALTER COLUMN \"active\" SET DEFAULT true">>}], Down
    ).

diff_default_escapes_embedded_single_quote_test() ->
    DbCols = [#kura_column{name = note, type = string, default = undefined}],
    DesiredCols = [#kura_column{name = note, type = string, default = <<"O'Brien">>}],
    {Up, _Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assertMatch(
        [{execute, <<"ALTER TABLE \"t\" ALTER COLUMN \"note\" SET DEFAULT 'O''Brien'">>}],
        Up
    ).

diff_doubles_an_embedded_double_quote_in_identifiers_test() ->
    DbCols = [#kura_column{name = active, type = boolean, default = undefined}],
    DesiredCols = [#kura_column{name = active, type = boolean, default = true}],
    {Up, _Down} = kura_schema_diff:diff(
        #{<<"ev\"il">> => DbCols}, #{<<"ev\"il">> => DesiredCols}
    ),
    ?assertMatch(
        [
            {execute, <<"ALTER TABLE \"ev\"\"il\" ALTER COLUMN \"active\" SET DEFAULT true">>}
        ],
        Up
    ).

diff_nullable_change_quotes_identifiers_test() ->
    DbCols = [#kura_column{name = name, type = string, nullable = true}],
    DesiredCols = [#kura_column{name = name, type = string, nullable = false}],
    {Up, _Down} = kura_schema_diff:diff(
        #{<<"ev\"il">> => DbCols}, #{<<"ev\"il">> => DesiredCols}
    ),
    ?assertMatch(
        [{execute, <<"ALTER TABLE \"ev\"\"il\" ALTER COLUMN \"name\" SET NOT NULL">>}],
        Up
    ).

diff_combined_type_and_nullable_test() ->
    DbCols = [#kura_column{name = age, type = integer, nullable = true}],
    DesiredCols = [#kura_column{name = age, type = float, nullable = false}],
    {Up, _Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assert(
        lists:any(
            fun
                ({alter_table, _, [{modify_column, age, float}]}) -> true;
                (_) -> false
            end,
            Up
        )
    ),
    ?assert(
        lists:any(
            fun
                ({execute, <<"ALTER TABLE \"t\" ALTER COLUMN \"age\" SET NOT NULL">>}) -> true;
                (_) -> false
            end,
            Up
        )
    ).

build_db_state_nullable_replay_test() ->
    meck:new(m_nra_create, [non_strict]),
    meck:new(m_nrb_alter, [non_strict]),
    meck:expect(m_nra_create, up, fun() ->
        [
            {create_table, <<"t">>, [
                #kura_column{name = id, type = id},
                #kura_column{name = name, type = string, nullable = true}
            ]}
        ]
    end),
    meck:expect(m_nrb_alter, up, fun() ->
        [{execute, <<"ALTER TABLE \"t\" ALTER COLUMN \"name\" SET NOT NULL">>}]
    end),
    State = cols(kura_schema_diff:build_db_state([m_nra_create, m_nrb_alter])),
    [_, NameCol] = maps:get(<<"t">>, State),
    ?assertEqual(false, NameCol#kura_column.nullable),
    meck:unload([m_nra_create, m_nrb_alter]).

build_db_state_default_replay_test() ->
    meck:new(m_dra_create, [non_strict]),
    meck:new(m_drb_alter, [non_strict]),
    meck:expect(m_dra_create, up, fun() ->
        [
            {create_table, <<"t">>, [
                #kura_column{name = active, type = boolean}
            ]}
        ]
    end),
    meck:expect(m_drb_alter, up, fun() ->
        [{execute, <<"ALTER TABLE \"t\" ALTER COLUMN \"active\" SET DEFAULT true">>}]
    end),
    State = cols(kura_schema_diff:build_db_state([m_dra_create, m_drb_alter])),
    [Col] = maps:get(<<"t">>, State),
    ?assertEqual(true, Col#kura_column.default),
    meck:unload([m_dra_create, m_drb_alter]).

diff_down_ops_are_reversible_test() ->
    %% Applying up then down should return to original state
    DbCols = [#kura_column{name = id, type = id}],
    DesiredCols = [
        #kura_column{name = id, type = id},
        #kura_column{name = x, type = string},
        #kura_column{name = y, type = integer}
    ],
    Db = #{<<"t">> => DbCols},
    Desired = #{<<"t">> => DesiredCols},
    {UpOps, DownOps} = kura_schema_diff:diff(Db, Desired),
    %% Simulate applying up, then down, should get back to Db state
    AfterUp = apply_ops_ext(UpOps, Db),
    AfterDown = apply_ops_ext(DownOps, AfterUp),
    ?assertEqual(Db, AfterDown).

%%====================================================================
%% field_to_column tests
%%====================================================================

field_to_column_test() ->
    Field = #kura_field{
        name = email, type = string, nullable = false, default = undefined, primary_key = false
    },
    Col = kura_schema_diff:field_to_column(Field),
    ?assertEqual(email, Col#kura_column.name),
    ?assertEqual(string, Col#kura_column.type),
    ?assertEqual(false, Col#kura_column.nullable),
    ?assertEqual(false, Col#kura_column.primary_key).

field_to_column_preserves_pk_test() ->
    Field = #kura_field{name = id, type = id, primary_key = true, nullable = false},
    Col = kura_schema_diff:field_to_column(Field),
    ?assertEqual(true, Col#kura_column.primary_key).

field_to_column_preserves_default_test() ->
    Field = #kura_field{name = active, type = boolean, default = true},
    Col = kura_schema_diff:field_to_column(Field),
    ?assertEqual(true, Col#kura_column.default).

field_to_column_preserves_nullable_test() ->
    Field = #kura_field{name = required, type = string, nullable = false},
    Col = kura_schema_diff:field_to_column(Field),
    ?assertEqual(false, Col#kura_column.nullable).

field_to_column_custom_column_name_test() ->
    Field = #kura_field{name = email, type = string, column = <<"email_address">>},
    Col = kura_schema_diff:field_to_column(Field),
    ?assertEqual(email_address, Col#kura_column.name),
    ?assertEqual(string, Col#kura_column.type).

field_to_column_undefined_column_uses_field_name_test() ->
    Field = #kura_field{name = email, type = string, column = undefined},
    Col = kura_schema_diff:field_to_column(Field),
    ?assertEqual(email, Col#kura_column.name).

field_to_column_all_types_test() ->
    Types = [
        id, integer, float, string, text, boolean, date, utc_datetime, uuid, jsonb, {array, string}
    ],
    lists:foreach(
        fun(T) ->
            Col = kura_schema_diff:field_to_column(#kura_field{name = x, type = T}),
            ?assertEqual(T, Col#kura_column.type)
        end,
        Types
    ).

%%====================================================================
%% belongs_to enrichment tests
%%====================================================================

build_desired_state_belongs_to_enrichment_test() ->
    meck:new(bt_user, [non_strict]),
    meck:new(bt_post, [non_strict]),
    meck:expect(bt_user, table, fun() -> <<"users">> end),
    meck:expect(bt_user, fields, fun() ->
        [#kura_field{name = id, type = id, primary_key = true}]
    end),
    meck:expect(bt_post, table, fun() -> <<"posts">> end),
    meck:expect(bt_post, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = user_id, type = integer}
        ]
    end),
    meck:expect(bt_post, associations, fun() ->
        [#kura_assoc{name = user, type = belongs_to, schema = bt_user, foreign_key = user_id}]
    end),
    State = cols(kura_schema_diff:build_desired_state([bt_post])),
    [_, FkCol] = maps:get(<<"posts">>, State),
    ?assertEqual({<<"users">>, id}, FkCol#kura_column.references),
    ?assertEqual(no_action, FkCol#kura_column.on_delete),
    meck:unload([bt_user, bt_post]).

build_desired_state_no_associations_callback_test() ->
    meck:new(na_schema, [non_strict]),
    meck:expect(na_schema, table, fun() -> <<"items">> end),
    meck:expect(na_schema, fields, fun() ->
        [#kura_field{name = id, type = id, primary_key = true}]
    end),
    State = cols(kura_schema_diff:build_desired_state([na_schema])),
    [Col] = maps:get(<<"items">>, State),
    ?assertEqual(undefined, Col#kura_column.references),
    meck:unload(na_schema).

build_desired_state_target_schema_unavailable_test() ->
    meck:new(ts_post, [non_strict]),
    meck:expect(ts_post, table, fun() -> <<"posts">> end),
    meck:expect(ts_post, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = user_id, type = integer}
        ]
    end),
    meck:expect(ts_post, associations, fun() ->
        [
            #kura_assoc{
                name = user, type = belongs_to, schema = nonexistent_schema, foreign_key = user_id
            }
        ]
    end),
    %% Skipping this used to emit a table with no foreign key at all, so the
    %% schema and the database disagreed with nothing on stdout to say so.
    ?assertError(
        {kura_schema_diff, {assoc_target_not_loadable, ts_post, user, nonexistent_schema, nofile}},
        kura_schema_diff:build_desired_state([ts_post])
    ),
    meck:unload(ts_post).

%%====================================================================
%% Integration: build_db_state + build_desired_state + diff
%%====================================================================

full_roundtrip_new_schema_test() ->
    %% No migrations, one schema → create_table
    meck:new(rt_schema, [non_strict]),
    meck:expect(rt_schema, table, fun() -> <<"items">> end),
    meck:expect(rt_schema, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true, nullable = false},
            #kura_field{name = title, type = string, nullable = false}
        ]
    end),
    DbState = kura_schema_diff:build_db_state([]),
    DesiredState = kura_schema_diff:build_desired_state([rt_schema]),
    {Up, Down} = kura_schema_diff:diff(DbState, DesiredState),
    ?assertMatch([{create_table, <<"items">>, [_, _]}], Up),
    ?assertMatch([{drop_table, <<"items">>}], Down),
    meck:unload(rt_schema).

full_roundtrip_schema_matches_migrations_test() ->
    %% Schema matches existing migrations → no diff
    meck:new(rt_mig, [non_strict]),
    meck:new(rt_schema2, [non_strict]),
    meck:expect(rt_mig, up, fun() ->
        [
            {create_table, <<"items">>, [
                #kura_column{name = id, type = id, primary_key = true, nullable = false},
                #kura_column{name = title, type = string, nullable = false}
            ]}
        ]
    end),
    meck:expect(rt_schema2, table, fun() -> <<"items">> end),
    meck:expect(rt_schema2, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true, nullable = false},
            #kura_field{name = title, type = string, nullable = false}
        ]
    end),
    DbState = kura_schema_diff:build_db_state([rt_mig]),
    DesiredState = kura_schema_diff:build_desired_state([rt_schema2]),
    ?assertEqual({[], []}, kura_schema_diff:diff(DbState, DesiredState)),
    meck:unload([rt_mig, rt_schema2]).

full_roundtrip_add_field_test() ->
    %% Migration creates table, schema adds a field → alter_table add_column
    meck:new(rt_mig2, [non_strict]),
    meck:new(rt_schema3, [non_strict]),
    meck:expect(rt_mig2, up, fun() ->
        [
            {create_table, <<"items">>, [
                #kura_column{name = id, type = id}
            ]}
        ]
    end),
    meck:expect(rt_schema3, table, fun() -> <<"items">> end),
    meck:expect(rt_schema3, fields, fun() ->
        [
            #kura_field{name = id, type = id},
            #kura_field{name = desc, type = text}
        ]
    end),
    DbState = kura_schema_diff:build_db_state([rt_mig2]),
    DesiredState = kura_schema_diff:build_desired_state([rt_schema3]),
    {Up, Down} = kura_schema_diff:diff(DbState, DesiredState),
    ?assertMatch(
        [{alter_table, <<"items">>, [{add_column, #kura_column{name = desc, type = text}}]}], Up
    ),
    ?assertMatch([{alter_table, <<"items">>, [{drop_column, desc}]}], Down),
    meck:unload([rt_mig2, rt_schema3]).

%%====================================================================
%% Index tests
%%====================================================================

build_db_state_tracks_indexes_test() ->
    meck:new(m_idx2, [non_strict]),
    meck:expect(m_idx2, up, fun() ->
        [
            {create_table, <<"users">>, [
                #kura_column{name = id, type = id},
                #kura_column{name = email, type = string}
            ]},
            {create_index, <<"users">>, [email], #{unique => true}}
        ]
    end),
    State = kura_schema_diff:build_db_state([m_idx2]),
    IdxState = idxs(State),
    ?assertEqual([{[email], #{unique => true}}], maps:get(<<"users">>, IdxState)),
    meck:unload(m_idx2).

build_db_state_tracks_5tuple_indexes_test() ->
    meck:new(m_idx5, [non_strict]),
    meck:expect(m_idx5, up, fun() ->
        [
            {create_table, <<"t">>, [#kura_column{name = id, type = id}]},
            {create_index, <<"t_id_index">>, <<"t">>, [id], [unique]}
        ]
    end),
    State = kura_schema_diff:build_db_state([m_idx5]),
    IdxState = idxs(State),
    %% The explicit name is kept: drop_index names the index, not its columns.
    ?assertEqual([{[id], #{unique => true, name => ~"t_id_index"}}], maps:get(~"t", IdxState)),
    meck:unload(m_idx5).

build_db_state_drop_index_test() ->
    meck:new(m_idx_create, [non_strict]),
    meck:new(m_idx_drop, [non_strict]),
    meck:expect(m_idx_create, up, fun() ->
        [
            {create_table, <<"t">>, [#kura_column{name = id, type = id}]},
            {create_index, <<"t">>, [id], #{unique => true}}
        ]
    end),
    meck:expect(m_idx_drop, up, fun() ->
        [{drop_index, <<"t_id_index">>}]
    end),
    State = kura_schema_diff:build_db_state([m_idx_create, m_idx_drop]),
    ?assertEqual([], maps:get(<<"t">>, idxs(State), [])),
    meck:unload([m_idx_create, m_idx_drop]).

build_db_state_drop_column_drops_covering_indexes_test() ->
    %% Taure/kura#160: Postgres drops the index with the column, so replaying
    %% history must too, or the next compile emits a drop_index that 42704s.
    meck:new(m_dci_create, [non_strict]),
    meck:new(m_dci_drop, [non_strict]),
    meck:expect(m_dci_create, up, fun() ->
        [
            {create_table, ~"t", [
                #kura_column{name = id, type = id},
                #kura_column{name = email, type = string},
                #kura_column{name = name, type = string}
            ]},
            {create_index, ~"t", [email], #{unique => true}},
            {create_index, ~"t_name_email_index", ~"t", [name, email], []},
            {create_index, ~"t", [name], #{}}
        ]
    end),
    meck:expect(m_dci_drop, up, fun() ->
        [{alter_table, ~"t", [{drop_column, email}]}]
    end),
    State = kura_schema_diff:build_db_state([m_dci_create, m_dci_drop]),
    ?assertEqual([{[name], #{}}], maps:get(~"t", idxs(State))),
    meck:unload([m_dci_create, m_dci_drop]).

build_desired_state_extracts_indexes_test() ->
    meck:new(idx_schema, [non_strict]),
    meck:expect(idx_schema, table, fun() -> <<"users">> end),
    meck:expect(idx_schema, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = email, type = string}
        ]
    end),
    meck:expect(idx_schema, indexes, fun() ->
        [{[email], #{unique => true}}, {[id], #{}}]
    end),
    State = kura_schema_diff:build_desired_state([idx_schema]),
    IdxState = idxs(State),
    ?assertEqual([{[email], #{unique => true}}, {[id], #{}}], maps:get(<<"users">>, IdxState)),
    meck:unload(idx_schema).

build_desired_state_no_indexes_callback_test() ->
    meck:new(no_idx_schema, [non_strict]),
    meck:expect(no_idx_schema, table, fun() -> <<"t">> end),
    meck:expect(no_idx_schema, fields, fun() ->
        [#kura_field{name = id, type = id}]
    end),
    State = kura_schema_diff:build_desired_state([no_idx_schema]),
    ?assertEqual([], maps:get(<<"t">>, idxs(State))),
    meck:unload(no_idx_schema).

diff_new_index_test() ->
    Db = #{columns => #{<<"t">> => [#kura_column{name = id, type = id}]}, indexes => #{}},
    Desired = #{
        columns => #{<<"t">> => [#kura_column{name = id, type = id}]},
        indexes => #{<<"t">> => [{[id], #{unique => true}}]}
    },
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    ?assertMatch([{create_index, <<"t">>, [id], #{unique := true}}], Up),
    ?assertMatch([{drop_index, <<"t_id_index">>}], Down).

diff_drop_index_test() ->
    Db = #{
        columns => #{<<"t">> => [#kura_column{name = id, type = id}]},
        indexes => #{<<"t">> => [{[id], #{unique => true}}]}
    },
    Desired = #{
        columns => #{<<"t">> => [#kura_column{name = id, type = id}]},
        indexes => #{<<"t">> => []}
    },
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    ?assertMatch([{drop_index, <<"t_id_index">>}], Up),
    ?assertMatch([{create_index, <<"t">>, [id], #{unique := true}}], Down).

diff_drop_column_drops_its_index_first_test() ->
    %% Taure/kura#160: after the column goes Postgres has already dropped
    %% the index (42704) and SQLite refuses the column drop, so the index
    %% has to go first, and come back only once the column is back.
    Db = #{
        columns => #{
            ~"t" => [#kura_column{name = id, type = id}, #kura_column{name = x, type = string}]
        },
        indexes => #{~"t" => [{[x], #{}}, {[id], #{unique => true}}]}
    },
    Desired = #{
        columns => #{~"t" => [#kura_column{name = id, type = id}]},
        indexes => #{~"t" => []}
    },
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    ?assertEqual(
        [
            {drop_index, ~"t_x_index"},
            {drop_index, ~"t_id_index"},
            {alter_table, ~"t", [{drop_column, x}]}
        ],
        Up
    ),
    ?assertMatch(
        [
            {alter_table, ~"t", [{add_column, #kura_column{name = x}}]},
            {create_index, ~"t", [x], _},
            {create_index, ~"t", [id], _}
        ],
        Down
    ).

diff_add_column_down_drops_its_index_first_test() ->
    Db = #{columns => #{~"t" => [#kura_column{name = id, type = id}]}, indexes => #{}},
    Desired = #{
        columns => #{
            ~"t" => [#kura_column{name = id, type = id}, #kura_column{name = x, type = string}]
        },
        indexes => #{~"t" => [{[x], #{}}]}
    },
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    ?assertMatch(
        [
            {alter_table, ~"t", [{add_column, #kura_column{name = x}}]},
            {create_index, ~"t", [x], _}
        ],
        Up
    ),
    ?assertEqual(
        [{drop_index, ~"t_x_index"}, {alter_table, ~"t", [{drop_column, x}]}], Down
    ).

diff_index_opts_change_drops_before_create_test() ->
    %% Same columns, so the same generated name: creating first fails 42P07.
    Cols = [#kura_column{name = id, type = id}, #kura_column{name = e, type = string}],
    Db = #{columns => #{~"t" => Cols}, indexes => #{~"t" => [{[e], #{}}]}},
    Desired = #{columns => #{~"t" => Cols}, indexes => #{~"t" => [{[e], #{unique => true}}]}},
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    ?assertEqual(
        [{drop_index, ~"t_e_index"}, {create_index, ~"t", [e], #{unique => true}}], Up
    ),
    ?assertEqual([{drop_index, ~"t_e_index"}, {create_index, ~"t", [e], #{}}], Down).

build_desired_state_index_on_unknown_column_aborts_test() ->
    meck:new(idx_unknown_schema, [non_strict]),
    meck:expect(idx_unknown_schema, table, fun() -> ~"t" end),
    meck:expect(idx_unknown_schema, fields, fun() ->
        [#kura_field{name = id, type = id, primary_key = true}]
    end),
    meck:expect(idx_unknown_schema, indexes, fun() -> [{[gone], #{}}] end),
    ?assertError(
        {kura_schema_diff, {index_unknown_column, idx_unknown_schema, gone}},
        kura_schema_diff:build_desired_state([idx_unknown_schema])
    ),
    meck:unload(idx_unknown_schema).

diff_no_index_changes_test() ->
    Db = #{
        columns => #{<<"t">> => [#kura_column{name = id, type = id}]},
        indexes => #{<<"t">> => [{[id], #{unique => true}}]}
    },
    Desired = #{
        columns => #{<<"t">> => [#kura_column{name = id, type = id}]},
        indexes => #{<<"t">> => [{[id], #{unique => true}}]}
    },
    ?assertEqual({[], []}, kura_schema_diff:diff(Db, Desired)).

diff_new_table_with_indexes_test() ->
    meck:new(new_idx_schema, [non_strict]),
    meck:expect(new_idx_schema, table, fun() -> <<"users">> end),
    meck:expect(new_idx_schema, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = email, type = string}
        ]
    end),
    meck:expect(new_idx_schema, indexes, fun() ->
        [{[email], #{unique => true}}]
    end),
    DbState = kura_schema_diff:build_db_state([]),
    DesiredState = kura_schema_diff:build_desired_state([new_idx_schema]),
    {Up, Down} = kura_schema_diff:diff(DbState, DesiredState),
    ?assert(
        lists:any(
            fun
                ({create_table, <<"users">>, _}) -> true;
                (_) -> false
            end,
            Up
        )
    ),
    ?assert(
        lists:any(
            fun
                ({create_index, <<"users">>, [email], _}) -> true;
                (_) -> false
            end,
            Up
        )
    ),
    ?assert(
        lists:any(
            fun
                ({drop_table, <<"users">>}) -> true;
                (_) -> false
            end,
            Down
        )
    ),
    %% The drop_table above already drops the index, so emitting a
    %% drop_index after it makes the rollback fail with 42704.
    ?assertNot(
        lists:any(
            fun
                ({drop_index, _}) -> true;
                (_) -> false
            end,
            Down
        )
    ),
    meck:unload(new_idx_schema).

diff_new_table_index_down_is_only_the_drop_table_test() ->
    %% Taure/rebar3_kura#43: two tables, each with two indexes, one of
    %% them unique and used as an ON CONFLICT target - the shape the
    %% first asobi extension generated, whose down/0 could not run.
    meck:new(quests_schema, [non_strict]),
    meck:expect(quests_schema, table, fun() -> <<"quests">> end),
    meck:expect(quests_schema, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = quest_key, type = string, nullable = false},
            #kura_field{name = counter, type = string, nullable = false},
            #kura_field{name = active, type = boolean, nullable = false}
        ]
    end),
    meck:expect(quests_schema, indexes, fun() ->
        [{[quest_key], #{unique => true}}, {[counter, active], #{}}]
    end),
    meck:new(progress_schema, [non_strict]),
    meck:expect(progress_schema, table, fun() -> <<"quest_progress">> end),
    meck:expect(progress_schema, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = player_id, type = uuid, nullable = false},
            #kura_field{name = quest_id, type = uuid, nullable = false},
            #kura_field{name = period_key, type = string, nullable = false},
            #kura_field{name = completed_at, type = utc_datetime}
        ]
    end),
    meck:expect(progress_schema, indexes, fun() ->
        [
            {[player_id, quest_id, period_key], #{unique => true}},
            {[player_id, completed_at], #{}}
        ]
    end),

    DbState = kura_schema_diff:build_db_state([]),
    DesiredState = kura_schema_diff:build_desired_state([quests_schema, progress_schema]),
    {Up, Down} = kura_schema_diff:diff(DbState, DesiredState),

    %% Every declared index is still created.
    ?assertEqual(
        4,
        length([Op || {create_index, _, _, _} = Op <- Up])
    ),
    %% The down is the two table drops and nothing else.
    ?assertEqual(
        [{drop_table, <<"quest_progress">>}, {drop_table, <<"quests">>}],
        Down
    ),

    meck:unload([quests_schema, progress_schema]).

%%====================================================================
%% Regression: schema declares an index that no migration creates
%%====================================================================
%%
%% Scenario from production (asobi): the schema's `indexes/0` declared
%% a unique index that's referenced by name in an `on_conflict` clause,
%% but the table-creating migration never emitted a `create_index` op.
%% `rebar3 kura compile` reported "schemas up to date" and downstream
%% upserts failed with `constraint "..." does not exist`.
%%
%% These tests pin the diff/3 contract for that case end-to-end:
%% - existing migration creates the table, no index op
%% - schema declares one or more indexes via `indexes/0`
%% - diff must emit `{create_index, ...}` ops (and matching downs)
%%
%% Coverage spans: single missing index, multiple missing indexes,
%% mixed existing+missing indexes, multi-column index, idempotency
%% after the generated migration is replayed.

diff_schema_declares_index_missing_from_migration_test() ->
    %% The classic asobi case: zone_snapshots table created without
    %% the unique index that the schema declares.
    meck:new(m_create_zs, [non_strict]),
    meck:expect(m_create_zs, up, fun() ->
        [
            {create_table, <<"zone_snapshots">>, [
                #kura_column{name = id, type = uuid, primary_key = true},
                #kura_column{name = world_id, type = uuid, nullable = false},
                #kura_column{name = zone_x, type = integer, nullable = false},
                #kura_column{name = zone_y, type = integer, nullable = false}
            ]}
        ]
    end),
    meck:new(zs_schema, [non_strict]),
    meck:expect(zs_schema, table, fun() -> <<"zone_snapshots">> end),
    meck:expect(zs_schema, fields, fun() ->
        [
            #kura_field{name = id, type = uuid, primary_key = true},
            #kura_field{name = world_id, type = uuid, nullable = false},
            #kura_field{name = zone_x, type = integer, nullable = false},
            #kura_field{name = zone_y, type = integer, nullable = false}
        ]
    end),
    meck:expect(zs_schema, indexes, fun() ->
        [{[world_id, zone_x, zone_y], #{unique => true}}]
    end),

    DbState = kura_schema_diff:build_db_state([m_create_zs]),
    DesiredState = kura_schema_diff:build_desired_state([zs_schema]),
    {Up, Down} = kura_schema_diff:diff(DbState, DesiredState),

    %% No column-level diff: table is in sync.
    ?assert(
        not lists:any(
            fun
                ({create_table, _, _}) -> true;
                ({alter_table, _, _}) -> true;
                (_) -> false
            end,
            Up
        )
    ),
    %% Index missing from DB state must surface as a create_index op.
    ?assertMatch(
        [{create_index, <<"zone_snapshots">>, [world_id, zone_x, zone_y], #{unique := true}}],
        Up
    ),
    ?assertMatch(
        [{drop_index, <<"zone_snapshots_world_id_zone_x_zone_y_index">>}],
        Down
    ),

    meck:unload([m_create_zs, zs_schema]).

diff_schema_declares_multiple_indexes_missing_test() ->
    %% Two declared indexes, neither in any migration. Both must
    %% appear in the up diff and both must have matching drops in the
    %% down diff.
    meck:new(m_create_users, [non_strict]),
    meck:expect(m_create_users, up, fun() ->
        [
            {create_table, <<"users">>, [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = email, type = string, nullable = false},
                #kura_column{name = username, type = string, nullable = false}
            ]}
        ]
    end),
    meck:new(users_schema, [non_strict]),
    meck:expect(users_schema, table, fun() -> <<"users">> end),
    meck:expect(users_schema, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = email, type = string, nullable = false},
            #kura_field{name = username, type = string, nullable = false}
        ]
    end),
    meck:expect(users_schema, indexes, fun() ->
        [
            {[email], #{unique => true}},
            {[username], #{unique => true}}
        ]
    end),

    DbState = kura_schema_diff:build_db_state([m_create_users]),
    DesiredState = kura_schema_diff:build_desired_state([users_schema]),
    {Up, _Down} = kura_schema_diff:diff(DbState, DesiredState),

    CreateIndexOps = [Op || {create_index, _, _, _} = Op <- Up],
    ?assertEqual(2, length(CreateIndexOps)),
    ?assert(
        lists:any(
            fun
                ({create_index, <<"users">>, [email], #{unique := true}}) -> true;
                (_) -> false
            end,
            CreateIndexOps
        )
    ),
    ?assert(
        lists:any(
            fun
                ({create_index, <<"users">>, [username], #{unique := true}}) -> true;
                (_) -> false
            end,
            CreateIndexOps
        )
    ),

    meck:unload([m_create_users, users_schema]).

diff_schema_declares_mixed_existing_and_missing_indexes_test() ->
    %% One index already in migrations, one declared but missing. Only
    %% the missing one is emitted — the existing one must NOT be
    %% re-created (regression bait for the diff function double-counting).
    meck:new(m_create_orders, [non_strict]),
    meck:expect(m_create_orders, up, fun() ->
        [
            {create_table, <<"orders">>, [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = customer_id, type = id, nullable = false},
                #kura_column{name = status, type = string, nullable = false}
            ]},
            {create_index, <<"orders">>, [customer_id], #{}}
        ]
    end),
    meck:new(orders_schema, [non_strict]),
    meck:expect(orders_schema, table, fun() -> <<"orders">> end),
    meck:expect(orders_schema, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = customer_id, type = id, nullable = false},
            #kura_field{name = status, type = string, nullable = false}
        ]
    end),
    meck:expect(orders_schema, indexes, fun() ->
        [
            {[customer_id], #{}},
            {[status], #{}}
        ]
    end),

    DbState = kura_schema_diff:build_db_state([m_create_orders]),
    DesiredState = kura_schema_diff:build_desired_state([orders_schema]),
    {Up, _Down} = kura_schema_diff:diff(DbState, DesiredState),

    CreateIndexOps = [Op || {create_index, _, _, _} = Op <- Up],
    ?assertEqual(1, length(CreateIndexOps)),
    ?assertMatch([{create_index, <<"orders">>, [status], _}], CreateIndexOps),
    %% Make sure customer_id is NOT in the up diff
    ?assertNot(
        lists:any(
            fun
                ({create_index, <<"orders">>, [customer_id], _}) -> true;
                (_) -> false
            end,
            Up
        )
    ),

    meck:unload([m_create_orders, orders_schema]).

diff_schema_idempotent_after_index_migration_replay_test() ->
    %% After the generator emits a create_index op and that op is
    %% replayed as a follow-up migration, the next diff must be empty.
    %% This is the idempotency property the user actually observes —
    %% if it fails the user sees `kura compile` re-emit the same
    %% migration on every run.
    meck:new(m_create_pets, [non_strict]),
    meck:expect(m_create_pets, up, fun() ->
        [
            {create_table, <<"pets">>, [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = owner_id, type = id, nullable = false}
            ]}
        ]
    end),
    %% Simulate the migration the generator would emit.
    meck:new(m_add_index_pets_owner, [non_strict]),
    meck:expect(m_add_index_pets_owner, up, fun() ->
        [{create_index, <<"pets">>, [owner_id], #{}}]
    end),
    meck:new(pets_schema, [non_strict]),
    meck:expect(pets_schema, table, fun() -> <<"pets">> end),
    meck:expect(pets_schema, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = owner_id, type = id, nullable = false}
        ]
    end),
    meck:expect(pets_schema, indexes, fun() ->
        [{[owner_id], #{}}]
    end),

    %% Before the index migration: diff emits create_index.
    DbBefore = kura_schema_diff:build_db_state([m_create_pets]),
    Desired = kura_schema_diff:build_desired_state([pets_schema]),
    {UpBefore, _DownBefore} = kura_schema_diff:diff(DbBefore, Desired),
    ?assertMatch([{create_index, <<"pets">>, [owner_id], _}], UpBefore),

    %% After the index migration is replayed: diff is empty.
    DbAfter = kura_schema_diff:build_db_state([m_create_pets, m_add_index_pets_owner]),
    ?assertEqual({[], []}, kura_schema_diff:diff(DbAfter, Desired)),

    meck:unload([m_create_pets, m_add_index_pets_owner, pets_schema]).

diff_schema_index_column_order_is_preserved_test() ->
    %% B-tree indexes on (a, b) and (b, a) are different — they serve
    %% different query patterns and produce different generated names
    %% (`t_a_b_index` vs `t_b_a_index`). Earlier diff code sorted the
    %% column list before comparing, which silently coerced the emitted
    %% op to alphabetic order. The asobi case happened to work by
    %% coincidence (world_id, zone_x, zone_y already in alpha order),
    %% but a schema declaring `[zone_y, zone_x, world_id]` would have
    %% had its index renamed and broken the `on_conflict` clause that
    %% references the original constraint name.
    meck:new(m_create_t, [non_strict]),
    meck:expect(m_create_t, up, fun() ->
        [
            {create_table, <<"t">>, [
                #kura_column{name = a, type = string},
                #kura_column{name = b, type = string}
            ]}
        ]
    end),
    meck:new(t_schema_ba, [non_strict]),
    meck:expect(t_schema_ba, table, fun() -> <<"t">> end),
    meck:expect(t_schema_ba, fields, fun() ->
        [
            #kura_field{name = a, type = string},
            #kura_field{name = b, type = string}
        ]
    end),
    meck:expect(t_schema_ba, indexes, fun() ->
        [{[b, a], #{}}]
    end),

    DbState = kura_schema_diff:build_db_state([m_create_t]),
    DesiredState = kura_schema_diff:build_desired_state([t_schema_ba]),
    {Up, Down} = kura_schema_diff:diff(DbState, DesiredState),

    %% Column order must round-trip exactly as declared.
    ?assertMatch([{create_index, <<"t">>, [b, a], _}], Up),
    ?assertMatch([{drop_index, <<"t_b_a_index">>}], Down),

    meck:unload([m_create_t, t_schema_ba]).

diff_schema_index_column_order_change_is_a_diff_test() ->
    %% Symmetric guarantee: if a migration created index `(a, b)` and
    %% the schema now declares `(b, a)`, the diff must emit BOTH a
    %% drop (of the old) and a create (of the new). A regression that
    %% silently treated those as equal would mask a real schema change.
    meck:new(m_create_t2, [non_strict]),
    meck:expect(m_create_t2, up, fun() ->
        [
            {create_table, <<"t">>, [
                #kura_column{name = a, type = string},
                #kura_column{name = b, type = string}
            ]},
            {create_index, <<"t">>, [a, b], #{}}
        ]
    end),
    meck:new(t_schema_ba2, [non_strict]),
    meck:expect(t_schema_ba2, table, fun() -> <<"t">> end),
    meck:expect(t_schema_ba2, fields, fun() ->
        [
            #kura_field{name = a, type = string},
            #kura_field{name = b, type = string}
        ]
    end),
    meck:expect(t_schema_ba2, indexes, fun() ->
        [{[b, a], #{}}]
    end),

    DbState = kura_schema_diff:build_db_state([m_create_t2]),
    DesiredState = kura_schema_diff:build_desired_state([t_schema_ba2]),
    {Up, _Down} = kura_schema_diff:diff(DbState, DesiredState),

    HasDrop = lists:any(
        fun
            ({drop_index, <<"t_a_b_index">>}) -> true;
            (_) -> false
        end,
        Up
    ),
    HasCreate = lists:any(
        fun
            ({create_index, <<"t">>, [b, a], _}) -> true;
            (_) -> false
        end,
        Up
    ),
    ?assert(HasDrop),
    ?assert(HasCreate),

    meck:unload([m_create_t2, t_schema_ba2]).

diff_schema_index_with_multi_column_keys_test() ->
    %% Multi-column composite indexes round-trip through the diff.
    meck:new(m_create_logs, [non_strict]),
    meck:expect(m_create_logs, up, fun() ->
        [
            {create_table, <<"logs">>, [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = tenant_id, type = id, nullable = false},
                #kura_column{name = created_at, type = utc_datetime, nullable = false}
            ]}
        ]
    end),
    meck:new(logs_schema, [non_strict]),
    meck:expect(logs_schema, table, fun() -> <<"logs">> end),
    meck:expect(logs_schema, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = tenant_id, type = id, nullable = false},
            #kura_field{name = created_at, type = utc_datetime, nullable = false}
        ]
    end),
    meck:expect(logs_schema, indexes, fun() ->
        [{[tenant_id, created_at], #{}}]
    end),

    DbState = kura_schema_diff:build_db_state([m_create_logs]),
    DesiredState = kura_schema_diff:build_desired_state([logs_schema]),
    {Up, Down} = kura_schema_diff:diff(DbState, DesiredState),

    ?assertMatch(
        [{create_index, <<"logs">>, [tenant_id, created_at], _}],
        Up
    ),
    %% Down emits drop_index using the conventional generated name.
    ?assertMatch(
        [{drop_index, <<"logs_tenant_id_created_at_index">>}],
        Down
    ),

    meck:unload([m_create_logs, logs_schema]).

%%====================================================================
%% Helpers
%%====================================================================

%% Re-implement apply_ops externally for testing reversibility
apply_ops_ext([], State) ->
    State;
apply_ops_ext([{create_table, Name, Cols} | Rest], State) ->
    apply_ops_ext(Rest, State#{Name => Cols});
apply_ops_ext([{drop_table, Name} | Rest], State) ->
    apply_ops_ext(Rest, maps:remove(Name, State));
apply_ops_ext([{alter_table, Name, AlterOps} | Rest], State) ->
    Cols = maps:get(Name, State, []),
    NewCols = apply_alter_ops_ext(AlterOps, Cols),
    apply_ops_ext(Rest, State#{Name => NewCols}).

apply_alter_ops_ext([], Cols) ->
    Cols;
apply_alter_ops_ext([{add_column, Col} | Rest], Cols) ->
    apply_alter_ops_ext(Rest, Cols ++ [Col]);
apply_alter_ops_ext([{drop_column, Name} | Rest], Cols) ->
    apply_alter_ops_ext(Rest, [C || C <- Cols, C#kura_column.name =/= Name]);
apply_alter_ops_ext([{modify_column, Name, Type} | Rest], Cols) ->
    NewCols = [
        case C#kura_column.name of
            Name -> C#kura_column{type = Type};
            _ -> C
        end
     || C <- Cols
    ],
    apply_alter_ops_ext(Rest, NewCols).

%%====================================================================
%% Replay fidelity: history that matches the schema reports no drift
%%====================================================================

replay(Ops) ->
    Mod = list_to_atom("m_replay_" ++ integer_to_list(erlang:unique_integer([positive]))),
    meck:new(Mod, [non_strict]),
    meck:expect(Mod, up, fun() -> Ops end),
    try
        kura_schema_diff:build_db_state([Mod])
    after
        meck:unload(Mod)
    end.

col(State, Table, Name) ->
    [Col] = [C || C <- maps:get(Table, cols(State)), C#kura_column.name =:= Name],
    Col.

users_and(Table, Cols) ->
    [
        {create_table, ~"users", [#kura_column{name = id, type = id, primary_key = true}]},
        {create_table, Table, [#kura_column{name = id, type = id, primary_key = true} | Cols]}
    ].

fk_desired(State, Table, Col, OnDelete) ->
    Cols = cols(State),
    Changed = [
        case C#kura_column.name of
            Col ->
                C#kura_column{
                    references = {~"users", id}, on_delete = OnDelete, on_update = undefined
                };
            _ ->
                C
        end
     || C <- maps:get(Table, Cols)
    ],
    #{columns => Cols#{Table => Changed}, indexes => idxs(State)}.

replay_hand_written_unquoted_fk_test() ->
    State = replay([
        {create_table, ~"users", [#kura_column{name = id, type = id, primary_key = true}]},
        {create_table, ~"user_tokens", [
            #kura_column{name = id, type = id, primary_key = true},
            #kura_column{name = user_id, type = integer, nullable = false}
        ]},
        {execute,
            ~"ALTER TABLE user_tokens ADD CONSTRAINT user_tokens_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE"}
    ]),
    C = col(State, ~"user_tokens", user_id),
    ?assertEqual({~"users", id}, C#kura_column.references),
    ?assertEqual(cascade, C#kura_column.on_delete).

replay_lowercase_alter_column_test() ->
    State = replay([
        {create_table, ~"pets", [#kura_column{name = name, type = string}]},
        {execute, ~"alter table pets alter column name set not null;"}
    ]),
    ?assertEqual(false, (col(State, ~"pets", name))#kura_column.nullable).

replay_quoted_identifier_with_embedded_quote_test() ->
    State = replay([
        {create_table, ~"we\"ird", [#kura_column{name = c, type = string}]},
        {execute, ~"ALTER TABLE \"we\"\"ird\" ALTER COLUMN \"c\" SET DEFAULT 'x'"}
    ]),
    ?assertEqual(~"x", (col(State, ~"we\"ird", c))#kura_column.default).

diff_primary_key_is_not_null_either_way_test() ->
    %% A migration usually leaves nullable at its default on the key column;
    %% the schema often spells out nullable = false. Postgres has it NOT NULL.
    Db = #{~"t" => [#kura_column{name = id, type = id, primary_key = true}]},
    Desired = #{
        ~"t" => [#kura_column{name = id, type = id, primary_key = true, nullable = false}]
    },
    ?assertEqual({[], []}, kura_schema_diff:diff(Db, Desired)).

replay_drop_index_by_explicit_name_test() ->
    State = replay([
        {create_table, ~"t", [#kura_column{name = a, type = string}]},
        {create_index, ~"custom_name", ~"t", [a], []},
        {drop_index, ~"custom_name"}
    ]),
    ?assertEqual([], maps:get(~"t", idxs(State))).

diff_drops_a_named_index_by_its_name_test() ->
    State = replay([
        {create_table, ~"t", [#kura_column{name = a, type = string}]},
        {create_index, ~"custom_name", ~"t", [a], []}
    ]),
    Desired = #{columns => cols(State), indexes => #{~"t" => []}},
    {Up, _Down} = kura_schema_diff:diff(State, Desired),
    ?assertEqual([{drop_index, ~"custom_name"}], Up).

replay_rename_then_drop_column_drops_its_index_test() ->
    %% Taure/rebar3_kura#49
    State = replay([
        {create_table, ~"t", [#kura_column{name = old, type = string}]},
        {create_index, ~"t", [old], #{}},
        {alter_table, ~"t", [{rename_column, old, new}]},
        {alter_table, ~"t", [{drop_column, new}]}
    ]),
    ?assertEqual([], maps:get(~"t", idxs(State))).

diff_drops_a_renamed_columns_index_by_its_original_name_test() ->
    State = replay([
        {create_table, ~"t", [#kura_column{name = old, type = string}]},
        {create_index, ~"t", [old], #{}},
        {alter_table, ~"t", [{rename_column, old, new}]}
    ]),
    ?assertMatch([{[new], _}], maps:get(~"t", idxs(State))),
    Desired = #{columns => cols(State), indexes => #{~"t" => []}},
    {Up, _Down} = kura_schema_diff:diff(State, Desired),
    ?assertEqual([{drop_index, ~"t_old_index"}], Up).

replay_drop_column_named_in_a_partial_index_where_test() ->
    %% Taure/rebar3_kura#49
    State = replay([
        {create_table, ~"t", [
            #kura_column{name = a, type = string},
            #kura_column{name = deleted_at, type = utc_datetime},
            #kura_column{name = deleted_by, type = string}
        ]},
        {create_index, ~"t", [a], #{where => ~"deleted_at IS NULL"}},
        {create_index, ~"t_a_by", ~"t", [a], [{where, ~"deleted_by IS NULL"}]},
        {alter_table, ~"t", [{drop_column, deleted_at}]}
    ]),
    ?assertMatch([{[a], #{name := ~"t_a_by"}}], maps:get(~"t", idxs(State))).

pet_store_auth_history_matches_its_schemas_test() ->
    %% The history and schemas from Taure/kura#178: consistent, yet the
    %% differ asked to drop three live indexes, re-add an existing foreign
    %% key and SET NOT NULL on two primary keys.
    Db = replay([
        {execute, ~"CREATE EXTENSION IF NOT EXISTS citext"},
        {create_table, ~"users", [
            #kura_column{name = id, type = id, primary_key = true},
            #kura_column{name = email, type = string, nullable = false}
        ]},
        {create_index, ~"users_email_index", ~"users", [email], [unique]},
        {create_table, ~"user_tokens", [
            #kura_column{name = id, type = id, primary_key = true},
            #kura_column{name = user_id, type = integer, nullable = false},
            #kura_column{name = token, type = string, nullable = false},
            #kura_column{name = context, type = string, nullable = false}
        ]},
        {create_index, ~"user_tokens_user_id_index", ~"user_tokens", [user_id], []},
        {create_index, ~"user_tokens_context_token_index", ~"user_tokens", [context, token], [
            unique
        ]},
        {execute,
            ~"ALTER TABLE user_tokens ADD CONSTRAINT user_tokens_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE"}
    ]),
    Desired = #{
        columns => #{
            ~"users" => [
                #kura_column{name = id, type = id, primary_key = true, nullable = false},
                #kura_column{name = email, type = string, nullable = false}
            ],
            ~"user_tokens" => [
                #kura_column{name = id, type = id, primary_key = true, nullable = false},
                #kura_column{
                    name = user_id,
                    type = integer,
                    nullable = false,
                    references = {~"users", id},
                    on_delete = cascade
                },
                #kura_column{name = token, type = string, nullable = false},
                #kura_column{name = context, type = string, nullable = false}
            ]
        },
        indexes => #{
            ~"users" => [{[email], #{unique => true}}],
            ~"user_tokens" => [{[user_id], #{}}, {[context, token], #{unique => true}}]
        }
    },
    ?assertEqual({[], []}, kura_schema_diff:diff(Db, Desired)).

replay_lowercase_semicolon_fk_and_drop_test() ->
    Base =
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute,
                    ~"  alter table k add constraint k_u_id_fkey foreign key (u_id) references users (id) on delete cascade;"}
            ],
    Added = col(replay(Base), ~"k", u_id),
    ?assertEqual({~"users", id}, Added#kura_column.references),
    ?assertEqual(cascade, Added#kura_column.on_delete),
    Dropped = col(
        replay(Base ++ [{execute, ~"alter table k drop constraint k_u_id_fkey;"}]), ~"k", u_id
    ),
    ?assertEqual(undefined, Dropped#kura_column.references).

replay_quoted_fk_identifiers_with_embedded_quotes_test() ->
    State = replay([
        {create_table, ~"us\"ers", [#kura_column{name = id, type = id, primary_key = true}]},
        {create_table, ~"k", [#kura_column{name = u_id, type = integer}]},
        {execute,
            ~"ALTER TABLE \"k\" ADD CONSTRAINT \"k\"\"fk\" FOREIGN KEY (\"u_id\") REFERENCES \"us\"\"ers\" (\"id\")"},
        {execute, ~"ALTER TABLE \"k\" DROP CONSTRAINT \"k\"\"fk\""}
    ]),
    ?assertEqual(undefined, (col(State, ~"k", u_id))#kura_column.references),
    Kept = replay([
        {create_table, ~"us\"ers", [#kura_column{name = id, type = id, primary_key = true}]},
        {create_table, ~"k", [#kura_column{name = u_id, type = integer}]},
        {execute,
            ~"ALTER TABLE \"k\" ADD CONSTRAINT \"k\"\"fk\" FOREIGN KEY (\"u_id\") REFERENCES \"us\"\"ers\" (\"id\")"}
    ]),
    ?assertEqual({~"us\"ers", id}, (col(Kept, ~"k", u_id))#kura_column.references).

replay_fk_on_update_is_recorded_test() ->
    State = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute,
                    ~"ALTER TABLE k ADD CONSTRAINT k_u_id_fkey FOREIGN KEY (u_id) REFERENCES users(id) ON DELETE CASCADE ON UPDATE CASCADE"}
            ]
    ),
    C = col(State, ~"k", u_id),
    ?assertEqual({cascade, cascade}, {C#kura_column.on_delete, C#kura_column.on_update}),
    %% Regenerating the constraint would drop ON UPDATE, so changing it is refused.
    ?assertError(
        {kura_schema_diff, {fk_on_update_not_owned, ~"k", u_id, cascade}},
        kura_schema_diff:diff(State, fk_desired(State, ~"k", u_id, restrict))
    ).

replay_fk_tail_kura_cannot_express_is_left_alone_test() ->
    State = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute,
                    ~"ALTER TABLE k ADD CONSTRAINT k_u_id_fkey FOREIGN KEY (u_id) REFERENCES users(id) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED"}
            ]
    ),
    ?assertEqual({~"users", id}, (col(State, ~"k", u_id))#kura_column.references),
    ?assertEqual({[], []}, kura_schema_diff:diff(State, fk_desired(State, ~"k", u_id, cascade))),
    ?assertError(
        {kura_schema_diff, {fk_not_owned, ~"k", u_id}},
        kura_schema_diff:diff(State, fk_desired(State, ~"k", u_id, restrict))
    ),
    Elsewhere = fk_desired(State, ~"k", u_id, cascade),
    #{columns := #{~"k" := KCols} = DesCols} = Elsewhere,
    Moved = [
        case C#kura_column.name of
            u_id -> C#kura_column{references = {~"users", other}};
            _ -> C
        end
     || C <- KCols
    ],
    ?assertError(
        {kura_schema_diff, {fk_not_owned, ~"k", u_id}},
        kura_schema_diff:diff(State, Elsewhere#{columns => DesCols#{~"k" => Moved}})
    ).

diff_drops_a_custom_named_fk_by_its_name_test() ->
    State = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute,
                    ~"ALTER TABLE k ADD CONSTRAINT fk_u FOREIGN KEY (u_id) REFERENCES users(id)"}
            ]
    ),
    {Up, Down} = kura_schema_diff:diff(State, fk_desired(State, ~"k", u_id, cascade)),
    ?assertMatch(
        [
            {execute, ~"ALTER TABLE \"k\" DROP CONSTRAINT \"fk_u\""},
            {execute, <<"ALTER TABLE \"k\" ADD CONSTRAINT \"fk_u\"", _/binary>>}
        ],
        Up
    ),
    ?assertMatch(
        [{execute, _}, {execute, <<"ALTER TABLE \"k\" ADD CONSTRAINT \"fk_u\"", _/binary>>}], Down
    ),
    Dropped = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute,
                    ~"ALTER TABLE k ADD CONSTRAINT fk_u FOREIGN KEY (u_id) REFERENCES users(id)"},
                {execute, ~"ALTER TABLE k DROP CONSTRAINT fk_u"}
            ]
    ),
    ?assertEqual(undefined, (col(Dropped, ~"k", u_id))#kura_column.references).

replay_unnamed_fk_takes_the_postgres_name_test() ->
    State = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute, ~"ALTER TABLE k ADD FOREIGN KEY (u_id) REFERENCES users(id)"},
                {execute, ~"ALTER TABLE k DROP CONSTRAINT k_u_id_fkey"}
            ]
    ),
    ?assertEqual(undefined, (col(State, ~"k", u_id))#kura_column.references).

replay_renamed_fk_column_keeps_its_constraint_name_test() ->
    State = replay(
        users_and(~"k", [
            #kura_column{name = u_id, type = integer, references = {~"users", id}}
        ]) ++
            [
                {alter_table, ~"k", [{rename_column, u_id, owner_id}]}
            ]
    ),
    {Up, _} = kura_schema_diff:diff(State, fk_desired(State, ~"k", owner_id, cascade)),
    ?assertMatch([{execute, ~"ALTER TABLE \"k\" DROP CONSTRAINT \"k_u_id_fkey\""} | _], Up),
    Dropped = replay(
        users_and(~"k", [
            #kura_column{name = u_id, type = integer, references = {~"users", id}}
        ]) ++
            [
                {alter_table, ~"k", [{rename_column, u_id, owner_id}]},
                {execute, ~"ALTER TABLE k DROP CONSTRAINT k_u_id_fkey"}
            ]
    ),
    ?assertEqual(undefined, (col(Dropped, ~"k", owner_id))#kura_column.references).

replay_statement_split_across_lines_test() ->
    State = replay([
        {create_table, ~"orders", [#kura_column{name = note, type = string}]},
        {execute, ~"ALTER TABLE orders\n  ALTER COLUMN note\n  SET NOT NULL;\n"}
    ]),
    ?assertEqual(false, (col(State, ~"orders", note))#kura_column.nullable).

replay_bare_identifiers_fold_to_lower_case_test() ->
    State = replay([
        {create_table, ~"orders", [#kura_column{name = user_id, type = integer}]},
        {execute, ~"ALTER TABLE ORDERS ALTER USER_ID SET NOT NULL"}
    ]),
    ?assertEqual(false, (col(State, ~"orders", user_id))#kura_column.nullable).

replay_long_whitespace_still_matches_test() ->
    Padding = binary:copy(~" ", 5000),
    State = replay([
        {create_table, ~"orders", [#kura_column{name = note, type = string}]},
        {execute, <<"ALTER TABLE orders ALTER COLUMN note SET NOT NULL", Padding/binary, ";">>}
    ]),
    ?assertEqual(false, (col(State, ~"orders", note))#kura_column.nullable).

replay_default_round_trips_an_escaped_quote_test() ->
    Db = #{~"t" => [#kura_column{name = c, type = string}]},
    Desired = #{~"t" => [#kura_column{name = c, type = string, default = ~"it's"}]},
    {[], [], [{execute, SetDefault}], _} = diff_parts(Db, Desired),
    State = replay([
        {create_table, ~"t", [#kura_column{name = c, type = string}]}, {execute, SetDefault}
    ]),
    ?assertEqual(~"it's", (col(State, ~"t", c))#kura_column.default).

replay_default_expression_is_not_replayed_test() ->
    [
        begin
            SQL = <<"ALTER TABLE t ALTER COLUMN c SET DEFAULT ", Value/binary>>,
            State = replay([
                {create_table, ~"t", [#kura_column{name = c, type = string}]},
                {execute, SQL}
            ]),
            ?assertEqual(undefined, (col(State, ~"t", c))#kura_column.default),
            ?assertEqual([SQL], kura_schema_diff:unreplayed_executes(State))
        end
     || Value <- [~"'x'::text", ~"'", ~"now()", ~"nextval('s')", ~"1e5"]
    ].

replay_default_literals_ignore_case_test() ->
    [
        ?assertEqual(
            Expected,
            (col(
                replay([
                    {create_table, ~"t", [#kura_column{name = c, type = boolean}]},
                    {execute, <<"ALTER TABLE t ALTER COLUMN c SET DEFAULT ", Value/binary>>}
                ]),
                ~"t",
                c
            ))#kura_column.default
        )
     || {Value, Expected} <- [{~"TRUE", true}, {~"False", false}, {~"-3", -3}, {~"1.5", 1.5}]
    ].

replay_fk_action_after_an_unreadable_one_is_recorded_test() ->
    State = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute,
                    ~"ALTER TABLE k ADD FOREIGN KEY (u_id) REFERENCES users(id) ON DELETE SET DEFAULT ON UPDATE CASCADE"}
            ]
    ),
    ?assertEqual(cascade, (col(State, ~"k", u_id))#kura_column.on_update).

replay_several_statements_are_not_replayed_test() ->
    SQL = ~"ALTER TABLE k ADD FOREIGN KEY (u_id) REFERENCES users(id); DROP TABLE k",
    State = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++ [{execute, SQL}]
    ),
    ?assertEqual(undefined, (col(State, ~"k", u_id))#kura_column.references),
    ?assertEqual([SQL], kura_schema_diff:unreplayed_executes(State)).

replay_fk_on_delete_after_an_unknown_clause_is_recorded_test() ->
    State = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute,
                    ~"ALTER TABLE k ADD FOREIGN KEY (u_id) REFERENCES users(id) DEFERRABLE ON DELETE CASCADE"}
            ]
    ),
    ?assertEqual(cascade, (col(State, ~"k", u_id))#kura_column.on_delete),
    ?assertEqual({[], []}, kura_schema_diff:diff(State, fk_desired(State, ~"k", u_id, cascade))).

replay_unknown_column_is_not_an_atom_leak_test() ->
    State = replay([
        {create_table, ~"orders", [#kura_column{name = note, type = string}]},
        {execute, ~"ALTER TABLE orders ALTER COLUMN never_an_atom_zq81 SET NOT NULL"}
    ]),
    ?assertError(badarg, binary_to_existing_atom(~"never_an_atom_zq81")),
    ?assertMatch([_], kura_schema_diff:unreplayed_executes(State)).

unreplayed_executes_name_a_known_table_test() ->
    State = replay([
        {execute, ~"CREATE EXTENSION IF NOT EXISTS citext"},
        {create_table, ~"users", [#kura_column{name = email, type = string}]},
        {execute, ~"CREATE INDEX CONCURRENTLY users_email_idx ON users (email)"},
        {execute,
            ~"CREATE FUNCTION touch() RETURNS trigger AS $$ BEGIN RETURN NEW; END $$ LANGUAGE plpgsql"}
    ]),
    ?assertEqual(
        [~"CREATE INDEX CONCURRENTLY users_email_idx ON users (email)"],
        kura_schema_diff:unreplayed_executes(State)
    ).

unreplayed_executes_skip_dml_test() ->
    State = replay([
        {create_table, ~"users", [#kura_column{name = email, type = string}]},
        {execute, ~"UPDATE users SET email = lower(email)"},
        {execute, ~"INSERT INTO users (email) VALUES ('a@b')"},
        {execute, ~"DELETE FROM users WHERE email IS NULL"}
    ]),
    ?assertEqual([], kura_schema_diff:unreplayed_executes(State)).

replay_rename_rewrites_a_partial_index_where_test() ->
    Base = [
        {create_table, ~"t", [
            #kura_column{name = user_id, type = integer},
            #kura_column{name = old, type = utc_datetime}
        ]},
        {create_index, ~"t", [user_id], #{where => ~"old IS NULL"}},
        {alter_table, ~"t", [{rename_column, old, new}]}
    ],
    ?assertMatch([{[user_id], #{where := ~"new IS NULL"}}], maps:get(~"t", idxs(replay(Base)))),
    Dropped = replay(Base ++ [{alter_table, ~"t", [{drop_column, new}]}]),
    ?assertEqual([], maps:get(~"t", idxs(Dropped))).

replay_rename_to_a_column_with_replacement_characters_test() ->
    State = replay([
        {create_table, ~"t", [#kura_column{name = old, type = utc_datetime}]},
        {create_index, ~"t", [old], #{where => ~"old IS NULL"}},
        {alter_table, ~"t", [{rename_column, old, 'a&b\\1'}]}
    ]),
    ?assertMatch([{_, #{where := ~"a&b\\1 IS NULL"}}], maps:get(~"t", idxs(State))).

diff_after_a_rename_matches_the_schema_by_structure_test() ->
    %% kura_schema_verify matches indexes by columns, uniqueness and WHERE,
    %% so the index keeping its pre-rename name is not drift either.
    State = replay([
        {create_table, ~"t", [#kura_column{name = old, type = string}]},
        {create_index, ~"t", [old], #{}},
        {alter_table, ~"t", [{rename_column, old, new}]}
    ]),
    Desired = #{columns => cols(State), indexes => #{~"t" => [{[new], #{}}]}},
    ?assertEqual({[], []}, kura_schema_diff:diff(State, Desired)).

diff_rollback_recreates_a_named_index_under_its_name_test() ->
    State = replay([
        {create_table, ~"t", [#kura_column{name = a, type = string}]},
        {create_index, ~"custom", ~"t", [a], [unique, {where, ~"a IS NOT NULL"}]}
    ]),
    Desired = #{columns => cols(State), indexes => #{~"t" => []}},
    ?assertEqual(
        {[{drop_index, ~"custom"}], [
            {create_index, ~"custom", ~"t", [a], [unique, {where, ~"a IS NOT NULL"}]}
        ]},
        kura_schema_diff:diff(State, Desired)
    ).

diff_drops_one_of_two_equivalent_indexes_test() ->
    State = replay([
        {create_table, ~"t", [#kura_column{name = a, type = string}]},
        {create_index, ~"t", [a], #{}},
        {create_index, ~"custom", ~"t", [a], []}
    ]),
    Desired = #{columns => cols(State), indexes => #{~"t" => [{[a], #{}}]}},
    {Up, Down} = kura_schema_diff:diff(State, Desired),
    ?assertEqual(1, length(Up)),
    ?assertEqual(1, length(Down)).

index_where_match_treats_the_column_name_literally_test() ->
    State = replay([
        {create_table, ~"t", [
            #kura_column{name = a, type = string},
            #kura_column{name = 'x.y', type = string}
        ]},
        {create_index, ~"t", [a], #{where => ~"xzy IS NULL"}},
        {alter_table, ~"t", [{drop_column, 'x.y'}]}
    ]),
    ?assertMatch([{[a], _}], maps:get(~"t", idxs(State))).

diff_does_not_drop_not_null_from_a_demoted_primary_key_test() ->
    Db = #{~"t" => [#kura_column{name = id, type = id, primary_key = true}]},
    Desired = #{~"t" => [#kura_column{name = id, type = id}]},
    ?assertEqual({[], []}, kura_schema_diff:diff(Db, Desired)).

diff_parts(Db, Desired) ->
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    {
        [Op || {alter_table, _, _} = Op <- Up],
        [Op || {alter_table, _, _} = Op <- Down],
        [Op || {execute, _} = Op <- Up],
        [Op || {execute, _} = Op <- Down]
    }.

replay_default_with_a_semicolon_round_trips_test() ->
    Db = #{~"t" => [#kura_column{name = c, type = string}]},
    Desired = #{~"t" => [#kura_column{name = c, type = string, default = ~"a;b"}]},
    {[], [], [{execute, SetDefault}], _} = diff_parts(Db, Desired),
    State = replay([
        {create_table, ~"t", [#kura_column{name = c, type = string}]}, {execute, SetDefault}
    ]),
    ?assertEqual(~"a;b", (col(State, ~"t", c))#kura_column.default),
    ?assertEqual([], kura_schema_diff:unreplayed_executes(State)).

replay_latin1_seed_data_does_not_crash_test() ->
    State = replay([
        {create_table, ~"cities", [#kura_column{name = name, type = string}]},
        {execute, <<"INSERT INTO cities (name) VALUES ('Malm", 246, "')">>},
        {execute, <<"ALTER TABLE cities ALTER COLUMN name SET DEFAULT 'Malm", 246, "'">>}
    ]),
    ?assertEqual(<<"Malm", 246>>, (col(State, ~"cities", name))#kura_column.default).

replay_latin1_ddl_is_reported_test() ->
    SQL = <<"ALTER TABLE cities ADD CHECK (name <> 'Malm", 246, "')">>,
    State = replay([
        {create_table, ~"cities", [#kura_column{name = name, type = string}]}, {execute, SQL}
    ]),
    ?assertEqual([SQL], kura_schema_diff:unreplayed_executes(State)).

replay_ddl_anywhere_in_an_execute_is_reported_test() ->
    [
        begin
            State = replay([
                {create_table, ~"t", [#kura_column{name = c, type = string}]}, {execute, SQL}
            ]),
            ?assertEqual([SQL], kura_schema_diff:unreplayed_executes(State))
        end
     || SQL <- [
            ~"UPDATE t SET c = 'x'; ALTER TABLE t ALTER COLUMN c SET NOT NULL",
            ~"DO $$ BEGIN ALTER TABLE t ALTER COLUMN c SET NOT NULL; END $$",
            ~"-- note\nALTER TABLE t ADD CHECK (c <> '')"
        ]
    ].

replay_ddl_words_inside_literals_are_not_ddl_test() ->
    State = replay([
        {create_table, ~"t", [#kura_column{name = c, type = string}]},
        {execute, ~"UPDATE t SET c = 'drop table t'"}
    ]),
    ?assertEqual([], kura_schema_diff:unreplayed_executes(State)).

replay_statement_that_changes_nothing_is_reported_test() ->
    [
        begin
            State = replay([
                {create_table, ~"t", [#kura_column{name = c, type = string}]}, {execute, SQL}
            ]),
            ?assertEqual([SQL], kura_schema_diff:unreplayed_executes(State))
        end
     || SQL <- [
            ~"ALTER TABLE t DROP CONSTRAINT t_c_check",
            ~"ALTER TABLE t ALTER COLUMN name SET NOT NULL",
            ~"ALTER TABLE t ADD FOREIGN KEY (name) REFERENCES users(id)"
        ]
    ].

replay_default_null_drops_the_default_test() ->
    State = replay([
        {create_table, ~"t", [#kura_column{name = c, type = string, default = ~"x"}]},
        {execute, ~"ALTER TABLE t ALTER COLUMN c SET DEFAULT NULL"}
    ]),
    ?assertEqual(undefined, (col(State, ~"t", c))#kura_column.default),
    ?assertEqual([], kura_schema_diff:unreplayed_executes(State)).

replay_default_exponent_is_not_replayed_test() ->
    [
        begin
            SQL = <<"ALTER TABLE t ALTER COLUMN c SET DEFAULT ", Value/binary>>,
            State = replay([
                {create_table, ~"t", [#kura_column{name = c, type = float}]}, {execute, SQL}
            ]),
            ?assertEqual(undefined, (col(State, ~"t", c))#kura_column.default),
            ?assertEqual([SQL], kura_schema_diff:unreplayed_executes(State))
        end
     || Value <- [~"1e5", ~"1.5e3"]
    ].

replay_reports_a_table_whose_name_holds_regex_syntax_test() ->
    SQL = ~"ALTER TABLE \"a\\Eb(\" ADD CHECK (c <> '')",
    State = replay([
        {create_table, ~"a\\Eb(", [#kura_column{name = c, type = string}]}, {execute, SQL}
    ]),
    ?assertEqual([SQL], kura_schema_diff:unreplayed_executes(State)).

replay_rename_keeps_a_hand_written_fk_unowned_test() ->
    State = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute, ~"ALTER TABLE k ADD FOREIGN KEY (u_id) REFERENCES users(id) DEFERRABLE"},
                {alter_table, ~"k", [{rename_column, u_id, owner_id}]}
            ]
    ),
    ?assertError(
        {kura_schema_diff, {fk_not_owned, ~"k", owner_id}},
        kura_schema_diff:diff(State, fk_desired(State, ~"k", owner_id, cascade))
    ).

replay_recreated_table_forgets_the_old_fk_meta_test() ->
    State = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute,
                    ~"ALTER TABLE k ADD CONSTRAINT fk_u FOREIGN KEY (u_id) REFERENCES users(id) DEFERRABLE"},
                {drop_table, ~"k"},
                {create_table, ~"k", [
                    #kura_column{name = u_id, type = integer, references = {~"users", id}}
                ]}
            ]
    ),
    {Up, _} = kura_schema_diff:diff(State, fk_desired(State, ~"k", u_id, cascade)),
    ?assertMatch([{execute, ~"ALTER TABLE \"k\" DROP CONSTRAINT \"k_u_id_fkey\""} | _], Up).

replay_readded_column_forgets_the_old_fk_meta_test() ->
    State = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute,
                    ~"ALTER TABLE k ADD CONSTRAINT fk_u FOREIGN KEY (u_id) REFERENCES users(id) DEFERRABLE"},
                {alter_table, ~"k", [{drop_column, u_id}]},
                {alter_table, ~"k", [
                    {add_column, #kura_column{
                        name = u_id, type = integer, references = {~"users", id}
                    }}
                ]}
            ]
    ),
    {Up, _} = kura_schema_diff:diff(State, fk_desired(State, ~"k", u_id, cascade)),
    ?assertMatch([{execute, ~"ALTER TABLE \"k\" DROP CONSTRAINT \"k_u_id_fkey\""} | _], Up).

replay_repeated_on_delete_is_hand_written_test() ->
    State = replay(
        users_and(~"k", [#kura_column{name = u_id, type = integer}]) ++
            [
                {execute,
                    ~"ALTER TABLE k ADD FOREIGN KEY (u_id) REFERENCES users(id) ON DELETE CASCADE ON DELETE RESTRICT"}
            ]
    ),
    ?assertError(
        {kura_schema_diff, {fk_not_owned, ~"k", u_id}},
        kura_schema_diff:diff(State, fk_desired(State, ~"k", u_id, restrict))
    ).

diff_drops_the_index_whose_options_differ_test() ->
    State = replay([
        {create_table, ~"t", [#kura_column{name = a, type = string}]},
        {create_index, ~"t_a_unique", ~"t", [a], [unique]},
        {create_index, ~"t_a_plain", ~"t", [a], []}
    ]),
    Desired = #{columns => cols(State), indexes => #{~"t" => [{[a], #{unique => true}}]}},
    ?assertEqual(
        {[{drop_index, ~"t_a_plain"}], [{create_index, ~"t_a_plain", ~"t", [a], []}]},
        kura_schema_diff:diff(State, Desired)
    ).

diff_recreates_an_unnamed_index_under_its_generated_name_test() ->
    State = replay([
        {create_table, ~"t", [#kura_column{name = a, type = string}]},
        {create_index, ~"t", [a], #{unique => true}}
    ]),
    Desired = #{columns => cols(State), indexes => #{~"t" => []}},
    ?assertEqual(
        {[{drop_index, ~"t_a_index"}], [{create_index, ~"t", [a], #{unique => true}}]},
        kura_schema_diff:diff(State, Desired)
    ).

diff_recreates_an_index_named_as_generated_without_its_name_test() ->
    State = replay([
        {create_table, ~"t", [#kura_column{name = a, type = string}]},
        {create_index, ~"t_a_index", ~"t", [a], [unique]}
    ]),
    Desired = #{columns => cols(State), indexes => #{~"t" => []}},
    ?assertEqual(
        {[{drop_index, ~"t_a_index"}], [{create_index, ~"t", [a], #{unique => true}}]},
        kura_schema_diff:diff(State, Desired)
    ).

%%====================================================================
%% down/0 undoes up/0 in reverse (Taure/rebar3_kura#41)
%%====================================================================

new_authors_table() ->
    [#kura_column{name = id, type = id, primary_key = true}].

diff_down_drops_a_new_reference_column_before_its_table_test() ->
    Db = #{~"posts" => [#kura_column{name = id, type = id, primary_key = true}]},
    AuthorId = #kura_column{name = author_id, type = integer, references = {~"authors", id}},
    Desired = #{
        ~"authors" => new_authors_table(),
        ~"posts" => [#kura_column{name = id, type = id, primary_key = true}, AuthorId]
    },
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    ?assertEqual(
        [
            {create_table, ~"authors", new_authors_table()},
            {alter_table, ~"posts", [{add_column, AuthorId}]}
        ],
        Up
    ),
    ?assertEqual(
        [{alter_table, ~"posts", [{drop_column, author_id}]}, {drop_table, ~"authors"}],
        Down
    ).

diff_down_drops_a_new_foreign_key_before_its_table_test() ->
    Db = #{
        ~"posts" => [
            #kura_column{name = id, type = id, primary_key = true},
            #kura_column{name = author_id, type = integer}
        ]
    },
    Desired = #{
        ~"authors" => new_authors_table(),
        ~"posts" => [
            #kura_column{name = id, type = id, primary_key = true},
            #kura_column{name = author_id, type = integer, references = {~"authors", id}}
        ]
    },
    {_Up, Down} = kura_schema_diff:diff(Db, Desired),
    ?assertEqual(
        [
            {execute, ~"ALTER TABLE \"posts\" DROP CONSTRAINT \"posts_author_id_fkey\""},
            {drop_table, ~"authors"}
        ],
        Down
    ).

diff_down_drops_a_referencing_new_table_first_test() ->
    Desired = #{
        ~"authors" => new_authors_table(),
        ~"books" => [
            #kura_column{name = id, type = id, primary_key = true},
            #kura_column{name = author_id, type = integer, references = {~"authors", id}}
        ],
        ~"zines" => [#kura_column{name = id, type = id, primary_key = true}]
    },
    {_Up, Down} = kura_schema_diff:diff(#{}, Desired),
    ?assertEqual(
        [{drop_table, ~"books"}, {drop_table, ~"zines"}, {drop_table, ~"authors"}],
        Down
    ).

diff_down_drops_a_self_referencing_new_table_test() ->
    Desired = #{
        ~"nodes" => [
            #kura_column{name = id, type = id, primary_key = true},
            #kura_column{name = parent_id, type = integer, references = {~"nodes", id}}
        ]
    },
    ?assertMatch({_, [{drop_table, ~"nodes"}]}, kura_schema_diff:diff(#{}, Desired)).

diff_down_keeps_mutually_referencing_new_tables_test() ->
    Desired = #{
        ~"a" => [#kura_column{name = b_id, type = integer, references = {~"b", id}}],
        ~"b" => [#kura_column{name = a_id, type = integer, references = {~"a", id}}]
    },
    ?assertMatch(
        {_, [{drop_table, ~"a"}, {drop_table, ~"b"}]}, kura_schema_diff:diff(#{}, Desired)
    ).
