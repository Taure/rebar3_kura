-module(kura_schema_diff_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

%%====================================================================
%% build_db_state tests
%%====================================================================

build_db_state_empty_test() ->
    ?assertEqual(#{}, kura_schema_diff:build_db_state([])).

build_db_state_create_table_test() ->
    meck:new(m_create, [non_strict]),
    meck:expect(m_create, up, fun() ->
        [{create_table, <<"pets">>, [
            #kura_column{name = id, type = id, primary_key = true},
            #kura_column{name = name, type = string}
        ]}]
    end),
    State = kura_schema_diff:build_db_state([m_create]),
    ?assert(maps:is_key(<<"pets">>, State)),
    ?assertEqual(2, length(maps:get(<<"pets">>, State))),
    meck:unload(m_create).

build_db_state_alter_table_test() ->
    meck:new(m_a_create, [non_strict]),
    meck:new(m_b_alter, [non_strict]),
    meck:expect(m_a_create, up, fun() ->
        [{create_table, <<"pets">>, [
            #kura_column{name = id, type = id}
        ]}]
    end),
    meck:expect(m_b_alter, up, fun() ->
        [{alter_table, <<"pets">>, [
            {add_column, #kura_column{name = age, type = integer}}
        ]}]
    end),
    State = kura_schema_diff:build_db_state([m_a_create, m_b_alter]),
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
    State = kura_schema_diff:build_db_state([m_create3, m_drop]),
    ?assertNot(maps:is_key(<<"tmp">>, State)),
    meck:unload([m_create3, m_drop]).

build_db_state_drop_column_test() ->
    meck:new(m_create4, [non_strict]),
    meck:new(m_dropcol, [non_strict]),
    meck:expect(m_create4, up, fun() ->
        [{create_table, <<"t">>, [
            #kura_column{name = id, type = id},
            #kura_column{name = old, type = string}
        ]}]
    end),
    meck:expect(m_dropcol, up, fun() ->
        [{alter_table, <<"t">>, [{drop_column, old}]}]
    end),
    State = kura_schema_diff:build_db_state([m_create4, m_dropcol]),
    Cols = maps:get(<<"t">>, State),
    ?assertEqual(1, length(Cols)),
    ?assertEqual(id, (hd(Cols))#kura_column.name),
    meck:unload([m_create4, m_dropcol]).

build_db_state_skips_index_ops_test() ->
    meck:new(m_idx, [non_strict]),
    meck:expect(m_idx, up, fun() ->
        [{create_table, <<"t">>, [#kura_column{name = id, type = id}]},
         {create_index, <<"idx_t_id">>, <<"t">>, [id], []},
         {execute, <<"SELECT 1">>}]
    end),
    State = kura_schema_diff:build_db_state([m_idx]),
    ?assertEqual(1, length(maps:get(<<"t">>, State))),
    meck:unload(m_idx).

%%====================================================================
%% build_desired_state tests
%%====================================================================

build_desired_state_test() ->
    meck:new(test_schema, [non_strict]),
    meck:expect(test_schema, table, fun() -> <<"users">> end),
    meck:expect(test_schema, fields, fun() ->
        [#kura_field{name = id, type = id, primary_key = true, nullable = false},
         #kura_field{name = name, type = string},
         #kura_field{name = display, type = string, virtual = true}]
    end),
    State = kura_schema_diff:build_desired_state([test_schema]),
    Cols = maps:get(<<"users">>, State),
    %% Virtual field excluded
    ?assertEqual(2, length(Cols)),
    Names = [C#kura_column.name || C <- Cols],
    ?assertNot(lists:member(display, Names)),
    meck:unload(test_schema).

%%====================================================================
%% diff tests
%%====================================================================

diff_no_changes_test() ->
    Cols = [#kura_column{name = id, type = id}, #kura_column{name = name, type = string}],
    Db = #{<<"t">> => Cols},
    Desired = #{<<"t">> => Cols},
    ?assertEqual({[], []}, kura_schema_diff:diff(Db, Desired)).

diff_new_table_test() ->
    Cols = [#kura_column{name = id, type = id}, #kura_column{name = name, type = string}],
    {Up, Down} = kura_schema_diff:diff(#{}, #{<<"users">> => Cols}),
    ?assertMatch([{create_table, <<"users">>, _}], Up),
    ?assertMatch([{drop_table, <<"users">>}], Down).

diff_add_column_test() ->
    DbCols = [#kura_column{name = id, type = id}],
    DesiredCols = [#kura_column{name = id, type = id}, #kura_column{name = age, type = integer}],
    {Up, Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assertMatch([{alter_table, <<"t">>, [{add_column, #kura_column{name = age}}]}], Up),
    ?assertMatch([{alter_table, <<"t">>, [{drop_column, age}]}], Down).

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

diff_multiple_changes_test() ->
    DbCols = [#kura_column{name = id, type = id}, #kura_column{name = old, type = string}],
    DesiredCols = [#kura_column{name = id, type = id}, #kura_column{name = new, type = integer}],
    {Up, _Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    %% Should have add_column for new and drop_column for old
    ?assertEqual(1, length(Up)),
    [{alter_table, <<"t">>, AlterOps}] = Up,
    ?assert(lists:any(fun({add_column, #kura_column{name = new}}) -> true; (_) -> false end, AlterOps)),
    ?assert(lists:any(fun({drop_column, old}) -> true; (_) -> false end, AlterOps)).

diff_multi_table_test() ->
    Db = #{<<"a">> => [#kura_column{name = id, type = id}]},
    Desired = #{
        <<"a">> => [#kura_column{name = id, type = id}, #kura_column{name = x, type = string}],
        <<"b">> => [#kura_column{name = id, type = id}]
    },
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    ?assertEqual(2, length(Up)),
    %% One create_table for b, one alter_table for a
    HasCreate = lists:any(fun({create_table, <<"b">>, _}) -> true; (_) -> false end, Up),
    HasAlter = lists:any(fun({alter_table, <<"a">>, _}) -> true; (_) -> false end, Up),
    ?assert(HasCreate),
    ?assert(HasAlter),
    ?assertEqual(2, length(Down)).

%%====================================================================
%% field_to_column tests
%%====================================================================

field_to_column_test() ->
    Field = #kura_field{name = email, type = string, nullable = false, default = undefined, primary_key = false},
    Col = kura_schema_diff:field_to_column(Field),
    ?assertEqual(email, Col#kura_column.name),
    ?assertEqual(string, Col#kura_column.type),
    ?assertEqual(false, Col#kura_column.nullable),
    ?assertEqual(false, Col#kura_column.primary_key).

field_to_column_preserves_pk_test() ->
    Field = #kura_field{name = id, type = id, primary_key = true, nullable = false},
    Col = kura_schema_diff:field_to_column(Field),
    ?assertEqual(true, Col#kura_column.primary_key).
