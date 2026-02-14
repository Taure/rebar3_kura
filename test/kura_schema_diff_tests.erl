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

build_db_state_rename_column_test() ->
    meck:new(m_rc_create, [non_strict]),
    meck:new(m_rc_rename, [non_strict]),
    meck:expect(m_rc_create, up, fun() ->
        [{create_table, <<"t">>, [
            #kura_column{name = id, type = id},
            #kura_column{name = old_name, type = string}
        ]}]
    end),
    meck:expect(m_rc_rename, up, fun() ->
        [{alter_table, <<"t">>, [{rename_column, old_name, new_name}]}]
    end),
    State = kura_schema_diff:build_db_state([m_rc_create, m_rc_rename]),
    Cols = maps:get(<<"t">>, State),
    Names = [C#kura_column.name || C <- Cols],
    ?assert(lists:member(new_name, Names)),
    ?assertNot(lists:member(old_name, Names)),
    meck:unload([m_rc_create, m_rc_rename]).

build_db_state_modify_column_test() ->
    meck:new(m_mc_create, [non_strict]),
    meck:new(m_mc_modify, [non_strict]),
    meck:expect(m_mc_create, up, fun() ->
        [{create_table, <<"t">>, [
            #kura_column{name = id, type = id},
            #kura_column{name = age, type = integer}
        ]}]
    end),
    meck:expect(m_mc_modify, up, fun() ->
        [{alter_table, <<"t">>, [{modify_column, age, float}]}]
    end),
    State = kura_schema_diff:build_db_state([m_mc_create, m_mc_modify]),
    [_, AgeCol] = maps:get(<<"t">>, State),
    ?assertEqual(float, AgeCol#kura_column.type),
    meck:unload([m_mc_create, m_mc_modify]).

build_db_state_multiple_tables_test() ->
    meck:new(m_multi, [non_strict]),
    meck:expect(m_multi, up, fun() ->
        [{create_table, <<"users">>, [#kura_column{name = id, type = id}]},
         {create_table, <<"posts">>, [
            #kura_column{name = id, type = id},
            #kura_column{name = user_id, type = integer}
         ]}]
    end),
    State = kura_schema_diff:build_db_state([m_multi]),
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
    State = kura_schema_diff:build_db_state([m_maa_create, m_mab_alter1, m_mac_alter2]),
    Cols = maps:get(<<"t">>, State),
    ?assertEqual(3, length(Cols)),
    meck:unload([m_maa_create, m_mab_alter1, m_mac_alter2]).

build_db_state_alter_nonexistent_table_test() ->
    %% Altering a table that doesn't exist yet (edge case) — should create empty then add
    meck:new(m_ghost, [non_strict]),
    meck:expect(m_ghost, up, fun() ->
        [{alter_table, <<"ghost">>, [{add_column, #kura_column{name = x, type = string}}]}]
    end),
    State = kura_schema_diff:build_db_state([m_ghost]),
    ?assertEqual([#kura_column{name = x, type = string}], maps:get(<<"ghost">>, State)),
    meck:unload(m_ghost).

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

build_desired_state_empty_test() ->
    ?assertEqual(#{}, kura_schema_diff:build_desired_state([])).

build_desired_state_multiple_schemas_test() ->
    meck:new(schema_a, [non_strict]),
    meck:new(schema_b, [non_strict]),
    meck:expect(schema_a, table, fun() -> <<"a">> end),
    meck:expect(schema_a, fields, fun() -> [#kura_field{name = id, type = id}] end),
    meck:expect(schema_b, table, fun() -> <<"b">> end),
    meck:expect(schema_b, fields, fun() ->
        [#kura_field{name = id, type = id}, #kura_field{name = x, type = string}]
    end),
    State = kura_schema_diff:build_desired_state([schema_a, schema_b]),
    ?assertEqual(2, maps:size(State)),
    ?assertEqual(1, length(maps:get(<<"a">>, State))),
    ?assertEqual(2, length(maps:get(<<"b">>, State))),
    meck:unload([schema_a, schema_b]).

build_desired_state_all_virtual_test() ->
    meck:new(schema_virt, [non_strict]),
    meck:expect(schema_virt, table, fun() -> <<"v">> end),
    meck:expect(schema_virt, fields, fun() ->
        [#kura_field{name = x, type = string, virtual = true},
         #kura_field{name = y, type = integer, virtual = true}]
    end),
    State = kura_schema_diff:build_desired_state([schema_virt]),
    ?assertEqual([], maps:get(<<"v">>, State)),
    meck:unload(schema_virt).

build_desired_state_preserves_defaults_test() ->
    meck:new(schema_def, [non_strict]),
    meck:expect(schema_def, table, fun() -> <<"t">> end),
    meck:expect(schema_def, fields, fun() ->
        [#kura_field{name = active, type = boolean, default = true}]
    end),
    State = kura_schema_diff:build_desired_state([schema_def]),
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
    Cols = [#kura_column{name = id, type = id, primary_key = true, nullable = false},
            #kura_column{name = email, type = string, nullable = false}],
    {[{create_table, <<"users">>, CreatedCols}], _} = kura_schema_diff:diff(#{}, #{<<"users">> => Cols}),
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
    DesiredCols = [#kura_column{name = id, type = id},
                   #kura_column{name = age, type = integer},
                   #kura_column{name = name, type = string}],
    {[{alter_table, <<"t">>, Ops}], _} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
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
    {[{alter_table, <<"t">>, Ops}], _} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assertEqual(2, length(Ops)),
    ?assert(lists:member({modify_column, a, float}, Ops)),
    ?assert(lists:member({modify_column, b, text}, Ops)).

diff_combined_add_drop_modify_test() ->
    DbCols = [#kura_column{name = id, type = id},
              #kura_column{name = old, type = string},
              #kura_column{name = age, type = integer}],
    DesiredCols = [#kura_column{name = id, type = id},
                   #kura_column{name = new, type = boolean},
                   #kura_column{name = age, type = float}],
    {[{alter_table, <<"t">>, Ops}], [_]} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
    ?assert(lists:any(fun({add_column, #kura_column{name = new}}) -> true; (_) -> false end, Ops)),
    ?assert(lists:member({drop_column, old}, Ops)),
    ?assert(lists:member({modify_column, age, float}, Ops)).

diff_multiple_changes_test() ->
    DbCols = [#kura_column{name = id, type = id}, #kura_column{name = old, type = string}],
    DesiredCols = [#kura_column{name = id, type = id}, #kura_column{name = new, type = integer}],
    {Up, _Down} = kura_schema_diff:diff(#{<<"t">> => DbCols}, #{<<"t">> => DesiredCols}),
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
    HasCreate = lists:any(fun({create_table, <<"b">>, _}) -> true; (_) -> false end, Up),
    HasAlter = lists:any(fun({alter_table, <<"a">>, _}) -> true; (_) -> false end, Up),
    ?assert(HasCreate),
    ?assert(HasAlter),
    ?assertEqual(2, length(Down)).

diff_does_not_drop_tables_test() ->
    %% Tables in DB but not in desired should NOT generate drop_table
    Db = #{<<"old">> => [#kura_column{name = id, type = id}],
           <<"keep">> => [#kura_column{name = id, type = id}]},
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

diff_down_ops_are_reversible_test() ->
    %% Applying up then down should return to original state
    DbCols = [#kura_column{name = id, type = id}],
    DesiredCols = [#kura_column{name = id, type = id},
                   #kura_column{name = x, type = string},
                   #kura_column{name = y, type = integer}],
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

field_to_column_preserves_default_test() ->
    Field = #kura_field{name = active, type = boolean, default = true},
    Col = kura_schema_diff:field_to_column(Field),
    ?assertEqual(true, Col#kura_column.default).

field_to_column_preserves_nullable_test() ->
    Field = #kura_field{name = required, type = string, nullable = false},
    Col = kura_schema_diff:field_to_column(Field),
    ?assertEqual(false, Col#kura_column.nullable).

field_to_column_all_types_test() ->
    Types = [id, integer, float, string, text, boolean, date, utc_datetime, uuid, jsonb, {array, string}],
    lists:foreach(fun(T) ->
        Col = kura_schema_diff:field_to_column(#kura_field{name = x, type = T}),
        ?assertEqual(T, Col#kura_column.type)
    end, Types).

%%====================================================================
%% Integration: build_db_state + build_desired_state + diff
%%====================================================================

full_roundtrip_new_schema_test() ->
    %% No migrations, one schema → create_table
    meck:new(rt_schema, [non_strict]),
    meck:expect(rt_schema, table, fun() -> <<"items">> end),
    meck:expect(rt_schema, fields, fun() ->
        [#kura_field{name = id, type = id, primary_key = true, nullable = false},
         #kura_field{name = title, type = string, nullable = false}]
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
        [{create_table, <<"items">>, [
            #kura_column{name = id, type = id, primary_key = true, nullable = false},
            #kura_column{name = title, type = string, nullable = false}
        ]}]
    end),
    meck:expect(rt_schema2, table, fun() -> <<"items">> end),
    meck:expect(rt_schema2, fields, fun() ->
        [#kura_field{name = id, type = id, primary_key = true, nullable = false},
         #kura_field{name = title, type = string, nullable = false}]
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
        [{create_table, <<"items">>, [
            #kura_column{name = id, type = id}
        ]}]
    end),
    meck:expect(rt_schema3, table, fun() -> <<"items">> end),
    meck:expect(rt_schema3, fields, fun() ->
        [#kura_field{name = id, type = id},
         #kura_field{name = desc, type = text}]
    end),
    DbState = kura_schema_diff:build_db_state([rt_mig2]),
    DesiredState = kura_schema_diff:build_desired_state([rt_schema3]),
    {Up, Down} = kura_schema_diff:diff(DbState, DesiredState),
    ?assertMatch([{alter_table, <<"items">>, [{add_column, #kura_column{name = desc, type = text}}]}], Up),
    ?assertMatch([{alter_table, <<"items">>, [{drop_column, desc}]}], Down),
    meck:unload([rt_mig2, rt_schema3]).

%%====================================================================
%% Helpers
%%====================================================================

%% Re-implement apply_ops externally for testing reversibility
apply_ops_ext([], State) -> State;
apply_ops_ext([{create_table, Name, Cols} | Rest], State) ->
    apply_ops_ext(Rest, State#{Name => Cols});
apply_ops_ext([{drop_table, Name} | Rest], State) ->
    apply_ops_ext(Rest, maps:remove(Name, State));
apply_ops_ext([{alter_table, Name, AlterOps} | Rest], State) ->
    Cols = maps:get(Name, State, []),
    NewCols = apply_alter_ops_ext(AlterOps, Cols),
    apply_ops_ext(Rest, State#{Name => NewCols}).

apply_alter_ops_ext([], Cols) -> Cols;
apply_alter_ops_ext([{add_column, Col} | Rest], Cols) ->
    apply_alter_ops_ext(Rest, Cols ++ [Col]);
apply_alter_ops_ext([{drop_column, Name} | Rest], Cols) ->
    apply_alter_ops_ext(Rest, [C || C <- Cols, C#kura_column.name =/= Name]);
apply_alter_ops_ext([{modify_column, Name, Type} | Rest], Cols) ->
    NewCols = [case C#kura_column.name of
        Name -> C#kura_column{type = Type};
        _ -> C
    end || C <- Cols],
    apply_alter_ops_ext(Rest, NewCols).
