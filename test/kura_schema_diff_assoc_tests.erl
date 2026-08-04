-module(kura_schema_diff_assoc_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

%%====================================================================
%% Foreign keys generated from associations
%%====================================================================

mock_parent(Mod) ->
    meck:new(Mod, [non_strict]),
    meck:expect(Mod, table, fun() -> <<"users">> end),
    meck:expect(Mod, fields, fun() ->
        [#kura_field{name = id, type = id, primary_key = true}]
    end).

mock_child(Mod, Assocs) ->
    meck:new(Mod, [non_strict]),
    meck:expect(Mod, table, fun() -> <<"posts">> end),
    meck:expect(Mod, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = user_id, type = integer}
        ]
    end),
    meck:expect(Mod, associations, fun() -> Assocs end).

fk_column(ChildMod) ->
    Cols = maps:get(
        <<"posts">>, maps:get(columns, kura_schema_diff:build_desired_state([ChildMod]))
    ),
    [Col] = [C || C <- Cols, C#kura_column.name =:= user_id],
    Col.

assoc_without_on_delete_stays_no_action_test() ->
    mock_parent(fk_p1),
    mock_child(fk_c1, [
        #kura_assoc{name = user, type = belongs_to, schema = fk_p1, foreign_key = user_id}
    ]),
    Col = fk_column(fk_c1),
    ?assertEqual({<<"users">>, id}, Col#kura_column.references),
    ?assertEqual(no_action, Col#kura_column.on_delete),
    meck:unload([fk_p1, fk_c1]).

assoc_on_delete_cascade_reaches_column_test() ->
    mock_parent(fk_p2),
    mock_child(fk_c2, [
        #kura_assoc{
            name = user,
            type = belongs_to,
            schema = fk_p2,
            foreign_key = user_id,
            on_delete = cascade
        }
    ]),
    ?assertEqual(cascade, (fk_column(fk_c2))#kura_column.on_delete),
    meck:unload([fk_p2, fk_c2]).

assoc_on_delete_set_null_reaches_column_test() ->
    mock_parent(fk_p3),
    mock_child(fk_c3, [
        #kura_assoc{
            name = user,
            type = belongs_to,
            schema = fk_p3,
            foreign_key = user_id,
            on_delete = set_null
        }
    ]),
    ?assertEqual(set_null, (fk_column(fk_c3))#kura_column.on_delete),
    meck:unload([fk_p3, fk_c3]).

assoc_on_delete_restrict_reaches_column_test() ->
    mock_parent(fk_p4),
    mock_child(fk_c4, [
        #kura_assoc{
            name = user,
            type = belongs_to,
            schema = fk_p4,
            foreign_key = user_id,
            on_delete = restrict
        }
    ]),
    ?assertEqual(restrict, (fk_column(fk_c4))#kura_column.on_delete),
    meck:unload([fk_p4, fk_c4]).

%% A ref-style association carries its target in #kura_ref{} and leaves
%% `schema` undefined. The old resolver called undefined:table/0 and let the
%% blanket catch drop the constraint.
assoc_ref_target_resolves_test() ->
    mock_parent(fk_p5),
    mock_child(fk_c5, [
        #kura_assoc{
            name = user,
            type = belongs_to,
            ref = #kura_ref{fields = [user_id], target = fk_p5},
            on_delete = cascade
        }
    ]),
    Col = fk_column(fk_c5),
    ?assertEqual({<<"users">>, id}, Col#kura_column.references),
    ?assertEqual(cascade, Col#kura_column.on_delete),
    meck:unload([fk_p5, fk_c5]).

%% has_many owns no column, so it must not silently contribute one either.
has_many_leaves_columns_alone_test() ->
    mock_child(fk_c6, [
        #kura_assoc{name = comments, type = has_many, schema = fk_missing6, foreign_key = post_id}
    ]),
    ?assertEqual(undefined, (fk_column(fk_c6))#kura_column.references),
    meck:unload(fk_c6).

%%====================================================================
%% Unresolvable targets abort by name instead of dropping the constraint
%%====================================================================

assoc_target_undeclared_aborts_test() ->
    mock_child(fk_c7, [
        #kura_assoc{name = user, type = belongs_to, foreign_key = user_id}
    ]),
    ?assertError(
        {kura_schema_diff, {assoc_target_undeclared, fk_c7, user}},
        kura_schema_diff:build_desired_state([fk_c7])
    ),
    meck:unload(fk_c7).

assoc_target_not_loadable_aborts_test() ->
    mock_child(fk_c8, [
        #kura_assoc{
            name = user, type = belongs_to, schema = fk_no_such_module, foreign_key = user_id
        }
    ]),
    ?assertError(
        {kura_schema_diff, {assoc_target_not_loadable, fk_c8, user, fk_no_such_module, nofile}},
        kura_schema_diff:build_desired_state([fk_c8])
    ),
    meck:unload(fk_c8).

assoc_target_not_a_schema_aborts_test() ->
    meck:new(fk_p9, [non_strict]),
    meck:expect(fk_p9, table, fun() -> <<"users">> end),
    mock_child(fk_c9, [
        #kura_assoc{name = user, type = belongs_to, schema = fk_p9, foreign_key = user_id}
    ]),
    ?assertError(
        {kura_schema_diff, {assoc_target_not_a_schema, fk_c9, user, fk_p9}},
        kura_schema_diff:build_desired_state([fk_c9])
    ),
    meck:unload([fk_p9, fk_c9]).

assoc_key_not_a_field_aborts_test() ->
    mock_parent(fk_p10),
    mock_child(fk_c10, [
        #kura_assoc{name = user, type = belongs_to, schema = fk_p10, foreign_key = owner_id}
    ]),
    ?assertError(
        {kura_schema_diff, {assoc_key_not_a_field, fk_c10, user, owner_id}},
        kura_schema_diff:build_desired_state([fk_c10])
    ),
    meck:unload([fk_p10, fk_c10]).

composite_assoc_aborts_test() ->
    mock_parent(fk_p11),
    mock_child(fk_c11, [
        #kura_assoc{
            name = user,
            type = belongs_to,
            ref = #kura_ref{fields = [tenant_id, user_id], target = fk_p11}
        }
    ]),
    ?assertError(
        {kura_schema_diff, {composite_assoc_unsupported, fk_c11, user}},
        kura_schema_diff:build_desired_state([fk_c11])
    ),
    meck:unload([fk_p11, fk_c11]).

invalid_on_delete_aborts_test() ->
    mock_parent(fk_p12),
    mock_child(fk_c12, [
        #kura_assoc{
            name = user,
            type = belongs_to,
            schema = fk_p12,
            foreign_key = user_id,
            on_delete = cascde
        }
    ]),
    ?assertError(
        {kura_schema_diff, {invalid_on_delete, fk_c12, user, cascde}},
        kura_schema_diff:build_desired_state([fk_c12])
    ),
    meck:unload([fk_p12, fk_c12]).

on_delete_on_has_many_aborts_test() ->
    mock_child(fk_c13, [
        #kura_assoc{
            name = comments,
            type = has_many,
            schema = fk_missing13,
            foreign_key = post_id,
            on_delete = cascade
        }
    ]),
    ?assertError(
        {kura_schema_diff, {on_delete_not_owned, fk_c13, comments, has_many}},
        kura_schema_diff:build_desired_state([fk_c13])
    ),
    meck:unload(fk_c13).

%%====================================================================
%% format_error/1
%%====================================================================

format_error_names_the_schema_and_assoc_test() ->
    Msg = lists:flatten(
        kura_schema_diff:format_error({assoc_target_not_loadable, my_post, user, my_user, nofile})
    ),
    ?assert(string:find(Msg, "my_post") =/= nomatch),
    ?assert(string:find(Msg, "user") =/= nomatch),
    ?assert(string:find(Msg, "my_user") =/= nomatch).

format_error_falls_back_to_term_test() ->
    ?assertEqual("{odd,thing}", lists:flatten(kura_schema_diff:format_error({odd, thing}))).
