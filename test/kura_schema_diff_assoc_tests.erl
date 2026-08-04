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

%% A composite foreign key is refused for its own schema only. Aborting the
%% run made every other table in the application uncheckable for as long as
%% the unsupported schema existed, which is forever.
composite_assoc_is_scoped_to_its_own_schema_test() ->
    mock_parent(fk_p11),
    mock_child(fk_c11, [
        #kura_assoc{
            name = user,
            type = belongs_to,
            ref = #kura_ref{fields = [tenant_id, user_id], target = fk_p11}
        }
    ]),
    meck:new(fk_other11, [non_strict]),
    meck:expect(fk_other11, table, fun() -> ~"widgets" end),
    meck:expect(fk_other11, fields, fun() ->
        [#kura_field{name = id, type = id, primary_key = true}]
    end),

    State = kura_schema_diff:build_desired_state([fk_c11, fk_other11]),

    %% The unrelated schema is still there to be diffed.
    ?assert(maps:is_key(~"widgets", maps:get(columns, State))),
    ?assertNot(maps:is_key(~"posts", maps:get(columns, State))),
    ?assertEqual(
        [{fk_c11, {composite_assoc_unsupported, fk_c11, user}}],
        kura_schema_diff:unsupported_schemas(State)
    ),
    meck:unload([fk_p11, fk_c11, fk_other11]).

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

%% Neither foreign_key nor ref fields: every other malformation is named, so
%% this one must be too instead of dropping the association on the floor.
assoc_key_undeclared_aborts_test() ->
    mock_parent(fk_p14),
    mock_child(fk_c14, [
        #kura_assoc{name = user, type = belongs_to, schema = fk_p14}
    ]),
    ?assertError(
        {kura_schema_diff, {assoc_key_undeclared, fk_c14, user}},
        kura_schema_diff:build_desired_state([fk_c14])
    ),
    meck:unload([fk_p14, fk_c14]).

%% A raising associations/0 gave a raw rebar3 stacktrace with no schema name.
associations_raising_is_named_test() ->
    meck:new(fk_c15, [non_strict]),
    meck:expect(fk_c15, table, fun() -> ~"posts" end),
    meck:expect(fk_c15, fields, fun() -> [#kura_field{name = id, type = id}] end),
    meck:expect(fk_c15, associations, fun() -> error(badarg) end),
    ?assertError(
        {kura_schema_diff, {associations_failed, fk_c15, error, badarg}},
        kura_schema_diff:build_desired_state([fk_c15])
    ),
    meck:unload(fk_c15).

%%====================================================================
%% Parent key resolution goes through kura_schema:key/1
%%====================================================================

%% key/0 is the schema's declared key. Rederiving it from primary_key = true
%% fields ignored the callback and generated REFERENCES "users"("id") against
%% a parent whose key is not id at all.
target_key_callback_is_honoured_test() ->
    meck:new(fk_p16, [non_strict]),
    meck:expect(fk_p16, table, fun() -> ~"users" end),
    meck:expect(fk_p16, key, fun() -> [uuid] end),
    meck:expect(fk_p16, fields, fun() -> [#kura_field{name = uuid, type = uuid}] end),
    mock_child(fk_c16, [
        #kura_assoc{name = user, type = belongs_to, schema = fk_p16, foreign_key = user_id}
    ]),
    ?assertEqual({~"users", uuid}, (fk_column(fk_c16))#kura_column.references),
    meck:unload([fk_p16, fk_c16]).

%% [PK | _] silently took the first column of a composite key and emitted a
%% foreign key pointing at one half of it.
composite_target_key_is_named_not_truncated_test() ->
    meck:new(fk_p17, [non_strict]),
    meck:expect(fk_p17, table, fun() -> ~"memberships" end),
    meck:expect(fk_p17, key, fun() -> [org_id, user_id] end),
    meck:expect(fk_p17, fields, fun() ->
        [
            #kura_field{name = org_id, type = integer},
            #kura_field{name = user_id, type = integer}
        ]
    end),
    mock_child(fk_c17, [
        #kura_assoc{name = member, type = belongs_to, schema = fk_p17, foreign_key = user_id}
    ]),
    State = kura_schema_diff:build_desired_state([fk_c17]),
    ?assertEqual(
        [{fk_c17, {composite_target_key, fk_c17, member, fk_p17, [org_id, user_id]}}],
        kura_schema_diff:unsupported_schemas(State)
    ),
    ?assertNot(maps:is_key(~"posts", maps:get(columns, State))),
    meck:unload([fk_p17, fk_c17]).

%% A parent with no key at all used to default to `id`, generating a
%% constraint against a column that does not exist.
target_without_key_is_named_test() ->
    meck:new(fk_p18, [non_strict]),
    meck:expect(fk_p18, table, fun() -> ~"users" end),
    meck:expect(fk_p18, fields, fun() -> [#kura_field{name = name, type = string}] end),
    mock_child(fk_c18, [
        #kura_assoc{name = user, type = belongs_to, schema = fk_p18, foreign_key = user_id}
    ]),
    ?assertError(
        {kura_schema_diff, {assoc_target_no_key, fk_c18, user, fk_p18}},
        kura_schema_diff:build_desired_state([fk_c18])
    ),
    meck:unload([fk_p18, fk_c18]).

%% An explicit ref target_key wins over the parent's own key.
ref_target_key_is_honoured_test() ->
    meck:new(fk_p19, [non_strict]),
    meck:expect(fk_p19, table, fun() -> ~"users" end),
    meck:expect(fk_p19, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = slug, type = string}
        ]
    end),
    mock_child(fk_c19, [
        #kura_assoc{
            name = user,
            type = belongs_to,
            ref = #kura_ref{fields = [user_id], target = fk_p19, target_key = [slug]}
        }
    ]),
    ?assertEqual({~"users", slug}, (fk_column(fk_c19))#kura_column.references),
    meck:unload([fk_p19, fk_c19]).

%%====================================================================
%% format_error/1
%%====================================================================

%% has_one/has_many do have a belongs_to on the other side. many_to_many does
%% not, and its foreign keys live on a join table the generator never emits,
%% so the two cannot share a message.
format_error_many_to_many_does_not_say_belongs_to_test() ->
    M2M = lists:flatten(
        kura_schema_diff:format_error({on_delete_not_owned, my_post, tags, many_to_many})
    ),
    HasMany = lists:flatten(
        kura_schema_diff:format_error({on_delete_not_owned, my_post, comments, has_many})
    ),
    ?assertEqual(nomatch, string:find(M2M, "belongs_to")),
    ?assert(string:find(M2M, "join table") =/= nomatch),
    ?assert(string:find(HasMany, "belongs_to") =/= nomatch).

format_error_names_the_schema_and_assoc_test() ->
    Msg = lists:flatten(
        kura_schema_diff:format_error({assoc_target_not_loadable, my_post, user, my_user, nofile})
    ),
    ?assert(string:find(Msg, "my_post") =/= nomatch),
    ?assert(string:find(Msg, "user") =/= nomatch),
    ?assert(string:find(Msg, "my_user") =/= nomatch).

format_error_falls_back_to_term_test() ->
    ?assertEqual("{odd,thing}", lists:flatten(kura_schema_diff:format_error({odd, thing}))).

%%====================================================================
%% Editing an association on a table that already exists
%%====================================================================

add_fk(RefTable, OnDeleteClause) ->
    Head = ~"ALTER TABLE \"posts\" ADD CONSTRAINT \"posts_user_id_fkey\" ",
    Key = ~"FOREIGN KEY (\"user_id\") REFERENCES ",
    iolist_to_binary([Head, Key, ~"\"", RefTable, ~"\" (\"id\")", OnDeleteClause]).

drop_fk() ->
    ~"ALTER TABLE \"posts\" DROP CONSTRAINT \"posts_user_id_fkey\"".

fk_col(Refs, OnDelete) ->
    [
        #kura_column{name = id, type = id, primary_key = true},
        #kura_column{name = user_id, type = integer, references = Refs, on_delete = OnDelete}
    ].

posts_state(Cols) ->
    #{columns => #{~"posts" => Cols}, indexes => #{}}.

%% create_table works and add_column of a new FK column works, but editing an
%% association on a table that already exists produced no migration at all.
fk_added_to_existing_table_test() ->
    Db = posts_state(fk_col(undefined, undefined)),
    Desired = posts_state(fk_col({~"users", id}, cascade)),
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    ?assertEqual([{execute, add_fk(~"users", ~" ON DELETE CASCADE")}], Up),
    ?assertEqual([{execute, drop_fk()}], Down).

%% No dialect kura targets can change a referential action in place, so the
%% constraint is dropped and re-added.
fk_on_delete_change_drops_then_adds_test() ->
    Db = posts_state(fk_col({~"users", id}, no_action)),
    Desired = posts_state(fk_col({~"users", id}, cascade)),
    {Up, _Down} = kura_schema_diff:diff(Db, Desired),
    ?assertEqual(
        [
            {execute, drop_fk()},
            {execute, add_fk(~"users", ~" ON DELETE CASCADE")}
        ],
        Up
    ).

fk_target_change_drops_then_adds_test() ->
    Db = posts_state(fk_col({~"users", id}, no_action)),
    Desired = posts_state(fk_col({~"accounts", id}, no_action)),
    {Up, _Down} = kura_schema_diff:diff(Db, Desired),
    ?assertEqual(
        [
            {execute, drop_fk()},
            {execute, add_fk(~"accounts", ~" ON DELETE NO ACTION")}
        ],
        Up
    ).

fk_removed_emits_drop_test() ->
    Db = posts_state(fk_col({~"users", id}, cascade)),
    Desired = posts_state(fk_col(undefined, undefined)),
    {Up, Down} = kura_schema_diff:diff(Db, Desired),
    ?assertEqual([{execute, drop_fk()}], Up),
    ?assertEqual([{execute, add_fk(~"users", ~" ON DELETE CASCADE")}], Down).

fk_unchanged_emits_nothing_test() ->
    Db = posts_state(fk_col({~"users", id}, cascade)),
    ?assertEqual({[], []}, kura_schema_diff:diff(Db, Db)).

%% The generated SQL has to replay back into the column state, or the next run
%% diffs the same change again and the drift never settles.
fk_change_settles_after_replay_test() ->
    Db = posts_state(fk_col({~"users", id}, no_action)),
    Desired = posts_state(fk_col({~"users", id}, cascade)),
    {Up, _Down} = kura_schema_diff:diff(Db, Desired),
    ?assertNotEqual([], Up),

    meck:new(m20260804000000_fk, [non_strict]),
    meck:expect(m20260804000000_fk, up, fun() ->
        [{create_table, ~"posts", fk_col({~"users", id}, no_action)} | Up]
    end),
    Replayed = kura_schema_diff:build_db_state([m20260804000000_fk]),

    ?assertEqual({[], []}, kura_schema_diff:diff(Replayed, Desired)),
    meck:unload(m20260804000000_fk).

fk_drop_settles_after_replay_test() ->
    Db = posts_state(fk_col({~"users", id}, cascade)),
    Desired = posts_state(fk_col(undefined, undefined)),
    {Up, _Down} = kura_schema_diff:diff(Db, Desired),
    ?assertNotEqual([], Up),

    meck:new(m20260804000001_fk, [non_strict]),
    meck:expect(m20260804000001_fk, up, fun() ->
        [{create_table, ~"posts", fk_col({~"users", id}, cascade)} | Up]
    end),
    Replayed = kura_schema_diff:build_db_state([m20260804000001_fk]),

    ?assertEqual({[], []}, kura_schema_diff:diff(Replayed, Desired)),
    meck:unload(m20260804000001_fk).

%% ON UPDATE can only come from a hand-written migration. Regenerating the
%% constraint from the schema would drop it, so the diff refuses instead.
fk_on_update_refuses_test() ->
    DbCols = [
        #kura_column{name = id, type = id, primary_key = true},
        #kura_column{
            name = user_id,
            type = integer,
            references = {~"users", id},
            on_delete = no_action,
            on_update = cascade
        }
    ],
    Db = posts_state(DbCols),
    Desired = posts_state(fk_col({~"users", id}, cascade)),
    ?assertError(
        {kura_schema_diff, {fk_on_update_not_owned, ~"posts", user_id, cascade}},
        kura_schema_diff:diff(Db, Desired)
    ).
