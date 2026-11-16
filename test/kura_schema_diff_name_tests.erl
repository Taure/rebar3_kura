-module(kura_schema_diff_name_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

%%====================================================================
%% A column named by a binary
%%====================================================================

names_test_() ->
    {foreach, fun() -> ok end, fun(_) -> meck:unload() end, [
        fun column_name_is_never_made_an_atom/0,
        fun multi_word_column_is_written_as_a_quoted_atom/0,
        fun generated_source_replays_to_no_diff/0,
        fun history_naming_a_column_both_ways_is_one_column/0,
        fun schema_names_win_in_the_operations/0,
        fun index_declared_by_atom_is_on_the_binary_column/0,
        fun statement_about_a_binary_column_is_replayed/0,
        fun reference_to_an_undeclared_table_compares_as_text/0,
        fun columns_are_ordered_by_their_text/0
    ]}.

column_name_is_never_made_an_atom() ->
    Name = never_an_atom(~"column"),
    Target = never_an_atom(~"key"),
    schema(name_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true, column = Target}
    ]),
    schema(name_posts, ~"posts", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = author_id, type = integer, column = Name}
    ]),
    meck:expect(name_posts, indexes, fun() -> [{[Name], #{unique => true}}] end),
    meck:expect(name_posts, associations, fun() ->
        [
            #kura_assoc{
                name = author, type = belongs_to, schema = name_users, foreign_key = author_id
            }
        ]
    end),
    Desired = kura_schema_diff:build_desired_state([name_users, name_posts]),
    {Up, Down} = kura_schema_diff:diff(#{}, Desired),
    Replayed = replayed([Up]),
    ?assertEqual({[], []}, kura_schema_diff:diff(Replayed, Desired)),
    meck:expect(name_posts, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = author_id, type = string, column = Name, nullable = false}
        ]
    end),
    meck:expect(name_posts, associations, fun() -> [] end),
    Changed = kura_schema_diff:build_desired_state([name_users, name_posts]),
    {ChangedUp, ChangedDown} = kura_schema_diff:diff(Replayed, Changed),
    ?assertMatch(
        [{alter_table, ~"posts", [{modify_column, Name, string}]}, {execute, _}, {execute, _}],
        ChangedUp
    ),
    ?assertEqual({[], []}, kura_schema_diff:diff(replayed([Up, ChangedUp]), Changed)),
    ?assertEqual({[], []}, kura_schema_diff:diff(replayed([Up, ChangedUp, ChangedDown]), Desired)),
    Source = iolist_to_binary([
        rebar3_kura_compile:render_migration("m_never", Up, Down, []),
        rebar3_kura_compile:render_migration("m_never_changed", ChangedUp, ChangedDown, []),
        [kura_schema_diff:format_error(Reason) || Reason <- [{fk_not_owned, ~"posts", Name}]]
    ]),
    ?assertNotEqual(nomatch, binary:match(Source, iolist_to_binary([~"name = '", Name, ~"'"]))),
    ?assertError(badarg, binary_to_existing_atom(Name)),
    ?assertError(badarg, binary_to_existing_atom(Target)).

multi_word_column_is_written_as_a_quoted_atom() ->
    schema(name_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = handle, type = string, column = ~"user handle"},
        #kura_field{name = motto, type = string, column = ~"it's \"mine\""},
        #kura_field{name = finish, type = string, column = ~"end"},
        #kura_field{name = email, type = string, column = ~"email"}
    ]),
    meck:expect(name_users, indexes, fun() -> [{[~"user handle", ~"end"], #{}}] end),
    {Up, Down} = kura_schema_diff:diff(#{}, kura_schema_diff:build_desired_state([name_users])),
    Source = iolist_to_binary(rebar3_kura_compile:render_migration("m_words", Up, Down, [])),
    [
        ?assertNotEqual(nomatch, binary:match(Source, Written), Written)
     || Written <- [
            ~"name = id,",
            ~"name = 'user handle'",
            ~"name = 'it\\'s \"mine\"'",
            ~"name = 'end'",
            ~"name = 'email'",
            ~"{create_index, ~\"users\", ['user handle','end'], #{}}"
        ]
    ],
    Mod = loaded("m_words", Source),
    ?assertMatch(
        [
            {create_table, ~"users", [
                #kura_column{name = id},
                #kura_column{name = 'user handle'},
                #kura_column{name = 'it\'s "mine"'},
                #kura_column{name = 'end'},
                #kura_column{name = email}
            ]},
            {create_index, ~"users", ['user handle', 'end'], #{}}
        ],
        Mod:up()
    ).

generated_source_replays_to_no_diff() ->
    schema(name_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true, column = ~"user id"},
        #kura_field{name = handle, type = string, column = ~"user handle", nullable = false}
    ]),
    meck:expect(name_users, indexes, fun() -> [{[~"user handle"], #{unique => true}}] end),
    schema(name_members, ~"team members", [
        #kura_field{name = team, type = integer, column = ~"team id"},
        #kura_field{name = user, type = integer, column = ~"user id"}
    ]),
    meck:expect(name_members, key, fun() -> [team, user] end),
    meck:expect(name_members, associations, fun() ->
        [
            #kura_assoc{
                name = member,
                type = belongs_to,
                schema = name_users,
                foreign_key = user,
                on_delete = cascade
            }
        ]
    end),
    Desired = kura_schema_diff:build_desired_state([name_users, name_members]),
    {Up, Down} = kura_schema_diff:diff(#{}, Desired),
    Mod = loaded(
        "m20260101000000_generated_names", render("m20260101000000_generated_names", Up, Down)
    ),
    FromSource = kura_schema_diff:build_db_state([Mod]),
    ?assertEqual({[], []}, kura_schema_diff:diff(FromSource, Desired)),
    meck:expect(name_members, associations, fun() ->
        [#kura_assoc{name = member, type = belongs_to, schema = name_users, foreign_key = user}]
    end),
    meck:expect(name_users, indexes, fun() -> [] end),
    Changed = kura_schema_diff:build_desired_state([name_users, name_members]),
    {ChangedUp, ChangedDown} = kura_schema_diff:diff(FromSource, Changed),
    ?assertEqual(
        [
            {drop_index, ~"users_user handle_index"},
            {execute,
                ~"ALTER TABLE \"team members\" DROP CONSTRAINT \"team members_user id_fkey\""},
            {execute,
                ~"ALTER TABLE \"team members\" ADD CONSTRAINT \"team members_user id_fkey\" FOREIGN KEY (\"user id\") REFERENCES \"users\" (\"user id\") ON DELETE NO ACTION"}
        ],
        ChangedUp
    ),
    Next = loaded(
        "m20260102000000_changed_names",
        render("m20260102000000_changed_names", ChangedUp, ChangedDown)
    ),
    ?assertEqual(
        {[], []}, kura_schema_diff:diff(kura_schema_diff:build_db_state([Mod, Next]), Changed)
    ).

%% A hand-written history may name a column by an atom in one operation and
%% by a binary in the next. Replay keeps the name it was created under.
history_naming_a_column_both_ways_is_one_column() ->
    Replayed = replayed([
        [
            {create_table, ~"users", [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = ~"email", type = string},
                #kura_column{name = gone, type = string},
                #kura_column{name = team, type = integer}
            ]},
            {create_table, ~"teams", [
                #kura_column{name = ~"id", type = id, primary_key = true},
                #kura_column{
                    name = ~"lead",
                    type = integer,
                    references = {~"users", ~"id"},
                    on_delete = no_action
                }
            ]}
        ],
        [
            {alter_table, ~"users", [
                {rename_column, email, ~"email_address"},
                {modify_column, email_address, text},
                {drop_column, ~"gone"},
                {add_column, #kura_column{
                    name = ~"owner",
                    type = integer,
                    references = {~"users", ~"id"},
                    on_delete = no_action
                }}
            ]},
            {create_index, ~"users", [email_address], #{unique => true}},
            {create_index, ~"users_team", ~"users", [~"team"], []},
            {execute, ~"ALTER TABLE users ALTER COLUMN email_address SET NOT NULL"},
            {execute,
                ~"ALTER TABLE users ADD CONSTRAINT users_team_fkey FOREIGN KEY (team) REFERENCES teams (id) ON DELETE NO ACTION"},
            {alter_table, ~"teams", [{rename_column, id, team_id}]}
        ]
    ]),
    ?assertEqual(
        [
            #kura_column{name = id, type = id, primary_key = true},
            #kura_column{name = ~"email_address", type = text, nullable = false},
            #kura_column{
                name = team,
                type = integer,
                references = {~"teams", team_id},
                on_delete = no_action
            },
            #kura_column{
                name = ~"owner",
                type = integer,
                references = {~"users", id},
                on_delete = no_action
            }
        ],
        maps:get(~"users", maps:get(columns, Replayed))
    ),
    ?assertEqual(
        [{[~"email_address"], #{unique => true}}, {[team], #{name => ~"users_team"}}],
        maps:get(~"users", maps:get(indexes, Replayed))
    ),
    ?assertEqual([], kura_schema_diff:unreplayed_executes(Replayed)),
    schema(name_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = email, type = text, column = ~"email_address", nullable = false},
        #kura_field{name = team, type = integer},
        #kura_field{name = owner, type = integer}
    ]),
    meck:expect(name_users, indexes, fun() ->
        [{[email_address], #{unique => true}}, {[team], #{name => ~"users_team"}}]
    end),
    schema(name_teams, ~"teams", [
        #kura_field{name = id, type = id, primary_key = true, column = ~"team_id"},
        #kura_field{name = lead, type = integer}
    ]),
    Assoc = fun(Name, Schema, Key) ->
        #kura_assoc{name = Name, type = belongs_to, schema = Schema, foreign_key = Key}
    end,
    meck:expect(name_users, associations, fun() ->
        [Assoc(squad, name_teams, team), Assoc(boss, name_users, owner)]
    end),
    meck:expect(name_teams, associations, fun() -> [Assoc(leader, name_users, lead)] end),
    ?assertEqual(
        {[], []},
        kura_schema_diff:diff(
            Replayed, kura_schema_diff:build_desired_state([name_users, name_teams])
        )
    ).

%% What the history calls `email_address` the schema calls ~"email_address".
%% An operation on it uses the schema's name, one that drops it the history's.
schema_names_win_in_the_operations() ->
    Replayed = replayed([
        [
            {create_table, ~"users", [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = email_address, type = string},
                #kura_column{name = legacy_name, type = string}
            ]},
            {create_index, ~"users", [email_address, legacy_name], #{}}
        ]
    ]),
    schema(name_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = email, type = text, column = ~"email_address", default = ~"none"}
    ]),
    ?assertEqual(
        {
            [
                {drop_index, ~"users_email_address_legacy_name_index"},
                {alter_table, ~"users", [
                    {drop_column, legacy_name}, {modify_column, ~"email_address", text}
                ]},
                {execute,
                    ~"ALTER TABLE \"users\" ALTER COLUMN \"email_address\" SET DEFAULT 'none'"}
            ],
            [
                {execute, ~"ALTER TABLE \"users\" ALTER COLUMN \"email_address\" DROP DEFAULT"},
                {alter_table, ~"users", [
                    {modify_column, ~"email_address", string},
                    {add_column, #kura_column{name = legacy_name, type = string}}
                ]},
                {create_index, ~"users", [~"email_address", legacy_name], #{}}
            ]
        },
        kura_schema_diff:diff(Replayed, kura_schema_diff:build_desired_state([name_users]))
    ).

index_declared_by_atom_is_on_the_binary_column() ->
    schema(name_users, ~"users", [
        #kura_field{name = id, type = id, primary_key = true},
        #kura_field{name = email, type = string, column = ~"email_address"}
    ]),
    meck:expect(name_users, indexes, fun() -> [{[id, email_address], [unique]}] end),
    Desired = kura_schema_diff:build_desired_state([name_users]),
    ?assertEqual(
        [{[id, ~"email_address"], [unique]}], maps:get(~"users", maps:get(indexes, Desired))
    ),
    {Up, _Down} = kura_schema_diff:diff(#{}, Desired),
    ?assertMatch(
        [_, {create_index, ~"users", [id, ~"email_address"], #{unique := true}}], Up
    ),
    ?assertEqual({[], []}, kura_schema_diff:diff(replayed([Up]), Desired)),
    meck:expect(name_users, indexes, fun() -> [{[{email_address, desc}], #{}}] end),
    ?assertError(
        {kura_schema_diff, {index_unknown_column, name_users, {email_address, desc}}},
        kura_schema_diff:build_desired_state([name_users])
    ).

%% A statement names a column by its text, whether or not that is an atom.
statement_about_a_binary_column_is_replayed() ->
    Name = never_an_atom(~"statement"),
    Other = never_an_atom(~"other"),
    Statement = fun(Parts) -> {execute, iolist_to_binary(Parts)} end,
    Unknown = Statement([~"ALTER TABLE users ALTER COLUMN \"", Other, ~"\" SET NOT NULL"]),
    Replayed = replayed([
        [
            {create_table, ~"users", [
                #kura_column{name = id, type = id},
                #kura_column{name = Name, type = integer}
            ]},
            Statement([~"ALTER TABLE users ALTER COLUMN \"", Name, ~"\" SET NOT NULL"]),
            Statement([~"ALTER TABLE users ADD PRIMARY KEY (id, \"", Name, ~"\")"]),
            Statement([
                ~"ALTER TABLE users ADD FOREIGN KEY (\"",
                Name,
                ~"\") REFERENCES elsewhere (\"",
                Other,
                ~"\")"
            ]),
            Unknown
        ]
    ]),
    ?assertEqual(
        [
            #kura_column{name = id, type = id, primary_key = true},
            #kura_column{
                name = Name,
                type = integer,
                nullable = false,
                primary_key = true,
                references = {~"elsewhere", Other}
            }
        ],
        maps:get(~"users", maps:get(columns, Replayed))
    ),
    ?assertEqual([element(2, Unknown)], kura_schema_diff:unreplayed_executes(Replayed)),
    ?assertError(badarg, binary_to_existing_atom(Name)),
    ?assertError(badarg, binary_to_existing_atom(Other)).

%% A foreign key to a table no schema declares has no column to take its name
%% from, so an atom and a binary of one text are the same reference.
reference_to_an_undeclared_table_compares_as_text() ->
    Column = fun(Ref) ->
        #{~"posts" => [#kura_column{name = user_id, type = integer, references = {~"users", Ref}}]}
    end,
    ?assertEqual({[], []}, kura_schema_diff:diff(Column(id), Column(~"id"))),
    ?assertEqual({[], []}, kura_schema_diff:diff(Column(~"id"), Column(id))),
    ?assertMatch({[_, _], [_, _]}, kura_schema_diff:diff(Column(id), Column(~"uid"))).

%% Atoms sort before binaries as terms. The operations keep the order a
%% history of atoms gave them.
columns_are_ordered_by_their_text() ->
    Db = #{~"t" => [#kura_column{name = id, type = id}]},
    Desired = #{
        ~"t" => [
            #kura_column{name = id, type = id},
            #kura_column{name = zeta, type = string},
            #kura_column{name = ~"alpha", type = string},
            #kura_column{name = ~"mid", type = string},
            #kura_column{name = beta, type = string}
        ]
    },
    {[{alter_table, ~"t", Up}], [{alter_table, ~"t", Down}]} = kura_schema_diff:diff(Db, Desired),
    ?assertEqual([~"alpha", beta, ~"mid", zeta], [N || {add_column, #kura_column{name = N}} <- Up]),
    ?assertEqual([zeta, ~"mid", beta, ~"alpha"], [N || {drop_column, N} <- Down]).

%%====================================================================
%% Generated schemas
%%====================================================================

proper_test_() ->
    {timeout, 120, [
        ?_assert(
            proper:quickcheck(prop_generated_migration_replays_to_no_diff(), [
                {to_file, user}, {numtests, 60}
            ])
        )
    ]}.

column_text() ->
    oneof([
        ~"user handle",
        ~"E-Mail",
        ~"it's",
        ~"say \"hi\"",
        ~"back\\slash",
        ~"plain_name",
        ~"end",
        ~"Mixed Case",
        ~"x.y",
        ~"a",
        ~"naïve"
    ]).

field() ->
    {
        oneof([a, b, c, d, e, f]),
        oneof([integer, string, text, boolean]),
        oneof([undefined, column_text()]),
        boolean()
    }.

table() ->
    {
        oneof([undefined, column_text()]),
        list(field()),
        list({non_empty(list(range(1, 8))), boolean()})
    }.

prop_generated_migration_replays_to_no_diff() ->
    ?FORALL(
        {Parent, Child, Link, Composite},
        {table(), table(), oneof([undefined, column_text()]), boolean()},
        replays_to_no_diff(Parent, Child, Link, Composite)
    ).

-spec replays_to_no_diff(dynamic(), dynamic(), dynamic(), dynamic()) -> boolean().
replays_to_no_diff({ParentKey, ParentFields, ParentIndexes}, Child, Link, Composite) ->
    {ChildKey, ChildFields, ChildIndexes} = Child,
    Parents = fields([{id, id, ParentKey, false} | ParentFields]),
    Children = fields([{id, id, ChildKey, false}, {parent_id, integer, Link, false} | ChildFields]),
    schema(prop_parent, ~"parents", Parents),
    schema(prop_child, ~"child rows", Children),
    meck:expect(prop_parent, indexes, fun() -> indexes(ParentIndexes, Parents) end),
    meck:expect(prop_child, indexes, fun() -> indexes(ChildIndexes, Children) end),
    meck:expect(prop_child, associations, fun() ->
        [
            #kura_assoc{
                name = parent, type = belongs_to, schema = prop_parent, foreign_key = parent_id
            }
        ]
    end),
    [meck:expect(prop_child, key, fun() -> [id, parent_id] end) || Composite],
    try
        Desired = kura_schema_diff:build_desired_state([prop_parent, prop_child]),
        {Up, Down} = kura_schema_diff:diff(#{}, Desired),
        Name = "m20260101000000_prop_names",
        Mod = loaded(Name, render(Name, Up, Down)),
        FromSource = kura_schema_diff:build_db_state([Mod]),
        {[], []} =:= kura_schema_diff:diff(FromSource, Desired) andalso
            {[], []} =:= kura_schema_diff:diff(replayed([Up]), Desired) andalso
            {[], []} =:= kura_schema_diff:diff(replayed([Up, Down, Up]), Desired)
    after
        meck:unload()
    end.

%% One field per name and one column per text. A field whose column is
%% taken is stored under its own name.
-spec fields(dynamic()) -> [#kura_field{}].
fields(Generated) ->
    fields(Generated, [], []).

fields([], _Names, _Texts) ->
    [];
fields([{Name, Type, Column, NotNull} | Rest], Names, Texts) ->
    Text =
        case Column of
            undefined -> atom_to_binary(Name);
            _ -> Column
        end,
    case {lists:member(Name, Names), lists:member(Text, Texts)} of
        {false, true} when Column =/= undefined ->
            fields([{Name, Type, undefined, NotNull} | Rest], Names, Texts);
        {Named, Stored} when Named; Stored ->
            fields(Rest, Names, Texts);
        {false, false} ->
            Field = #kura_field{
                name = Name,
                type = Type,
                column = Column,
                primary_key = Name =:= id,
                nullable = not NotNull
            },
            [Field | fields(Rest, [Name | Names], [Text | Texts])]
    end.

-spec indexes(dynamic(), [#kura_field{}]) -> [{[atom() | binary()], map()}].
indexes(Generated, Fields) ->
    Columns = [(kura_schema_diff:field_to_column(F))#kura_column.name || F <- Fields],
    Picked =
        #{
            lists:usort([lists:nth(1 + N rem length(Columns), Columns) || N <- Picks]) => Unique
         || {Picks, Unique} <- Generated
        },
    [{Cols, #{unique => Unique}} || Cols := Unique <- Picked].

%%====================================================================
%% Helpers
%%====================================================================

never_an_atom(Prefix) ->
    iolist_to_binary([
        ~"kura_", Prefix, ~"_never_an_atom_", integer_to_binary(erlang:unique_integer([positive]))
    ]).

schema(Mod, Table, Fields) ->
    meck:new(Mod, [non_strict]),
    meck:expect(Mod, table, fun() -> Table end),
    meck:expect(Mod, fields, fun() -> Fields end).

replayed(History) ->
    Mods = [
        {
            list_to_atom(
                "m" ++ integer_to_list(20260101000000 + N) ++ "_names_" ++
                    integer_to_list(erlang:unique_integer([positive]))
            ),
            Ops
        }
     || {N, Ops} <- lists:enumerate(History)
    ],
    [
        begin
            meck:new(Mod, [non_strict]),
            meck:expect(Mod, up, fun() -> Ops end)
        end
     || {Mod, Ops} <- Mods
    ],
    try
        kura_schema_diff:build_db_state([Mod || {Mod, _} <- Mods])
    after
        [meck:unload(Mod) || {Mod, _} <- Mods]
    end.

render(Name, Up, Down) ->
    rebar3_kura_compile:format_source(
        iolist_to_binary(rebar3_kura_compile:render_migration(Name, Up, Down, [])), []
    ).

loaded(Name, Source) ->
    Dir = filename:join(
        filename:basedir(user_cache, "rebar3_kura_name_tests"),
        integer_to_list(erlang:unique_integer([positive]))
    ),
    File = filename:join(Dir, Name ++ ".erl"),
    ok = filelib:ensure_dir(File),
    ok = file:write_file(File, Source),
    try
        case compile:file(File, [binary, {i, filename:join(code:lib_dir(kura), "include")}]) of
            {ok, Mod, Bin} when is_atom(Mod), is_binary(Bin) ->
                code:purge(Mod),
                {module, Mod} = code:load_binary(Mod, File, Bin),
                Mod;
            Failed ->
                error({not_compiled, Name, Failed})
        end
    after
        file:del_dir_r(Dir)
    end.
