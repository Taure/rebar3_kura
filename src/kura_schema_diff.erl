-module(kura_schema_diff).

-include_lib("kura/include/kura.hrl").

-export([
    build_db_state/1,
    build_desired_state/1,
    diff/2,
    field_to_column/1,
    format_error/1,
    unreplayed_executes/1,
    unsupported_schemas/1
]).

%% Identifiers quoted the way the generator writes them, or bare the way a
%% hand-written migration usually does. The two forms capture separately
%% because PostgreSQL folds only the bare one to lower case.
-define(IDENT, "(?:\"((?:[^\"]|\"\")+)\"|([A-Za-z_][A-Za-z0-9_$]*))").
-define(ALTER_COLUMN_RE, <<
    "(?is)^ALTER\\s+TABLE\\s+", ?IDENT, "\\s+ALTER\\s+(?:COLUMN\\s+)?", ?IDENT, "\\s+(.+)$"
>>).
-define(ADD_FK_RE, <<
    "(?is)^ALTER\\s+TABLE\\s+",
    ?IDENT,
    "\\s+ADD\\s+(?:CONSTRAINT\\s+",
    ?IDENT,
    "\\s+)?FOREIGN\\s+KEY\\s*\\(\\s*",
    ?IDENT,
    "\\s*\\)\\s*REFERENCES\\s+",
    ?IDENT,
    "\\s*\\(\\s*",
    ?IDENT,
    "\\s*\\)(.*)$"
>>).
-define(ADD_PK_RE, <<
    "(?is)^ALTER\\s+TABLE\\s+",
    ?IDENT,
    "\\s+ADD\\s+(?:CONSTRAINT\\s+",
    ?IDENT,
    "\\s+)?PRIMARY\\s+KEY\\s*\\((.+)\\)$"
>>).
-define(IDENT_LIST_RE, <<"(?s)^\\s*", ?IDENT, "\\s*(?:,(.+))?$">>).
-define(DROP_CONSTRAINT_RE,
    <<"(?is)^ALTER\\s+TABLE\\s+", ?IDENT, "\\s+DROP\\s+CONSTRAINT\\s+", ?IDENT, "$">>
).

-type col_state() :: #{binary() => [#kura_column{}]}.
-type index_entry() :: {[atom()], map()}.
-type index_state() :: #{binary() => [index_entry()]}.
-type fk_key() :: {binary(), atom()}.
-type db_state() :: #{
    columns => col_state(),
    indexes => index_state(),
    unsupported => [{module(), term()}],
    fk_names => #{fk_key() => binary()},
    fk_unowned => #{fk_key() => true},
    pk_names => #{binary() => binary()},
    unreplayed => [binary()]
}.
-type operation() ::
    {create_table, binary(), [#kura_column{}]}
    | {create_table, binary(), [#kura_column{}], [kura_migration:table_constraint()]}
    | {drop_table, binary()}
    | {alter_table, binary(), [alter_op()]}
    | {create_index, binary(), [atom()], map()}
    | {create_index, binary(), binary(), [atom()], [atom() | {atom(), term()}]}
    | {drop_index, binary()}
    | {execute, binary()}.
-type alter_op() ::
    {add_column, #kura_column{}}
    | {drop_column, atom()}
    | {rename_column, atom(), atom()}
    | {modify_column, atom(), kura_types:kura_type()}.

-export_type([db_state/0, operation/0, alter_op/0]).

%% Replay migrations to build current DB state
-spec build_db_state([module()]) -> db_state().
build_db_state(MigModules) ->
    Sorted = lists:sort(
        fun(A, B) ->
            atom_to_list(A) =< atom_to_list(B)
        end,
        MigModules
    ),
    lists:foldl(
        fun(Mod, Acc) ->
            Ops = Mod:up(),
            apply_ops(Ops, Acc)
        end,
        #{
            columns => #{},
            indexes => #{},
            fk_names => #{},
            fk_unowned => #{},
            pk_names => #{},
            unreplayed => []
        },
        Sorted
    ).

%% Convert schema modules to desired state (columns + indexes)
-spec build_desired_state([module()]) -> db_state().
build_desired_state(SchemaModules) ->
    lists:foldl(
        fun desired_state_for/2,
        #{columns => #{}, indexes => #{}, unsupported => []},
        SchemaModules
    ).

%% A schema the generator can never express is refused on its own rather than
%% aborting the run: one composite foreign key must not stop every other table
%% in the application being checked for drift. Everything else is a mistake the
%% author can fix, so it still aborts.
desired_state_for(Mod, #{columns := ColAcc, indexes := IdxAcc, unsupported := Unsup} = Acc) ->
    try
        Table = Mod:table(),
        Fields = Mod:fields(),
        Columns = [field_to_column(F) || F <- Fields, F#kura_field.virtual =/= true],
        Enriched = enrich_with_associations(Mod, Columns),
        Indexes = extract_indexes(Mod),
        ok = check_index_columns(Mod, Indexes, Enriched),
        Acc#{columns => ColAcc#{Table => Enriched}, indexes => IdxAcc#{Table => Indexes}}
    catch
        error:{kura_schema_diff, {composite_assoc_unsupported, _, _} = Reason} ->
            Acc#{unsupported => Unsup ++ [{Mod, Reason}]};
        error:{kura_schema_diff, {composite_target_key, _, _, _, _} = Reason} ->
            Acc#{unsupported => Unsup ++ [{Mod, Reason}]}
    end.

-doc """
Schemas excluded from a desired state because the generator cannot express
them, paired with the reason. Render each with `format_error/1`.
""".
-spec unsupported_schemas(db_state()) -> [{module(), term()}].
unsupported_schemas(State) ->
    maps:get(unsupported, State, []).

-doc """
`execute` statements in the migration history that name a known table but
match no shape replay understands, so drift reported on that table may be
false. See the README for the shapes it does understand. A `create_table/4`
unique or check constraint is listed too, rendered as the clause it adds.
""".
-spec unreplayed_executes(db_state()) -> [binary()].
unreplayed_executes(State) ->
    maps:get(unreplayed, State, []).

%% Otherwise dropping a field emits drop_column and the index declared on it
%% comes back as a create_index on a column that no longer exists.
check_index_columns(Mod, Indexes, Columns) ->
    Known = [C#kura_column.name || C <- Columns],
    case [Col || {Cols, _} <- Indexes, Col <- Cols, not lists:member(Col, Known)] of
        [] -> ok;
        [Col | _] -> error({kura_schema_diff, {index_unknown_column, Mod, Col}})
    end.

-spec extract_indexes(module()) -> [index_entry()].
extract_indexes(Mod) ->
    case erlang:function_exported(Mod, indexes, 0) of
        false ->
            [];
        true ->
            try
                Mod:indexes()
            catch
                _:_ -> []
            end
    end.

%% Diff DB state against desired state, returning {UpOps, DownOps}
-spec diff(db_state() | col_state(), db_state() | col_state()) -> {[operation()], [operation()]}.
diff(DbState, DesiredState) ->
    DbCols = maps:get(columns, ensure_structured(DbState), #{}),
    DesiredCols = maps:get(columns, ensure_structured(DesiredState), #{}),
    DbIdx = maps:get(indexes, ensure_structured(DbState), #{}),
    DesiredIdx = maps:get(indexes, ensure_structured(DesiredState), #{}),
    FkMeta = {maps:get(fk_names, DbState, #{}), maps:get(fk_unowned, DbState, #{})},
    PkNames = maps:get(pk_names, DbState, #{}),

    %% New tables: in desired but not in DB
    NewTables = lists:sort(maps:keys(DesiredCols) -- maps:keys(DbCols)),
    CreateOrder = create_order(NewTables, DesiredCols),
    CreateUp = [create_table(Table, maps:get(Table, DesiredCols)) || Table <- CreateOrder],
    CreateDown = [{drop_table, Table} || Table <- lists:reverse(CreateOrder)],

    %% Existing tables: diff columns
    ExistingTables = maps:keys(DesiredCols) -- NewTables,
    {AlterUp, AlterDown, ExecUp, ExecDown} = lists:foldl(
        fun(Table, {AUpAcc, ADownAcc, EUpAcc, EDownAcc}) ->
            DbTableCols = maps:get(Table, DbCols, []),
            DesiredTableCols = maps:get(Table, DesiredCols),
            ok = assert_primary_key_kept(Table, DbTableCols, DesiredTableCols, PkNames),
            case diff_columns(Table, DbTableCols, DesiredTableCols, FkMeta) of
                {[], [], [], []} ->
                    {AUpAcc, ADownAcc, EUpAcc, EDownAcc};
                {ColUp, ColDown, EU, ED} ->
                    AUp2 =
                        case ColUp of
                            [] -> AUpAcc;
                            _ -> AUpAcc ++ [{alter_table, Table, ColUp}]
                        end,
                    ADown2 =
                        case ColDown of
                            [] -> ADownAcc;
                            _ -> ADownAcc ++ [{alter_table, Table, ColDown}]
                        end,
                    {AUp2, ADown2, EUpAcc ++ EU, EDownAcc ++ ED}
            end
        end,
        {[], [], [], []},
        lists:sort(ExistingTables)
    ),

    %% Index diffing: all tables (new + existing)
    AllDesiredTables = maps:keys(DesiredCols),
    IdxOps = [
        {
            diff_indexes(Table, maps:get(Table, DbIdx, []), maps:get(Table, DesiredIdx, [])),
            lists:member(Table, NewTables)
        }
     || Table <- lists:sort(AllDesiredTables)
    ],
    IdxUp = lists:append([IU || {{IU, _}, _} <- IdxOps]),
    %% down/0 drops a new table, and its indexes with it.
    IdxDown = lists:append([ID || {{_, ID}, false} <- IdxOps]),
    {IdxDropUp, IdxCreateUp} = lists:partition(fun is_drop_index/1, IdxUp),
    {IdxDropDown, IdxCreateDown} = lists:partition(fun is_drop_index/1, IdxDown),

    %% Index drops go before the column ops and creates after them. Postgres
    %% drops an index along with a column it covers, so a later drop_index
    %% raises 42704; SQLite refuses to drop an indexed column at all.
    %%
    %% down/0 undoes the table, column and constraint ops in the reverse of
    %% the order up/0 ran them: a new table cannot be dropped while a column
    %% or foreign key added in the same migration still references it.
    {
        IdxDropUp ++ CreateUp ++ AlterUp ++ ExecUp ++ IdxCreateUp,
        IdxDropDown ++ ExecDown ++ AlterDown ++ CreateDown ++ IdxCreateDown
    }.

%% kura writes PRIMARY KEY inline on each column that has primary_key set,
%% which for a composite key declares the key twice and fails. A composite
%% key is a table constraint instead, in the column order the schema gives.
%% Its columns are written NOT NULL: SQLite, unlike PostgreSQL, lets a
%% composite key column hold NULL, and then (1, NULL) is not unique.
-spec create_table(binary(), [#kura_column{}]) -> operation().
create_table(Table, Cols) ->
    case key_columns(Cols) of
        [_, _ | _] = Key ->
            {create_table, Table, [unkeyed_not_null(C) || C <- Cols], [{primary_key, Key}]};
        _ ->
            {create_table, Table, Cols}
    end.

-spec unkeyed_not_null(#kura_column{}) -> #kura_column{}.
unkeyed_not_null(#kura_column{primary_key = true} = C) ->
    C#kura_column{primary_key = false, nullable = false};
unkeyed_not_null(C) ->
    C.

%% CREATE TABLE writes REFERENCES inline, so a new table is created once
%% every other new table it references exists. Tables that reference each
%% other have no such order and are refused: the cycle needs a foreign key
%% added after both tables exist, which is the author's call.
-spec create_order([binary()], col_state()) -> [binary()].
create_order([], _Cols) ->
    [];
create_order(Tables, Cols) ->
    case [T || T <- Tables, not references_any(T, Tables -- [T], Cols)] of
        [] -> error({kura_schema_diff, {reference_cycle, in_cycle(Tables, Cols)}});
        Ready -> Ready ++ create_order(Tables -- Ready, Cols)
    end.

%% Tables that only hang off a cycle are not part of it.
-spec in_cycle([binary()], col_state()) -> [binary()].
in_cycle(Tables, Cols) ->
    case [T || T <- Tables, not referenced_by(T, Tables -- [T], Cols)] of
        [] -> Tables;
        Leaves -> in_cycle(Tables -- Leaves, Cols)
    end.

-spec references_any(binary(), [binary()], col_state()) -> boolean().
references_any(Table, Others, Cols) ->
    [Ref || #kura_column{references = {Ref, _}} <- maps:get(Table, Cols), lists:member(Ref, Others)] =/=
        [].

-spec referenced_by(binary(), [binary()], col_state()) -> boolean().
referenced_by(Table, Others, Cols) ->
    [Other || Other <- Others, references_any(Other, [Table], Cols)] =/= [].

%% Convert a kura_field to a kura_column (skipping virtual fields)
-spec field_to_column(#kura_field{}) -> #kura_column{}.
field_to_column(#kura_field{
    name = N, type = T, column = Col, nullable = Null, default = Def, primary_key = PK
}) ->
    ColName =
        case Col of
            undefined -> N;
            _ when is_binary(Col) -> binary_to_atom(Col, utf8)
        end,
    #kura_column{name = ColName, type = T, nullable = Null, default = Def, primary_key = PK}.

enrich_with_associations(Mod, Columns) ->
    case erlang:function_exported(Mod, associations, 0) of
        false ->
            Columns;
        true ->
            Assocs = schema_associations(Mod),
            %% Validate every association, not just the ones that own a
            %% column: this is what turns an on_delete declared on a
            %% has_many into a named failure instead of a no-op.
            lists:foreach(fun(A) -> assoc_on_delete(Mod, A) end, Assocs),
            BelongsTo = [A || A <- Assocs, A#kura_assoc.type =:= belongs_to],
            lists:foldl(fun enrich_column/2, Columns, [{Mod, A} || A <- BelongsTo])
    end.

%% associations/0 is user code. Called bare it produces a raw rebar3
%% stacktrace instead of a message naming the schema that failed.
schema_associations(Mod) ->
    try
        Mod:associations()
    catch
        Class:Reason ->
            error({kura_schema_diff, {associations_failed, Mod, Class, Reason}})
    end.

enrich_column({Mod, Assoc}, Columns) ->
    Name = Assoc#kura_assoc.name,
    case kura_schema:assoc_fields(Assoc) of
        [] ->
            %% Every other malformation is named, so this one is too: a
            %% belongs_to with neither foreign_key nor ref fields has no
            %% column to hang its constraint on.
            error({kura_schema_diff, {assoc_key_undeclared, Mod, Name}});
        [FK] ->
            Target = resolve_target(Mod, Assoc),
            Refs = {Target:table(), target_key(Mod, Name, Assoc, Target)},
            OnDelete = assoc_on_delete(Mod, Assoc),
            enrich_fk_column(Mod, Assoc, FK, Refs, OnDelete, Columns);
        _Composite ->
            %% A composite foreign key is a table-level constraint, which the
            %% generator does not emit yet. Say so rather than emit a partial
            %% one-column constraint that looks right.
            error({kura_schema_diff, {composite_assoc_unsupported, Mod, Name}})
    end.

%% kura_schema:key/1 is the schema's own key resolution: the key/0 callback
%% wins, primary_key = true fields are only the fallback. Rederiving it here
%% missed key/0 entirely and truncated a composite key to its first column,
%% which generated a foreign key pointing at one half of a two-column key.
target_key(Mod, Name, Assoc, Target) ->
    case declared_target_key(Assoc, Target) of
        [Col] ->
            Col;
        [] ->
            error({kura_schema_diff, {assoc_target_no_key, Mod, Name, Target}});
        Cols ->
            error({kura_schema_diff, {composite_target_key, Mod, Name, Target, Cols}})
    end.

declared_target_key(Assoc, Target) ->
    case kura_schema:assoc_target_key(Assoc) of
        undefined -> target_schema_key(Target);
        Cols -> Cols
    end.

target_schema_key(Target) ->
    try
        kura_schema:key(Target)
    catch
        error:{no_primary_key, Target} -> []
    end.

enrich_fk_column(Mod, Assoc, FK, Refs, OnDelete, Columns) ->
    case lists:any(fun(C) -> C#kura_column.name =:= FK end, Columns) of
        false ->
            error({kura_schema_diff, {assoc_key_not_a_field, Mod, Assoc#kura_assoc.name, FK}});
        true ->
            [
                case C#kura_column.name of
                    FK when C#kura_column.references =:= undefined ->
                        C#kura_column{references = Refs, on_delete = OnDelete};
                    _ ->
                        C
                end
             || C <- Columns
            ]
    end.

%% An association whose target cannot be resolved used to be swallowed by a
%% blanket catch, so the foreign key was silently dropped from the generated
%% migration and the schema and the database disagreed forever.
resolve_target(Mod, Assoc) ->
    Name = Assoc#kura_assoc.name,
    case kura_schema:assoc_target(Assoc) of
        undefined ->
            error({kura_schema_diff, {assoc_target_undeclared, Mod, Name}});
        Target ->
            case code:ensure_loaded(Target) of
                {error, Reason} ->
                    error(
                        {kura_schema_diff, {assoc_target_not_loadable, Mod, Name, Target, Reason}}
                    );
                {module, Target} ->
                    ok = ensure_schema_module(Mod, Name, Target),
                    Target
            end
    end.

assoc_on_delete(Mod, Assoc) ->
    try
        kura_schema:assoc_on_delete(Assoc)
    catch
        error:{invalid_on_delete, Name, Action} ->
            error({kura_schema_diff, {invalid_on_delete, Mod, Name, Action}});
        error:{on_delete_not_owned, Name, Type} ->
            error({kura_schema_diff, {on_delete_not_owned, Mod, Name, Type}})
    end.

ensure_schema_module(Mod, Name, Target) ->
    Exported =
        erlang:function_exported(Target, table, 0) andalso
            erlang:function_exported(Target, fields, 0),
    case Exported of
        true ->
            ok;
        false ->
            error({kura_schema_diff, {assoc_target_not_a_schema, Mod, Name, Target}})
    end.

-spec format_error(term()) -> iolist().
format_error({assoc_target_undeclared, Mod, Name}) ->
    io_lib:format(
        "~s: association '~s' declares neither schema nor ref target, so its "
        "foreign key cannot be generated",
        [Mod, Name]
    );
format_error({assoc_target_not_loadable, Mod, Name, Target, Reason}) ->
    io_lib:format(
        "~s: association '~s' targets ~s, which could not be loaded (~p). If it "
        "lives in a dependency, check that the dependency is built",
        [Mod, Name, Target, Reason]
    );
format_error({assoc_target_not_a_schema, Mod, Name, Target}) ->
    io_lib:format(
        "~s: association '~s' targets ~s, which does not export table/0 and fields/0",
        [Mod, Name, Target]
    );
format_error({assoc_key_undeclared, Mod, Name}) ->
    io_lib:format(
        "~s: association '~s' declares neither foreign_key nor ref fields, so "
        "there is no column to attach its foreign key to",
        [Mod, Name]
    );
format_error({assoc_target_no_key, Mod, Name, Target}) ->
    io_lib:format(
        "~s: association '~s' targets ~s, which declares no primary key, so "
        "there is nothing for the foreign key to reference",
        [Mod, Name, Target]
    );
format_error({composite_target_key, Mod, Name, Target, Cols}) ->
    io_lib:format(
        "~s: association '~s' targets ~s, whose primary key is composite (~s). "
        "A single-column foreign key cannot reference it, so declare the "
        "constraint in a hand-written migration",
        [Mod, Name, Target, format_atoms(Cols)]
    );
format_error({associations_failed, Mod, Class, Reason}) ->
    io_lib:format("~s: associations/0 raised ~s:~p", [Mod, Class, Reason]);
format_error({fk_on_update_not_owned, Table, Col, OnUpdate}) when is_atom(OnUpdate) ->
    io_lib:format(
        "table \"~s\": the foreign key on '~s' changed, but the existing "
        "constraint carries ON UPDATE ~s, which no schema can declare. "
        "Regenerating the constraint would silently drop that clause, so "
        "write this change as a hand-written migration",
        [Table, Col, string:uppercase(atom_to_list(OnUpdate))]
    );
format_error({fk_not_owned, Table, Col}) ->
    io_lib:format(
        "table \"~s\": the foreign key on '~s' changed, but the existing "
        "constraint was written by hand with clauses kura cannot express. "
        "Regenerating it would silently drop them, so write this change as a "
        "hand-written migration",
        [Table, Col]
    );
format_error({primary_key_changed, Table, Col, Joins, Constraint}) when
    is_binary(Table), is_atom(Col), is_boolean(Joins), is_binary(Constraint)
->
    AlterTable = <<"ALTER TABLE ", (quote(Table))/binary>>,
    io_lib:format(
        "table \"~s\": the schema ~s, but the generator does not change a "
        "primary key: dropping the old one fails while a foreign key depends "
        "on it. Write this change as a hand-written migration, which replay "
        "reads. On PostgreSQL: {execute, ~s} then {execute, ~s}. "
        "On SQLite, which cannot alter a key, rebuild the table: create_table/4 "
        "a copy with {primary_key, [...]}, copy the rows with an execute, "
        "drop_table the original, create_table/4 it again and copy the rows "
        "back, then drop_table the copy",
        [
            Table,
            key_change(Joins, Col),
            sigil(<<AlterTable/binary, " DROP CONSTRAINT ", (quote(Constraint))/binary>>),
            sigil(<<AlterTable/binary, " ADD PRIMARY KEY (...)">>)
        ]
    );
format_error({reference_cycle, Tables}) when is_list(Tables) ->
    io_lib:format(
        "new tables ~s reference each other, so no order of CREATE TABLE "
        "can declare all their foreign keys inline. Create them in a "
        "hand-written migration that adds one of the foreign keys after both "
        "tables exist",
        [lists:join(", ", [[$", T, $"] || T <- Tables, is_binary(T)])]
    );
format_error({assoc_key_not_a_field, Mod, Name, FK}) ->
    io_lib:format(
        "~s: association '~s' names foreign key '~s', which is not a field on ~s",
        [Mod, Name, FK, Mod]
    );
format_error({index_unknown_column, Mod, Col}) ->
    io_lib:format(
        "~s: indexes/0 names column '~s', which is not a column of ~s",
        [Mod, Col, Mod]
    );
format_error({composite_assoc_unsupported, Mod, Name}) ->
    io_lib:format(
        "~s: association '~s' has a composite foreign key; the generator cannot "
        "emit a composite constraint, so write the migration by hand",
        [Mod, Name]
    );
format_error({invalid_on_delete, Mod, Name, Action}) ->
    io_lib:format(
        "~s: association '~s' declares on_delete = ~p; expected one of "
        "cascade, restrict, set_null, no_action",
        [Mod, Name, Action]
    );
format_error({on_delete_not_owned, Mod, Name, many_to_many}) ->
    %% There is no belongs_to on the other side of a many_to_many, and the
    %% foreign keys live on the join table, which this generator never emits.
    io_lib:format(
        "~s: association '~s' is a many_to_many and owns no foreign-key column. "
        "Its foreign keys belong to the join table, which the generator does "
        "not emit, so declare on_delete in that table's own migration",
        [Mod, Name]
    );
format_error({on_delete_not_owned, Mod, Name, Type}) ->
    io_lib:format(
        "~s: association '~s' is a ~s and owns no foreign-key column, so its "
        "on_delete would never reach the database. Declare it on the belongs_to "
        "on the other side",
        [Mod, Name, Type]
    );
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Escaped the way the migration renderer escapes, so the snippet pastes as
%% Erlang whatever the table and constraint are called.
-spec sigil(binary()) -> iodata().
sigil(Bin) ->
    [$~, $", binary:replace(Bin, [<<$\\>>, <<$">>], <<$\\>>, [global, {insert_replaced, 1}]), $"].

-spec key_change(boolean(), atom()) -> iolist().
key_change(true, Col) -> io_lib:format("adds column '~s' to the primary key", [Col]);
key_change(false, Col) -> io_lib:format("takes column '~s' out of the primary key", [Col]).

format_atoms(Atoms) ->
    lists:join(", ", [atom_to_list(A) || A <- Atoms]).

%%% Internal

apply_ops([], State) ->
    State;
apply_ops([{create_table, Name, Cols} | Rest], #{columns := ColState} = State) ->
    apply_ops(Rest, State#{columns => ColState#{Name => Cols}});
apply_ops([{create_table, Name, Cols, Constraints} | Rest], State) ->
    Keyed = with_primary_key(Cols, lists:append([Key || {primary_key, Key} <- Constraints])),
    State1 = apply_ops([{create_table, Name, Keyed}], State),
    apply_ops(Rest, note_constraints(Name, Constraints, State1));
apply_ops([{drop_table, Name} | Rest], #{columns := ColState, indexes := IdxState} = State) ->
    State1 = State#{
        columns => maps:remove(Name, ColState),
        indexes => maps:remove(Name, IdxState),
        pk_names => maps:remove(Name, maps:get(pk_names, State, #{}))
    },
    apply_ops(Rest, forget_fk_meta(fun({T, _}) -> T =/= Name end, State1));
apply_ops(
    [{alter_table, Name, AlterOps} | Rest], #{columns := ColState, indexes := IdxState} = State
) ->
    Cols = maps:get(Name, ColState, []),
    NewCols = apply_alter_ops(AlterOps, Cols),
    NewIdxState =
        case IdxState of
            #{Name := Entries} -> IdxState#{Name => alter_indexes(AlterOps, Name, Entries)};
            #{} -> IdxState
        end,
    State1 = alter_fk_meta(AlterOps, Name, Cols, State),
    NewColState = retarget_references(AlterOps, Name, ColState#{Name => NewCols}),
    apply_ops(Rest, State1#{columns => NewColState, indexes => NewIdxState});
apply_ops([{create_index, Table, Columns, Opts} | Rest], #{indexes := IdxState} = State) ->
    TableIdx = maps:get(Table, IdxState, []),
    Entry = {Columns, Opts},
    apply_ops(Rest, State#{indexes => IdxState#{Table => TableIdx ++ [Entry]}});
apply_ops([{create_index, Name, Table, Columns, Opts} | Rest], #{indexes := IdxState} = State) ->
    TableIdx = maps:get(Table, IdxState, []),
    OptsMap = (proplist_to_map(Opts))#{name => Name},
    Entry = {Columns, OptsMap},
    apply_ops(Rest, State#{indexes => IdxState#{Table => TableIdx ++ [Entry]}});
apply_ops([{drop_index, IdxName} | Rest], #{indexes := IdxState} = State) ->
    NewIdxState = maps:map(
        fun(Table, Entries) ->
            [E || E <- Entries, entry_name(Table, E) =/= IdxName]
        end,
        IdxState
    ),
    apply_ops(Rest, State#{indexes => NewIdxState});
apply_ops([{execute, SQL} | Rest], State) ->
    apply_ops(Rest, replay_execute(SQL, State));
apply_ops([_Other | Rest], State) ->
    apply_ops(Rest, State).

-spec with_primary_key([#kura_column{}], [atom()]) -> [#kura_column{}].
with_primary_key(Cols, Key) ->
    [
        case lists:member(C#kura_column.name, Key) of
            true -> C#kura_column{primary_key = true};
            false -> C
        end
     || C <- Cols
    ].

%% A unique constraint is an index replay does not record, and a check is
%% nothing replay records, so both are reported the way an unread execute is.
-spec note_constraints(binary(), [kura_migration:table_constraint()], db_state()) -> db_state().
note_constraints(Table, Constraints, State) ->
    Noted = [
        iolist_to_binary([~"CREATE TABLE ", quote(Table), ~" (... ", Clause, ~")"])
     || C <- Constraints,
        {unmodelled, Clause} <- [constraint_clause(C)]
    ],
    State#{unreplayed => maps:get(unreplayed, State, []) ++ Noted}.

-spec constraint_clause(kura_migration:table_constraint()) -> modelled | {unmodelled, iodata()}.
constraint_clause({primary_key, _Cols}) ->
    modelled;
constraint_clause({unique, Cols}) ->
    {unmodelled, [~"UNIQUE (", quoted_columns(Cols), ~")"]};
constraint_clause({check, Expr}) ->
    {unmodelled, [~"CHECK (", Expr, ~")"]}.

-spec quoted_columns([atom()]) -> iodata().
quoted_columns(Cols) ->
    lists:join(~", ", [quote(atom_to_binary(C)) || C <- Cols]).

%% Convert legacy flat map format to structured format
-spec ensure_structured(map()) -> db_state().
ensure_structured(#{columns := _} = State) -> State;
ensure_structured(FlatMap) -> #{columns => FlatMap, indexes => #{}}.

-spec index_name(binary(), [atom()]) -> binary().
index_name(Table, Cols) ->
    ColsBin = lists:join(~"_", [atom_to_binary(C, utf8) || C <- Cols]),
    iolist_to_binary([Table, ~"_", ColsBin, ~"_index"]).

-spec proplist_to_map([atom() | {atom(), term()}]) -> map().
proplist_to_map(Opts) ->
    lists:foldl(
        fun
            (unique, Acc) -> Acc#{unique => true};
            ({K, V}, Acc) -> Acc#{K => V};
            (_, Acc) -> Acc
        end,
        #{},
        Opts
    ).

%% Postgres drops an index along with any column it covers or its WHERE
%% names. A column rename leaves the index's name as it was and rewrites its
%% predicate, which Postgres stores by column number rather than text.
%%
%% The WHERE text is matched as words, case-insensitively: a column name
%% that also appears inside a string literal in the predicate is a known
%% false positive.
alter_indexes([], _Table, Entries) ->
    Entries;
alter_indexes([{drop_column, Col} | Rest], Table, Entries) ->
    alter_indexes(Rest, Table, [E || E <- Entries, not index_uses(E, Col)]);
alter_indexes([{rename_column, Old, New} | Rest], Table, Entries) ->
    alter_indexes(Rest, Table, [rename_entry(Table, Old, New, E) || E <- Entries]);
alter_indexes([_Other | Rest], Table, Entries) ->
    alter_indexes(Rest, Table, Entries).

rename_entry(Table, Old, New, {Cols, Opts} = E) ->
    case index_uses(E, Old) of
        true ->
            Renamed = Opts#{name => entry_name(Table, E)},
            {[rename(C, Old, New) || C <- Cols], rename_in_where(Renamed, Old, New)};
        false ->
            E
    end.

rename_in_where(#{where := Where} = Opts, Old, New) when is_binary(Where) ->
    Opts#{
        where => re:replace(Where, column_word(Old), replacement(New), [global, {return, binary}])
    };
rename_in_where(Opts, _Old, _New) ->
    Opts.

%% `&` and `\` mean something in a re:replace replacement.
replacement(Col) ->
    binary:replace(atom_to_binary(Col), [<<$\\>>, <<$&>>], <<$\\>>, [global, {insert_replaced, 1}]).

index_uses({Cols, Opts}, Col) ->
    lists:member(Col, Cols) orelse
        case maps:get(where, Opts, undefined) of
            Where when is_binary(Where) ->
                re:run(Where, column_word(Col), [{capture, none}]) =:= match;
            _ ->
                false
        end.

column_word(Col) ->
    <<"(?i)", (word(atom_to_binary(Col)))/binary>>.

%% Postgres names an inline REFERENCES constraint after the column and keeps
%% that name across a rename, so a renamed column's constraint is recorded
%% under its old name.
alter_fk_meta([], _Table, _Cols, State) ->
    State;
alter_fk_meta([{drop_column, Col} = Op | Rest], Table, Cols, State) ->
    State1 = forget_fk_meta(fun(Key) -> Key =/= {Table, Col} end, State),
    alter_fk_meta(Rest, Table, apply_alter_ops([Op], Cols), State1);
alter_fk_meta([{rename_column, Old, New} = Op | Rest], Table, Cols, State) ->
    FkNames = maps:get(fk_names, State, #{}),
    State1 =
        case lists:keyfind(Old, #kura_column.name, Cols) of
            #kura_column{references = Refs} when Refs =/= undefined ->
                Name = fk_name(FkNames, Table, Old),
                Unowned = maps:is_key({Table, Old}, maps:get(fk_unowned, State, #{})),
                State0 = forget_fk_meta(fun(Key) -> Key =/= {Table, Old} end, State),
                S1 = put_fk_meta(fk_names, {Table, New}, name_if_custom(Table, New, Name), State0),
                put_fk_meta(fk_unowned, {Table, New}, Unowned, S1);
            _ ->
                State
        end,
    alter_fk_meta(Rest, Table, apply_alter_ops([Op], Cols), State1);
alter_fk_meta([Op | Rest], Table, Cols, State) ->
    alter_fk_meta(Rest, Table, apply_alter_ops([Op], Cols), State).

%% Postgres keeps a foreign key pointing at a column through its rename, so
%% every table's reference to the old name, the renamed table's own included,
%% follows it.
-spec retarget_references([alter_op()], binary(), col_state()) -> col_state().
retarget_references([], _Table, ColState) ->
    ColState;
retarget_references([{rename_column, Old, New} | Rest], Table, ColState) ->
    Retargeted = #{T => [retarget(C, Table, Old, New) || C <- Cols] || T := Cols <- ColState},
    retarget_references(Rest, Table, Retargeted);
retarget_references([_Op | Rest], Table, ColState) ->
    retarget_references(Rest, Table, ColState).

-spec retarget(#kura_column{}, binary(), atom(), atom()) -> #kura_column{}.
retarget(#kura_column{references = {Table, Old}} = C, Table, Old, New) ->
    C#kura_column{references = {Table, New}};
retarget(C, _Table, _Old, _New) ->
    C.

forget_fk_meta(Keep, State) ->
    State#{
        fk_names => maps:filter(fun(K, _) -> Keep(K) end, maps:get(fk_names, State, #{})),
        fk_unowned => maps:filter(fun(K, _) -> Keep(K) end, maps:get(fk_unowned, State, #{}))
    }.

rename(Old, Old, New) -> New;
rename(Col, _Old, _New) -> Col.

entry_name(Table, {Cols, Opts}) ->
    maps:get(name, Opts, index_name(Table, Cols)).

apply_alter_ops([], Cols) ->
    Cols;
apply_alter_ops([{add_column, Col} | Rest], Cols) ->
    apply_alter_ops(Rest, Cols ++ [Col]);
apply_alter_ops([{drop_column, Name} | Rest], Cols) ->
    apply_alter_ops(Rest, [C || C <- Cols, C#kura_column.name =/= Name]);
apply_alter_ops([{rename_column, Old, New} | Rest], Cols) ->
    NewCols = [
        case C#kura_column.name of
            Old -> C#kura_column{name = New};
            _ -> C
        end
     || C <- Cols
    ],
    apply_alter_ops(Rest, NewCols);
apply_alter_ops([{modify_column, Name, Type} | Rest], Cols) ->
    NewCols = [
        case C#kura_column.name of
            Name -> C#kura_column{type = Type};
            _ -> C
        end
     || C <- Cols
    ],
    apply_alter_ops(Rest, NewCols);
apply_alter_ops([_Other | Rest], Cols) ->
    apply_alter_ops(Rest, Cols).

diff_indexes(Table, DbIndexes, DesiredIndexes) ->
    %% Normalize to sets of {Columns, Opts} for comparison
    DbSet = normalize_indexes(DbIndexes),
    DesiredSet = normalize_indexes(DesiredIndexes),
    %% New indexes: in desired but not in DB
    NewIdx = DesiredSet -- DbSet,
    CreateUp = [{create_index, Table, Cols, Opts} || {Cols, Opts} <- NewIdx],
    CreateDown = [{drop_index, index_name(Table, Cols)} || {Cols, _} <- NewIdx],
    %% Dropped indexes: in DB but not in desired
    Dropped = dropped_entries(DbSet -- DesiredSet, DbIndexes),
    DropUp = [{drop_index, entry_name(Table, E)} || E <- Dropped],
    DropDown = [recreate_index(Table, E) || E <- Dropped],
    {CreateUp ++ DropUp, CreateDown ++ DropDown}.

%% Each normalised index dropped takes one DB entry: two indexes that
%% normalise alike, one of which the schema still wants, lose only one.
dropped_entries([], _DbIndexes) ->
    [];
dropped_entries([{Cols, Opts} | Rest], DbIndexes) ->
    {Taken, Left} = take_entry(Cols, Opts, DbIndexes, []),
    [Taken | dropped_entries(Rest, Left)].

take_entry(Cols, Opts, [{Cols, EntryOpts} = E | Rest], Seen) ->
    case normalize_opts(EntryOpts) of
        Opts -> {E, lists:reverse(Seen, Rest)};
        _ -> take_entry(Cols, Opts, Rest, [E | Seen])
    end;
take_entry(Cols, Opts, [E | Rest], Seen) ->
    take_entry(Cols, Opts, Rest, [E | Seen]).

%% Rolling back recreates an index under the name it had, so a later drop
%% by that name still finds it.
recreate_index(Table, {Cols, #{name := Name} = Opts}) ->
    case index_name(Table, Cols) of
        Name -> {create_index, Table, Cols, normalize_opts(Opts)};
        _ -> {create_index, Name, Table, Cols, opts_to_proplist(normalize_opts(Opts))}
    end;
recreate_index(Table, {Cols, Opts}) ->
    {create_index, Table, Cols, normalize_opts(Opts)}.

opts_to_proplist(Opts) ->
    [unique || maps:get(unique, Opts, false) =:= true] ++
        [{K, V} || K := V <- maps:without([unique], Opts)].

is_drop_index({drop_index, _}) -> true;
is_drop_index(_Op) -> false.

normalize_indexes(Indexes) ->
    %% Don't sort Cols: column order is part of the index identity.
    %% B-tree indexes on (a, b) and (b, a) serve different query patterns
    %% and produce different generated index names, so reordering would
    %% silently mask a real schema change. Normalize options only.
    [{Cols, normalize_opts(Opts)} || {Cols, Opts} <- Indexes].

normalize_opts(Opts) when is_map(Opts) ->
    maps:without([name], Opts);
normalize_opts(Opts) when is_list(Opts) ->
    proplist_to_map(Opts).

diff_columns(Table, DbCols, DesiredCols, FkMeta) ->
    DbMap = col_map(DbCols),
    DesiredMap = col_map(DesiredCols),
    DbNames = maps:keys(DbMap),
    DesiredNames = maps:keys(DesiredMap),

    %% Added columns
    Added = DesiredNames -- DbNames,
    AddUp = [{add_column, maps:get(N, DesiredMap)} || N <- lists:sort(Added)],
    AddDown = [{drop_column, N} || N <- lists:sort(Added)],

    %% Dropped columns
    Dropped = DbNames -- DesiredNames,
    DropUp = [{drop_column, N} || N <- lists:sort(Dropped)],
    DropDown = [{add_column, maps:get(N, DbMap)} || N <- lists:sort(Dropped)],

    %% Changes on existing columns (type, nullable, default)
    Common = DesiredNames -- Added,
    {ModUp, ModDown, ExecUp, ExecDown} = lists:foldl(
        fun(Name, {MU, MD, EU, ED}) ->
            DbCol = maps:get(Name, DbMap),
            DesCol = maps:get(Name, DesiredMap),
            %% Type changes
            {MU2, MD2} =
                case types_equal(DbCol#kura_column.type, DesCol#kura_column.type) of
                    true ->
                        {MU, MD};
                    false ->
                        {
                            MU ++ [{modify_column, Name, DesCol#kura_column.type}],
                            MD ++ [{modify_column, Name, DbCol#kura_column.type}]
                        }
                end,
            ColBin = atom_to_binary(Name, utf8),
            %% Nullable changes
            {EU2, ED2} =
                case nullable_changed(DbCol, DesCol) of
                    false ->
                        {EU, ED};
                    true ->
                        case is_nullable(DesCol) of
                            true ->
                                {
                                    EU ++
                                        [
                                            {execute, <<
                                                (alter_column(Table, ColBin))/binary,
                                                " DROP NOT NULL"
                                            >>}
                                        ],
                                    ED ++
                                        [
                                            {execute, <<
                                                (alter_column(Table, ColBin))/binary,
                                                " SET NOT NULL"
                                            >>}
                                        ]
                                };
                            false ->
                                {
                                    EU ++
                                        [
                                            {execute, <<
                                                (alter_column(Table, ColBin))/binary,
                                                " SET NOT NULL"
                                            >>}
                                        ],
                                    ED ++
                                        [
                                            {execute, <<
                                                (alter_column(Table, ColBin))/binary,
                                                " DROP NOT NULL"
                                            >>}
                                        ]
                                }
                        end
                end,
            %% Default changes
            {EU3, ED3} =
                case DbCol#kura_column.default =:= DesCol#kura_column.default of
                    true ->
                        {EU2, ED2};
                    false ->
                        UpDef = default_sql(Table, ColBin, DesCol#kura_column.default),
                        DownDef = default_sql(Table, ColBin, DbCol#kura_column.default),
                        {EU2 ++ [{execute, UpDef}], ED2 ++ [{execute, DownDef}]}
                end,
            %% Foreign key changes
            {FkUp, FkDown} = fk_ops(Table, Name, DbCol, DesCol, FkMeta),
            {MU2, MD2, EU3 ++ FkUp, ED3 ++ FkDown}
        end,
        {[], [], [], []},
        lists:sort(Common)
    ),

    {AddUp ++ DropUp ++ ModUp, AddDown ++ DropDown ++ ModDown, ExecUp, ExecDown}.

%% An association edited on a table that already exists reaches the diff as a
%% changed `references`/`on_delete` on a column both sides already have. There
%% is no alter_op for a constraint, so it lowers to explicit SQL, and both
%% directions drop the old constraint before adding the new one: no dialect
%% kura targets can change a referential action in place.
%%
%% The constraint keeps the name it was created under, which a hand-written
%% migration or a column rename may have made something other than the
%% generated one, so a regeneration re-adds it under that name. A constraint
%% whose clauses replay could not all read is left alone while the schema
%% agrees with what it could.
fk_ops(Table, Name, DbCol, DesCol, {FkNames, Unowned}) ->
    DbFk = {DbCol#kura_column.references, DbCol#kura_column.on_delete},
    DesFk = {DesCol#kura_column.references, DesCol#kura_column.on_delete},
    case DbFk =:= DesFk of
        true ->
            {[], []};
        false ->
            ok = assert_fk_regenerable(Table, Name, DbCol, maps:is_key({Table, Name}, Unowned)),
            DbName = fk_name(FkNames, Table, Name),
            {
                fk_transition(Table, Name, {DbCol, DbName}, {DesCol, DbName}),
                fk_transition(Table, Name, {DesCol, DbName}, {DbCol, DbName})
            }
    end.

%% ON UPDATE can only come from a hand-written migration - no schema can
%% declare it - so regenerating the constraint from the schema would drop it.
assert_fk_regenerable(Table, Name, #kura_column{references = Refs}, true) when
    Refs =/= undefined
->
    error({kura_schema_diff, {fk_not_owned, Table, Name}});
assert_fk_regenerable(Table, Name, #kura_column{references = Refs, on_update = OnUpdate}, _) when
    Refs =/= undefined, OnUpdate =/= undefined
->
    error({kura_schema_diff, {fk_on_update_not_owned, Table, Name, OnUpdate}});
assert_fk_regenerable(_Table, _Name, _DbCol, _Unowned) ->
    ok.

fk_transition(Table, Name, {From, FromName}, {To, ToName}) ->
    Drop =
        case From#kura_column.references of
            undefined -> [];
            _ -> [{execute, drop_fk_sql(Table, FromName)}]
        end,
    Add =
        case To#kura_column.references of
            undefined -> [];
            Refs -> [{execute, add_fk_sql(Table, Name, ToName, Refs, To#kura_column.on_delete)}]
        end,
    Drop ++ Add.

%% PostgreSQL's own name for the constraint an inline REFERENCES creates, so a
%% DROP here finds the one create_table/add_column generated.
-spec fk_constraint_name(binary(), atom()) -> binary().
fk_constraint_name(Table, Col) ->
    iolist_to_binary([Table, ~"_", atom_to_binary(Col, utf8), ~"_fkey"]).

drop_fk_sql(Table, Constraint) ->
    iolist_to_binary([
        ~"ALTER TABLE ",
        quote(Table),
        ~" DROP CONSTRAINT ",
        quote(Constraint)
    ]).

add_fk_sql(Table, Col, Constraint, {RefTable, RefCol}, OnDelete) ->
    iolist_to_binary([
        ~"ALTER TABLE ",
        quote(Table),
        ~" ADD CONSTRAINT ",
        quote(Constraint),
        ~" FOREIGN KEY (",
        quote(atom_to_binary(Col, utf8)),
        ~") REFERENCES ",
        quote(RefTable),
        ~" (",
        quote(atom_to_binary(RefCol, utf8)),
        ~")",
        on_delete_clause(OnDelete)
    ]).

on_delete_clause(undefined) -> <<>>;
on_delete_clause(cascade) -> ~" ON DELETE CASCADE";
on_delete_clause(restrict) -> ~" ON DELETE RESTRICT";
on_delete_clause(set_null) -> ~" ON DELETE SET NULL";
on_delete_clause(no_action) -> ~" ON DELETE NO ACTION".

-spec quote(binary()) -> binary().
quote(Bin) ->
    Escaped = binary:replace(Bin, <<"\"">>, <<"\"\"">>, [global]),
    <<"\"", Escaped/binary, "\"">>.

%% A default is DDL text, not a bind parameter, so an embedded quote has to
%% be doubled or it closes the literal early.
-spec quote_literal(binary()) -> binary().
quote_literal(Bin) ->
    Escaped = binary:replace(Bin, <<"'">>, <<"''">>, [global]),
    <<"'", Escaped/binary, "'">>.

default_sql(Table, ColBin, undefined) ->
    <<(alter_column(Table, ColBin))/binary, " DROP DEFAULT">>;
default_sql(Table, ColBin, Val) ->
    ValBin = format_default(Val),
    <<(alter_column(Table, ColBin))/binary, " SET DEFAULT ", ValBin/binary>>.

-spec alter_column(binary(), binary()) -> binary().
alter_column(Table, ColBin) ->
    <<"ALTER TABLE ", (quote(Table))/binary, " ALTER COLUMN ", (quote(ColBin))/binary>>.

format_default(true) -> <<"true">>;
format_default(false) -> <<"false">>;
format_default(V) when is_integer(V) -> integer_to_binary(V);
format_default(V) when is_float(V) -> float_to_binary(V, [{decimals, 10}, compact]);
format_default(V) when is_binary(V) -> quote_literal(V);
format_default(V) -> list_to_binary(io_lib:format("~p", [V])).

types_equal({enum, _}, {enum, _}) -> true;
types_equal(A, B) -> A =:= B.

%% A primary key is NOT NULL whether or not the column says so, on PostgreSQL
%% and MySQL; SQLite lets a non-INTEGER primary key hold NULL, but the
%% ALTER COLUMN this guards cannot run there anyway.
is_nullable(#kura_column{primary_key = true}) -> false;
is_nullable(#kura_column{nullable = Nullable}) -> Nullable.

nullable_changed(DbCol, DesCol) ->
    is_nullable(DbCol) =/= is_nullable(DesCol).

%% A primary key change is refused rather than generated, whether a column
%% joins or leaves the key or a key column is added or dropped. Dropping the
%% old key fails while another table's foreign key depends on it, adding a
%% PRIMARY KEY column to a keyed table fails outright, and only the author
%% knows which dependent constraints to move first. Replay reads the
%% hand-written migration that does it, so the refusal clears once it exists.
-spec assert_primary_key_kept(binary(), [#kura_column{}], [#kura_column{}], #{
    binary() => binary()
}) -> ok.
assert_primary_key_kept(Table, DbCols, DesiredCols, PkNames) ->
    DbKey = key_columns(DbCols),
    DesiredKey = key_columns(DesiredCols),
    case (DbKey -- DesiredKey) ++ (DesiredKey -- DbKey) of
        [] ->
            ok;
        [Col | _] ->
            Name = maps:get(Table, PkNames, default_pk_name(Table)),
            Joins = lists:member(Col, DesiredKey),
            error({kura_schema_diff, {primary_key_changed, Table, Col, Joins, Name}})
    end.

-spec key_columns([#kura_column{}]) -> [atom()].
key_columns(Cols) ->
    [C#kura_column.name || C <- Cols, C#kura_column.primary_key].

col_map(Cols) ->
    maps:from_list([{C#kura_column.name, C} || C <- Cols]).

replay_execute(SQL, State) ->
    Stmt = statement(SQL),
    case binary:match(unquoted(Stmt), ~";") of
        nomatch ->
            case replay_statement(Stmt, State) of
                {ok, NewState} -> NewState;
                nomatch -> note_unreplayed(SQL, Stmt, State)
            end;
        _ ->
            note_unreplayed(SQL, Stmt, State)
    end.

%% Trimmed in Erlang rather than by the patterns, whose lazy tail and
%% optional terminator backtracked polynomially on long whitespace runs.
statement(SQL) ->
    string:trim(string:trim(string:trim(SQL), trailing, ";"), trailing).

%% A `;` or a DDL verb inside a literal or a quoted identifier is not one.
unquoted(Stmt) ->
    re:replace(Stmt, ~"'(?:[^']++|'')*+'|\"(?:[^\"]++|\"\")*+\"", ~"", [global, {return, binary}]).

replay_statement(Stmt, State) ->
    case captures(Stmt, ?ALTER_COLUMN_RE, 5) of
        {match, [TableQ, TableB, ColQ, ColB, Action]} ->
            replay_alter_column(ident(TableQ, TableB), ident(ColQ, ColB), Action, State);
        nomatch ->
            replay_fk_add(Stmt, State)
    end.

replay_alter_column(Table, Col, Action, State) ->
    case {existing_column(Table, Col, State), parse_action(Action)} of
        {{ok, ColAtom}, {ok, Parsed}} -> {ok, update_col(Table, ColAtom, Parsed, State)};
        _ -> nomatch
    end.

replay_fk_add(Stmt, State) ->
    case captures(Stmt, ?ADD_FK_RE, 11) of
        {match, [TQ, TB, NQ, NB, CQ, CB, RTQ, RTB, RCQ, RCB, Tail]} ->
            Table = ident(TQ, TB),
            case {existing_column(Table, ident(CQ, CB), State), existing_atom(ident(RCQ, RCB))} of
                {{ok, Col}, {ok, RefCol}} ->
                    Refs = {ident(RTQ, RTB), RefCol},
                    Name = constraint_name(Table, Col, ident(NQ, NB)),
                    {ok, add_fk(Table, Col, Refs, Name, parse_fk_actions(Tail), State)};
                _ ->
                    nomatch
            end;
        nomatch ->
            replay_pk_add(Stmt, State)
    end.

-spec replay_pk_add(binary(), db_state()) -> {ok, db_state()} | nomatch.
replay_pk_add(Stmt, State) ->
    case captures(Stmt, ?ADD_PK_RE, 5) of
        {match, [TQ, TB, NQ, NB, ColList]} ->
            Table = ident(TQ, TB),
            case existing_columns(Table, ident_list(ColList), State) of
                {ok, Cols} -> {ok, add_pk(Table, Cols, ident(NQ, NB), State)};
                error -> nomatch
            end;
        nomatch ->
            replay_constraint_drop(Stmt, State)
    end.

-spec replay_constraint_drop(binary(), db_state()) -> {ok, db_state()} | nomatch.
replay_constraint_drop(Stmt, State) ->
    case captures(Stmt, ?DROP_CONSTRAINT_RE, 4) of
        {match, [TQ, TB, NQ, NB]} -> drop_constraint(ident(TQ, TB), ident(NQ, NB), State);
        nomatch -> nomatch
    end.

-spec drop_constraint(binary(), binary(), db_state()) -> {ok, db_state()} | nomatch.
drop_constraint(Table, Constraint, State) ->
    case drop_fk(Table, Constraint, State) of
        {ok, State1} -> {ok, State1};
        nomatch -> drop_pk(Table, Constraint, State)
    end.

-spec ident_list(binary()) -> [binary()] | error.
ident_list(Bin) ->
    case captures(Bin, ?IDENT_LIST_RE, 3) of
        {match, [Q, B, <<>>]} -> [ident(Q, B)];
        {match, [Q, B, Rest]} -> more_idents(ident(Q, B), ident_list(Rest));
        nomatch -> error
    end.

-spec more_idents(binary(), [binary()] | error) -> [binary()] | error.
more_idents(_Ident, error) -> error;
more_idents(Ident, Idents) -> [Ident | Idents].

-spec existing_columns(binary(), [binary()] | error, db_state()) -> {ok, [atom()]} | error.
existing_columns(_Table, error, _State) ->
    error;
existing_columns(Table, Cols, State) ->
    Found = [Atom || Col <- Cols, {ok, Atom} <- [existing_column(Table, Col, State)]],
    case length(Found) =:= length(Cols) of
        true -> {ok, Found};
        false -> error
    end.

-spec pk_name(binary(), db_state()) -> binary().
pk_name(Table, State) ->
    maps:get(Table, maps:get(pk_names, State, #{}), default_pk_name(Table)).

%% PostgreSQL's own name for an unnamed primary key constraint. An identifier
%% holds at most 63 bytes, so the table part is cut to fit the suffix, and
%% never inside a UTF-8 character.
-spec default_pk_name(binary()) -> binary().
default_pk_name(Table) ->
    <<(clip(Table, 58))/binary, "_pkey">>.

-spec clip(binary(), non_neg_integer()) -> binary().
clip(Bin, Max) when byte_size(Bin) =< Max ->
    Bin;
clip(_Bin, 0) ->
    <<>>;
clip(Bin, Max) ->
    case binary:at(Bin, Max) of
        Continuation when Continuation band 16#C0 =:= 16#80 -> clip(Bin, Max - 1);
        _ -> binary:part(Bin, 0, Max)
    end.

-spec add_pk(binary(), [atom()], binary(), db_state()) -> db_state().
add_pk(Table, Key, Name, #{columns := ColState} = State) ->
    PkNames = maps:get(pk_names, State, #{}),
    State1 =
        case Name of
            <<>> -> State#{pk_names => maps:remove(Table, PkNames)};
            _ -> State#{pk_names => PkNames#{Table => Name}}
        end,
    State1#{columns => ColState#{Table => with_primary_key(maps:get(Table, ColState), Key)}}.

%% The key's columns stay NOT NULL after the constraint goes.
-spec drop_pk(binary(), binary(), db_state()) -> {ok, db_state()} | nomatch.
drop_pk(Table, Constraint, #{columns := ColState} = State) ->
    Cols = maps:get(Table, ColState, []),
    Keyed = lists:keymember(true, #kura_column.primary_key, Cols),
    case Keyed andalso pk_name(Table, State) =:= Constraint of
        true ->
            Unkeyed = [unkey(C) || C <- Cols],
            PkNames = maps:remove(Table, maps:get(pk_names, State, #{})),
            {ok, State#{columns => ColState#{Table => Unkeyed}, pk_names => PkNames}};
        false ->
            nomatch
    end.

-spec unkey(#kura_column{}) -> #kura_column{}.
unkey(#kura_column{primary_key = true} = C) ->
    C#kura_column{primary_key = false, nullable = false};
unkey(C) ->
    C.

%% Every group is asked for by number: PCRE leaves unset trailing groups out
%% of an all_but_first capture, and an identifier's unused form is unset.
captures(Stmt, Pattern, Groups) ->
    re:run(Stmt, Pattern, [{capture, lists:seq(1, Groups), binary}]).

ident(<<>>, Bare) -> string:lowercase(Bare);
ident(Quoted, _Bare) -> unquote(Quoted).

unquote(Ident) ->
    binary:replace(Ident, ~"\"\"", ~"\"", [global]).

%% A column that is not already an atom cannot be in the state, so the
%% statement cannot be about it.
existing_atom(Bin) ->
    try
        {ok, binary_to_existing_atom(Bin)}
    catch
        error:badarg -> error
    end.

existing_column(Table, Col, #{columns := ColState}) ->
    case existing_atom(Col) of
        {ok, Atom} ->
            case lists:keymember(Atom, #kura_column.name, maps:get(Table, ColState, [])) of
                true -> {ok, Atom};
                false -> error
            end;
        error ->
            error
    end.

constraint_name(Table, Col, <<>>) -> fk_constraint_name(Table, Col);
constraint_name(_Table, _Col, Name) -> Name.

%% Only DDL can change what a table looks like; a backfill that names the
%% table does not.
note_unreplayed(SQL, Stmt, #{columns := Cols} = State) ->
    case
        is_ddl(Stmt) andalso
            lists:any(fun(Table) -> names_table(Stmt, Table) end, maps:keys(Cols))
    of
        true -> State#{unreplayed => maps:get(unreplayed, State, []) ++ [SQL]};
        false -> State
    end.

is_ddl(Stmt) ->
    re:run(unquoted(Stmt), ~"\\b(?:ALTER|CREATE|DROP)\\b", [caseless, {capture, none}]) =:= match.

names_table(Stmt, Table) ->
    re:run(Stmt, word(Table), [caseless, {capture, none}]) =:= match.

%% Escaped byte by byte rather than with \Q...\E, which a name holding
%% `\E` would end early.
word(Name) ->
    <<"(?<![A-Za-z0-9_$])", <<<<(escape(B))/binary>> || <<B>> <= Name>>/binary,
        "(?![A-Za-z0-9_$])">>.

escape(B) when B >= $a, B =< $z; B >= $A, B =< $Z; B >= $0, B =< $9; B >= 128 -> <<B>>;
escape(B) -> <<$\\, B>>.

%% An ON DELETE/ON UPDATE kura can express is recorded wherever it sits in the
%% tail. Anything else (DEFERRABLE, SET DEFAULT, MATCH FULL, ...) also marks the
%% constraint as hand-written: the diff leaves it alone while the schema agrees
%% with it, and refuses to regenerate it otherwise.
parse_fk_actions(Tail) ->
    fk_actions(words(Tail), #{}, owned).

fk_actions([], Acc, Ownership) ->
    {Ownership, Acc};
fk_actions([~"ON", Event | Rest], Acc, Ownership) when
    Event =:= ~"DELETE"; Event =:= ~"UPDATE"
->
    Key = fk_event(Event),
    case {maps:is_key(Key, Acc), referential_action(Rest)} of
        {false, {ok, Action, Rest1}} -> fk_actions(Rest1, Acc#{Key => Action}, Ownership);
        _ -> fk_actions(Rest, Acc, unowned)
    end;
fk_actions([_Other | Rest], Acc, _Ownership) ->
    fk_actions(Rest, Acc, unowned).

fk_event(~"DELETE") -> on_delete;
fk_event(~"UPDATE") -> on_update.

referential_action([~"CASCADE" | Rest]) -> {ok, cascade, Rest};
referential_action([~"RESTRICT" | Rest]) -> {ok, restrict, Rest};
referential_action([~"SET", ~"NULL" | Rest]) -> {ok, set_null, Rest};
referential_action([~"NO", ~"ACTION" | Rest]) -> {ok, no_action, Rest};
referential_action(_) -> error.

add_fk(Table, Col, Refs, Name, {Ownership, Parsed}, State) ->
    OnDelete = maps:get(on_delete, Parsed, undefined),
    OnUpdate = maps:get(on_update, Parsed, undefined),
    Unowned = Ownership =:= unowned,
    State1 = update_col(Table, Col, {fk, Refs, OnDelete, OnUpdate}, State),
    State2 = put_fk_meta(fk_names, {Table, Col}, name_if_custom(Table, Col, Name), State1),
    put_fk_meta(fk_unowned, {Table, Col}, Unowned, State2).

name_if_custom(Table, Col, Name) ->
    case fk_constraint_name(Table, Col) of
        Name -> undefined;
        _ -> Name
    end.

put_fk_meta(Key, ColKey, Value, State) when Value =:= undefined; Value =:= false ->
    State#{Key => maps:remove(ColKey, maps:get(Key, State, #{}))};
put_fk_meta(Key, ColKey, Value, State) ->
    State#{Key => (maps:get(Key, State, #{}))#{ColKey => Value}}.

fk_name(FkNames, Table, Col) ->
    maps:get({Table, Col}, FkNames, fk_constraint_name(Table, Col)).

drop_fk(Table, Constraint, #{columns := ColState} = State) ->
    FkNames = maps:get(fk_names, State, #{}),
    Cols = maps:get(Table, ColState, []),
    case
        [
            C#kura_column.name
         || C <- Cols,
            C#kura_column.references =/= undefined,
            fk_name(FkNames, Table, C#kura_column.name) =:= Constraint
        ]
    of
        [Col | _] ->
            State1 = update_col(Table, Col, {fk, undefined, undefined, undefined}, State),
            State2 = put_fk_meta(fk_names, {Table, Col}, undefined, State1),
            {ok, put_fk_meta(fk_unowned, {Table, Col}, false, State2)};
        [] ->
            nomatch
    end.

parse_action(Action) ->
    case re:run(Action, ~"(?is)^SET\\s+DEFAULT\\s+(.+)$", [{capture, all_but_first, binary}]) of
        {match, [Value]} ->
            case parse_default_value(Value) of
                {ok, Default} -> {ok, {default, Default}};
                error -> nomatch
            end;
        nomatch ->
            parse_fixed_action(words(Action))
    end.

parse_fixed_action([~"SET", ~"NOT", ~"NULL"]) -> {ok, {nullable, false}};
parse_fixed_action([~"DROP", ~"NOT", ~"NULL"]) -> {ok, {nullable, true}};
parse_fixed_action([~"DROP", ~"DEFAULT"]) -> {ok, {default, undefined}};
parse_fixed_action(_) -> nomatch.

%% Only a literal is read. An expression's value is the database's to decide,
%% so recording its text would compare a string with whatever the schema says.
parse_default_value(Bin) ->
    case ascii_upper(Bin) of
        ~"TRUE" -> {ok, true};
        ~"FALSE" -> {ok, false};
        ~"NULL" -> {ok, undefined};
        _ -> parse_literal(Bin)
    end.

parse_literal(Bin) ->
    case re:run(Bin, ~"(?s)^'((?:[^']|'')*)'$", [{capture, all_but_first, binary}]) of
        {match, [Inner]} -> {ok, binary:replace(Inner, ~"''", ~"'", [global])};
        nomatch -> parse_number(Bin)
    end.

%% The forms format_default/1 writes. An exponent is left to the database,
%% which reads 1e5 as numeric where kura would have to guess integer or float.
parse_number(Bin) ->
    case re:run(Bin, ~"^-?[0-9]+(\\.[0-9]+)?$", [{capture, all_but_first, list}]) of
        {match, []} -> {ok, binary_to_integer(Bin)};
        {match, [_]} -> {ok, binary_to_float(Bin)};
        nomatch -> error
    end.

%% SQL keywords are ASCII. string:uppercase/1 would also raise on a statement
%% that is not valid UTF-8, such as Latin-1 seed data.
words(Bin) ->
    binary:split(ascii_upper(Bin), [~" ", ~"\t", ~"\r", ~"\n"], [global, trim_all]).

ascii_upper(Bin) ->
    <<<<(upper(B))>> || <<B>> <= Bin>>.

upper(B) when B >= $a, B =< $z -> B - 32;
upper(B) -> B.

update_col(Table, ColName, Change, #{columns := ColState} = State) ->
    case ColState of
        #{Table := Cols} ->
            NewCols = [change_col(C, ColName, Change) || C <- Cols],
            State#{columns => ColState#{Table => NewCols}};
        #{} ->
            State
    end.

change_col(#kura_column{name = Name} = C, Name, {nullable, Val}) ->
    C#kura_column{nullable = Val};
change_col(#kura_column{name = Name} = C, Name, {default, Val}) ->
    C#kura_column{default = Val};
change_col(#kura_column{name = Name} = C, Name, {fk, Refs, OnDelete, OnUpdate}) ->
    C#kura_column{references = Refs, on_delete = OnDelete, on_update = OnUpdate};
change_col(C, _Name, _Change) ->
    C.
