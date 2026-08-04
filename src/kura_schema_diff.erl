-module(kura_schema_diff).

-include_lib("kura/include/kura.hrl").

-export([
    build_db_state/1,
    build_desired_state/1,
    diff/2,
    field_to_column/1,
    format_error/1
]).

-type col_state() :: #{binary() => [#kura_column{}]}.
-type index_entry() :: {[atom()], map()}.
-type index_state() :: #{binary() => [index_entry()]}.
-type db_state() :: #{columns => col_state(), indexes => index_state()}.
-type operation() ::
    {create_table, binary(), [#kura_column{}]}
    | {drop_table, binary()}
    | {alter_table, binary(), [alter_op()]}
    | {create_index, binary(), [atom()], map()}
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
        #{columns => #{}, indexes => #{}},
        Sorted
    ).

%% Convert schema modules to desired state (columns + indexes)
-spec build_desired_state([module()]) -> db_state().
build_desired_state(SchemaModules) ->
    lists:foldl(
        fun(Mod, #{columns := ColAcc, indexes := IdxAcc} = _Acc) ->
            Table = Mod:table(),
            Fields = Mod:fields(),
            Columns = [field_to_column(F) || F <- Fields, F#kura_field.virtual =/= true],
            Enriched = enrich_with_associations(Mod, Columns),
            Indexes = extract_indexes(Mod),
            #{columns => ColAcc#{Table => Enriched}, indexes => IdxAcc#{Table => Indexes}}
        end,
        #{columns => #{}, indexes => #{}},
        SchemaModules
    ).

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
-spec diff(db_state(), db_state()) -> {[operation()], [operation()]}.
diff(DbState, DesiredState) ->
    DbCols = maps:get(columns, ensure_structured(DbState), #{}),
    DesiredCols = maps:get(columns, ensure_structured(DesiredState), #{}),
    DbIdx = maps:get(indexes, ensure_structured(DbState), #{}),
    DesiredIdx = maps:get(indexes, ensure_structured(DesiredState), #{}),

    %% New tables: in desired but not in DB
    NewTables = maps:keys(DesiredCols) -- maps:keys(DbCols),
    {CreateUp, CreateDown} = lists:foldl(
        fun(Table, {UpAcc, DownAcc}) ->
            Cols = maps:get(Table, DesiredCols),
            {UpAcc ++ [{create_table, Table, Cols}], DownAcc ++ [{drop_table, Table}]}
        end,
        {[], []},
        lists:sort(NewTables)
    ),

    %% Existing tables: diff columns
    ExistingTables = maps:keys(DesiredCols) -- NewTables,
    {AlterUp, AlterDown, ExecUp, ExecDown} = lists:foldl(
        fun(Table, {AUpAcc, ADownAcc, EUpAcc, EDownAcc}) ->
            DbTableCols = maps:get(Table, DbCols, []),
            DesiredTableCols = maps:get(Table, DesiredCols),
            case diff_columns(Table, DbTableCols, DesiredTableCols) of
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
    {IdxUp, IdxDown} = lists:foldl(
        fun(Table, {IUpAcc, IDownAcc}) ->
            DbTableIdx = maps:get(Table, DbIdx, []),
            DesiredTableIdx = maps:get(Table, DesiredIdx, []),
            {IU, ID} = diff_indexes(Table, DbTableIdx, DesiredTableIdx),
            {IUpAcc ++ IU, IDownAcc ++ ID}
        end,
        {[], []},
        lists:sort(AllDesiredTables)
    ),

    {CreateUp ++ AlterUp ++ ExecUp ++ IdxUp, CreateDown ++ AlterDown ++ ExecDown ++ IdxDown}.

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
            Assocs = Mod:associations(),
            %% Validate every association, not just the ones that own a
            %% column: this is what turns an on_delete declared on a
            %% has_many into a named failure instead of a no-op.
            lists:foreach(fun(A) -> assoc_on_delete(Mod, A) end, Assocs),
            BelongsTo = [A || A <- Assocs, A#kura_assoc.type =:= belongs_to],
            lists:foldl(fun enrich_column/2, Columns, [{Mod, A} || A <- BelongsTo])
    end.

enrich_column({Mod, Assoc}, Columns) ->
    case kura_schema:assoc_fields(Assoc) of
        [] ->
            Columns;
        [FK] ->
            Target = resolve_target(Mod, Assoc),
            Refs = {Target:table(), find_primary_key(Target:fields())},
            OnDelete = assoc_on_delete(Mod, Assoc),
            enrich_fk_column(Mod, Assoc, FK, Refs, OnDelete, Columns);
        _Composite ->
            %% A composite foreign key is a table-level constraint, which the
            %% generator does not emit yet. Say so rather than emit a partial
            %% one-column constraint that looks right.
            error(
                {kura_schema_diff, {composite_assoc_unsupported, Mod, Assoc#kura_assoc.name}}
            )
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
format_error({assoc_key_not_a_field, Mod, Name, FK}) ->
    io_lib:format(
        "~s: association '~s' names foreign key '~s', which is not a field on ~s",
        [Mod, Name, FK, Mod]
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
format_error({on_delete_not_owned, Mod, Name, Type}) ->
    io_lib:format(
        "~s: association '~s' is a ~s and owns no foreign-key column, so its "
        "on_delete would never reach the database. Declare it on the belongs_to "
        "on the other side",
        [Mod, Name, Type]
    );
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

find_primary_key(Fields) ->
    case [F#kura_field.name || F <- Fields, F#kura_field.primary_key =:= true] of
        [PK | _] -> PK;
        [] -> id
    end.

%%% Internal

apply_ops([], State) ->
    State;
apply_ops([{create_table, Name, Cols} | Rest], #{columns := ColState} = State) ->
    apply_ops(Rest, State#{columns => ColState#{Name => Cols}});
apply_ops([{drop_table, Name} | Rest], #{columns := ColState, indexes := IdxState} = State) ->
    apply_ops(Rest, State#{
        columns => maps:remove(Name, ColState),
        indexes => maps:remove(Name, IdxState)
    });
apply_ops([{alter_table, Name, AlterOps} | Rest], #{columns := ColState} = State) ->
    Cols = maps:get(Name, ColState, []),
    NewCols = apply_alter_ops(AlterOps, Cols),
    apply_ops(Rest, State#{columns => ColState#{Name => NewCols}});
apply_ops([{create_index, Table, Columns, Opts} | Rest], #{indexes := IdxState} = State) ->
    TableIdx = maps:get(Table, IdxState, []),
    Entry = {Columns, Opts},
    apply_ops(Rest, State#{indexes => IdxState#{Table => TableIdx ++ [Entry]}});
apply_ops([{create_index, _Name, Table, Columns, Opts} | Rest], #{indexes := IdxState} = State) ->
    TableIdx = maps:get(Table, IdxState, []),
    OptsMap = proplist_to_map(Opts),
    Entry = {Columns, OptsMap},
    apply_ops(Rest, State#{indexes => IdxState#{Table => TableIdx ++ [Entry]}});
apply_ops([{drop_index, IdxName} | Rest], #{indexes := IdxState} = State) ->
    %% Find and remove the index by its generated name
    NewIdxState = maps:map(
        fun(Table, Entries) ->
            [E || {Cols, _} = E <- Entries, index_name(Table, Cols) =/= IdxName]
        end,
        IdxState
    ),
    apply_ops(Rest, State#{indexes => NewIdxState});
apply_ops([{execute, SQL} | Rest], #{columns := ColState} = State) ->
    apply_ops(Rest, State#{columns => try_apply_execute(SQL, ColState)});
apply_ops([_Other | Rest], State) ->
    apply_ops(Rest, State).

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
    DroppedIdx = DbSet -- DesiredSet,
    DropUp = [{drop_index, index_name(Table, Cols)} || {Cols, _} <- DroppedIdx],
    DropDown = [{create_index, Table, Cols, Opts} || {Cols, Opts} <- DroppedIdx],
    {CreateUp ++ DropUp, CreateDown ++ DropDown}.

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

diff_columns(Table, DbCols, DesiredCols) ->
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
                case DbCol#kura_column.nullable =:= DesCol#kura_column.nullable of
                    true ->
                        {EU, ED};
                    false ->
                        case DesCol#kura_column.nullable of
                            true ->
                                {
                                    EU ++
                                        [
                                            {execute,
                                                <<"ALTER TABLE \"", Table/binary,
                                                    "\" ALTER COLUMN \"", ColBin/binary,
                                                    "\" DROP NOT NULL">>}
                                        ],
                                    ED ++
                                        [
                                            {execute,
                                                <<"ALTER TABLE \"", Table/binary,
                                                    "\" ALTER COLUMN \"", ColBin/binary,
                                                    "\" SET NOT NULL">>}
                                        ]
                                };
                            false ->
                                {
                                    EU ++
                                        [
                                            {execute,
                                                <<"ALTER TABLE \"", Table/binary,
                                                    "\" ALTER COLUMN \"", ColBin/binary,
                                                    "\" SET NOT NULL">>}
                                        ],
                                    ED ++
                                        [
                                            {execute,
                                                <<"ALTER TABLE \"", Table/binary,
                                                    "\" ALTER COLUMN \"", ColBin/binary,
                                                    "\" DROP NOT NULL">>}
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
            {MU2, MD2, EU3, ED3}
        end,
        {[], [], [], []},
        lists:sort(Common)
    ),

    {AddUp ++ DropUp ++ ModUp, AddDown ++ DropDown ++ ModDown, ExecUp, ExecDown}.

default_sql(Table, ColBin, undefined) ->
    <<"ALTER TABLE \"", Table/binary, "\" ALTER COLUMN \"", ColBin/binary, "\" DROP DEFAULT">>;
default_sql(Table, ColBin, Val) ->
    ValBin = format_default(Val),
    <<"ALTER TABLE \"", Table/binary, "\" ALTER COLUMN \"", ColBin/binary, "\" SET DEFAULT ",
        ValBin/binary>>.

format_default(true) -> <<"true">>;
format_default(false) -> <<"false">>;
format_default(V) when is_integer(V) -> integer_to_binary(V);
format_default(V) when is_float(V) -> float_to_binary(V, [{decimals, 10}, compact]);
format_default(V) when is_binary(V) -> <<"'", V/binary, "'">>;
format_default(V) -> list_to_binary(io_lib:format("~p", [V])).

types_equal({enum, _}, {enum, _}) -> true;
types_equal(A, B) -> A =:= B.

col_map(Cols) ->
    maps:from_list([{C#kura_column.name, C} || C <- Cols]).

%% Parse known SQL patterns from {execute, SQL} ops to update column state
-spec try_apply_execute(binary(), col_state()) -> col_state().
try_apply_execute(SQL, State) ->
    case
        re:run(
            SQL,
            <<"^ALTER TABLE \"([^\"]+)\" ALTER COLUMN \"([^\"]+)\" (.+)$">>,
            [{capture, all_but_first, binary}]
        )
    of
        {match, [Table, Col, Action]} ->
            ColAtom = binary_to_atom(Col, utf8),
            case parse_action(Action) of
                {ok, Parsed} -> update_col(Table, ColAtom, Parsed, State);
                nomatch -> State
            end;
        nomatch ->
            State
    end.

parse_action(<<"SET NOT NULL">>) -> {ok, {nullable, false}};
parse_action(<<"DROP NOT NULL">>) -> {ok, {nullable, true}};
parse_action(<<"DROP DEFAULT">>) -> {ok, {default, undefined}};
parse_action(<<"SET DEFAULT ", ValBin/binary>>) -> {ok, {default, parse_default_value(ValBin)}};
parse_action(_) -> nomatch.

parse_default_value(<<"true">>) ->
    true;
parse_default_value(<<"false">>) ->
    false;
parse_default_value(<<"'", Rest/binary>>) ->
    binary:part(Rest, 0, byte_size(Rest) - 1);
parse_default_value(Bin) ->
    case binary:match(Bin, <<".">>) of
        nomatch ->
            try
                binary_to_integer(Bin)
            catch
                _:_ -> Bin
            end;
        _ ->
            try
                binary_to_float(Bin)
            catch
                _:_ -> Bin
            end
    end.

update_col(Table, ColName, {nullable, Val}, State) ->
    case maps:find(Table, State) of
        {ok, Cols} ->
            NewCols = [
                case C#kura_column.name of
                    ColName -> C#kura_column{nullable = Val};
                    _ -> C
                end
             || C <- Cols
            ],
            State#{Table => NewCols};
        error ->
            State
    end;
update_col(Table, ColName, {default, Val}, State) ->
    case maps:find(Table, State) of
        {ok, Cols} ->
            NewCols = [
                case C#kura_column.name of
                    ColName -> C#kura_column{default = Val};
                    _ -> C
                end
             || C <- Cols
            ],
            State#{Table => NewCols};
        error ->
            State
    end.
