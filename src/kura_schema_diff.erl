-module(kura_schema_diff).

-include_lib("kura/include/kura.hrl").

-export([
    build_db_state/1,
    build_desired_state/1,
    diff/2,
    field_to_column/1
]).

-type db_state() :: #{binary() => [#kura_column{}]}.
-type operation() ::
    {create_table, binary(), [#kura_column{}]}
    | {drop_table, binary()}
    | {alter_table, binary(), [alter_op()]}
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
        #{},
        Sorted
    ).

%% Convert schema modules to desired column state
-spec build_desired_state([module()]) -> db_state().
build_desired_state(SchemaModules) ->
    lists:foldl(
        fun(Mod, Acc) ->
            Table = Mod:table(),
            Fields = Mod:fields(),
            Columns = [field_to_column(F) || F <- Fields, F#kura_field.virtual =/= true],
            Enriched = enrich_with_associations(Mod, Columns),
            Acc#{Table => Enriched}
        end,
        #{},
        SchemaModules
    ).

%% Diff DB state against desired state, returning {UpOps, DownOps}
-spec diff(db_state(), db_state()) -> {[operation()], [operation()]}.
diff(DbState, DesiredState) ->
    %% New tables: in desired but not in DB
    NewTables = maps:keys(DesiredState) -- maps:keys(DbState),
    {CreateUp, CreateDown} = lists:foldl(
        fun(Table, {UpAcc, DownAcc}) ->
            Cols = maps:get(Table, DesiredState),
            {UpAcc ++ [{create_table, Table, Cols}], DownAcc ++ [{drop_table, Table}]}
        end,
        {[], []},
        lists:sort(NewTables)
    ),

    %% Existing tables: diff columns
    ExistingTables = maps:keys(DesiredState) -- NewTables,
    {AlterUp, AlterDown, ExecUp, ExecDown} = lists:foldl(
        fun(Table, {AUpAcc, ADownAcc, EUpAcc, EDownAcc}) ->
            DbCols = maps:get(Table, DbState, []),
            DesiredCols = maps:get(Table, DesiredState),
            case diff_columns(Table, DbCols, DesiredCols) of
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

    {CreateUp ++ AlterUp ++ ExecUp, CreateDown ++ AlterDown ++ ExecDown}.

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
            try
                Assocs = Mod:associations(),
                BelongsTo = [A || A <- Assocs, A#kura_assoc.type =:= belongs_to],
                lists:foldl(fun enrich_column/2, Columns, BelongsTo)
            catch
                _:_ -> Columns
            end
    end.

enrich_column(#kura_assoc{foreign_key = FK, schema = TargetSchema}, Columns) ->
    case FK of
        undefined ->
            Columns;
        _ ->
            try
                TargetTable = TargetSchema:table(),
                TargetFields = TargetSchema:fields(),
                TargetPK = find_primary_key(TargetFields),
                [
                    case C#kura_column.name of
                        FK when C#kura_column.references =:= undefined ->
                            C#kura_column{
                                references = {TargetTable, TargetPK},
                                on_delete = no_action
                            };
                        _ ->
                            C
                    end
                 || C <- Columns
                ]
            catch
                _:_ -> Columns
            end
    end.

find_primary_key(Fields) ->
    case [F#kura_field.name || F <- Fields, F#kura_field.primary_key =:= true] of
        [PK | _] -> PK;
        [] -> id
    end.

%%% Internal

apply_ops([], State) ->
    State;
apply_ops([{create_table, Name, Cols} | Rest], State) ->
    apply_ops(Rest, State#{Name => Cols});
apply_ops([{drop_table, Name} | Rest], State) ->
    apply_ops(Rest, maps:remove(Name, State));
apply_ops([{alter_table, Name, AlterOps} | Rest], State) ->
    Cols = maps:get(Name, State, []),
    NewCols = apply_alter_ops(AlterOps, Cols),
    apply_ops(Rest, State#{Name => NewCols});
apply_ops([{execute, SQL} | Rest], State) ->
    apply_ops(Rest, try_apply_execute(SQL, State));
apply_ops([_Other | Rest], State) ->
    %% Skip create_index, drop_index, etc.
    apply_ops(Rest, State).

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
