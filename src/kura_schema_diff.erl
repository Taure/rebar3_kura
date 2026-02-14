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
    | {alter_table, binary(), [alter_op()]}.
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
            Acc#{Table => Columns}
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
    {AlterUp, AlterDown} = lists:foldl(
        fun(Table, {UpAcc, DownAcc}) ->
            DbCols = maps:get(Table, DbState, []),
            DesiredCols = maps:get(Table, DesiredState),
            case diff_columns(DbCols, DesiredCols) of
                {[], []} ->
                    {UpAcc, DownAcc};
                {ColUp, ColDown} ->
                    {
                        UpAcc ++ [{alter_table, Table, ColUp}],
                        DownAcc ++ [{alter_table, Table, ColDown}]
                    }
            end
        end,
        {[], []},
        lists:sort(ExistingTables)
    ),

    {CreateUp ++ AlterUp, CreateDown ++ AlterDown}.

%% Convert a kura_field to a kura_column (skipping virtual fields)
-spec field_to_column(#kura_field{}) -> #kura_column{}.
field_to_column(#kura_field{name = N, type = T, nullable = Null, default = Def, primary_key = PK}) ->
    #kura_column{name = N, type = T, nullable = Null, default = Def, primary_key = PK}.

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
apply_ops([_Other | Rest], State) ->
    %% Skip create_index, drop_index, execute, etc.
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

diff_columns(DbCols, DesiredCols) ->
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

    %% Type changes on existing columns
    Common = DesiredNames -- Added,
    {ModUp, ModDown} = lists:foldl(
        fun(Name, {MU, MD}) ->
            DbCol = maps:get(Name, DbMap),
            DesCol = maps:get(Name, DesiredMap),
            case DbCol#kura_column.type =:= DesCol#kura_column.type of
                true ->
                    {MU, MD};
                false ->
                    {
                        MU ++ [{modify_column, Name, DesCol#kura_column.type}],
                        MD ++ [{modify_column, Name, DbCol#kura_column.type}]
                    }
            end
        end,
        {[], []},
        lists:sort(Common)
    ),

    {AddUp ++ DropUp ++ ModUp, AddDown ++ DropDown ++ ModDown}.

col_map(Cols) ->
    maps:from_list([{C#kura_column.name, C} || C <- Cols]).
