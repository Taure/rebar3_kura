-module(kura_schema_diff_prop_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

%%====================================================================
%% EUnit wrapper for PropEr tests
%%====================================================================

proper_test_() ->
    {timeout, 60, [
        ?_assert(
            proper:quickcheck(prop_diff_empty_when_identical(), [{to_file, user}, {numtests, 100}])
        ),
        ?_assert(
            proper:quickcheck(prop_diff_detects_all_added_columns(), [
                {to_file, user}, {numtests, 100}
            ])
        ),
        ?_assert(
            proper:quickcheck(prop_diff_detects_all_dropped_columns(), [
                {to_file, user}, {numtests, 100}
            ])
        ),
        ?_assert(proper:quickcheck(prop_up_down_reversible(), [{to_file, user}, {numtests, 100}])),
        ?_assert(
            proper:quickcheck(prop_new_table_creates_all_columns(), [
                {to_file, user}, {numtests, 100}
            ])
        ),
        ?_assert(proper:quickcheck(prop_diff_op_count(), [{to_file, user}, {numtests, 100}])),
        ?_assert(
            proper:quickcheck(prop_field_to_column_preserves_type(), [
                {to_file, user}, {numtests, 200}
            ])
        ),
        ?_assert(
            proper:quickcheck(prop_build_db_state_idempotent(), [{to_file, user}, {numtests, 50}])
        )
    ]}.

-type col_state() :: #{binary() => [#kura_column{}]}.

%%====================================================================
%% Generators
%%====================================================================

kura_type() ->
    oneof([
        id,
        integer,
        float,
        string,
        text,
        boolean,
        date,
        utc_datetime,
        uuid,
        jsonb,
        {array, oneof([string, integer, boolean])}
    ]).

column_name() ->
    oneof([
        id,
        name,
        email,
        age,
        weight,
        title,
        body,
        active,
        score,
        tags,
        color,
        status,
        role,
        bio,
        url,
        count,
        price,
        qty,
        ref,
        code
    ]).

kura_column() ->
    ?LET(
        {Name, Type},
        {column_name(), kura_type()},
        column(Name, Type)
    ).

%% Generate a list of columns with unique names
unique_columns() ->
    ?LET(
        Cols,
        non_empty(list(kura_column())),
        unique_by_name(Cols)
    ).

table_name() ->
    oneof([
        <<"users">>,
        <<"posts">>,
        <<"items">>,
        <<"tags">>,
        <<"pets">>,
        <<"orders">>,
        <<"logs">>,
        <<"events">>
    ]).

%% Generate a db_state with 1-3 tables
db_state() ->
    ?LET(
        Tables,
        non_empty(list({table_name(), unique_columns()})),
        maps:from_list(unique_tables(Tables))
    ).

kura_field() ->
    ?LET(
        {Name, Type, Virtual},
        {column_name(), kura_type(), boolean()},
        field(Name, Type, Virtual)
    ).

%%====================================================================
%% Properties
%%====================================================================

%% Diffing identical states produces no ops
prop_diff_empty_when_identical() ->
    ?FORALL(State, db_state(), identical_diff_is_empty(State)).

%% All columns added in desired but not in DB are detected
prop_diff_detects_all_added_columns() ->
    ?FORALL(
        {Table, BaseCols, ExtraCols},
        {table_name(), unique_columns(), unique_columns()},
        added_columns_detected(Table, BaseCols, ExtraCols)
    ).

%% All columns dropped from desired are detected
prop_diff_detects_all_dropped_columns() ->
    ?FORALL(
        {Table, AllCols},
        {table_name(), unique_columns()},
        dropped_columns_detected(Table, AllCols)
    ).

%% Applying up then down returns to original state
prop_up_down_reversible() ->
    ?FORALL({Db, Desired}, {db_state(), db_state()}, up_down_reversible(Db, Desired)).

%% New tables get create_table with all columns
prop_new_table_creates_all_columns() ->
    ?FORALL(
        {Table, Cols},
        {table_name(), unique_columns()},
        new_table_creates_all_columns(Table, Cols)
    ).

%% Number of alter ops matches the actual changes
prop_diff_op_count() ->
    ?FORALL(
        {Table, DbCols, DesiredCols},
        {table_name(), unique_columns(), unique_columns()},
        op_count_matches_changes(Table, DbCols, DesiredCols)
    ).

%% field_to_column always preserves the type
prop_field_to_column_preserves_type() ->
    ?FORALL(
        {Name, Type},
        {column_name(), kura_type()},
        begin
            Col = kura_schema_diff:field_to_column(field(Name, Type, false)),
            Col#kura_column.name =:= Name andalso Col#kura_column.type =:= Type
        end
    ).

%% build_db_state is deterministic — same input always gives same output
prop_build_db_state_idempotent() ->
    ?FORALL(
        {Table, Cols},
        {table_name(), unique_columns()},
        begin
            meck:new(prop_mig, [non_strict]),
            meck:expect(prop_mig, up, fun() ->
                [{create_table, Table, Cols}]
            end),
            S1 = kura_schema_diff:build_db_state([prop_mig]),
            S2 = kura_schema_diff:build_db_state([prop_mig]),
            meck:unload(prop_mig),
            S1 =:= S2
        end
    ).

%%====================================================================
%% Property bodies
%%
%% PropEr passes generated values to a property as untyped instances, so
%% each body takes them as dynamic().
%%====================================================================

-spec identical_diff_is_empty(dynamic()) -> boolean().
identical_diff_is_empty(State) ->
    {Up, Down} = kura_schema_diff:diff(State, State),
    Up =:= [] andalso Down =:= [].

-spec added_columns_detected(dynamic(), dynamic(), dynamic()) -> boolean().
added_columns_detected(Table, BaseCols, ExtraCols) ->
    Base = unique_by_name(BaseCols),
    Extra = [
        C
     || C <- ExtraCols, not lists:keymember(C#kura_column.name, #kura_column.name, Base)
    ],
    Db = #{Table => Base},
    Desired = #{Table => Base ++ Extra},
    {UpOps, _} = kura_schema_diff:diff(Db, Desired),
    case Extra of
        [] ->
            UpOps =:= [];
        _ ->
            AddedNames = extract_added_names(UpOps),
            ExpectedNames = lists:sort([C#kura_column.name || C <- Extra]),
            lists:sort(AddedNames) =:= ExpectedNames
    end.

-spec dropped_columns_detected(dynamic(), dynamic()) -> proper:test().
dropped_columns_detected(Table, AllCols) ->
    ?IMPLIES(
        length(AllCols) >= 2,
        begin
            {Keep, Drop} = lists:split(length(AllCols) div 2, AllCols),
            Db = #{Table => AllCols},
            Desired = #{Table => Keep},
            {UpOps, _} = kura_schema_diff:diff(Db, Desired),
            DroppedNames = extract_dropped_names(UpOps),
            ExpectedDropped = lists:sort([C#kura_column.name || C <- Drop]),
            lists:sort(DroppedNames) =:= ExpectedDropped
        end
    ).

-spec up_down_reversible(dynamic(), dynamic()) -> boolean().
up_down_reversible(Db, Desired) ->
    %% Only test tables that exist in desired (we don't generate drop_table)
    FilteredDb = maps:with(maps:keys(Desired), Db),
    {UpOps, DownOps} = kura_schema_diff:diff(FilteredDb, Desired),
    AfterUp = apply_ops(UpOps, FilteredDb),
    AfterDown = apply_ops(DownOps, AfterUp),
    normalize_state(FilteredDb) =:= normalize_state(AfterDown).

-spec new_table_creates_all_columns(dynamic(), dynamic()) -> boolean().
new_table_creates_all_columns(Table, Cols) ->
    {[{create_table, Table, Created}], [{drop_table, Table}]} =
        kura_schema_diff:diff(#{}, #{Table => Cols}),
    Created =:= Cols.

-spec op_count_matches_changes(dynamic(), dynamic(), dynamic()) -> boolean().
op_count_matches_changes(Table, DbCols, DesiredCols) ->
    Db = #{Table => DbCols},
    Desired = #{Table => DesiredCols},
    {UpOps, _} = kura_schema_diff:diff(Db, Desired),
    DbNames = [C#kura_column.name || C <- DbCols],
    DesNames = [C#kura_column.name || C <- DesiredCols],
    Added = length(DesNames -- DbNames),
    Dropped = length(DbNames -- DesNames),
    Common = DesNames -- (DesNames -- DbNames),
    DbMap = maps:from_list([{C#kura_column.name, C#kura_column.type} || C <- DbCols]),
    DesMap = maps:from_list([{C#kura_column.name, C#kura_column.type} || C <- DesiredCols]),
    Modified = length([N || N <- Common, maps:get(N, DbMap) =/= maps:get(N, DesMap)]),
    TotalChanges = Added + Dropped + Modified,
    case TotalChanges of
        0 ->
            UpOps =:= [];
        _ ->
            [{alter_table, Table, Ops}] = UpOps,
            length(Ops) =:= TotalChanges
    end.

%%====================================================================
%% Helpers
%%====================================================================

%% Generated values reach these as untyped PropEr instances.
-spec column(dynamic(), dynamic()) -> #kura_column{}.
column(Name, Type) ->
    #kura_column{name = Name, type = Type}.

-spec field(dynamic(), dynamic(), dynamic()) -> #kura_field{}.
field(Name, Type, Virtual) ->
    #kura_field{name = Name, type = Type, virtual = Virtual}.

-spec unique_by_name(dynamic()) -> [#kura_column{}].
unique_by_name(Cols) ->
    unique_by_name(Cols, []).

unique_by_name([], Acc) ->
    Acc;
unique_by_name([C | Rest], Acc) ->
    case lists:keymember(C#kura_column.name, #kura_column.name, Acc) of
        true -> unique_by_name(Rest, Acc);
        false -> unique_by_name(Rest, Acc ++ [C])
    end.

-spec unique_tables(dynamic()) -> [{binary(), [#kura_column{}]}].
unique_tables(Tables) ->
    unique_tables(Tables, []).

unique_tables([], Acc) ->
    Acc;
unique_tables([{Name, Cols} | Rest], Acc) ->
    case lists:keymember(Name, 1, Acc) of
        true -> unique_tables(Rest, Acc);
        false -> unique_tables(Rest, Acc ++ [{Name, Cols}])
    end.

-spec extract_added_names([kura_schema_diff:operation()]) -> [atom()].
extract_added_names(Ops) ->
    lists:flatmap(
        fun
            ({alter_table, _, AlterOps}) ->
                [N || {add_column, #kura_column{name = N}} <- AlterOps];
            (_) ->
                []
        end,
        Ops
    ).

-spec extract_dropped_names([kura_schema_diff:operation()]) -> [atom()].
extract_dropped_names(Ops) ->
    lists:flatmap(
        fun
            ({alter_table, _, AlterOps}) ->
                [N || {drop_column, N} <- AlterOps];
            (_) ->
                []
        end,
        Ops
    ).

-spec apply_ops([kura_schema_diff:operation()], col_state()) -> col_state().
apply_ops([], State) ->
    State;
apply_ops([{create_table, Name, Cols} | Rest], State) ->
    apply_ops(Rest, State#{Name => Cols});
apply_ops([{drop_table, Name} | Rest], State) ->
    apply_ops(Rest, maps:remove(Name, State));
apply_ops([{alter_table, Name, AlterOps} | Rest], State) ->
    Cols = maps:get(Name, State, []),
    NewCols = apply_alter_ops(AlterOps, Cols),
    apply_ops(Rest, State#{Name => NewCols}).

-spec apply_alter_ops([kura_schema_diff:alter_op()], [#kura_column{}]) -> [#kura_column{}].
apply_alter_ops([], Cols) ->
    Cols;
apply_alter_ops([{add_column, Col} | Rest], Cols) ->
    apply_alter_ops(Rest, Cols ++ [Col]);
apply_alter_ops([{drop_column, Name} | Rest], Cols) ->
    apply_alter_ops(Rest, [C || C <- Cols, C#kura_column.name =/= Name]);
apply_alter_ops([{modify_column, Name, Type} | Rest], Cols) ->
    NewCols = [
        case C#kura_column.name of
            Name -> C#kura_column{type = Type};
            _ -> C
        end
     || C <- Cols
    ],
    apply_alter_ops(Rest, NewCols).

%% Normalize state for comparison (sort columns by name)
-spec normalize_state(col_state()) -> col_state().
normalize_state(State) ->
    maps:map(
        fun(_K, Cols) ->
            lists:sort(fun(A, B) -> A#kura_column.name =< B#kura_column.name end, Cols)
        end,
        State
    ).
