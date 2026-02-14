-module(kura_schema_diff_prop_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

%%====================================================================
%% EUnit wrapper for PropEr tests
%%====================================================================

proper_test_() ->
    {timeout, 60, [
        ?_assert(proper:quickcheck(prop_diff_empty_when_identical(), [{to_file, user}, {numtests, 100}])),
        ?_assert(proper:quickcheck(prop_diff_detects_all_added_columns(), [{to_file, user}, {numtests, 100}])),
        ?_assert(proper:quickcheck(prop_diff_detects_all_dropped_columns(), [{to_file, user}, {numtests, 100}])),
        ?_assert(proper:quickcheck(prop_up_down_reversible(), [{to_file, user}, {numtests, 100}])),
        ?_assert(proper:quickcheck(prop_new_table_creates_all_columns(), [{to_file, user}, {numtests, 100}])),
        ?_assert(proper:quickcheck(prop_diff_op_count(), [{to_file, user}, {numtests, 100}])),
        ?_assert(proper:quickcheck(prop_field_to_column_preserves_type(), [{to_file, user}, {numtests, 200}])),
        ?_assert(proper:quickcheck(prop_build_db_state_idempotent(), [{to_file, user}, {numtests, 50}]))
    ]}.

%%====================================================================
%% Generators
%%====================================================================

kura_type() ->
    oneof([id, integer, float, string, text, boolean, date, utc_datetime, uuid, jsonb,
           {array, oneof([string, integer, boolean])}]).

column_name() ->
    oneof([id, name, email, age, weight, title, body, active, score, tags,
           color, status, role, bio, url, count, price, qty, ref, code]).

kura_column() ->
    ?LET({Name, Type}, {column_name(), kura_type()},
         #kura_column{name = Name, type = Type}).

%% Generate a list of columns with unique names
unique_columns() ->
    ?LET(Cols, non_empty(list(kura_column())),
         unique_by_name(Cols)).

table_name() ->
    oneof([<<"users">>, <<"posts">>, <<"items">>, <<"tags">>, <<"pets">>,
           <<"orders">>, <<"logs">>, <<"events">>]).

%% Generate a db_state with 1-3 tables
db_state() ->
    ?LET(Tables, non_empty(list({table_name(), unique_columns()})),
         maps:from_list(unique_tables(Tables))).

kura_field() ->
    ?LET({Name, Type, Virtual}, {column_name(), kura_type(), boolean()},
         #kura_field{name = Name, type = Type, virtual = Virtual}).

%%====================================================================
%% Properties
%%====================================================================

%% Diffing identical states produces no ops
prop_diff_empty_when_identical() ->
    ?FORALL(State, db_state(),
        begin
            {Up, Down} = kura_schema_diff:diff(State, State),
            Up =:= [] andalso Down =:= []
        end).

%% All columns added in desired but not in DB are detected
prop_diff_detects_all_added_columns() ->
    ?FORALL({Table, BaseCols, ExtraCols},
            {table_name(), unique_columns(), unique_columns()},
        begin
            Base = unique_by_name(BaseCols),
            Extra = [C || C <- ExtraCols, not lists:keymember(C#kura_column.name, #kura_column.name, Base)],
            Db = #{Table => Base},
            Desired = #{Table => Base ++ Extra},
            {UpOps, _} = kura_schema_diff:diff(Db, Desired),
            case Extra of
                [] -> UpOps =:= [];
                _ ->
                    AddedNames = extract_added_names(UpOps),
                    ExpectedNames = lists:sort([C#kura_column.name || C <- Extra]),
                    lists:sort(AddedNames) =:= ExpectedNames
            end
        end).

%% All columns dropped from desired are detected
prop_diff_detects_all_dropped_columns() ->
    ?FORALL({Table, AllCols}, {table_name(), unique_columns()},
        ?IMPLIES(length(AllCols) >= 2,
            begin
                {Keep, Drop} = lists:split(length(AllCols) div 2, AllCols),
                Db = #{Table => AllCols},
                Desired = #{Table => Keep},
                {UpOps, _} = kura_schema_diff:diff(Db, Desired),
                DroppedNames = extract_dropped_names(UpOps),
                ExpectedDropped = lists:sort([C#kura_column.name || C <- Drop]),
                lists:sort(DroppedNames) =:= ExpectedDropped
            end)).

%% Applying up then down returns to original state
prop_up_down_reversible() ->
    ?FORALL({Db, Desired}, {db_state(), db_state()},
        begin
            %% Only test tables that exist in desired (we don't generate drop_table)
            FilteredDb = maps:with(maps:keys(Desired), Db),
            {UpOps, DownOps} = kura_schema_diff:diff(FilteredDb, Desired),
            AfterUp = apply_ops(UpOps, FilteredDb),
            AfterDown = apply_ops(DownOps, AfterUp),
            normalize_state(FilteredDb) =:= normalize_state(AfterDown)
        end).

%% New tables get create_table with all columns
prop_new_table_creates_all_columns() ->
    ?FORALL({Table, Cols}, {table_name(), unique_columns()},
        begin
            {[{create_table, Table, Created}], [{drop_table, Table}]} =
                kura_schema_diff:diff(#{}, #{Table => Cols}),
            Created =:= Cols
        end).

%% Number of alter ops matches the actual changes
prop_diff_op_count() ->
    ?FORALL({Table, DbCols, DesiredCols}, {table_name(), unique_columns(), unique_columns()},
        begin
            Db = #{Table => DbCols},
            Desired = #{Table => DesiredCols},
            {UpOps, _} = kura_schema_diff:diff(Db, Desired),
            DbNames = [C#kura_column.name || C <- DbCols],
            DesNames = [C#kura_column.name || C <- DesiredCols],
            Added = length(DesNames -- DbNames),
            Dropped = length(DbNames -- DesNames),
            Common = DesNames -- (DesNames -- DbNames),
            DbMap = maps:from_list([{C#kura_column.name, C#kura_column.type} || C <- DbCols]),
            Modified = length([N || N <- Common,
                               maps:get(N, DbMap) =/=
                               (lists:keyfind(N, #kura_column.name, DesiredCols))#kura_column.type]),
            TotalChanges = Added + Dropped + Modified,
            case TotalChanges of
                0 -> UpOps =:= [];
                _ ->
                    [{alter_table, Table, Ops}] = UpOps,
                    length(Ops) =:= TotalChanges
            end
        end).

%% field_to_column always preserves the type
prop_field_to_column_preserves_type() ->
    ?FORALL({Name, Type}, {column_name(), kura_type()},
        begin
            Field = #kura_field{name = Name, type = Type},
            Col = kura_schema_diff:field_to_column(Field),
            Col#kura_column.name =:= Name andalso Col#kura_column.type =:= Type
        end).

%% build_db_state is deterministic — same input always gives same output
prop_build_db_state_idempotent() ->
    ?FORALL({Table, Cols}, {table_name(), unique_columns()},
        begin
            meck:new(prop_mig, [non_strict]),
            meck:expect(prop_mig, up, fun() ->
                [{create_table, Table, Cols}]
            end),
            S1 = kura_schema_diff:build_db_state([prop_mig]),
            S2 = kura_schema_diff:build_db_state([prop_mig]),
            meck:unload(prop_mig),
            S1 =:= S2
        end).

%%====================================================================
%% Helpers
%%====================================================================

unique_by_name(Cols) ->
    lists:foldl(fun(C, Acc) ->
        case lists:keymember(C#kura_column.name, #kura_column.name, Acc) of
            true -> Acc;
            false -> Acc ++ [C]
        end
    end, [], Cols).

unique_tables(Tables) ->
    lists:foldl(fun({Name, Cols}, Acc) ->
        case lists:keymember(Name, 1, Acc) of
            true -> Acc;
            false -> Acc ++ [{Name, Cols}]
        end
    end, [], Tables).

extract_added_names(Ops) ->
    lists:flatmap(fun
        ({alter_table, _, AlterOps}) ->
            [N || {add_column, #kura_column{name = N}} <- AlterOps];
        (_) -> []
    end, Ops).

extract_dropped_names(Ops) ->
    lists:flatmap(fun
        ({alter_table, _, AlterOps}) ->
            [N || {drop_column, N} <- AlterOps];
        (_) -> []
    end, Ops).

apply_ops([], State) -> State;
apply_ops([{create_table, Name, Cols} | Rest], State) ->
    apply_ops(Rest, State#{Name => Cols});
apply_ops([{drop_table, Name} | Rest], State) ->
    apply_ops(Rest, maps:remove(Name, State));
apply_ops([{alter_table, Name, AlterOps} | Rest], State) ->
    Cols = maps:get(Name, State, []),
    NewCols = apply_alter_ops(AlterOps, Cols),
    apply_ops(Rest, State#{Name => NewCols}).

apply_alter_ops([], Cols) -> Cols;
apply_alter_ops([{add_column, Col} | Rest], Cols) ->
    apply_alter_ops(Rest, Cols ++ [Col]);
apply_alter_ops([{drop_column, Name} | Rest], Cols) ->
    apply_alter_ops(Rest, [C || C <- Cols, C#kura_column.name =/= Name]);
apply_alter_ops([{modify_column, Name, Type} | Rest], Cols) ->
    NewCols = [case C#kura_column.name of
        Name -> C#kura_column{type = Type};
        _ -> C
    end || C <- Cols],
    apply_alter_ops(Rest, NewCols).

%% Normalize state for comparison (sort columns by name)
normalize_state(State) ->
    maps:map(fun(_K, Cols) ->
        lists:sort(fun(A, B) -> A#kura_column.name =< B#kura_column.name end, Cols)
    end, State).
