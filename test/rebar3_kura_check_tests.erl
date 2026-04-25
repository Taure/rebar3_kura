-module(rebar3_kura_check_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

%%====================================================================
%% Drift detection logic (covered without rebar state)
%%====================================================================
%%
%% The provider's `do/1` is wrapped around the existing diff
%% machinery, so the meaningful test surface is "does the diff result
%% become an error vs ok?". These tests pin that contract by calling
%% kura_schema_diff:diff/2 with the same inputs `do/1` would build,
%% mirroring the production gate.

drift_is_detected_when_schema_declares_index_missing_from_migrations_test() ->
    %% The asobi case at compile time: schema declares an index that
    %% no migration created. `kura check` must surface this so a CI
    %% gate can fail the PR before merge.
    meck:new(m_create_t, [non_strict]),
    meck:expect(m_create_t, up, fun() ->
        [
            {create_table, <<"t">>, [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = email, type = string}
            ]}
        ]
    end),
    meck:new(t_schema, [non_strict]),
    meck:expect(t_schema, table, fun() -> <<"t">> end),
    meck:expect(t_schema, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = email, type = string}
        ]
    end),
    meck:expect(t_schema, indexes, fun() ->
        [{[email], #{unique => true}}]
    end),

    DbState = kura_schema_diff:build_db_state([m_create_t]),
    DesiredState = kura_schema_diff:build_desired_state([t_schema]),
    Result = kura_schema_diff:diff(DbState, DesiredState),

    %% A non-empty diff is what the provider keys off.
    ?assertNotEqual({[], []}, Result),
    {Up, _Down} = Result,
    ?assertMatch([{create_index, <<"t">>, [email], _}], Up),

    meck:unload([m_create_t, t_schema]).

no_drift_when_migrations_match_schemas_test() ->
    %% Baseline for the green-build path: a fully migrated table with
    %% all declared indexes covered must produce empty diff. If this
    %% test fails the gate would false-positive on every CI run.
    meck:new(m_create_t2, [non_strict]),
    meck:expect(m_create_t2, up, fun() ->
        [
            {create_table, <<"t">>, [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = email, type = string}
            ]},
            {create_index, <<"t">>, [email], #{unique => true}}
        ]
    end),
    meck:new(t_schema2, [non_strict]),
    meck:expect(t_schema2, table, fun() -> <<"t">> end),
    meck:expect(t_schema2, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = email, type = string}
        ]
    end),
    meck:expect(t_schema2, indexes, fun() ->
        [{[email], #{unique => true}}]
    end),

    DbState = kura_schema_diff:build_db_state([m_create_t2]),
    DesiredState = kura_schema_diff:build_desired_state([t_schema2]),
    ?assertEqual({[], []}, kura_schema_diff:diff(DbState, DesiredState)),

    meck:unload([m_create_t2, t_schema2]).

drift_when_schema_adds_a_new_field_test() ->
    %% Common workflow: someone added a field to a schema and forgot
    %% to run `kura compile`. The check must catch the missing
    %% alter_table.
    meck:new(m_create_t3, [non_strict]),
    meck:expect(m_create_t3, up, fun() ->
        [
            {create_table, <<"t">>, [
                #kura_column{name = id, type = id, primary_key = true}
            ]}
        ]
    end),
    meck:new(t_schema3, [non_strict]),
    meck:expect(t_schema3, table, fun() -> <<"t">> end),
    meck:expect(t_schema3, fields, fun() ->
        [
            #kura_field{name = id, type = id, primary_key = true},
            #kura_field{name = name, type = string}
        ]
    end),

    DbState = kura_schema_diff:build_db_state([m_create_t3]),
    DesiredState = kura_schema_diff:build_desired_state([t_schema3]),
    {Up, _} = kura_schema_diff:diff(DbState, DesiredState),
    ?assert(
        lists:any(
            fun
                ({alter_table, <<"t">>, _}) -> true;
                (_) -> false
            end,
            Up
        )
    ),

    meck:unload([m_create_t3, t_schema3]).

drift_when_schema_declares_a_new_table_test() ->
    %% Schema added but never migrated → compile-time check catches
    %% the missing create_table.
    meck:new(t_schema4, [non_strict]),
    meck:expect(t_schema4, table, fun() -> <<"new_table">> end),
    meck:expect(t_schema4, fields, fun() ->
        [#kura_field{name = id, type = id, primary_key = true}]
    end),

    DbState = kura_schema_diff:build_db_state([]),
    DesiredState = kura_schema_diff:build_desired_state([t_schema4]),
    {Up, _Down} = kura_schema_diff:diff(DbState, DesiredState),
    ?assert(
        lists:any(
            fun
                ({create_table, <<"new_table">>, _}) -> true;
                (_) -> false
            end,
            Up
        )
    ),

    meck:unload(t_schema4).

%%====================================================================
%% format_error
%%====================================================================

format_error_for_schema_drift_test() ->
    %% The exact phrasing is part of the contract: it points users at
    %% the next action (`rebar3 kura compile`) so a CI failure is
    %% self-explanatory.
    Msg = lists:flatten(rebar3_kura_check:format_error(schema_drift)),
    ?assert(string:str(Msg, "schema drift") > 0),
    ?assert(string:str(Msg, "kura compile") > 0).

format_error_for_other_reason_uses_io_lib_format_test() ->
    Msg = lists:flatten(rebar3_kura_check:format_error({some, error})),
    %% Just verify it doesn't crash and returns a non-empty string.
    ?assert(length(Msg) > 0).
