-module(rebar3_kura_lint_migrations_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Lint contract (covered without rebar state)
%%
%% do/1 is a thin wrapper around kura_migrator:check_unsafe_operations
%% over each migration's up/0. The meaningful surface is "does an
%% unsafe op in up/0 become a finding, and does safe/0 opt it out?".
%%====================================================================

unsafe_op_is_flagged_test() ->
    meck:new(m_drop_t, [non_strict]),
    meck:expect(m_drop_t, up, fun() -> [{drop_table, <<"users">>}] end),
    try
        [Finding] = rebar3_kura_lint_migrations:lint_module(m_drop_t),
        ?assertMatch(#{op := drop_table, target := <<"users">>}, Finding)
    after
        meck:unload(m_drop_t)
    end.

safe_callback_opts_out_test() ->
    meck:new(m_drop_safe, [non_strict]),
    meck:expect(m_drop_safe, up, fun() -> [{drop_table, <<"users">>}] end),
    meck:expect(m_drop_safe, safe, fun() -> [drop_table] end),
    try
        ?assertEqual([], rebar3_kura_lint_migrations:lint_module(m_drop_safe))
    after
        meck:unload(m_drop_safe)
    end.

safe_op_produces_no_finding_test() ->
    meck:new(m_create_t, [non_strict]),
    meck:expect(m_create_t, up, fun() -> [{create_table, <<"t">>, []}] end),
    try
        ?assertEqual([], rebar3_kura_lint_migrations:lint_module(m_create_t))
    after
        meck:unload(m_create_t)
    end.

safe_entries_defaults_to_empty_test() ->
    meck:new(m_no_safe, [non_strict]),
    meck:expect(m_no_safe, up, fun() -> [] end),
    try
        ?assertEqual([], rebar3_kura_lint_migrations:safe_entries(m_no_safe))
    after
        meck:unload(m_no_safe)
    end.

%% The gate contract: no findings -> clean; findings fail unless --warn.
outcome_no_findings_is_clean_test() ->
    ?assertEqual(clean, rebar3_kura_lint_migrations:outcome([], false)),
    ?assertEqual(clean, rebar3_kura_lint_migrations:outcome([], true)).

outcome_findings_without_warn_fail_test() ->
    ?assertEqual(fail, rebar3_kura_lint_migrations:outcome([finding], false)).

outcome_findings_with_warn_downgrade_test() ->
    ?assertEqual(warn, rebar3_kura_lint_migrations:outcome([finding], true)).

safe_entries_reads_callback_test() ->
    meck:new(m_with_safe, [non_strict]),
    meck:expect(m_with_safe, safe, fun() -> [{drop_column, email}] end),
    try
        ?assertEqual(
            [{drop_column, email}],
            rebar3_kura_lint_migrations:safe_entries(m_with_safe)
        )
    after
        meck:unload(m_with_safe)
    end.
