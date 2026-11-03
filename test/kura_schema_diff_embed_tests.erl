-module(kura_schema_diff_embed_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

mock_post(Mod, Fields, Embeds) ->
    meck:new(Mod, [non_strict]),
    meck:expect(Mod, table, fun() -> ~"posts" end),
    meck:expect(Mod, fields, fun() ->
        [#kura_field{name = id, type = id, primary_key = true} | Fields]
    end),
    meck:expect(Mod, embeds, fun() -> Embeds end).

mock_embed(Mod, Table) ->
    meck:new(Mod, [non_strict]),
    meck:expect(Mod, table, fun() -> Table end),
    meck:expect(Mod, fields, fun() -> [#kura_field{name = label, type = string}] end).

posts_columns(State) ->
    maps:get(~"posts", maps:get(columns, State)).

embed_declared_only_in_embeds_gets_a_jsonb_column_test() ->
    mock_post(emb_post1, [], [
        #kura_embed{name = metadata, type = embeds_one, schema = emb_meta1},
        #kura_embed{name = tags, type = embeds_many, schema = emb_meta1}
    ]),
    try
        State = kura_schema_diff:build_desired_state([emb_post1]),
        ?assertEqual(
            [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = metadata, type = jsonb},
                #kura_column{name = tags, type = jsonb}
            ],
            posts_columns(State)
        ),
        ?assertEqual([], kura_schema_diff:embedded_schemas(State))
    after
        meck:unload(emb_post1)
    end.

embed_also_declared_as_a_field_keeps_the_field_column_test() ->
    Field = #kura_field{name = metadata, type = jsonb, nullable = false},
    mock_post(emb_post2, [Field], [
        #kura_embed{name = metadata, type = embeds_one, schema = emb_meta2}
    ]),
    try
        ?assertEqual(
            [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = metadata, type = jsonb, nullable = false}
            ],
            posts_columns(kura_schema_diff:build_desired_state([emb_post2]))
        )
    after
        meck:unload(emb_post2)
    end.

embed_added_to_an_existing_table_is_an_add_column_test() ->
    mock_post(emb_post3, [], [
        #kura_embed{name = metadata, type = embeds_one, schema = emb_meta3}
    ]),
    Db = #{~"posts" => [#kura_column{name = id, type = id, primary_key = true}]},
    try
        Desired = kura_schema_diff:build_desired_state([emb_post3]),
        ?assertEqual(
            {
                [
                    {alter_table, ~"posts", [
                        {add_column, #kura_column{name = metadata, type = jsonb}}
                    ]}
                ],
                [{alter_table, ~"posts", [{drop_column, metadata}]}]
            },
            kura_schema_diff:diff(Db, Desired)
        )
    after
        meck:unload(emb_post3)
    end.

embedded_schema_module_gets_no_table_test() ->
    mock_post(emb_post4, [], [
        #kura_embed{name = metadata, type = embeds_one, schema = emb_meta4}
    ]),
    mock_embed(emb_meta4, ~"_embedded"),
    try
        State = kura_schema_diff:build_desired_state([emb_meta4, emb_post4]),
        ?assertEqual([~"posts"], maps:keys(maps:get(columns, State))),
        ?assertEqual([{emb_meta4, emb_post4, skipped}], kura_schema_diff:embedded_schemas(State)),
        {Up, _Down} = kura_schema_diff:diff(#{}, State),
        ?assertEqual([~"posts"], [T || {create_table, T, _} <- Up])
    after
        meck:unload([emb_post4, emb_meta4])
    end.

schema_embedded_by_two_parents_is_reported_once_test() ->
    Embed = #kura_embed{name = metadata, type = embeds_one, schema = emb_meta5},
    mock_post(emb_post5, [], [Embed]),
    meck:new(emb_page5, [non_strict]),
    meck:expect(emb_page5, table, fun() -> ~"pages" end),
    meck:expect(emb_page5, fields, fun() -> [#kura_field{name = id, type = id}] end),
    meck:expect(emb_page5, embeds, fun() -> [Embed] end),
    mock_embed(emb_meta5, ~"_embedded"),
    try
        State = kura_schema_diff:build_desired_state([emb_post5, emb_page5, emb_meta5]),
        ?assertEqual([{emb_meta5, emb_post5, skipped}], kura_schema_diff:embedded_schemas(State)),
        ?assertEqual([~"pages", ~"posts"], lists:sort(maps:keys(maps:get(columns, State))))
    after
        meck:unload([emb_post5, emb_page5, emb_meta5])
    end.

embedded_schema_naming_a_real_table_keeps_it_test() ->
    mock_post(emb_post6, [], [
        #kura_embed{name = address, type = embeds_one, schema = emb_addr6}
    ]),
    mock_embed(emb_addr6, ~"addresses"),
    try
        State = kura_schema_diff:build_desired_state([emb_addr6, emb_post6]),
        ?assertEqual([~"addresses", ~"posts"], lists:sort(maps:keys(maps:get(columns, State)))),
        ?assertEqual(
            [{emb_addr6, emb_post6, {table, ~"addresses"}}],
            kura_schema_diff:embedded_schemas(State)
        )
    after
        meck:unload([emb_post6, emb_addr6])
    end.

embedded_schema_without_table_is_skipped_test() ->
    mock_post(emb_post7, [], [
        #kura_embed{name = metadata, type = embeds_one, schema = emb_meta7}
    ]),
    meck:new(emb_meta7, [non_strict]),
    meck:expect(emb_meta7, fields, fun() -> [#kura_field{name = label, type = string}] end),
    try
        State = kura_schema_diff:build_desired_state([emb_meta7, emb_post7]),
        ?assertEqual([~"posts"], maps:keys(maps:get(columns, State))),
        ?assertEqual([{emb_meta7, emb_post7, skipped}], kura_schema_diff:embedded_schemas(State))
    after
        meck:unload([emb_post7, emb_meta7])
    end.

embed_field_with_a_column_override_uses_the_embed_name_test() ->
    Field = #kura_field{name = metadata, type = jsonb, column = ~"meta_json"},
    mock_post(emb_post8, [Field], [
        #kura_embed{name = metadata, type = embeds_one, schema = emb_meta8}
    ]),
    try
        ?assertEqual(
            [
                #kura_column{name = id, type = id, primary_key = true},
                #kura_column{name = metadata, type = jsonb}
            ],
            posts_columns(kura_schema_diff:build_desired_state([emb_post8]))
        )
    after
        meck:unload(emb_post8)
    end.

raising_embeds_names_the_schema_test() ->
    meck:new(emb_post9, [non_strict]),
    meck:expect(emb_post9, table, fun() -> ~"posts" end),
    meck:expect(emb_post9, fields, fun() -> [#kura_field{name = id, type = id}] end),
    meck:expect(emb_post9, embeds, fun() -> error(badarg) end),
    try
        ?assertError(
            {kura_schema_diff, {embeds_failed, emb_post9, error, badarg}},
            kura_schema_diff:build_desired_state([emb_post9])
        ),
        ?assertEqual(
            "emb_post9: embeds/0 raised error:badarg",
            lists:flatten(
                kura_schema_diff:format_error({embeds_failed, emb_post9, error, badarg})
            )
        )
    after
        meck:unload(emb_post9)
    end.
