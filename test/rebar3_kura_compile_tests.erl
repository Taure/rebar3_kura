-module(rebar3_kura_compile_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

%%====================================================================
%% Migration file generation tests
%%
%% These test the generated migration source by writing it to a temp
%% file, compiling it, and verifying the up/0 and down/0 results.
%%====================================================================

generate_create_table_test() ->
    UpOps = [
        {create_table, <<"users">>, [
            #kura_column{name = id, type = id, primary_key = true, nullable = false},
            #kura_column{name = name, type = string, nullable = false}
        ]}
    ],
    DownOps = [{drop_table, <<"users">>}],
    {ok, Mod} = compile_generated_migration("create_users", UpOps, DownOps),
    Up = Mod:up(),
    Down = Mod:down(),
    ?assertMatch([{create_table, <<"users">>, [_, _]}], Up),
    ?assertMatch([{drop_table, <<"users">>}], Down),
    %% Verify column details
    [{create_table, _, [IdCol, NameCol]}] = Up,
    ?assertEqual(id, IdCol#kura_column.name),
    ?assertEqual(true, IdCol#kura_column.primary_key),
    ?assertEqual(false, IdCol#kura_column.nullable),
    ?assertEqual(name, NameCol#kura_column.name),
    ?assertEqual(false, NameCol#kura_column.nullable),
    cleanup_mod(Mod).

generate_alter_add_column_test() ->
    UpOps = [
        {alter_table, <<"pets">>, [
            {add_column, #kura_column{name = color, type = string}}
        ]}
    ],
    DownOps = [
        {alter_table, <<"pets">>, [
            {drop_column, color}
        ]}
    ],
    {ok, Mod} = compile_generated_migration("alter_pets", UpOps, DownOps),
    ?assertMatch(
        [{alter_table, <<"pets">>, [{add_column, #kura_column{name = color, type = string}}]}],
        Mod:up()
    ),
    ?assertMatch(
        [{alter_table, <<"pets">>, [{drop_column, color}]}],
        Mod:down()
    ),
    cleanup_mod(Mod).

generate_alter_drop_column_test() ->
    UpOps = [{alter_table, <<"t">>, [{drop_column, old}]}],
    DownOps = [{alter_table, <<"t">>, [{add_column, #kura_column{name = old, type = text}}]}],
    {ok, Mod} = compile_generated_migration("alter_t_drop", UpOps, DownOps),
    ?assertMatch([{alter_table, <<"t">>, [{drop_column, old}]}], Mod:up()),
    [{alter_table, <<"t">>, [{add_column, Col}]}] = Mod:down(),
    ?assertEqual(old, Col#kura_column.name),
    ?assertEqual(text, Col#kura_column.type),
    cleanup_mod(Mod).

generate_alter_modify_column_test() ->
    UpOps = [{alter_table, <<"t">>, [{modify_column, age, float}]}],
    DownOps = [{alter_table, <<"t">>, [{modify_column, age, integer}]}],
    {ok, Mod} = compile_generated_migration("alter_t_mod", UpOps, DownOps),
    ?assertMatch([{alter_table, <<"t">>, [{modify_column, age, float}]}], Mod:up()),
    ?assertMatch([{alter_table, <<"t">>, [{modify_column, age, integer}]}], Mod:down()),
    cleanup_mod(Mod).

generate_multiple_ops_test() ->
    UpOps = [
        {create_table, <<"a">>, [#kura_column{name = id, type = id}]},
        {alter_table, <<"b">>, [{add_column, #kura_column{name = x, type = string}}]}
    ],
    DownOps = [
        {drop_table, <<"a">>},
        {alter_table, <<"b">>, [{drop_column, x}]}
    ],
    {ok, Mod} = compile_generated_migration("multi_ops", UpOps, DownOps),
    ?assertEqual(2, length(Mod:up())),
    ?assertEqual(2, length(Mod:down())),
    cleanup_mod(Mod).

generate_column_with_default_test() ->
    UpOps = [
        {create_table, <<"t">>, [
            #kura_column{name = active, type = boolean, default = true}
        ]}
    ],
    DownOps = [{drop_table, <<"t">>}],
    {ok, Mod} = compile_generated_migration("with_default", UpOps, DownOps),
    [{create_table, _, [Col]}] = Mod:up(),
    ?assertEqual(true, Col#kura_column.default),
    cleanup_mod(Mod).

generate_array_type_test() ->
    UpOps = [
        {create_table, <<"t">>, [
            #kura_column{name = tags, type = {array, string}}
        ]}
    ],
    DownOps = [{drop_table, <<"t">>}],
    {ok, Mod} = compile_generated_migration("with_array", UpOps, DownOps),
    [{create_table, _, [Col]}] = Mod:up(),
    ?assertEqual({array, string}, Col#kura_column.type),
    cleanup_mod(Mod).

generate_rename_column_test() ->
    UpOps = [{alter_table, <<"t">>, [{rename_column, old, new}]}],
    DownOps = [{alter_table, <<"t">>, [{rename_column, new, old}]}],
    {ok, Mod} = compile_generated_migration("rename", UpOps, DownOps),
    ?assertMatch([{alter_table, <<"t">>, [{rename_column, old, new}]}], Mod:up()),
    ?assertMatch([{alter_table, <<"t">>, [{rename_column, new, old}]}], Mod:down()),
    cleanup_mod(Mod).

generate_execute_op_test() ->
    SQL = <<"ALTER TABLE \"users\" ALTER COLUMN \"name\" SET NOT NULL">>,
    UpOps = [{execute, SQL}],
    DownOps = [{execute, <<"ALTER TABLE \"users\" ALTER COLUMN \"name\" DROP NOT NULL">>}],
    {ok, Mod} = compile_generated_migration("execute_op", UpOps, DownOps),
    ?assertMatch([{execute, SQL}], Mod:up()),
    cleanup_mod(Mod).

generate_column_with_references_test() ->
    UpOps = [
        {create_table, <<"posts">>, [
            #kura_column{name = id, type = id, primary_key = true},
            #kura_column{
                name = user_id,
                type = integer,
                references = {<<"users">>, id},
                on_delete = cascade,
                on_update = no_action
            }
        ]}
    ],
    DownOps = [{drop_table, <<"posts">>}],
    {ok, Mod} = compile_generated_migration("with_refs", UpOps, DownOps),
    [{create_table, _, [_, FkCol]}] = Mod:up(),
    ?assertEqual({<<"users">>, id}, FkCol#kura_column.references),
    ?assertEqual(cascade, FkCol#kura_column.on_delete),
    ?assertEqual(no_action, FkCol#kura_column.on_update),
    cleanup_mod(Mod).

%%====================================================================
%% Schema file detection tests
%%====================================================================

is_schema_file_test() ->
    Dir = make_temp_dir(),
    %% Schema file
    SchemaFile = filename:join(Dir, "my_schema.erl"),
    ok = file:write_file(SchemaFile, <<"-module(my_schema).\n-behaviour(kura_schema).\n">>),
    %% Non-schema file
    NonSchema = filename:join(Dir, "my_controller.erl"),
    ok = file:write_file(NonSchema, <<"-module(my_controller).\n-export([index/1]).\n">>),
    %% British spelling
    BritishSchema = filename:join(Dir, "brit_schema.erl"),
    ok = file:write_file(BritishSchema, <<"-module(brit_schema).\n-behaviour(kura_schema).\n">>),
    %% American spelling
    AmericanSchema = filename:join(Dir, "us_schema.erl"),
    ok = file:write_file(AmericanSchema, <<"-module(us_schema).\n-behavior(kura_schema).\n">>),

    ?assert(is_schema_file(SchemaFile)),
    ?assertNot(is_schema_file(NonSchema)),
    ?assert(is_schema_file(BritishSchema)),
    ?assert(is_schema_file(AmericanSchema)),
    cleanup_dir(Dir).

%% Names and SQL reach the generated source from schemas and migration
%% history. Whatever they hold, the module must compile and hand back exactly
%% the operations it was rendered from.
generated_source_round_trips_hostile_text_test_() ->
    Texts = [
        ~"plain",
        ~"quote\"inside",
        ~"back\\slash",
        ~"\\\"",
        ~"tilde~s~p",
        ~"line\nbreak",
        ~"'^\\d+\\n$'",
        ~"x\"), erlang:display(pwned), (\""
    ],
    [
        {binary_to_list(T), ?_test(assert_round_trip(T))}
     || T <- Texts
    ].

assert_round_trip(Text) ->
    Col = #kura_column{name = a, type = string, references = {Text, id}},
    UpOps = [
        {create_table, Text, [Col]},
        {alter_table, Text, [{add_column, Col}]},
        {create_index, Text, [a], #{where => Text}},
        {create_index, Text, Text, [a], [unique, {where, Text}]},
        {execute, Text}
    ],
    DownOps = [{drop_index, Text}, {drop_table, Text}],
    Desc = binary_to_list(binary:encode_hex(crypto:hash(md5, Text))),
    {ok, Mod} = compile_generated_migration("round_trip_" ++ Desc, UpOps, DownOps),
    try
        ?assertEqual(UpOps, Mod:up()),
        ?assertEqual(DownOps, Mod:down())
    after
        cleanup_mod(Mod)
    end.

%%====================================================================
%% Helpers
%%====================================================================

compile_generated_migration(Desc, UpOps, DownOps) ->
    ModName = "m_test_" ++ Desc,
    Source = rebar3_kura_compile:render_migration(ModName, UpOps, DownOps),
    Dir = make_temp_dir(),
    File = filename:join(Dir, ModName ++ ".erl"),
    ok = file:write_file(File, Source),
    case compile:file(File, [binary, return_errors, {i, code:lib_dir(kura, include)}]) of
        {ok, Mod, Bin} ->
            {module, Mod} = code:load_binary(Mod, File, Bin),
            cleanup_dir(Dir),
            {ok, Mod};
        {ok, Mod, Bin, _Warnings} ->
            {module, Mod} = code:load_binary(Mod, File, Bin),
            cleanup_dir(Dir),
            {ok, Mod};
        {error, Errors, _} ->
            cleanup_dir(Dir),
            {error, Errors}
    end.

cleanup_forgets_the_schema_so_a_reload_reads_the_new_key_test() ->
    %% Taure/kura#169: kura check after kura compile in one rebar3 run, or two
    %% apps declaring the same module name, load a second binary under it.
    Dir = make_temp_dir(),
    try
        Mod = load_key_schema(Dir, "a"),
        ?assertEqual([a], kura_schema:key(Mod)),
        rebar3_kura_compile:cleanup([Mod]),
        ?assertEqual(Mod, load_key_schema(Dir, "b")),
        ?assertEqual([b], kura_schema:key(Mod))
    after
        rebar3_kura_compile:cleanup([reload_key_schema]),
        cleanup_dir(Dir)
    end.

load_key_schema(Dir, Key) ->
    File = filename:join(Dir, "reload_key_schema.erl"),
    Source = [
        "-module(reload_key_schema).\n",
        "-export([table/0, fields/0, key/0]).\n",
        "table() -> <<\"t\">>.\n",
        "fields() -> [].\n",
        "key() -> [",
        Key,
        "].\n"
    ],
    ok = file:write_file(File, Source),
    [Mod] = element(1, rebar3_kura_compile:compile_all([File], [], [binary, return_errors])),
    Mod.

cleanup_mod(Mod) ->
    code:purge(Mod),
    code:delete(Mod).

is_schema_file(File) ->
    {ok, Bin} = file:read_file(File),
    re:run(Bin, "-behaviou?r\\(kura_schema\\)") =/= nomatch.

%% Tests compile and load code from this directory, so its name must not be
%% guessable and it must not already exist.
make_temp_dir() ->
    Dir = filename:join(
        "/tmp",
        "rebar3_kura_test_" ++ binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(16)))
    ),
    ok = file:make_dir(Dir),
    Dir.

cleanup_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, Files} = file:list_dir(Dir),
            [file:delete(filename:join(Dir, F)) || F <- Files],
            file:del_dir(Dir);
        false ->
            ok
    end.
