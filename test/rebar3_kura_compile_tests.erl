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

%%====================================================================
%% Helpers
%%====================================================================

compile_generated_migration(Desc, UpOps, DownOps) ->
    ModName = "m_test_" ++ Desc,
    Source = render_migration(ModName, UpOps, DownOps),
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

cleanup_mod(Mod) ->
    code:purge(Mod),
    code:delete(Mod).

%% Replicate the render logic from rebar3_kura_compile (since it's not exported)
render_migration(ModName, UpOps, DownOps) ->
    io_lib:format(
        "-module(~s).~n"
        "-behaviour(kura_migration).~n"
        "-include_lib(\"kura/include/kura.hrl\").~n"
        "-export([up/0, down/0]).~n"
        "~n"
        "up() ->~n"
        "    ~s.~n"
        "~n"
        "down() ->~n"
        "    ~s.~n",
        [ModName, render_ops(UpOps), render_ops(DownOps)]
    ).

render_ops(Ops) ->
    Inner = lists:join(",\n     ", [render_op(Op) || Op <- Ops]),
    io_lib:format("[~s]", [Inner]).

render_op({create_table, Table, Cols}) ->
    ColStrs = lists:join(",\n        ", [render_column(C) || C <- Cols]),
    io_lib:format("{create_table, <<\"~s\">>, [~n        ~s~n    ]}", [Table, ColStrs]);
render_op({drop_table, Table}) ->
    io_lib:format("{drop_table, <<\"~s\">>}", [Table]);
render_op({alter_table, Table, AlterOps}) ->
    OpStrs = lists:join(",\n        ", [render_alter_op(Op) || Op <- AlterOps]),
    io_lib:format("{alter_table, <<\"~s\">>, [~n        ~s~n    ]}", [Table, OpStrs]);
render_op({execute, SQL}) ->
    Escaped = string:replace(binary_to_list(SQL), "\"", "\\\"", all),
    io_lib:format("{execute, <<\"~s\">>}", [Escaped]).

render_alter_op({add_column, Col}) ->
    io_lib:format("{add_column, ~s}", [render_column(Col)]);
render_alter_op({drop_column, Name}) ->
    io_lib:format("{drop_column, ~p}", [Name]);
render_alter_op({modify_column, Name, Type}) ->
    io_lib:format("{modify_column, ~p, ~p}", [Name, Type]);
render_alter_op({rename_column, Old, New}) ->
    io_lib:format("{rename_column, ~p, ~p}", [Old, New]).

render_column(#kura_column{
    name = N,
    type = T,
    nullable = Null,
    default = Def,
    primary_key = PK,
    references = Refs,
    on_delete = OnDel,
    on_update = OnUpd
}) ->
    Parts = [io_lib:format("name = ~p", [N]), io_lib:format("type = ~p", [T])],
    Parts2 =
        case PK of
            true -> Parts ++ ["primary_key = true"];
            false -> Parts
        end,
    Parts3 =
        case Null of
            false -> Parts2 ++ ["nullable = false"];
            true -> Parts2
        end,
    Parts4 =
        case Def of
            undefined -> Parts3;
            _ -> Parts3 ++ [io_lib:format("default = ~p", [Def])]
        end,
    Parts5 =
        case Refs of
            undefined -> Parts4;
            _ -> Parts4 ++ [io_lib:format("references = ~p", [Refs])]
        end,
    Parts6 =
        case OnDel of
            undefined -> Parts5;
            _ -> Parts5 ++ [io_lib:format("on_delete = ~p", [OnDel])]
        end,
    Parts7 =
        case OnUpd of
            undefined -> Parts6;
            _ -> Parts6 ++ [io_lib:format("on_update = ~p", [OnUpd])]
        end,
    io_lib:format("#kura_column{~s}", [lists:join(", ", Parts7)]).

is_schema_file(File) ->
    {ok, Bin} = file:read_file(File),
    re:run(Bin, "-behaviou?r\\(kura_schema\\)") =/= nomatch.

make_temp_dir() ->
    Dir = filename:join(
        "/tmp", "rebar3_kura_test_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_dir(filename:join(Dir, ".")),
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
