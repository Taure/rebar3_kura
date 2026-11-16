-module(rebar3_kura_gen_schemas_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kura/include/kura.hrl").

-define(TMPDIR, "/tmp").

source(App, TableInfo) ->
    iolist_to_binary(rebar3_kura_gen_schemas:schema_source(App, TableInfo)).

has(Src, Sub) ->
    binary:match(Src, Sub) =/= nomatch.

schema_source_basic_test() ->
    Src = source("myapp", #{
        table => <<"users">>,
        fields => [
            #{name => <<"id">>, type => id, primary_key => true, nullable => false},
            #{name => <<"email">>, type => string, primary_key => false, nullable => false},
            #{name => <<"bio">>, type => text, primary_key => false, nullable => true}
        ],
        assocs => []
    }),
    ?assert(has(Src, <<"-module(myapp_users).">>)),
    ?assert(has(Src, <<"-behaviour(kura_schema).">>)),
    ?assert(has(Src, <<"-export([table/0, fields/0]).">>)),
    ?assert(has(Src, ~"table() -> ~\"users\".")),
    ?assert(has(Src, <<"name = id">>)),
    ?assert(has(Src, <<"type = string">>)),
    ?assert(has(Src, <<"primary_key = true">>)),
    %% nullable field omits the nullable key; NOT NULL emits nullable = false
    ?assert(has(Src, <<"nullable = false">>)).

schema_source_with_associations_test() ->
    Src = source("myapp", #{
        table => <<"posts">>,
        fields => [
            #{name => <<"id">>, type => id, primary_key => true, nullable => false},
            #{name => <<"author_id">>, type => id, primary_key => false, nullable => false}
        ],
        assocs => [{belongs_to, <<"user">>, <<"users">>, <<"author_id">>}]
    }),
    ?assert(has(Src, <<"-export([table/0, fields/0, associations/0]).">>)),
    ?assert(has(Src, <<"associations() ->">>)),
    %% schema targets the generated module for the foreign table (myapp_users)
    ?assert(
        has(
            Src,
            <<
                "#kura_assoc{name = user, type = belongs_to, schema = myapp_users, "
                "foreign_key = author_id}"
            >>
        )
    ).

schema_source_array_type_test() ->
    Src = source("myapp", #{
        table => <<"t">>,
        fields => [
            #{name => <<"tags">>, type => {array, text}, primary_key => false, nullable => true}
        ],
        assocs => []
    }),
    ?assert(has(Src, <<"type = {array,text}">>)).

schema_source_is_valid_erlang_test() ->
    Src = source("myapp", #{
        table => <<"users">>,
        fields => [#{name => <<"id">>, type => id, primary_key => true, nullable => false}],
        assocs => []
    }),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Src)),
    Forms = split_dots(Tokens),
    ?assert(lists:all(fun(F) -> element(1, erl_parse:parse_form(F)) =:= ok end, Forms)).

%% The real driver returns maps with atom keys; the tuple order must follow
%% the SELECT, not the sorted key order.
column_row_follows_select_order_test() ->
    Row = #{
        table_name => <<"users">>,
        column_name => <<"email">>,
        udt_name => <<"varchar">>,
        character_maximum_length => 255,
        is_nullable => <<"NO">>,
        column_default => null,
        atttypmod => 259
    },
    ?assertEqual(
        {<<"users">>, <<"email">>, <<"varchar">>, 255, <<"NO">>, null, 259},
        rebar3_kura_gen_schemas:column_row(Row)
    ).

constraint_row_follows_select_order_test() ->
    Row = #{
        table_name => <<"posts">>,
        column_name => <<"author_id">>,
        constraint_type => <<"FOREIGN KEY">>,
        foreign_table => <<"users">>
    },
    ?assertEqual(
        {<<"posts">>, <<"author_id">>, <<"FOREIGN KEY">>, <<"users">>},
        rebar3_kura_gen_schemas:constraint_row(Row)
    ).

%%----------------------------------------------------------------------
%% Names that are not plain atoms
%%----------------------------------------------------------------------

reserved_words() ->
    [
        ~"after",
        ~"and",
        ~"andalso",
        ~"band",
        ~"begin",
        ~"bnot",
        ~"bor",
        ~"bsl",
        ~"bsr",
        ~"bxor",
        ~"case",
        ~"catch",
        ~"cond",
        ~"div",
        ~"else",
        ~"end",
        ~"fun",
        ~"if",
        ~"let",
        ~"maybe",
        ~"not",
        ~"of",
        ~"or",
        ~"orelse",
        ~"receive",
        ~"rem",
        ~"try",
        ~"when",
        ~"xor"
    ].

%% What a database may hold and the introspection refuses. The generator is
%% given them here as if it had not.
hostile_names() ->
    [
        ~"x'). evil() -> ok. %",
        ~"x\">>. evil() -> ok. %",
        ~"x}]. evil() -> ok. %",
        ~"line\nbreak",
        ~"back\\slash",
        ~"Upper",
        ~"Variable_like",
        ~"with space",
        ~"användare",
        ~"用户",
        ~"1digit",
        ~"a.b",
        ~"$dollar",
        ~"'quoted'"
    ].

column(Name) ->
    #{name => Name, type => text, primary_key => false, nullable => true}.

reserved_words_as_columns_test() ->
    Names = reserved_words() ++ [~"_", ~"_hidden", ~"__"],
    Module = compiled("myapp", #{
        table => ~"words", fields => [column(Name) || Name <- Names], assocs => []
    }),
    try
        ?assertEqual(
            [binary_to_atom(Name) || Name <- Names],
            [Field#kura_field.name || Field <- Module:fields()]
        )
    after
        unloaded(Module)
    end.

reserved_words_as_tables_and_associations_test_() ->
    [
        fun() ->
            Plural = <<Word/binary, "s">>,
            Module = compiled("myapp", #{
                table => Word,
                fields => [column(Word)],
                assocs => [{belongs_to, Word, Plural, Word}]
            }),
            try
                ?assertEqual(binary_to_atom(<<"myapp_", Word/binary>>), Module),
                ?assertEqual(Word, Module:table()),
                ?assertEqual(
                    [
                        #kura_assoc{
                            name = binary_to_atom(Word),
                            type = belongs_to,
                            schema = binary_to_atom(<<"myapp_", Plural/binary>>),
                            foreign_key = binary_to_atom(Word)
                        }
                    ],
                    Module:associations()
                )
            after
                unloaded(Module)
            end
        end
     || Word <- reserved_words()
    ].

hostile_columns_stay_inside_their_term_test_() ->
    [
        fun() ->
            Module = compiled("myapp", #{
                table => ~"things",
                fields => [column(Name)],
                assocs => [{belongs_to, Name, ~"others", Name}]
            }),
            try
                Atom = binary_to_atom(Name),
                ?assertEqual(schema_exports(), lists:sort(Module:module_info(exports))),
                ?assertMatch([#kura_field{name = Atom, type = text}], Module:fields()),
                ?assertMatch(
                    [#kura_assoc{name = Atom, schema = myapp_others, foreign_key = Atom}],
                    Module:associations()
                )
            after
                unloaded(Module)
            end
        end
     || Name <- hostile_names()
    ].

hostile_tables_stay_inside_their_term_test_() ->
    [
        fun() ->
            Module = compiled("myapp", #{
                table => Name,
                fields => [column(~"id")],
                assocs => [{belongs_to, ~"other", Name, ~"id"}]
            }),
            try
                ?assertEqual(binary_to_atom(<<"myapp_", Name/binary>>), Module),
                ?assertEqual(schema_exports(), lists:sort(Module:module_info(exports))),
                ?assertEqual(Name, Module:table()),
                ?assertMatch([#kura_assoc{schema = Module}], Module:associations())
            after
                unloaded(Module)
            end
        end
     || Name <- hostile_names(), not is_map_key(Name, uncompilable_tables())
    ].

%% The compiler takes no module of such a name, so the module such a table
%% would name is never built.
uncompilable_tables() ->
    #{
        ~"line\nbreak" => ctrl_chars_in_module_name,
        ~"用户" => non_latin1_module_unsupported
    }.

an_uncompilable_table_name_is_refused_by_the_compiler_test_() ->
    [
        ?_assertMatch(
            {error, [{_Path, [{_Location, erl_lint, Reason}]}], []},
            compile_source("myapp", #{table => Name, fields => [column(~"id")], assocs => []})
        )
     || Name := Reason <- uncompilable_tables()
    ].

schema_exports() ->
    [{associations, 0}, {fields, 0}, {module_info, 0}, {module_info, 1}, {table, 0}].

compiled(App, TableInfo) ->
    {ok, Module, Beam} = compile_source(App, TableInfo),
    {module, Module} = code:load_binary(Module, "generated.erl", Beam),
    Module.

compile_source(App, TableInfo) ->
    Source = utf8(rebar3_kura_gen_schemas:schema_source(App, TableInfo)),
    Dir = filename:join(
        ?TMPDIR,
        "rebar3_kura_gen_schemas_" ++ os:getpid() ++ "_" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    Path = filename:join(Dir, "generated.erl"),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Source),
    Compiled = compile:file(Path, [binary, return_errors, warnings_as_errors]),
    _ = file:del_dir_r(Dir),
    Compiled.

utf8(Source) ->
    case unicode:characters_to_binary(Source) of
        Encoded when is_binary(Encoded) -> Encoded
    end.

unloaded(Module) ->
    _ = code:purge(Module),
    _ = code:delete(Module),
    ok.

split_dots(Tokens) ->
    split_dots(Tokens, [], []).

split_dots([], _Cur, Acc) ->
    lists:reverse(Acc);
split_dots([{dot, _} = D | Rest], Cur, Acc) ->
    split_dots(Rest, [], [lists:reverse([D | Cur]) | Acc]);
split_dots([T | Rest], Cur, Acc) ->
    split_dots(Rest, [T | Cur], Acc).

%%----------------------------------------------------------------------
%% ensure_backend_on_path/2
%%----------------------------------------------------------------------

fake_state(Dir, Deps) ->
    AppInfos = [
        begin
            {ok, A} = rebar_app_info:new(Name, "1.0.0", filename:join(Dir, Name)),
            A
        end
     || Name <- Deps
    ],
    rebar_state:all_deps(rebar_state:new(), AppInfos).

%% Regression: the backend's driver (minato) is a transitive dep, and
%% start_pool/2 starts it as an application - so adding only the backend's
%% own ebin left minato.app unreachable and introspection crashed.
ensure_backend_on_path_adds_transitive_deps_test() ->
    Deps = ["kura", "kura_postgres", "minato"],
    Dir = filename:join(?TMPDIR, "rebar3_kura_path_test"),
    _ = [ok = filelib:ensure_path(filename:join([Dir, D, "ebin"])) || D <- Deps],
    State = fake_state(Dir, Deps),
    ok = rebar3_kura_gen_schemas:ensure_backend_on_path(State, #{
        backend => kura_backend_postgres
    }),
    Path = code:get_path(),
    ?assert(lists:member(filename:join([Dir, "kura_postgres", "ebin"]), Path)),
    ?assert(lists:member(filename:join([Dir, "minato", "ebin"]), Path)),
    _ = [code:del_path(filename:join([Dir, D, "ebin"])) || D <- Deps],
    _ = file:del_dir_r(Dir),
    ok.

ensure_backend_on_path_aborts_without_backend_test() ->
    State = fake_state(filename:join(?TMPDIR, "rebar3_kura_path_test_2"), ["kura"]),
    ?assertThrow(
        rebar_abort,
        rebar3_kura_gen_schemas:ensure_backend_on_path(State, #{backend => kura_backend_postgres})
    ).
