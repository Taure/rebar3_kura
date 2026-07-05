-module(rebar3_kura_gen_schemas_tests).
-include_lib("eunit/include/eunit.hrl").

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
    ?assert(has(Src, <<"table() -> <<\"users\">>.">>)),
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
        column_default => null
    },
    ?assertEqual(
        {<<"users">>, <<"email">>, <<"varchar">>, 255, <<"NO">>, null},
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

split_dots(Tokens) ->
    split_dots(Tokens, [], []).

split_dots([], _Cur, Acc) ->
    lists:reverse(Acc);
split_dots([{dot, _} = D | Rest], Cur, Acc) ->
    split_dots(Rest, [], [lists:reverse([D | Cur]) | Acc]);
split_dots([T | Rest], Cur, Acc) ->
    split_dots(Rest, [T | Cur], Acc).
