-module(rebar3_kura).

-export([init/1]).

init(State0) ->
    {ok, State1} = rebar3_kura_compile:init(State0),
    {ok, State2} = rebar3_kura_setup:init(State1),
    {ok, State3} = rebar3_kura_gen_auth:init(State2),
    {ok, State4} = rebar3_kura_check:init(State3),
    {ok, State4}.
