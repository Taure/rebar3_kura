-module(rebar3_kura).

-export([init/1]).

init(State0) ->
    {ok, State1} = rebar3_kura_compile:init(State0),
    {ok, State2} = rebar3_kura_setup:init(State1),
    {ok, State2}.
