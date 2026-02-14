-module(rebar3_kura).

-export([init/1]).

init(State) ->
    rebar3_kura_compile:init(State).
