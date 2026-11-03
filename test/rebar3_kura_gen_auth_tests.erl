-module(rebar3_kura_gen_auth_tests).

-include_lib("eunit/include/eunit.hrl").

deprecation_message_names_the_replacement_test() ->
    Msg = rebar3_kura_gen_auth:deprecation_message(),
    ?assertNotEqual(nomatch, string:find(Msg, "rebar3 kura gen_auth is deprecated")),
    ?assertNotEqual(nomatch, string:find(Msg, "rebar3 nova gen_auth")),
    ?assertNotEqual(nomatch, string:find(Msg, "nova_auth 0.3")).
