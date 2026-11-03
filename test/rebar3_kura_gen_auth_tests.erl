-module(rebar3_kura_gen_auth_tests).

-include_lib("eunit/include/eunit.hrl").

warns_that_gen_auth_is_deprecated_test() ->
    ok = rebar3_kura_gen_auth:warn_deprecated(),
    Out = ?capturedOutput,
    ?assertNotEqual(nomatch, string:find(Out, "rebar3 kura gen_auth is deprecated")),
    ?assertNotEqual(nomatch, string:find(Out, "rebar3 nova gen_auth")),
    ?assertNotEqual(nomatch, string:find(Out, "nova_auth 0.3")).
