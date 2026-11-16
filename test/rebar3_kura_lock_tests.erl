-module(rebar3_kura_lock_tests).
-include_lib("eunit/include/eunit.hrl").

%% rebar.lock goes into the Hex package, and rebar3 builds a plugin against
%% the kura its lock names, not the one its rebar.config asks for. A lock
%% left behind when the floor is raised ships the plugin with a kura it says
%% it does not work with.
locked_kura_is_not_older_than_the_floor_test() ->
    case locked_kura() of
        unlocked -> ok;
        Locked -> ?assert(version(Locked) >= declared_floor(kura_requirement()))
    end.

locked_kura_is_in_the_major_of_the_floor_test() ->
    [Major | _] = declared_floor(kura_requirement()),
    case locked_kura() of
        unlocked -> ok;
        Locked -> ?assertMatch([Major | _], version(Locked))
    end.

version_order_test_() ->
    [
        ?_assert(version(~"2.28.0") >= declared_floor("~> 2.28")),
        ?_assert(version(~"2.30.0") >= declared_floor("~> 2.28")),
        ?_assert(version(~"2.100.0") >= declared_floor("~> 2.28")),
        ?_assertNot(version(~"2.27.9") >= declared_floor("~> 2.28")),
        ?_assertNot(version(~"2.20.7") >= declared_floor("~> 2.28")),
        ?_assert(version(~"2.28.1") >= declared_floor("~> 2.28.1")),
        ?_assertNot(version(~"2.28.0") >= declared_floor("~> 2.28.1")),
        ?_assert(version(~"2.28.0-rc.1") >= declared_floor(">= 2.28.0"))
    ].

kura_requirement() ->
    {ok, Config} = file:consult("rebar.config"),
    {kura, Requirement} = lists:keyfind(kura, 1, proplists:get_value(deps, Config)),
    Requirement.

%% A build against _checkouts/kura rewrites the lock without kura. Such a
%% lock pins nothing, and rebar3 then resolves kura from the requirement.
locked_kura() ->
    {ok, [{_LockVersion, Locks} | _]} = file:consult("rebar.lock"),
    case lists:keyfind(~"kura", 1, Locks) of
        {~"kura", {pkg, ~"kura", Locked}, 0} -> Locked;
        false -> unlocked
    end.

declared_floor(Requirement) ->
    {match, [Version]} = re:run(Requirement, ~"[0-9]+(?:\\.[0-9]+)*", [{capture, first, binary}]),
    version(Version).

version(Version) ->
    [Release | _PreRelease] = binary:split(Version, ~"-"),
    [binary_to_integer(Part) || Part <- binary:split(Release, ~".", [global])].
