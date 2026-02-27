-module(rebar3_kura_setup_tests).

-include_lib("eunit/include/eunit.hrl").

render_repo_test_() ->
    Content = lists:flatten(rebar3_kura_setup:render_repo("my_app_db", "my_app")),
    [
        {"module name", ?_assert(string:find(Content, "-module(my_app_db).") =/= nomatch)},
        {"behaviour", ?_assert(string:find(Content, "-behaviour(kura_repo).") =/= nomatch)},
        {"config exported", ?_assert(string:find(Content, "config/0") =/= nomatch)},
        {"start exported", ?_assert(string:find(Content, "start/0") =/= nomatch)},
        {"all exported", ?_assert(string:find(Content, "all/1") =/= nomatch)},
        {"get exported", ?_assert(string:find(Content, "get/2") =/= nomatch)},
        {"insert/1 exported", ?_assert(string:find(Content, "insert/1") =/= nomatch)},
        {"insert/2 exported", ?_assert(string:find(Content, "insert/2") =/= nomatch)},
        {"update exported", ?_assert(string:find(Content, "update/1") =/= nomatch)},
        {"delete exported", ?_assert(string:find(Content, "delete/1") =/= nomatch)},
        {"insert_all exported", ?_assert(string:find(Content, "insert_all/2") =/= nomatch)},
        {"update_all exported", ?_assert(string:find(Content, "update_all/2") =/= nomatch)},
        {"delete_all exported", ?_assert(string:find(Content, "delete_all/1") =/= nomatch)},
        {"preload exported", ?_assert(string:find(Content, "preload/3") =/= nomatch)},
        {"transaction exported", ?_assert(string:find(Content, "transaction/1") =/= nomatch)},
        {"multi exported", ?_assert(string:find(Content, "multi/1") =/= nomatch)},
        {"query exported", ?_assert(string:find(Content, "query/2") =/= nomatch)},
        {"database uses app env",
            ?_assert(string:find(Content, "application:get_env(my_app") =/= nomatch)},
        {"default database name", ?_assert(string:find(Content, "my_app_dev") =/= nomatch)},
        {"pool uses ?MODULE", ?_assert(string:find(Content, "pool => ?MODULE") =/= nomatch)},
        {"exists exported", ?_assert(string:find(Content, "exists/1") =/= nomatch)},
        {"reload exported", ?_assert(string:find(Content, "reload/2") =/= nomatch)},
        {"insert_all/3 exported", ?_assert(string:find(Content, "insert_all/3") =/= nomatch)},
        {"delegates to kura_repo_worker",
            ?_assert(string:find(Content, "kura_repo_worker:") =/= nomatch)}
    ].

render_repo_compiles_test() ->
    Content = lists:flatten(rebar3_kura_setup:render_repo("test_repo", "test_app")),
    TmpDir = filename:join([
        "/tmp", "rebar3_kura_test_" ++ integer_to_list(erlang:unique_integer([positive]))
    ]),
    ok = filelib:ensure_dir(filename:join(TmpDir, ".")),
    File = filename:join(TmpDir, "test_repo.erl"),
    ok = file:write_file(File, Content),
    Result = compile:file(File, [
        binary,
        return_errors,
        {i, code:lib_dir(kura, include)}
    ]),
    file:delete(File),
    file:del_dir(TmpDir),
    ?assertMatch({ok, test_repo, _}, Result).

render_repo_default_name_test() ->
    Content = lists:flatten(rebar3_kura_setup:render_repo("pet_store_repo", "pet_store")),
    ?assert(string:find(Content, "-module(pet_store_repo).") =/= nomatch),
    ?assert(string:find(Content, "pet_store_dev") =/= nomatch).

render_repo_custom_name_test() ->
    Content = lists:flatten(rebar3_kura_setup:render_repo("my_db", "my_app")),
    ?assert(string:find(Content, "-module(my_db).") =/= nomatch).
