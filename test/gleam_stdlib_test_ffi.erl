-module(gleam_stdlib_test_ffi).

-export([
    main/0, should_equal/2, should_not_equal/2, should_be_ok/1,
    should_be_error/1, improper_list_append/3
]).

-include_lib("eunit/include/eunit.hrl").

main() ->
    Options = [
        no_tty, {report, {eunit_progress, [colored]}}
    ],
    Files = filelib:wildcard("test/**/*.{erl,gleam}"),
    Modules = lists:map(fun filepath_to_module/1, Files),
    case eunit:test(Modules, Options) of
        ok -> erlang:halt(0);
        _ -> erlang:halt(1)
    end.

filepath_to_module(Path0) ->
    Path1 = string:replace(Path0, "test/", ""),
    Path2 = string:replace(Path1, ".erl", ""),
    Path3 = string:replace(Path2, ".gleam", ""),
    Path4 = string:replace(Path3, "/", "@", all),
    Path5 = list_to_binary(Path4),
    binary_to_atom(Path5).

should_equal(Actual, Expected) ->
    ?assertEqual(Expected, Actual),
    nil.
should_not_equal(Actual, Expected) ->
    ?assertNotEqual(Expected, Actual),
    nil.
should_be_ok(A) ->
    ?assertMatch({ok, _}, A),
    nil.
should_be_error(A) ->
    ?assertMatch({error, _}, A),
    nil.

improper_list_append(ItemA, ItemB, ImproperTail) ->
    [ItemA, ItemB | ImproperTail].
