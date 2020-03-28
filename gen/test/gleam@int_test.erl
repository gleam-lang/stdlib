-module(gleam@int_test).
-compile(no_auto_import).

-export([to_string/0, parse/0, to_base_string/0, compare_test/0, min_test/0, max_test/0, is_even_test/0, is_odd_test/0]).

to_string() ->
    gleam@should:equal(gleam@int:to_string(123), <<"123">>),
    gleam@should:equal(gleam@int:to_string(-123), <<"-123">>),
    gleam@should:equal(gleam@int:to_string(123), <<"123">>).

parse() ->
    gleam@should:equal(gleam@int:parse(<<"123">>), {ok, 123}),
    gleam@should:equal(gleam@int:parse(<<"-123">>), {ok, -123}),
    gleam@should:equal(gleam@int:parse(<<"0123">>), {ok, 123}),
    gleam@should:equal(gleam@int:parse(<<"">>), {error, nil}),
    gleam@should:equal(gleam@int:parse(<<"what">>), {error, nil}),
    gleam@should:equal(gleam@int:parse(<<"1.23">>), {error, nil}).

to_base_string() ->
    gleam@should:equal(gleam@int:to_base_string(100, 16), <<"64">>),
    gleam@should:equal(gleam@int:to_base_string(-100, 16), <<"-64">>).

compare_test() ->
    gleam@should:equal(gleam@int:compare(0, 0), eq),
    gleam@should:equal(gleam@int:compare(1, 1), eq),
    gleam@should:equal(gleam@int:compare(0, 1), lt),
    gleam@should:equal(gleam@int:compare(-2, -1), lt),
    gleam@should:equal(gleam@int:compare(2, 1), gt),
    gleam@should:equal(gleam@int:compare(-1, -2), gt).

min_test() ->
    gleam@should:equal(gleam@int:min(0, 0), 0),
    gleam@should:equal(gleam@int:min(0, 1), 0),
    gleam@should:equal(gleam@int:min(1, 0), 0),
    gleam@should:equal(gleam@int:min(-1, 2), -1),
    gleam@should:equal(gleam@int:min(2, -2), -2),
    gleam@should:equal(gleam@int:min(-1, -1), -1).

max_test() ->
    gleam@should:equal(gleam@int:max(0, 0), 0),
    gleam@should:equal(gleam@int:max(0, 1), 1),
    gleam@should:equal(gleam@int:max(1, 0), 1),
    gleam@should:equal(gleam@int:max(-1, 2), 2),
    gleam@should:equal(gleam@int:max(2, -2), 2),
    gleam@should:equal(gleam@int:max(-1, -1), -1).

is_even_test() ->
    gleam@should:be_true(gleam@int:is_even(0)),
    gleam@should:be_true(gleam@int:is_even(2)),
    gleam@should:be_true(gleam@int:is_even(-2)),
    gleam@should:be_true(gleam@int:is_even(10006)),
    gleam@should:be_false(gleam@int:is_even(1)),
    gleam@should:be_false(gleam@int:is_even(-3)),
    gleam@should:be_false(gleam@int:is_even(10005)).

is_odd_test() ->
    gleam@should:be_false(gleam@int:is_odd(0)),
    gleam@should:be_false(gleam@int:is_odd(2)),
    gleam@should:be_false(gleam@int:is_odd(-2)),
    gleam@should:be_false(gleam@int:is_odd(10006)),
    gleam@should:be_true(gleam@int:is_odd(1)),
    gleam@should:be_true(gleam@int:is_odd(-3)),
    gleam@should:be_true(gleam@int:is_odd(10005)).
