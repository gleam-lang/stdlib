-module(gleam@float_test).
-compile(no_auto_import).

-export([parse_test/0, to_string_test/0, compare_test/0, ceiling_test/0, floor_test/0, round_test/0, truncate_test/0, min_test/0, max_test/0]).

parse_test() ->
    gleam@should:equal(gleam@float:parse(<<"1.23">>), {ok, 1.23}),
    gleam@should:equal(gleam@float:parse(<<"5.0">>), {ok, 5.0}),
    gleam@should:equal(gleam@float:parse(<<"0.123456789">>), {ok, 0.123456789}),
    gleam@should:equal(gleam@float:parse(<<"">>), {error, nil}),
    gleam@should:equal(gleam@float:parse(<<"what">>), {error, nil}),
    gleam@should:equal(gleam@float:parse(<<"1">>), {error, nil}).

to_string_test() ->
    gleam@should:equal(gleam@float:to_string(123.0), <<"123.0">>),
    gleam@should:equal(gleam@float:to_string(-8.1), <<"-8.1">>).

compare_test() ->
    gleam@should:equal(gleam@float:compare(0.0, 0.0), eq),
    gleam@should:equal(gleam@float:compare(0.1, 0.1), eq),
    gleam@should:equal(gleam@float:compare(0.0, 0.1), lt),
    gleam@should:equal(gleam@float:compare(-2.0, -1.9), lt),
    gleam@should:equal(gleam@float:compare(2.0, 1.9), gt),
    gleam@should:equal(gleam@float:compare(-1.9, -2.0), gt).

ceiling_test() ->
    gleam@should:equal(gleam@float:ceiling(8.1), 9.0),
    gleam@should:equal(gleam@float:ceiling(-8.1), -8.0),
    gleam@should:equal(gleam@float:ceiling(-8.0), -8.0).

floor_test() ->
    gleam@should:equal(gleam@float:floor(8.1), 8.0),
    gleam@should:equal(gleam@float:floor(-8.1), -9.0),
    gleam@should:equal(gleam@float:floor(-8.0), -8.0).

round_test() ->
    gleam@should:equal(gleam@float:round(8.1), 8),
    gleam@should:equal(gleam@float:round(8.4), 8),
    gleam@should:equal(gleam@float:round(8.499), 8),
    gleam@should:equal(gleam@float:round(8.5), 9),
    gleam@should:equal(gleam@float:round(-8.1), -8),
    gleam@should:equal(gleam@float:round(-7.5), -8).

truncate_test() ->
    gleam@should:equal(gleam@float:truncate(8.1), 8),
    gleam@should:equal(gleam@float:truncate(8.4), 8),
    gleam@should:equal(gleam@float:truncate(8.499), 8),
    gleam@should:equal(gleam@float:truncate(8.5), 8),
    gleam@should:equal(gleam@float:truncate(-8.1), -8),
    gleam@should:equal(gleam@float:truncate(-7.5), -7).

min_test() ->
    gleam@should:equal(gleam@float:min(0.0, 0.0), 0.0),
    gleam@should:equal(gleam@float:min(0.3, 1.5), 0.3),
    gleam@should:equal(gleam@float:min(1.0, 0.0), 0.0),
    gleam@should:equal(gleam@float:min(-1.7, 2.5), -1.7),
    gleam@should:equal(gleam@float:min(-2.2, -2.2), -2.2),
    gleam@should:equal(gleam@float:min(-1.0, -1.0), -1.0),
    gleam@should:equal(gleam@float:min(-1.1, -1.0), -1.1).

max_test() ->
    gleam@should:equal(gleam@float:max(0.0, 0.0), 0.0),
    gleam@should:equal(gleam@float:max(0.3, 1.5), 1.5),
    gleam@should:equal(gleam@float:max(1.0, 0.0), 1.0),
    gleam@should:equal(gleam@float:max(-1.7, 2.5), 2.5),
    gleam@should:equal(gleam@float:max(-2.2, -2.2), -2.2),
    gleam@should:equal(gleam@float:max(-1.0, -1.0), -1.0),
    gleam@should:equal(gleam@float:max(-1.1, -1.0), -1.0).
