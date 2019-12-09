-module(gleam@option_test).
-compile(no_auto_import).

-export([is_some_test/0, is_none_test/0, map_test/0, flatten_test/0, then_test/0, unwrap_test/0]).

is_some_test() ->
    gleam@expect:true(gleam@option:is_some({some, 1})),
    gleam@expect:false(gleam@option:is_some(none)).

is_none_test() ->
    gleam@expect:false(gleam@option:is_none({some, 1})),
    gleam@expect:true(gleam@option:is_none(none)).

map_test() ->
    gleam@expect:equal(
        gleam@option:map({some, 1}, fun(X) -> X + 1 end),
        {some, 2}
    ),
    gleam@expect:equal(
        gleam@option:map({some, 1}, fun(_) -> <<"2">> end),
        {some, <<"2">>}
    ),
    gleam@expect:equal(gleam@option:map(none, fun(X1) -> X1 + 1 end), none).

flatten_test() ->
    gleam@expect:equal(gleam@option:flatten({some, {some, 1}}), {some, 1}),
    gleam@expect:equal(gleam@option:flatten({some, none}), none),
    gleam@expect:equal(gleam@option:flatten(none), none),
    gleam@expect:equal(gleam@option:flatten(none), none).

then_test() ->
    gleam@expect:equal(
        gleam@option:then(none, fun(X) -> {some, X + 1} end),
        none
    ),
    gleam@expect:equal(
        gleam@option:then({some, 1}, fun(X1) -> {some, X1 + 1} end),
        {some, 2}
    ),
    gleam@expect:equal(
        gleam@option:then({some, 1}, fun(_) -> {some, <<"type change">>} end),
        {some, <<"type change">>}
    ),
    gleam@expect:equal(gleam@option:then({some, 1}, fun(_) -> none end), none).

unwrap_test() ->
    gleam@expect:equal(gleam@option:unwrap({some, 1}, 50), 1),
    gleam@expect:equal(gleam@option:unwrap(none, 50), 50).
