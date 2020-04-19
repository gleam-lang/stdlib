-module(gleam@result_test).
-compile(no_auto_import).

-export([is_ok_test/0, is_error_test/0, map_test/0, map_error_test/0, flatten_test/0, then_test/0, unwrap_test/0]).

is_ok_test() ->
    gleam@should:be_true(gleam@result:is_ok({ok, 1})),
    gleam@should:be_false(gleam@result:is_ok({error, 1})).

is_error_test() ->
    gleam@should:be_false(gleam@result:is_error({ok, 1})),
    gleam@should:be_true(gleam@result:is_error({error, 1})).

map_test() ->
    gleam@should:equal(gleam@result:map({ok, 1}, fun(X) -> X + 1 end), {ok, 2}),
    gleam@should:equal(
        gleam@result:map({ok, 1}, fun(_) -> <<"2"/utf8>> end),
        {ok, <<"2"/utf8>>}
    ),
    gleam@should:equal(
        gleam@result:map({error, 1}, fun(X1) -> X1 + 1 end),
        {error, 1}
    ).

map_error_test() ->
    gleam@should:equal(
        gleam@result:map_error({ok, 1}, fun(X) -> X + 1 end),
        {ok, 1}
    ),
    gleam@should:equal(
        gleam@result:map_error(
            {error, 1},
            fun(X1) -> {<<"ok"/utf8>>, X1 + 1} end
        ),
        {error, {<<"ok"/utf8>>, 2}}
    ).

flatten_test() ->
    gleam@should:equal(gleam@result:flatten({ok, {ok, 1}}), {ok, 1}),
    gleam@should:equal(gleam@result:flatten({ok, {error, 1}}), {error, 1}),
    gleam@should:equal(gleam@result:flatten({error, 1}), {error, 1}),
    gleam@should:equal(
        gleam@result:flatten({error, {error, 1}}),
        {error, {error, 1}}
    ).

then_test() ->
    gleam@should:equal(
        gleam@result:then({error, 1}, fun(X) -> {ok, X + 1} end),
        {error, 1}
    ),
    gleam@should:equal(
        gleam@result:then({ok, 1}, fun(X1) -> {ok, X1 + 1} end),
        {ok, 2}
    ),
    gleam@should:equal(
        gleam@result:then({ok, 1}, fun(_) -> {ok, <<"type change"/utf8>>} end),
        {ok, <<"type change"/utf8>>}
    ),
    gleam@should:equal(
        gleam@result:then({ok, 1}, fun(_) -> {error, 1} end),
        {error, 1}
    ).

unwrap_test() ->
    gleam@should:equal(gleam@result:unwrap({ok, 1}, 50), 1),
    gleam@should:equal(gleam@result:unwrap({error, <<"nope"/utf8>>}, 50), 50).
