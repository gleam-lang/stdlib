-module(gleam@map_test).
-compile(no_auto_import).

-export([from_list_test/0, has_key_test/0, new_test/0, get_test/0, insert_test/0, map_values_test/0, keys_test/0, values_test/0, take_test/0, drop_test/0, merge_test/0, delete_test/0, update_test/0, fold_test/0]).

from_list_test() ->
    gleam@should:equal(
        gleam@map:size(gleam@map:from_list([{4, 0}, {1, 0}])),
        2
    ).

has_key_test() ->
    gleam@should:be_false(gleam@map:has_key(gleam@map:from_list([]), 1)),
    gleam@should:be_true(gleam@map:has_key(gleam@map:from_list([{1, 0}]), 1)),
    gleam@should:be_true(
        gleam@map:has_key(gleam@map:from_list([{4, 0}, {1, 0}]), 1)
    ),
    gleam@should:be_false(
        gleam@map:has_key(gleam@map:from_list([{4, 0}, {1, 0}]), 0)
    ).

new_test() ->
    gleam@should:equal(gleam@map:size(gleam@map:new()), 0),
    gleam@should:equal(gleam@map:to_list(gleam@map:new()), []).

get_test() ->
    Proplist = [{4, 0}, {1, 1}],
    M = gleam@map:from_list(Proplist),
    gleam@should:equal(gleam@map:get(M, 4), {ok, 0}),
    gleam@should:equal(gleam@map:get(M, 1), {ok, 1}),
    gleam@should:equal(gleam@map:get(M, 2), {error, nil}).

insert_test() ->
    gleam@should:equal(
        gleam@map:insert(
            gleam@map:insert(
                gleam@map:insert(gleam@map:new(), <<"a"/utf8>>, 0),
                <<"b"/utf8>>,
                1
            ),
            <<"c"/utf8>>,
            2
        ),
        gleam@map:from_list(
            [{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}, {<<"c"/utf8>>, 2}]
        )
    ).

map_values_test() ->
    gleam@should:equal(
        gleam@map:map_values(
            gleam@map:from_list([{1, 0}, {2, 1}, {3, 2}]),
            fun(K, V) -> K + V end
        ),
        gleam@map:from_list([{1, 1}, {2, 3}, {3, 5}])
    ).

keys_test() ->
    gleam@should:equal(
        gleam@map:keys(
            gleam@map:from_list(
                [{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}, {<<"c"/utf8>>, 2}]
            )
        ),
        [<<"a"/utf8>>, <<"b"/utf8>>, <<"c"/utf8>>]
    ).

values_test() ->
    gleam@should:equal(
        gleam@map:values(
            gleam@map:from_list(
                [{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}, {<<"c"/utf8>>, 2}]
            )
        ),
        [0, 1, 2]
    ).

take_test() ->
    gleam@should:equal(
        gleam@map:take(
            gleam@map:from_list(
                [{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}, {<<"c"/utf8>>, 2}]
            ),
            [<<"a"/utf8>>, <<"b"/utf8>>, <<"d"/utf8>>]
        ),
        gleam@map:from_list([{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}])
    ).

drop_test() ->
    gleam@should:equal(
        gleam@map:drop(
            gleam@map:from_list(
                [{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}, {<<"c"/utf8>>, 2}]
            ),
            [<<"a"/utf8>>, <<"b"/utf8>>, <<"d"/utf8>>]
        ),
        gleam@map:from_list([{<<"c"/utf8>>, 2}])
    ).

merge_test() ->
    A = gleam@map:from_list(
        [{<<"a"/utf8>>, 2}, {<<"c"/utf8>>, 4}, {<<"d"/utf8>>, 3}]
    ),
    B = gleam@map:from_list(
        [{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}, {<<"c"/utf8>>, 2}]
    ),
    gleam@should:equal(
        gleam@map:merge(A, B),
        gleam@map:from_list(
            [{<<"a"/utf8>>, 0},
             {<<"b"/utf8>>, 1},
             {<<"c"/utf8>>, 2},
             {<<"d"/utf8>>, 3}]
        )
    ),
    gleam@should:equal(
        gleam@map:merge(B, A),
        gleam@map:from_list(
            [{<<"a"/utf8>>, 2},
             {<<"b"/utf8>>, 1},
             {<<"c"/utf8>>, 4},
             {<<"d"/utf8>>, 3}]
        )
    ).

delete_test() ->
    gleam@should:equal(
        gleam@map:delete(
            gleam@map:delete(
                gleam@map:from_list(
                    [{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}, {<<"c"/utf8>>, 2}]
                ),
                <<"a"/utf8>>
            ),
            <<"d"/utf8>>
        ),
        gleam@map:from_list([{<<"b"/utf8>>, 1}, {<<"c"/utf8>>, 2}])
    ).

update_test() ->
    Dict = gleam@map:from_list(
        [{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}, {<<"c"/utf8>>, 2}]
    ),
    IncOrZero = fun(X) -> case X of
            {ok, I} ->
                I + 1;

            {error, _} ->
                0
        end end,
    gleam@should:equal(
        gleam@map:update(Dict, <<"a"/utf8>>, IncOrZero),
        gleam@map:from_list(
            [{<<"a"/utf8>>, 1}, {<<"b"/utf8>>, 1}, {<<"c"/utf8>>, 2}]
        )
    ),
    gleam@should:equal(
        gleam@map:update(Dict, <<"b"/utf8>>, IncOrZero),
        gleam@map:from_list(
            [{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 2}, {<<"c"/utf8>>, 2}]
        )
    ),
    gleam@should:equal(
        gleam@map:update(Dict, <<"z"/utf8>>, IncOrZero),
        gleam@map:from_list(
            [{<<"a"/utf8>>, 0},
             {<<"b"/utf8>>, 1},
             {<<"c"/utf8>>, 2},
             {<<"z"/utf8>>, 0}]
        )
    ).

fold_test() ->
    Dict = gleam@map:from_list(
        [{<<"a"/utf8>>, 0},
         {<<"b"/utf8>>, 1},
         {<<"c"/utf8>>, 2},
         {<<"d"/utf8>>, 3}]
    ),
    Add = fun(_, V, Acc) -> V + Acc end,
    gleam@should:equal(gleam@map:fold(Dict, 0, Add), 6),
    Concat = fun(K, _, Acc1) -> gleam@string:append(Acc1, K) end,
    gleam@should:equal(
        gleam@map:fold(Dict, <<""/utf8>>, Concat),
        <<"abcd"/utf8>>
    ),
    gleam@should:equal(gleam@map:fold(gleam@map:from_list([]), 0, Add), 0).
