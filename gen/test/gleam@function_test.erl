-module(gleam@function_test).
-compile(no_auto_import).

-export([compose_test/0, flip_test/0, identity_test/0]).

compose_test() ->
    AddTwo = fun(Int) -> Int + 2 end,
    AddThree = fun(Int1) -> Int1 + 3 end,
    AddFive = gleam@function:compose(AddTwo, AddThree),
    gleam@should:equal(AddFive(1), 6),
    HeadToString = gleam@function:compose(
        gleam@function:compose(
            fun gleam@list:head/1,
            fun(Capture1) -> gleam@result:unwrap(Capture1, 0) end
        ),
        fun gleam@int:to_string/1
    ),
    gleam@should:equal(HeadToString([1]), <<"1"/utf8>>),
    gleam@should:equal(HeadToString([]), <<"0"/utf8>>).

flip_test() ->
    Fun = fun(S, I) ->
        gleam@string:append(
            gleam@string:append(
                gleam@string:append(
                    gleam@string:append(<<"String: '"/utf8>>, S),
                    <<"', Int: '"/utf8>>
                ),
                gleam@int:to_string(I)
            ),
            <<"'"/utf8>>
        )
    end,
    FlippedFun = gleam@function:flip(Fun),
    gleam@should:equal(
        Fun(<<"Bob"/utf8>>, 1),
        <<"String: 'Bob', Int: '1'"/utf8>>
    ),
    gleam@should:equal(
        FlippedFun(2, <<"Alice"/utf8>>),
        <<"String: 'Alice', Int: '2'"/utf8>>
    ).

identity_test() ->
    gleam@should:equal(gleam@function:identity(1), 1),
    gleam@should:equal(gleam@function:identity(<<""/utf8>>), <<""/utf8>>),
    gleam@should:equal(gleam@function:identity([]), []),
    gleam@should:equal(gleam@function:identity({1, 2.0}), {1, 2.0}).
