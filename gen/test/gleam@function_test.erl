-module(gleam@function_test).
-compile(no_auto_import).

-export([compose_test/0, flip_test/0]).

compose_test() ->
    AddTwo = fun(Int) -> Int + 2 end,
    AddThree = fun(Int1) -> Int1 + 3 end,
    AddFive = gleam@function:compose(AddTwo, AddThree),
    gleam@expect:equal(AddFive(1), 6),
    HeadToString = gleam@function:compose(
        fun gleam@list:head/1,
        fun(IntResult) ->
            gleam@int:to_string(gleam@result:unwrap(IntResult, 0))
        end
    ),
    gleam@expect:equal(HeadToString([1]), <<"1">>),
    gleam@expect:equal(HeadToString([]), <<"0">>).

flip_test() ->
    Fun = fun(String, Int) ->
        gleam@string:append(
            gleam@string:append(
                gleam@string:append(
                    gleam@string:append(<<"String: '">>, String),
                    <<"', Int: '">>
                ),
                gleam@int:to_string(Int)
            ),
            <<"'">>
        )
    end,
    FlippedFun = gleam@function:flip(Fun),
    gleam@expect:equal(Fun(<<"Bob">>, 1), <<"String: 'Bob', Int: '1'">>),
    gleam@expect:equal(
        FlippedFun(2, <<"Alice">>),
        <<"String: 'Alice', Int: '2'">>
    ).
