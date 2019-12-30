-module(gleam@generic_test).
-compile(no_auto_import).

-export([flip_test/0, compose_test/0]).

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
    FlippedFun = gleam@generic:flip(Fun),
    gleam@expect:equal(Fun(<<"Bob">>, 1), <<"String: 'Bob', Int: '1'">>),
    gleam@expect:equal(
        FlippedFun(2, <<"Alice">>),
        <<"String: 'Alice', Int: '2'">>
    ).

compose_test() ->
    AddTwo = fun(Int) -> Int + 2 end,
    AddThree = fun(Int1) -> Int1 + 3 end,
    AddFive = gleam@generic:compose(AddTwo, AddThree),
    gleam@expect:equal(AddFive(1), 6),
    HeadToString = gleam@generic:compose(
        fun gleam@list:head/1,
        fun(IntResult) ->
            gleam@int:to_string(gleam@result:unwrap(IntResult, 0))
        end
    ),
    gleam@expect:equal(HeadToString([1]), <<"1">>),
    gleam@expect:equal(HeadToString([]), <<"0">>).
