-module(gleam@function).
-compile(no_auto_import).

-export([compose/2, flip/1]).

compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.
