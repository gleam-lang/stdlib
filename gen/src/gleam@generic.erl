-module(gleam@generic).
-compile(no_auto_import).

-export([flip/1, compose/2]).

flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.
