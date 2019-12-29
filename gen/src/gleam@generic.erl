-module(gleam@generic).
-compile(no_auto_import).

-export([identity/1, always/2, flip/1, compose/2]).

identity(A) ->
    A.

always(_, B) ->
    B.

flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.
