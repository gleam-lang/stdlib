-module(gleam@pair).
-compile(no_auto_import).

-export([first/1, second/1, swap/1, map_first/2, map_second/2]).

first(Pair) ->
    {A, _} = Pair,
    A.

second(Pair) ->
    {_, A} = Pair,
    A.

swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.
