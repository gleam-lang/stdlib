-module(gleam@int).
-compile(no_auto_import).

-export([parse/1, to_string/1, to_base_string/2, compare/2, min/2, max/2, is_even/1]).

parse(A) ->
    gleam_stdlib:parse_int(A).

to_string(A) ->
    erlang:integer_to_binary(A).

to_base_string(A, B) ->
    erlang:integer_to_binary(A, B).

compare(A, B) ->
    case A =:= B of
        true ->
            eq;

        false ->
            case A < B of
                true ->
                    lt;

                false ->
                    gt
            end
    end.

min(A, B) ->
    case A < B of
        true ->
            A;

        false ->
            B
    end.

max(A, B) ->
    case A > B of
        true ->
            A;

        false ->
            B
    end.

is_even(X) ->
    X rem 2 =:= 0.
