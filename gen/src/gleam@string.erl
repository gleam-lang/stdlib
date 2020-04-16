-module(gleam@string).
-compile(no_auto_import).

-export([is_empty/1, length/1, reverse/1, replace/3, lowercase/1, uppercase/1, compare/2, contains/2, split/2, append/2, concat/1, repeat/2, join/2]).

is_empty(Str) ->
    Str =:= <<"">>.

length(A) ->
    string:length(A).

reverse(String) ->
    gleam@iodata:to_string(gleam@iodata:reverse(gleam@iodata:new(String))).

replace(String, Pattern, Substitute) ->
    gleam@iodata:to_string(
        gleam@iodata:replace(gleam@iodata:new(String), Pattern, Substitute)
    ).

lowercase(A) ->
    string:lowercase(A).

uppercase(A) ->
    string:uppercase(A).

compare(A, B) ->
    gleam_stdlib:compare_strings(A, B).

erl_contains(A, B) ->
    gleam_stdlib:string_contains(A, B).

contains(Haystack, Needle) ->
    erl_contains(Haystack, Needle).

split(X, Substring) ->
    gleam@list:map(
        gleam@iodata:split(gleam@iodata:new(X), Substring),
        fun gleam@iodata:to_string/1
    ).

append(First, Second) ->
    gleam@iodata:to_string(
        gleam@iodata:append(gleam@iodata:new(First), Second)
    ).

concat(Strings) ->
    gleam@iodata:to_string(gleam@iodata:from_strings(Strings)).

repeat_help(Chunk, Result, Repeats) ->
    case Repeats =< 0 of
        true ->
            concat(Result);

        false ->
            repeat_help(Chunk, [Chunk | Result], Repeats - 1)
    end.

repeat(String, Times) ->
    repeat_help(String, [], Times).

join(Strings, Separator) ->
    gleam@iodata:to_string(
        gleam@iodata:from_strings(gleam@list:intersperse(Strings, Separator))
    ).
