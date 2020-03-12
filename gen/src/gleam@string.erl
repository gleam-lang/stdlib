-module(gleam@string).
-compile(no_auto_import).

-export([is_empty/1, length/1, reverse/1, replace/3, lowercase/1, uppercase/1, compare/2, split/2, append/2, concat/1, join/2]).

is_empty(Str) ->
    case Str of
        <<"">> ->
            true;

        _ ->
            false
    end.

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

split(X, Pattern) ->
    gleam@list:map(
        gleam@iodata:split(gleam@iodata:new(X), Pattern),
        fun gleam@iodata:to_string/1
    ).

append(First, Second) ->
    gleam@iodata:to_string(
        gleam@iodata:append(gleam@iodata:new(First), Second)
    ).

concat(Strings) ->
    gleam@iodata:to_string(gleam@iodata:from_strings(Strings)).

join(Strings, Separator) ->
    gleam@iodata:to_string(
        gleam@iodata:from_strings(gleam@list:intersperse(Strings, Separator))
    ).
