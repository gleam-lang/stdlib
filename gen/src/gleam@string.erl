-module(gleam@string).
-compile(no_auto_import).

-export([is_empty/1, length/1, reverse/1, replace/3, lowercase/1, uppercase/1, compare/2, slice/3, contains/2, starts_with/2, ends_with/2, split/2, append/2, concat/1, repeat/2, join/2, trim/1, trim_left/1, trim_right/1]).

is_empty(Str) ->
    Str =:= <<""/utf8>>.

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

erl_slice(A, B, C) ->
    string:slice(A, B, C).

slice(String, Idx, Len) ->
    case Idx < 0 of
        true ->
            TranslatedIdx = length(String) + Idx,
            case TranslatedIdx < 0 of
                true ->
                    <<""/utf8>>;

                false ->
                    erl_slice(String, TranslatedIdx, Len)
            end;

        false ->
            erl_slice(String, Idx, Len)
    end.

erl_contains(A, B) ->
    gleam_stdlib:string_contains(A, B).

contains(Haystack, Needle) ->
    erl_contains(Haystack, Needle).

starts_with(A, B) ->
    gleam_stdlib:string_starts_with(A, B).

ends_with(A, B) ->
    gleam_stdlib:string_ends_with(A, B).

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

erl_trim(A, B) ->
    string:trim(A, B).

trim(String) ->
    erl_trim(String, both).

trim_left(String) ->
    erl_trim(String, leading).

trim_right(String) ->
    erl_trim(String, trailing).
