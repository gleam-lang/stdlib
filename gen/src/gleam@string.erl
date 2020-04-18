-module(gleam@string).
-compile(no_auto_import).

-export([is_empty/1, length/1, reverse/1, replace/3, lowercase/1, uppercase/1, compare/2, slice/3, drop_left/2, drop_right/2, contains/2, starts_with/2, ends_with/2, split/2, append/2, concat/1, repeat/2, join/2, pad_left/3, pad_right/3, trim/1, trim_left/1, trim_right/1, to_graphemes/1, next_grapheme/1]).

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

erl_string_slice(A, B, C) ->
    string:slice(A, B, C).

slice(String, Start, Length) ->
    case Start < 0 orelse Length =< 0 of
        true ->
            <<""/utf8>>;

        false ->
            erl_string_slice(String, Start, Length)
    end.

erl_string_slice_to_infinity(A, B) ->
    string:slice(A, B).

drop_left(From, UpTo) ->
    case UpTo =< 0 of
        true ->
            From;

        false ->
            erl_string_slice_to_infinity(From, UpTo)
    end.

drop_right(From, Drop) ->
    case Drop =< 0 of
        true ->
            From;

        false ->
            Start = length(From) - Drop,
            case Start =< 0 of
                true ->
                    <<""/utf8>>;

                false ->
                    erl_string_slice(From, 0, Start)
            end
    end.

contains(A, B) ->
    gleam_stdlib:string_contains(A, B).

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

concat(A) ->
    unicode:characters_to_binary(A).

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

pad_fill_help(Result, Chunk, ChunkLength, ToFill) ->
    One = 1,
    case true of
        true when One > ToFill ->
            concat(Result);

        true when ToFill > ChunkLength ->
            pad_fill_help(
                [Chunk | Result],
                Chunk,
                ChunkLength,
                ToFill
                - ChunkLength
            );

        true ->
            concat(gleam@list:reverse([slice(Chunk, 0, ToFill) | Result]))
    end.

pad_help(Pad, ToLength, With, PadSide) ->
    One = 1,
    PadLen = length(Pad),
    WithLen = length(With),
    case true of
        true when PadLen > WithLen ->
            Pad;

        true when One > WithLen ->
            Pad;

        true ->
            case PadSide of
                left ->
                    concat(
                        [pad_fill_help([], With, WithLen, ToLength - PadLen),
                         Pad]
                    );

                right ->
                    concat(
                        [Pad,
                         pad_fill_help([], With, WithLen, ToLength - PadLen)]
                    )
            end
    end.

pad_left(Pad, ToLength, With) ->
    pad_help(Pad, ToLength, With, left).

pad_right(Pad, ToLength, With) ->
    pad_help(Pad, ToLength, With, right).

erl_string_trim(A, B) ->
    string:trim(A, B).

trim(String) ->
    erl_string_trim(String, both).

trim_left(String) ->
    erl_string_trim(String, leading).

trim_right(String) ->
    erl_string_trim(String, trailing).

to_graphemes(A) ->
    gleam_stdlib:string_to_graphemes(A).

next_grapheme(A) ->
    gleam_stdlib:string_next_grapheme(A).
