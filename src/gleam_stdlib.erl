-module(gleam_stdlib).

-export([
    map_get/2, iodata_append/2, identity/1, parse_int/1, parse_float/1,
    less_than/2, string_pop_grapheme/1, string_pop_codeunit/1,
    string_starts_with/2, wrap_list/1, string_ends_with/2, string_pad/4,
    uri_parse/1, bit_array_slice/3, percent_encode/1, percent_decode/1,
    base_decode64/1, parse_query/1, bit_array_concat/1,
    bit_array_base64_encode/2, tuple_get/2, classify_dynamic/1, print/1,
    println/1, print_error/1, println_error/1, inspect/1, float_to_string/1,
    int_from_base_string/2, utf_codepoint_list_to_string/1, contains_string/2,
    crop_string/2, base16_encode/1, base16_decode/1, string_replace/3, slice/3,
    bit_array_to_int_and_size/1, bit_array_pad_to_bytes/1, index/2, list/5,
    dict/1, int/1, float/1, bit_array/1, is_null/1
]).

%% Taken from OTP's uri_string module
-define(DEC2HEX(X),
    if ((X) >= 0) andalso ((X) =< 9) -> (X) + $0;
        ((X) >= 10) andalso ((X) =< 15) -> (X) + $A - 10
    end).

%% Taken from OTP's uri_string module
-define(HEX2DEC(X),
    if ((X) >= $0) andalso ((X) =< $9) -> (X) - $0;
        ((X) >= $A) andalso ((X) =< $F) -> (X) - $A + 10;
        ((X) >= $a) andalso ((X) =< $f) -> (X) - $a + 10
    end).

-define(is_lowercase_char(X),
        (X > 96 andalso X < 123)).
-define(is_underscore_char(X),
        (X == 95)).
-define(is_digit_char(X),
        (X > 47 andalso X < 58)).
-define(is_ascii_character(X),
        (erlang:is_integer(X) andalso X >= 32 andalso X =< 126)).

uppercase(X) -> X - 32.

map_get(Map, Key) ->
    case maps:find(Key, Map) of
        error -> {error, nil};
        OkFound -> OkFound
    end.

iodata_append(Iodata, String) -> [Iodata, String].

identity(X) -> X.

classify_dynamic(nil) -> <<"Nil">>;
classify_dynamic(null) -> <<"Nil">>;
classify_dynamic(undefined) -> <<"Nil">>;
classify_dynamic(X) when is_boolean(X) -> <<"Bool">>;
classify_dynamic(X) when is_atom(X) -> <<"Atom">>;
classify_dynamic(X) when is_binary(X) -> <<"String">>;
classify_dynamic(X) when is_bitstring(X) -> <<"BitArray">>;
classify_dynamic(X) when is_integer(X) -> <<"Int">>;
classify_dynamic(X) when is_float(X) -> <<"Float">>;
classify_dynamic(X) when is_list(X) -> <<"List">>;
classify_dynamic(X) when is_map(X) -> <<"Dict">>;
classify_dynamic(X) when is_tuple(X) -> <<"Array">>;
classify_dynamic(X) when is_reference(X) -> <<"Reference">>;
classify_dynamic(X) when is_pid(X) -> <<"Pid">>;
classify_dynamic(X) when is_port(X) -> <<"Port">>;
classify_dynamic(X) when
    is_function(X, 0) orelse is_function(X, 1) orelse is_function(X, 2) orelse
    is_function(X, 3) orelse is_function(X, 4) orelse is_function(X, 5) orelse
    is_function(X, 6) orelse is_function(X, 7) orelse is_function(X, 8) orelse
    is_function(X, 9) orelse is_function(X, 10) orelse is_function(X, 11) orelse
    is_function(X, 12) -> <<"Function">>;
classify_dynamic(_) -> <<"Unknown">>.

tuple_get(_tup, Index) when Index < 0 -> {error, nil};
tuple_get(Data, Index) when Index >= tuple_size(Data) -> {error, nil};
tuple_get(Data, Index) -> {ok, element(Index + 1, Data)}.

int_from_base_string(String, Base) ->
    case catch binary_to_integer(String, Base) of
        Int when is_integer(Int) -> {ok, Int};
        _ -> {error, nil}
    end.

parse_int(String) ->
    case catch binary_to_integer(String) of
        Int when is_integer(Int) -> {ok, Int};
        _ -> {error, nil}
    end.

parse_float(String) ->
    case catch binary_to_float(String) of
        Float when is_float(Float) -> {ok, Float};
        _ -> {error, nil}
    end.

less_than(Lhs, Rhs) ->
    Lhs < Rhs.

string_starts_with(_, <<>>) -> true;
string_starts_with(String, Prefix) when byte_size(Prefix) > byte_size(String) -> false;
string_starts_with(String, Prefix) ->
    PrefixSize = byte_size(Prefix),
    Prefix == binary_part(String, 0, PrefixSize).

string_ends_with(_, <<>>) -> true;
string_ends_with(String, Suffix) when byte_size(Suffix) > byte_size(String) -> false;
string_ends_with(String, Suffix) ->
    SuffixSize = byte_size(Suffix),
    Suffix == binary_part(String, byte_size(String) - SuffixSize, SuffixSize).

string_pad(String, Length, Dir, PadString) ->
    Chars = string:pad(String, Length, Dir, binary_to_list(PadString)),
    case unicode:characters_to_binary(Chars) of
        Bin when is_binary(Bin) -> Bin;
        Error -> erlang:error({gleam_error, {string_invalid_utf8, Error}})
    end.

string_pop_grapheme(String) ->
    case string:next_grapheme(String) of
        [ Next | Rest ] when is_binary(Rest) ->
            {ok, {unicode:characters_to_binary([Next]), Rest}};

        [ Next | Rest ]  ->
            {ok, {unicode:characters_to_binary([Next]), unicode:characters_to_binary(Rest)}};

        _ -> {error, nil}
    end.

string_pop_codeunit(<<Cp/integer, Rest/binary>>) -> {Cp, Rest};
string_pop_codeunit(Binary) -> {0, Binary}.

bit_array_pad_to_bytes(Bin) ->
    case erlang:bit_size(Bin) rem 8 of
        0 -> Bin;
        TrailingBits ->
            PaddingBits = 8 - TrailingBits,
            <<Bin/bits, 0:PaddingBits>>
    end.

bit_array_concat(BitArrays) ->
    list_to_bitstring(BitArrays).

-if(?OTP_RELEASE >= 26).
bit_array_base64_encode(Bin, Padding) ->
    PaddedBin = bit_array_pad_to_bytes(Bin),
    base64:encode(PaddedBin, #{padding => Padding}).
-else.
bit_array_base64_encode(_Bin, _Padding) ->
    erlang:error(<<"Erlang OTP/26 or higher is required to use base64:encode">>).
-endif.

bit_array_slice(Bin, Pos, Len) ->
    try {ok, binary:part(Bin, Pos, Len)}
    catch error:badarg -> {error, nil}
    end.

base_decode64(S) ->
    try {ok, base64:decode(S)}
    catch error:_ -> {error, nil}
    end.

wrap_list(X) when is_list(X) -> X;
wrap_list(X) -> [X].

parse_query(Query) ->
    case uri_string:dissect_query(Query) of
        {error, _, _} -> {error, nil};
        Pairs ->
            Pairs1 = lists:map(fun
                ({K, true}) -> {K, <<"">>};
                (Pair) -> Pair
            end, Pairs),
            {ok, Pairs1}
    end.

percent_encode(B) -> percent_encode(B, <<>>).
percent_encode(<<>>, Acc) ->
    Acc;
percent_encode(<<H,T/binary>>, Acc) ->
    case percent_ok(H) of
        true ->
            percent_encode(T, <<Acc/binary,H>>);
        false ->
            <<A:4,B:4>> = <<H>>,
            percent_encode(T, <<Acc/binary,$%,(?DEC2HEX(A)),(?DEC2HEX(B))>>)
    end.

percent_decode(Cs) -> percent_decode(Cs, <<>>).
percent_decode(<<$%, C0, C1, Cs/binary>>, Acc) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true ->
            B = ?HEX2DEC(C0)*16+?HEX2DEC(C1),
            percent_decode(Cs, <<Acc/binary, B>>);
        false ->
            {error, nil}
    end;
percent_decode(<<C,Cs/binary>>, Acc) ->
    percent_decode(Cs, <<Acc/binary, C>>);
percent_decode(<<>>, Acc) ->
    check_utf8(Acc).

percent_ok($!) -> true;
percent_ok($$) -> true;
percent_ok($') -> true;
percent_ok($() -> true;
percent_ok($)) -> true;
percent_ok($*) -> true;
percent_ok($+) -> true;
percent_ok($-) -> true;
percent_ok($.) -> true;
percent_ok($_) -> true;
percent_ok($~) -> true;
percent_ok(C) when $0 =< C, C =< $9 -> true;
percent_ok(C) when $A =< C, C =< $Z -> true;
percent_ok(C) when $a =< C, C =< $z -> true;
percent_ok(_) -> false.

is_hex_digit(C) ->
  ($0 =< C andalso C =< $9) orelse ($a =< C andalso C =< $f) orelse ($A =< C andalso C =< $F).

check_utf8(Cs) ->
    case unicode:characters_to_list(Cs) of
        {incomplete, _, _} -> {error, nil};
        {error, _, _} -> {error, nil};
        _ -> {ok, Cs}
    end.

uri_parse(String) ->
    case uri_string:parse(String) of
        {error, _, _} -> {error, nil};
        Uri ->
            Port =
                try maps:get(port, Uri) of
                    undefined -> none;
                    Value -> {some, Value}
                catch _:_ -> none
                end,
            {ok, {uri,
                maps_get_optional(Uri, scheme),
                maps_get_optional(Uri, userinfo),
                maps_get_optional(Uri, host),
                Port,
                maps_get_or(Uri, path, <<>>),
                maps_get_optional(Uri, query),
                maps_get_optional(Uri, fragment)
            }}
    end.

maps_get_optional(Map, Key) ->
    try {some, maps:get(Key, Map)}
    catch _:_ -> none
    end.

maps_get_or(Map, Key, Default) ->
    try maps:get(Key, Map)
    catch _:_ -> Default
    end.

print(String) ->
    io:put_chars(String),
    nil.

println(String) ->
    io:put_chars([String, $\n]),
    nil.

print_error(String) ->
    io:put_chars(standard_error, String),
    nil.

println_error(String) ->
    io:put_chars(standard_error, [String, $\n]),
    nil.

inspect(true) ->
    "True";
inspect(false) ->
    "False";
inspect(nil) ->
    "Nil";
inspect(Data) when is_map(Data) ->
    Fields = [
        [<<"#(">>, inspect(Key), <<", ">>, inspect(Value), <<")">>]
        || {Key, Value} <- maps:to_list(Data)
    ],
    ["dict.from_list([", lists:join(", ", Fields), "])"];
inspect(Atom) when is_atom(Atom) ->
    erlang:element(2, inspect_atom(Atom));
inspect(Any) when is_integer(Any) ->
    erlang:integer_to_list(Any);
inspect(Any) when is_float(Any) ->
    io_lib_format:fwrite_g(Any);
inspect(Binary) when is_binary(Binary) ->
    case inspect_maybe_utf8_string(Binary, <<>>) of
        {ok, InspectedUtf8String} -> InspectedUtf8String;
        {error, not_a_utf8_string} ->
            Segments = [erlang:integer_to_list(X) || <<X>> <= Binary],
            ["<<", lists:join(", ", Segments), ">>"]
    end;
inspect(Bits) when is_bitstring(Bits) ->
    inspect_bit_array(Bits);
inspect(List) when is_list(List) ->
    case inspect_list(List, true) of
        {charlist, _} -> ["charlist.from_string(\"", list_to_binary(List), "\")"];
        {proper, Elements} -> ["[", Elements, "]"];
        {improper, Elements} -> ["//erl([", Elements, "])"]
    end;
inspect(Any) when is_tuple(Any) % Record constructors
  andalso is_atom(element(1, Any))
  andalso element(1, Any) =/= false
  andalso element(1, Any) =/= true
  andalso element(1, Any) =/= nil
->
    [Atom | ArgsList] = erlang:tuple_to_list(Any),
    InspectedArgs = lists:map(fun inspect/1, ArgsList),
    case inspect_atom(Atom) of
        {gleam_atom, GleamAtom} ->
            Args = lists:join(<<", ">>, InspectedArgs),
            [GleamAtom, "(", Args, ")"];
        {erlang_atom, ErlangAtom} ->
            Args = lists:join(<<", ">>, [ErlangAtom | InspectedArgs]),
            ["#(", Args, ")"]
    end;
inspect(Tuple) when is_tuple(Tuple) ->
    Elements = lists:map(fun inspect/1, erlang:tuple_to_list(Tuple)),
    ["#(", lists:join(", ", Elements), ")"];
inspect(Any) when is_function(Any) ->
    {arity, Arity} = erlang:fun_info(Any, arity),
    ArgsAsciiCodes = lists:seq($a, $a + Arity - 1),
    Args = lists:join(<<", ">>,
        lists:map(fun(Arg) -> <<Arg>> end, ArgsAsciiCodes)
    ),
    ["//fn(", Args, ") { ... }"];
inspect(Any) ->
    ["//erl(", io_lib:format("~p", [Any]), ")"].

inspect_atom(Atom) ->
    Binary = erlang:atom_to_binary(Atom),
    case inspect_maybe_gleam_atom(Binary, none, <<>>) of
        {ok, Inspected} -> {gleam_atom, Inspected};
        {error, _} -> {erlang_atom, ["atom.create(\"", Binary, "\")"]}
	end.

inspect_maybe_gleam_atom(<<>>, none, _) ->
    {error, nil};
inspect_maybe_gleam_atom(<<First, _Rest/binary>>, none, _) when ?is_digit_char(First) ->
    {error, nil};
inspect_maybe_gleam_atom(<<"_", _Rest/binary>>, none, _) ->
    {error, nil};
inspect_maybe_gleam_atom(<<"_">>, _PrevChar, _Acc) ->
    {error, nil};
inspect_maybe_gleam_atom(<<"_",  _Rest/binary>>, $_, _Acc) ->
    {error, nil};
inspect_maybe_gleam_atom(<<First, _Rest/binary>>, _PrevChar, _Acc)
    when not (?is_lowercase_char(First) orelse ?is_underscore_char(First) orelse ?is_digit_char(First)) ->
    {error, nil};
inspect_maybe_gleam_atom(<<First, Rest/binary>>, none, Acc) ->
    inspect_maybe_gleam_atom(Rest, First, <<Acc/binary, (uppercase(First))>>);
inspect_maybe_gleam_atom(<<"_", Rest/binary>>, _PrevChar, Acc) ->
    inspect_maybe_gleam_atom(Rest, $_, Acc);
inspect_maybe_gleam_atom(<<First, Rest/binary>>, $_, Acc) ->
    inspect_maybe_gleam_atom(Rest, First, <<Acc/binary, (uppercase(First))>>);
inspect_maybe_gleam_atom(<<First, Rest/binary>>, _PrevChar, Acc) ->
    inspect_maybe_gleam_atom(Rest, First, <<Acc/binary, First>>);
inspect_maybe_gleam_atom(<<>>, _PrevChar, Acc) ->
    {ok, Acc};
inspect_maybe_gleam_atom(A, B, C) ->
    erlang:display({A, B, C}),
    throw({gleam_error, A, B, C}).

inspect_list([], _) ->
    {proper, []};
inspect_list([First], true) when ?is_ascii_character(First) ->
    {charlist, nil};
inspect_list([First], _) ->
    {proper, [inspect(First)]};
inspect_list([First | Rest], ValidCharlist) when is_list(Rest) ->
    StillValidCharlist = ValidCharlist andalso ?is_ascii_character(First),
    {Kind, Inspected} = inspect_list(Rest, StillValidCharlist),
    {Kind, [inspect(First), <<", ">> | Inspected]};
inspect_list([First | ImproperTail], _) ->
    {improper, [inspect(First), <<" | ">>, inspect(ImproperTail)]}.

inspect_bit_array(Bits) ->
    Text = inspect_bit_array(Bits, <<"<<">>),
    <<Text/binary, ">>">>.

inspect_bit_array(<<>>, Acc) ->
    Acc;
inspect_bit_array(<<X, Rest/bitstring>>, Acc) ->
    inspect_bit_array(Rest, append_segment(Acc, erlang:integer_to_binary(X)));
inspect_bit_array(Rest, Acc) ->
    Size = bit_size(Rest),
    <<X:Size>> = Rest,
    X1 = erlang:integer_to_binary(X),
    Size1 = erlang:integer_to_binary(Size),
    Segment = <<X1/binary, ":size(", Size1/binary, ")">>,
    inspect_bit_array(<<>>, append_segment(Acc, Segment)).

bit_array_to_int_and_size(A) ->
    Size = bit_size(A),
    <<A1:Size>> = A,
    {A1, Size}.

append_segment(<<"<<">>, Segment) ->
    <<"<<", Segment/binary>>;
append_segment(Acc, Segment) ->
    <<Acc/binary, ", ", Segment/binary>>.


inspect_maybe_utf8_string(Binary, Acc) ->
    case Binary of
        <<>> -> {ok, <<$", Acc/binary, $">>};
        <<First/utf8, Rest/binary>> ->
            Escaped = case First of
                $" -> <<$\\, $">>;
                $\\ -> <<$\\, $\\>>;
                $\r -> <<$\\, $r>>;
                $\n -> <<$\\, $n>>;
                $\t -> <<$\\, $t>>;
                $\f -> <<$\\, $f>>;
                X when X > 126, X < 160 -> convert_to_u(X);
                X when X < 32 -> convert_to_u(X);
                Other -> <<Other/utf8>>
            end,
            inspect_maybe_utf8_string(Rest, <<Acc/binary, Escaped/binary>>);
        _ -> {error, not_a_utf8_string}
    end.

convert_to_u(Code) ->
    list_to_binary(io_lib:format("\\u{~4.16.0B}", [Code])).

float_to_string(Float) when is_float(Float) ->
    erlang:iolist_to_binary(io_lib_format:fwrite_g(Float)).

utf_codepoint_list_to_string(List) ->
    case unicode:characters_to_binary(List) of
        {error, _} -> erlang:error({gleam_error, {string_invalid_utf8, List}});
        Binary -> Binary
    end.

crop_string(String, Prefix) ->
    case string:find(String, Prefix) of
        nomatch -> String;
        New -> New
    end.

contains_string(String, Substring) ->
    is_bitstring(string:find(String, Substring)).

base16_encode(Bin) ->
    PaddedBin = bit_array_pad_to_bytes(Bin),
    binary:encode_hex(PaddedBin).

base16_decode(String) ->
    try
        {ok, binary:decode_hex(String)}
    catch
        _:_ -> {error, nil}
    end.

string_replace(String, Pattern, Replacement) ->
    string:replace(String, Pattern, Replacement, all).

slice(String, Index, Length) ->
    case string:slice(String, Index, Length) of
        X when is_binary(X) -> X;
        X when is_list(X) -> unicode:characters_to_binary(X)
    end.


index([X | _], 0) ->
    {ok, {some, X}};
index([_, X | _], 1) ->
    {ok, {some, X}};
index([_, _, X | _], 2) ->
    {ok, {some, X}};
index([_, _, _, X | _], 3) ->
    {ok, {some, X}};
index([_, _, _, _, X | _], 4) ->
    {ok, {some, X}};
index([_, _, _, _, _, X | _], 5) ->
    {ok, {some, X}};
index([_, _, _, _, _, _, X | _], 6) ->
    {ok, {some, X}};
index([_, _, _, _, _, _, _, X | _], 7) ->
    {ok, {some, X}};
index(Tuple, Index) when is_tuple(Tuple) andalso is_integer(Index) ->
    {ok, try
        {some, element(Index + 1, Tuple)}
    catch _:_ ->
        none
    end};
index(Map, Key) when is_map(Map) ->
    {ok, try
        {some, maps:get(Key, Map)}
    catch _:_ ->
        none
    end};
index(_, Index) when is_integer(Index) ->
    {error, <<"Indexable">>};
index(_, _) ->
    {error, <<"Dict">>}.

list(T, A, B, C, D) when is_tuple(T) ->
    list(tuple_to_list(T), A, B, C, D);
list([], _, _, _, Acc) ->
    {lists:reverse(Acc), []};
list([X | Xs], Decode, PushPath, Index, Acc) ->
    {Out, Errors} = Decode(X),
    case Errors of
        [] -> list(Xs, Decode, PushPath, Index + 1, [Out | Acc]);
        _ -> PushPath({[], Errors}, integer_to_binary(Index))
    end;
list(Unexpected, _, _, _, []) ->
    Found = gleam@dynamic:classify(Unexpected),
    Error = {decode_error, <<"List"/utf8>>, Found, []},
    {[], [Error]};
list(_, _, _, _, Acc) ->
    {lists:reverse(Acc), []}.

dict(#{} = Data) -> {ok, Data};
dict(_) -> {error, nil}.

int(I) when is_integer(I) -> {ok, I};
int(_) -> {error, 0}.

float(F) when is_float(F) -> {ok, F};
float(_) -> {error, 0.0}.

bit_array(B) when is_bitstring(B) -> {ok, B};
bit_array(_) -> {error, <<>>}.

is_null(X) ->
    X =:= undefined orelse X =:= null orelse X =:= nil.
