-module(gleam_stdlib).
-include_lib("eunit/include/eunit.hrl").

-export([should_equal/2, should_not_equal/2, should_be_ok/1, should_be_error/1,
         atom_from_string/1, atom_create_from_string/1, atom_to_string/1,
         map_get/2, iodata_append/2, iodata_prepend/2, identity/1,
         decode_int/1, decode_bool/1, decode_float/1,
         decode_thunk/1, decode_atom/1, decode_list/1, decode_field/2,
         decode_element/2, parse_int/1, parse_float/1, compare_strings/2,
         string_pop_grapheme/1, string_starts_with/2, string_ends_with/2,
         string_pad/4, decode_tuple2/1, decode_map/1, bit_string_int_to_u32/1,
         bit_string_int_from_u32/1, bit_string_append/2, bit_string_part_/3,
         decode_bit_string/1, compile_regex/2, regex_match/2, regex_split/2,
         regex_scan/2, base_decode64/1, wrap_list/1, rescue/1]).

should_equal(Actual, Expected) -> ?assertEqual(Expected, Actual).
should_not_equal(Actual, Expected) -> ?assertNotEqual(Expected, Actual).
should_be_ok(A) -> ?assertMatch({ok, _}, A).
should_be_error(A) -> ?assertMatch({error, _}, A).

map_get(Map, Key) ->
    case maps:find(Key, Map) of
        error -> {error, nil};
        OkFound -> OkFound
    end.

atom_create_from_string(S) ->
    binary_to_atom(S, utf8).

atom_to_string(S) ->
    atom_to_binary(S, utf8).

atom_from_string(S) ->
    try {ok, binary_to_existing_atom(S, utf8)}
    catch error:badarg -> {error, atom_not_loaded}
    end.

iodata_append(Iodata, String) -> [Iodata, String].
iodata_prepend(Iodata, String) -> [String, Iodata].

identity(X) -> X.

decode_error_msg(Type, Data) ->
    {error, iolist_to_binary(io_lib:format("Expected ~s, got ~s", [Type, classify(Data)]))}.

classify(X) when is_atom(X) -> "an atom";
classify(X) when is_binary(X) -> "a binary";
classify(X) when is_integer(X) -> "an int";
classify(X) when is_float(X) -> "a float";
classify(X) when is_list(X) -> "a list";
classify(X) when is_boolean(X) -> "a bool";
classify(X) when is_function(X, 0) -> "a zero arity function";
classify(X) when is_tuple(X) -> ["a ", integer_to_list(tuple_size(X)), " element tuple"];
classify(_) -> "some other type".

decode_tuple2({_, _} = T) -> {ok, T};
decode_tuple2(Data) -> decode_error_msg("a 2 element tuple", Data).

decode_map(Data) when is_map(Data) -> {ok, Data};
decode_map(Data) -> decode_error_msg("a map", Data).

decode_atom(Data) when is_atom(Data) -> {ok, Data};
decode_atom(Data) -> decode_error_msg("an atom", Data).

decode_bit_string(Data) when is_bitstring(Data) -> {ok, Data};
decode_bit_string(Data) -> decode_error_msg("a bit_string", Data).

decode_int(Data) when is_integer(Data) -> {ok, Data};
decode_int(Data) -> decode_error_msg("an int", Data).

decode_float(Data) when is_float(Data) -> {ok, Data};
decode_float(Data) -> decode_error_msg("a float", Data).

decode_bool(Data) when is_boolean(Data) -> {ok, Data};
decode_bool(Data) -> decode_error_msg("a bool", Data).

decode_thunk(Data) when is_function(Data, 0) -> {ok, Data};
decode_thunk(Data) -> decode_error_msg("a zero arity function", Data).

decode_list(Data) when is_list(Data) -> {ok, Data};
decode_list(Data) -> decode_error_msg("a list", Data).

decode_field(Data, Key) ->
    case Data of
        #{Key := Value} ->
            {ok, Value};

        _ ->
            decode_error_msg(io_lib:format("a map with key `~p`", [Key]), Data)
    end.

decode_element(Data, Position) when is_tuple(Data) ->
    case catch element(Position + 1, Data) of
        {'EXIT', _Reason} ->
            decode_error_msg(["a tuple of at least ", integer_to_list(Position + 1), " size"], Data);

        Value ->
            {ok, Value}
    end;
decode_element(Data, _Position) -> decode_error_msg("a tuple", Data).

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

compare_strings(Lhs, Rhs) ->
    if
        Lhs == Rhs -> eq;
        Lhs < Rhs -> lt;
        true -> gt
    end.

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
    unicode:characters_to_binary(string:pad(String, Length, Dir, PadString)).

string_pop_grapheme(String) ->
    case string:next_grapheme(String) of
        [ Next | Rest ] ->
            {ok, {unicode:characters_to_binary([Next]), unicode:characters_to_binary(Rest)}};
        _ -> {error, nil}
    end.

bit_string_append(First, Second) ->
    <<First/bitstring, Second/bitstring>>.

bit_string_part_(Bin, Pos, Len) ->
    try {ok, binary:part(Bin, Pos, Len)}
    catch error:badarg -> {error, nil}
    end.

bit_string_int_to_u32(I) when 0 =< I, I < 4294967296 ->
    {ok, <<I:32>>};
bit_string_int_to_u32(_) ->
    {error, nil}.

bit_string_int_from_u32(<<I:32>>) ->
    {ok, I};
bit_string_int_from_u32(_) ->
    {error, nil}.

compile_regex(String, Options) ->
    {options, Caseless, Multiline} = Options,
    OptionsList = [
        unicode,
        Caseless andalso caseless,
        Multiline andalso multiline
    ],
    FilteredOptions = [Option || Option <- OptionsList, Option /= false],
    case re:compile(String, FilteredOptions) of
        {ok, MP} -> {ok, MP};
        {error, {Str, Pos}} ->
            {error, {compile_error, unicode:characters_to_binary(Str), Pos}}
    end.

regex_match(Regex, String) ->
    re:run(String, Regex) /= nomatch.

regex_split(Regex, String) ->
    re:split(String, Regex).

regex_submatches(String, {S, L}) ->
    SubMatch = string:slice(String, S, L),
    case string:is_empty(SubMatch) of
        true -> none;
        false -> {some, SubMatch}
    end.

regex_matches(String, [{S, L} | Submatches]) ->
    {match, binary:part(String, S, L), S,
     lists:map(fun(X) -> regex_submatches(String, X) end, Submatches)}.

regex_scan(Regex, String) ->
    case re:run(String, Regex, [global]) of
        {match, Captured} -> lists:map(fun(X) -> regex_matches(String, X) end, Captured);
        nomatch -> []
    end.

base_decode64(S) ->
    try {ok, base64:decode(S)}
    catch error:badarith -> {error, nil}
    end.

wrap_list(X) when is_list(X) -> X;
wrap_list(X) -> [X].

rescue(F) ->
    try {ok, F()}
    catch
        throw:X -> {error, {thrown, X}};
        error:X -> {error, {errored, X}};
        exit:X -> {error, {exited, X}}
    end.
