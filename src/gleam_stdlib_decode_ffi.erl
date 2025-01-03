-module(gleam_stdlib_decode_ffi).

-export([
    strict_index/2, list/5, dict/1, bit_array/1, int/1, float/1, option/1
]).

strict_index([X | _], 0) ->
    {ok, {some, X}};
strict_index([_, X | _], 1) ->
    {ok, {some, X}};
strict_index(Tuple, Index) when is_tuple(Tuple) andalso is_integer(Index) ->
    {ok, try
        {some, element(Index + 1, Tuple)}
    catch _:_ -> 
        none
    end};
strict_index(Map, Key) when is_map(Map) ->
    {ok, try
        {some, maps:get(Key, Map)}
    catch _:_ -> 
        none
    end};
strict_index(_, Index) when is_integer(Index) ->
    {error, <<"Indexable">>};
strict_index(_, _) ->
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

bit_array(B) when is_bitstring(B) -> {ok, B};
bit_array(_) -> {error, nil}.

int(X) when is_integer(X) -> {ok, X};
int(_) -> {error, nil}.

float(X) when is_float(X) -> {ok, X};
float(_) -> {error, nil}.

option(nil) -> none;
option(null) -> none;
option(none) -> none;
option(error) -> none;
option(undefined) -> none;
option(X) -> {some, X}.
