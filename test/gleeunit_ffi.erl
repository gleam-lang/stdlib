-module(gleeunit_ffi).

-export([find_files/2, run_eunit/2]).

find_files(Pattern, In) ->
  Results = filelib:wildcard(binary_to_list(Pattern), binary_to_list(In)),
  lists:map(fun list_to_binary/1, Results).

run_eunit(Tests, Options) ->
    case eunit:test(Tests, Options) of
        ok -> {ok, nil};
        error -> {error, nil};
        {error, Term} -> {error, Term}
    end.
