-module(gleam@string_test).
-compile(no_auto_import).

-export([length_test/0, lowercase_test/0, uppercase_test/0, reverse_test/0, split_test/0, replace_test/0, append_test/0, compare_test/0, concat_test/0, join_test/0]).

length_test() ->
    gleam@should:equal(gleam@string:length(<<"ß↑e̊">>), 3),
    gleam@should:equal(gleam@string:length(<<"Gleam">>), 5),
    gleam@should:equal(gleam@string:length(<<"">>), 0).

lowercase_test() ->
    gleam@should:equal(gleam@string:lowercase(<<"Gleam">>), <<"gleam">>).

uppercase_test() ->
    gleam@should:equal(gleam@string:uppercase(<<"Gleam">>), <<"GLEAM">>).

reverse_test() ->
    gleam@should:equal(gleam@string:reverse(<<"Gleam">>), <<"maelG">>).

split_test() ->
    gleam@should:equal(
        gleam@string:split(<<"Gleam,Erlang,Elixir">>, <<",">>),
        [<<"Gleam">>, <<"Erlang">>, <<"Elixir">>]
    ),
    gleam@should:equal(
        gleam@string:split(<<"Gleam, Erlang,Elixir">>, <<", ">>),
        [<<"Gleam">>, <<"Erlang,Elixir">>]
    ).

replace_test() ->
    gleam@should:equal(
        gleam@string:replace(<<"Gleam,Erlang,Elixir">>, <<",">>, <<"++">>),
        <<"Gleam++Erlang++Elixir">>
    ).

append_test() ->
    gleam@should:equal(
        gleam@string:append(<<"Test">>, <<" Me">>),
        <<"Test Me">>
    ).

compare_test() ->
    gleam@should:equal(gleam@string:compare(<<"">>, <<"">>), eq),
    gleam@should:equal(gleam@string:compare(<<"a">>, <<"">>), gt),
    gleam@should:equal(gleam@string:compare(<<"a">>, <<"A">>), gt),
    gleam@should:equal(gleam@string:compare(<<"A">>, <<"B">>), lt),
    gleam@should:equal(gleam@string:compare(<<"t">>, <<"ABC">>), gt).

concat_test() ->
    gleam@should:equal(
        gleam@string:concat([<<"Hello">>, <<", ">>, <<"world!">>]),
        <<"Hello, world!">>
    ).

join_test() ->
    gleam@should:equal(
        gleam@string:join([<<"Hello">>, <<"world!">>], <<", ">>),
        <<"Hello, world!">>
    ),
    gleam@should:equal(
        gleam@string:join([<<"Hello">>, <<"world!">>], <<"-">>),
        <<"Hello-world!">>
    ).
