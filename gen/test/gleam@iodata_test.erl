-module(gleam@iodata_test).
-compile(no_auto_import).

-export([iodata_test/0, lowercase_test/0, uppercase_test/0, split_test/0, is_equal_test/0, is_empty_test/0]).

iodata_test() ->
    Data = gleam@iodata:prepend(
        gleam@iodata:append(
            gleam@iodata:append(gleam@iodata:new(<<"ello"/utf8>>), <<","/utf8>>),
            <<" world!"/utf8>>
        ),
        <<"H"/utf8>>
    ),
    gleam@should:equal(gleam@iodata:to_string(Data), <<"Hello, world!"/utf8>>),
    gleam@should:equal(gleam@iodata:byte_size(Data), 13),
    Data1 = gleam@iodata:prepend_iodata(
        gleam@iodata:append_iodata(
            gleam@iodata:append_iodata(
                gleam@iodata:new(<<"ello"/utf8>>),
                gleam@iodata:new(<<","/utf8>>)
            ),
            gleam@iodata:concat(
                [gleam@iodata:new(<<" wo"/utf8>>),
                 gleam@iodata:new(<<"rld!"/utf8>>)]
            )
        ),
        gleam@iodata:new(<<"H"/utf8>>)
    ),
    gleam@should:equal(gleam@iodata:to_string(Data1), <<"Hello, world!"/utf8>>),
    gleam@should:equal(gleam@iodata:byte_size(Data1), 13).

lowercase_test() ->
    gleam@should:equal(
        gleam@iodata:to_string(
            gleam@iodata:lowercase(
                gleam@iodata:from_strings([<<"Gleam"/utf8>>, <<"Gleam"/utf8>>])
            )
        ),
        <<"gleamgleam"/utf8>>
    ).

uppercase_test() ->
    gleam@should:equal(
        gleam@iodata:to_string(
            gleam@iodata:uppercase(
                gleam@iodata:from_strings([<<"Gleam"/utf8>>, <<"Gleam"/utf8>>])
            )
        ),
        <<"GLEAMGLEAM"/utf8>>
    ).

split_test() ->
    gleam@should:equal(
        gleam@iodata:split(
            gleam@iodata:new(<<"Gleam,Erlang,Elixir"/utf8>>),
            <<","/utf8>>
        ),
        [gleam@iodata:new(<<"Gleam"/utf8>>),
         gleam@iodata:new(<<"Erlang"/utf8>>),
         gleam@iodata:new(<<"Elixir"/utf8>>)]
    ),
    gleam@should:equal(
        gleam@iodata:split(
            gleam@iodata:from_strings(
                [<<"Gleam, Erl"/utf8>>, <<"ang,Elixir"/utf8>>]
            ),
            <<", "/utf8>>
        ),
        [gleam@iodata:new(<<"Gleam"/utf8>>),
         gleam@iodata:from_strings([<<"Erl"/utf8>>, <<"ang,Elixir"/utf8>>])]
    ).

is_equal_test() ->
    gleam@should:be_true(
        gleam@iodata:is_equal(
            gleam@iodata:new(<<"12"/utf8>>),
            gleam@iodata:from_strings([<<"1"/utf8>>, <<"2"/utf8>>])
        )
    ),
    gleam@should:be_true(
        gleam@iodata:is_equal(
            gleam@iodata:new(<<"12"/utf8>>),
            gleam@iodata:new(<<"12"/utf8>>)
        )
    ),
    gleam@should:be_false(
        gleam@iodata:is_equal(
            gleam@iodata:new(<<"12"/utf8>>),
            gleam@iodata:new(<<"2"/utf8>>)
        )
    ).

is_empty_test() ->
    gleam@should:be_true(gleam@iodata:is_empty(gleam@iodata:new(<<""/utf8>>))),
    gleam@should:be_false(
        gleam@iodata:is_empty(gleam@iodata:new(<<"12"/utf8>>))
    ),
    gleam@should:be_true(gleam@iodata:is_empty(gleam@iodata:from_strings([]))),
    gleam@should:be_true(
        gleam@iodata:is_empty(
            gleam@iodata:from_strings([<<""/utf8>>, <<""/utf8>>])
        )
    ).
