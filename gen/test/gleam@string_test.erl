-module(gleam@string_test).
-compile(no_auto_import).

-export([length_test/0, lowercase_test/0, uppercase_test/0, reverse_test/0, split_test/0, replace_test/0, append_test/0, compare_test/0, contains_test/0, concat_test/0, repeat_test/0, join_test/0, slice_test/0, drop_left_test/0, drop_right_test/0, starts_with_test/0, ends_with_test/0, pad_left_test/0, pad_right_test/0, trim_test/0, trim_left_test/0, trim_right_test/0, to_graphemes_test/0, next_grapheme_test/0]).

length_test() ->
    gleam@should:equal(gleam@string:length(<<"ß↑e̊"/utf8>>), 3),
    gleam@should:equal(gleam@string:length(<<"Gleam"/utf8>>), 5),
    gleam@should:equal(gleam@string:length(<<""/utf8>>), 0).

lowercase_test() ->
    gleam@should:equal(
        gleam@string:lowercase(<<"Gleam"/utf8>>),
        <<"gleam"/utf8>>
    ).

uppercase_test() ->
    gleam@should:equal(
        gleam@string:uppercase(<<"Gleam"/utf8>>),
        <<"GLEAM"/utf8>>
    ).

reverse_test() ->
    gleam@should:equal(
        gleam@string:reverse(<<"Gleam"/utf8>>),
        <<"maelG"/utf8>>
    ).

split_test() ->
    gleam@should:equal(
        gleam@string:split(<<"Gleam,Erlang,Elixir"/utf8>>, <<","/utf8>>),
        [<<"Gleam"/utf8>>, <<"Erlang"/utf8>>, <<"Elixir"/utf8>>]
    ),
    gleam@should:equal(
        gleam@string:split(<<"Gleam, Erlang,Elixir"/utf8>>, <<", "/utf8>>),
        [<<"Gleam"/utf8>>, <<"Erlang,Elixir"/utf8>>]
    ).

replace_test() ->
    gleam@should:equal(
        gleam@string:replace(
            <<"Gleam,Erlang,Elixir"/utf8>>,
            <<","/utf8>>,
            <<"++"/utf8>>
        ),
        <<"Gleam++Erlang++Elixir"/utf8>>
    ).

append_test() ->
    gleam@should:equal(
        gleam@string:append(<<"Test"/utf8>>, <<" Me"/utf8>>),
        <<"Test Me"/utf8>>
    ).

compare_test() ->
    gleam@should:equal(gleam@string:compare(<<""/utf8>>, <<""/utf8>>), eq),
    gleam@should:equal(gleam@string:compare(<<"a"/utf8>>, <<""/utf8>>), gt),
    gleam@should:equal(gleam@string:compare(<<"a"/utf8>>, <<"A"/utf8>>), gt),
    gleam@should:equal(gleam@string:compare(<<"A"/utf8>>, <<"B"/utf8>>), lt),
    gleam@should:equal(gleam@string:compare(<<"t"/utf8>>, <<"ABC"/utf8>>), gt).

contains_test() ->
    gleam@should:equal(
        gleam@string:contains(<<"gleam"/utf8>>, <<"ea"/utf8>>),
        true
    ),
    gleam@should:equal(
        gleam@string:contains(<<"gleam"/utf8>>, <<"x"/utf8>>),
        false
    ),
    gleam@should:equal(
        gleam@string:contains(<<"bellwether"/utf8>>, <<"bell"/utf8>>),
        true
    ).

concat_test() ->
    gleam@should:equal(
        gleam@string:concat(
            [<<"Hello"/utf8>>, <<", "/utf8>>, <<"world!"/utf8>>]
        ),
        <<"Hello, world!"/utf8>>
    ).

repeat_test() ->
    gleam@should:equal(
        gleam@string:repeat(<<"hi"/utf8>>, 3),
        <<"hihihi"/utf8>>
    ),
    gleam@should:equal(gleam@string:repeat(<<"hi"/utf8>>, 0), <<""/utf8>>),
    gleam@should:equal(gleam@string:repeat(<<"hi"/utf8>>, -1), <<""/utf8>>).

join_test() ->
    gleam@should:equal(
        gleam@string:join([<<"Hello"/utf8>>, <<"world!"/utf8>>], <<", "/utf8>>),
        <<"Hello, world!"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:join([<<"Hello"/utf8>>, <<"world!"/utf8>>], <<"-"/utf8>>),
        <<"Hello-world!"/utf8>>
    ).

slice_test() ->
    Unicode = <<"Hello 👨‍👩‍👧‍👧, I 愛 you."/utf8>>,
    gleam@should:equal(gleam@string:slice(Unicode, 0, 5), <<"Hello"/utf8>>),
    gleam@should:equal(
        gleam@string:slice(Unicode, 6, 4),
        <<"👨‍👩‍👧‍👧, I"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:slice(Unicode, 9, 1000),
        <<"I 愛 you."/utf8>>
    ),
    gleam@should:equal(gleam@string:slice(Unicode, 1000, 1000), <<""/utf8>>),
    gleam@should:equal(gleam@string:slice(Unicode, 5, -1), <<""/utf8>>),
    gleam@should:equal(gleam@string:slice(Unicode, -5, 1), <<""/utf8>>).

drop_left_test() ->
    Unicode = <<"Hello 👨‍👩‍👧‍👧!"/utf8>>,
    gleam@should:equal(
        gleam@string:drop_left(Unicode, 6),
        <<"👨‍👩‍👧‍👧!"/utf8>>
    ),
    gleam@should:equal(gleam@string:drop_left(Unicode, 7), <<"!"/utf8>>),
    gleam@should:equal(gleam@string:drop_left(Unicode, 1000), <<""/utf8>>),
    gleam@should:equal(gleam@string:drop_left(Unicode, -1), Unicode).

drop_right_test() ->
    Unicode = <<"Hello 👨‍👩‍👧‍👧!"/utf8>>,
    gleam@should:equal(gleam@string:drop_right(Unicode, 6), <<"He"/utf8>>),
    gleam@should:equal(gleam@string:drop_right(Unicode, 7), <<"H"/utf8>>),
    gleam@should:equal(gleam@string:drop_right(Unicode, 1000), <<""/utf8>>),
    gleam@should:equal(gleam@string:drop_right(Unicode, -1), Unicode).

starts_with_test() ->
    Unicode = <<"Hello 👨‍👩‍👧‍👧, I 愛 you."/utf8>>,
    gleam@should:equal(
        gleam@string:starts_with(Unicode, <<"Hello "/utf8>>),
        true
    ),
    gleam@should:equal(
        gleam@string:starts_with(
            Unicode,
            <<"Hello 👨‍👩‍👧‍👧,"/utf8>>
        ),
        true
    ),
    gleam@should:equal(
        gleam@string:starts_with(<<"Hello"/utf8>>, Unicode),
        false
    ),
    gleam@should:equal(
        gleam@string:starts_with(<<""/utf8>>, <<""/utf8>>),
        true
    ).

ends_with_test() ->
    Unicode = <<"Hello 👨‍👩‍👧‍👧, I 愛 you."/utf8>>,
    gleam@should:equal(gleam@string:ends_with(Unicode, <<"you."/utf8>>), true),
    gleam@should:equal(
        gleam@string:ends_with(Unicode, <<"I 愛 you."/utf8>>),
        true
    ),
    gleam@should:equal(gleam@string:ends_with(<<"you."/utf8>>, Unicode), false),
    gleam@should:equal(gleam@string:ends_with(<<""/utf8>>, <<""/utf8>>), true).

pad_left_test() ->
    gleam@should:equal(
        gleam@string:pad_left(<<"愛"/utf8>>, 10, <<"*"/utf8>>),
        <<"*********愛"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_left(<<"愛"/utf8>>, 10, <<"abcd"/utf8>>),
        <<"abcdabcda愛"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_left(
            <<"愛"/utf8>>,
            5,
            <<"👨‍👩‍👧‍👧🌵"/utf8>>
        ),
        <<"👨‍👩‍👧‍👧🌵👨‍👩‍👧‍👧🌵愛"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_left(<<"1234"/utf8>>, -1, <<"x"/utf8>>),
        <<"1234"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_left(<<"1234"/utf8>>, 1, <<"x"/utf8>>),
        <<"1234"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_left(<<""/utf8>>, 5, <<"-"/utf8>>),
        <<"-----"/utf8>>
    ).

pad_right_test() ->
    gleam@should:equal(
        gleam@string:pad_right(<<"愛"/utf8>>, 10, <<"*"/utf8>>),
        <<"愛*********"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_right(<<"愛"/utf8>>, 10, <<"abcd"/utf8>>),
        <<"愛abcdabcda"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_right(
            <<"愛"/utf8>>,
            5,
            <<"👨‍👩‍👧‍👧🌵"/utf8>>
        ),
        <<"愛👨‍👩‍👧‍👧🌵👨‍👩‍👧‍👧🌵"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_right(<<"1234"/utf8>>, -1, <<"x"/utf8>>),
        <<"1234"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_right(<<"1234"/utf8>>, 1, <<"x"/utf8>>),
        <<"1234"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_right(<<""/utf8>>, 5, <<"-"/utf8>>),
        <<"-----"/utf8>>
    ).

trim_test() ->
    gleam@should:equal(
        gleam@string:trim(<<"\f\v\t\s\n\r  愛  \f\v\t\s\n\r"/utf8>>),
        <<"愛"/utf8>>
    ).

trim_left_test() ->
    gleam@should:equal(
        gleam@string:trim_left(<<"\f\v\t\s\n\r  愛  \f\v\t\s\n\r"/utf8>>),
        <<"愛  \f\v\t\s\n\r"/utf8>>
    ).

trim_right_test() ->
    gleam@should:equal(
        gleam@string:trim_right(<<"\f\v\t\s\n\r  愛  \f\v\t\s\n\r"/utf8>>),
        <<"\f\v\t\s\n\r  愛"/utf8>>
    ).

to_graphemes_test() ->
    gleam@should:equal(
        gleam@string:to_graphemes(<<"abcd"/utf8>>),
        [<<"a"/utf8>>, <<"b"/utf8>>, <<"c"/utf8>>, <<"d"/utf8>>]
    ),
    gleam@should:equal(
        gleam@string:to_graphemes(
            <<"--👨‍👩‍👧‍👧--🌵--"/utf8>>
        ),
        [<<"-"/utf8>>,
         <<"-"/utf8>>,
         <<"👨‍👩‍👧‍👧"/utf8>>,
         <<"-"/utf8>>,
         <<"-"/utf8>>,
         <<"🌵"/utf8>>,
         <<"-"/utf8>>,
         <<"-"/utf8>>]
    ),
    gleam@should:equal(gleam@string:to_graphemes(<<""/utf8>>), []).

next_grapheme_test() ->
    gleam@should:equal(
        gleam@string:next_grapheme(<<"abc"/utf8>>),
        {ok, {<<"a"/utf8>>, <<"bc"/utf8>>}}
    ),
    gleam@should:equal(
        gleam@string:next_grapheme(<<"👨‍👩‍👧‍👧-🌵"/utf8>>),
        {ok, {<<"👨‍👩‍👧‍👧"/utf8>>, <<"-🌵"/utf8>>}}
    ),
    gleam@should:equal(gleam@string:next_grapheme(<<""/utf8>>), {error, nil}).
