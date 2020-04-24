-module(gleam@atom_test).
-compile(no_auto_import).

-export([from_string_test/0, create_from_string_test/0, to_string_test/0]).

from_string_test() ->
    gleam@should:be_ok(gleam@atom:from_string(<<"ok"/utf8>>)),
    gleam@should:be_ok(gleam@atom:from_string(<<"expect"/utf8>>)),
    gleam@should:equal(
        gleam@atom:from_string(
            <<"this is not an atom we have seen before"/utf8>>
        ),
        {error, atom_not_loaded}
    ).

create_from_string_test() ->
    gleam@should:equal(
        {ok, gleam@atom:create_from_string(<<"ok"/utf8>>)},
        gleam@atom:from_string(<<"ok"/utf8>>)
    ),
    gleam@should:equal(
        {ok, gleam@atom:create_from_string(<<"expect"/utf8>>)},
        gleam@atom:from_string(<<"expect"/utf8>>)
    ),
    gleam@should:equal(
        {ok,
         gleam@atom:create_from_string(
             <<"this is another atom we have not seen before"/utf8>>
         )},
        gleam@atom:from_string(
            <<"this is another atom we have not seen before"/utf8>>
        )
    ).

to_string_test() ->
    gleam@should:equal(
        gleam@atom:to_string(gleam@atom:create_from_string(<<"ok"/utf8>>)),
        <<"ok"/utf8>>
    ),
    gleam@should:equal(
        gleam@atom:to_string(gleam@atom:create_from_string(<<"expect"/utf8>>)),
        <<"expect"/utf8>>
    ).
