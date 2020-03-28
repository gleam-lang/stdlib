-module(gleam@dynamic_test).
-compile(no_auto_import).

-export([string_test/0, int_test/0, float_test/0, thunk_test/0, bool_test/0, atom_test/0, list_test/0, field_test/0, element_test/0]).

string_test() ->
    gleam@should:equal(
        gleam@dynamic:string(gleam@dynamic:from(<<"">>)),
        {ok, <<"">>}
    ),
    gleam@should:equal(
        gleam@dynamic:string(gleam@dynamic:from(<<"Hello">>)),
        {ok, <<"Hello">>}
    ),
    gleam@should:equal(
        gleam@dynamic:string(gleam@dynamic:from(1)),
        {error, <<"Expected a String, got `1`">>}
    ),
    gleam@should:equal(
        gleam@dynamic:string(gleam@dynamic:from([])),
        {error, <<"Expected a String, got `[]`">>}
    ).

int_test() ->
    gleam@should:equal(gleam@dynamic:int(gleam@dynamic:from(1)), {ok, 1}),
    gleam@should:equal(gleam@dynamic:int(gleam@dynamic:from(2)), {ok, 2}),
    gleam@should:equal(
        gleam@dynamic:int(gleam@dynamic:from(1.0)),
        {error, <<"Expected an Int, got `1.0`">>}
    ),
    gleam@should:equal(
        gleam@dynamic:int(gleam@dynamic:from([])),
        {error, <<"Expected an Int, got `[]`">>}
    ).

float_test() ->
    gleam@should:equal(gleam@dynamic:float(gleam@dynamic:from(1.0)), {ok, 1.0}),
    gleam@should:equal(gleam@dynamic:float(gleam@dynamic:from(2.2)), {ok, 2.2}),
    gleam@should:equal(
        gleam@dynamic:float(gleam@dynamic:from(1)),
        {error, <<"Expected a Float, got `1`">>}
    ),
    gleam@should:equal(
        gleam@dynamic:float(gleam@dynamic:from([])),
        {error, <<"Expected a Float, got `[]`">>}
    ).

thunk_test() ->
    gleam@should:be_ok(gleam@dynamic:thunk(gleam@dynamic:from(fun() -> 1 end))),
    gleam@should:equal(
        gleam@result:map(
            gleam@dynamic:thunk(gleam@dynamic:from(fun() -> 1 end)),
            fun(F) -> F() end
        ),
        {ok, gleam@dynamic:from(1)}
    ),
    gleam@should:be_error(
        gleam@dynamic:thunk(gleam@dynamic:from(fun(X) -> X end))
    ),
    gleam@should:be_error(gleam@dynamic:thunk(gleam@dynamic:from(1))),
    gleam@should:be_error(gleam@dynamic:thunk(gleam@dynamic:from([]))).

bool_test() ->
    gleam@should:equal(
        gleam@dynamic:bool(gleam@dynamic:from(true)),
        {ok, true}
    ),
    gleam@should:equal(
        gleam@dynamic:bool(gleam@dynamic:from(false)),
        {ok, false}
    ),
    gleam@should:equal(
        gleam@dynamic:bool(gleam@dynamic:from(1)),
        {error, <<"Expected a Bool, got `1`">>}
    ),
    gleam@should:equal(
        gleam@dynamic:bool(gleam@dynamic:from([])),
        {error, <<"Expected a Bool, got `[]`">>}
    ).

atom_test() ->
    gleam@should:equal(
        gleam@dynamic:atom(
            gleam@dynamic:from(gleam@atom:create_from_string(<<"">>))
        ),
        {ok, gleam@atom:create_from_string(<<"">>)}
    ),
    gleam@should:equal(
        gleam@dynamic:atom(
            gleam@dynamic:from(gleam@atom:create_from_string(<<"ok">>))
        ),
        {ok, gleam@atom:create_from_string(<<"ok">>)}
    ),
    gleam@should:be_error(gleam@dynamic:atom(gleam@dynamic:from(1))),
    gleam@should:be_error(gleam@dynamic:atom(gleam@dynamic:from([]))).

list_test() ->
    gleam@should:equal(
        gleam@dynamic:list(gleam@dynamic:from([]), fun gleam@dynamic:string/1),
        {ok, []}
    ),
    gleam@should:equal(
        gleam@dynamic:list(gleam@dynamic:from([]), fun gleam@dynamic:int/1),
        {ok, []}
    ),
    gleam@should:equal(
        gleam@dynamic:list(
            gleam@dynamic:from([1, 2, 3]),
            fun gleam@dynamic:int/1
        ),
        {ok, [1, 2, 3]}
    ),
    gleam@should:equal(
        gleam@dynamic:list(
            gleam@dynamic:from([[1], [2], [3]]),
            fun(Capture1) ->
                gleam@dynamic:list(Capture1, fun gleam@dynamic:int/1)
            end
        ),
        {ok, [[1], [2], [3]]}
    ),
    gleam@should:be_error(
        gleam@dynamic:list(gleam@dynamic:from(1), fun gleam@dynamic:string/1)
    ),
    gleam@should:be_error(
        gleam@dynamic:list(gleam@dynamic:from(1.0), fun gleam@dynamic:int/1)
    ),
    gleam@should:be_error(
        gleam@dynamic:list(
            gleam@dynamic:from([<<"">>]),
            fun gleam@dynamic:int/1
        )
    ),
    gleam@should:be_error(
        gleam@dynamic:list(
            gleam@dynamic:from(
                [gleam@dynamic:from(1), gleam@dynamic:from(<<"not an int">>)]
            ),
            fun gleam@dynamic:int/1
        )
    ).

field_test() ->
    {ok, OkAtom} = gleam@atom:from_string(<<"ok">>),
    {ok, ErrorAtom} = gleam@atom:from_string(<<"error">>),
    gleam@should:equal(
        gleam@dynamic:field(
            gleam@dynamic:from(gleam@map:insert(gleam@map:new(), OkAtom, 1)),
            OkAtom
        ),
        {ok, gleam@dynamic:from(1)}
    ),
    gleam@should:equal(
        gleam@dynamic:field(
            gleam@dynamic:from(
                gleam@map:insert(
                    gleam@map:insert(gleam@map:new(), OkAtom, 3),
                    ErrorAtom,
                    1
                )
            ),
            OkAtom
        ),
        {ok, gleam@dynamic:from(3)}
    ),
    gleam@should:be_error(
        gleam@dynamic:field(gleam@dynamic:from(gleam@map:new()), OkAtom)
    ),
    gleam@should:be_error(gleam@dynamic:field(gleam@dynamic:from(1), OkAtom)),
    gleam@should:be_error(gleam@dynamic:field(gleam@dynamic:from([]), [])).

element_test() ->
    {ok, OkAtom} = gleam@atom:from_string(<<"ok">>),
    OkOneTuple = {OkAtom, 1},
    gleam@should:equal(
        gleam@dynamic:element(gleam@dynamic:from(OkOneTuple), 0),
        {ok, gleam@dynamic:from(OkAtom)}
    ),
    gleam@should:equal(
        gleam@dynamic:element(gleam@dynamic:from(OkOneTuple), 1),
        {ok, gleam@dynamic:from(1)}
    ),
    gleam@should:be_error(
        gleam@dynamic:element(gleam@dynamic:from(OkOneTuple), 2)
    ),
    gleam@should:be_error(
        gleam@dynamic:element(gleam@dynamic:from(OkOneTuple), -1)
    ),
    gleam@should:be_error(gleam@dynamic:element(gleam@dynamic:from(1), 0)),
    gleam@should:be_error(
        gleam@dynamic:element(
            gleam@dynamic:from(gleam@map:insert(gleam@map:new(), 1, OkAtom)),
            0
        )
    ).
