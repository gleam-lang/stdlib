-module(gleam@option).
-compile(no_auto_import).

-export([is_some/1, is_none/1, map/2, flatten/1, then/2, unwrap/2]).

is_some(Option) ->
    case Option of
        none ->
            false;

        {some, _} ->
            true
    end.

is_none(Option) ->
    case Option of
        {some, _} ->
            false;

        none ->
            true
    end.

map(Option, Fun) ->
    case Option of
        {some, X} ->
            {some, Fun(X)};

        none ->
            none
    end.

flatten(Option) ->
    case Option of
        {some, X} ->
            X;

        none ->
            none
    end.

then(Option, Fun) ->
    case Option of
        {some, X} ->
            Fun(X);

        none ->
            none
    end.

unwrap(Option, Default) ->
    case Option of
        {some, V} ->
            V;

        none ->
            Default
    end.
