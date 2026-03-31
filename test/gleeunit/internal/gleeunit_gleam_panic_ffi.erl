-module(gleeunit_gleam_panic_ffi).
-export([from_dynamic/1]).

from_dynamic(#{
  gleam_error := assert,
  start := Start,
  'end' := End,
  expression_start := EStart
} = E) ->
    wrap(E, {assert, Start, End, EStart, assert_kind(E)});
from_dynamic(#{
  gleam_error := let_assert,
  start := Start,
  'end' := End,
  pattern_start := PStart,
  pattern_end := PEnd,
  value := Value
} = E) ->
    wrap(E, {let_assert, Start, End, PStart, PEnd, Value});
from_dynamic(#{gleam_error := panic} = E) ->
    wrap(E, panic);
from_dynamic(#{gleam_error := todo} = E) ->
    wrap(E, todo);
from_dynamic(_) ->
    {error, nil}.

assert_kind(#{kind := binary_operator, left := L, right := R, operator := O}) ->
    {binary_operator, atom_to_binary(O), expression(L), expression(R)};
assert_kind(#{kind := function_call, arguments := Arguments}) ->
    {function_call, lists:map(fun expression/1, Arguments)};
assert_kind(#{kind := expression, expression := Expression}) ->
    {other_expression, expression(Expression)}.

expression(#{start := S, 'end' := E, kind := literal, value := Value}) ->
    {asserted_expression, S, E, {literal, Value}};
expression(#{start := S, 'end' := E, kind := expression, value := Value}) ->
    {asserted_expression, S, E, {expression, Value}};
expression(#{start := S, 'end' := E, kind := unevaluated}) ->
    {asserted_expression, S, E, unevaluated}.

wrap(#{
    gleam_error := _,
    file := File,
    message := Message,
    module := Module,
    function := Function,
    line := Line
}, Kind) ->
    {ok, {gleam_panic, Message, File, Module, Function, Line, Kind}}.
