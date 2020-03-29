-module(gleam@should).
-compile(no_auto_import).

-export([equal/2, not_equal/2, be_true/1, be_false/1, be_ok/1, be_error/1, fail/0]).

equal(A, B) ->
    gleam_stdlib:should_equal(A, B).

not_equal(A, B) ->
    gleam_stdlib:should_not_equal(A, B).

be_true(A) ->
    gleam_stdlib:should_be_true(A).

be_false(A) ->
    gleam_stdlib:should_be_false(A).

be_ok(A) ->
    gleam_stdlib:should_be_ok(A).

be_error(A) ->
    gleam_stdlib:should_be_error(A).

fail() ->
    be_true(false).
