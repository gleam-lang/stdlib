-module(gleam@dynamic).
-compile(no_auto_import).

-export([from/1, unsafe_coerce/1, string/1, int/1, float/1, atom/1, bool/1, thunk/1, list/2, field/2, element/2]).

from(A) ->
    gleam_stdlib:identity(A).

unsafe_coerce(A) ->
    gleam_stdlib:identity(A).

string(A) ->
    gleam_stdlib:decode_string(A).

int(A) ->
    gleam_stdlib:decode_int(A).

float(A) ->
    gleam_stdlib:decode_float(A).

atom(A) ->
    gleam_stdlib:decode_atom(A).

bool(A) ->
    gleam_stdlib:decode_bool(A).

thunk(A) ->
    gleam_stdlib:decode_thunk(A).

list_dynamic(A) ->
    gleam_stdlib:decode_list(A).

list(Dynamic, DecoderType) ->
    gleam@result:then(
        list_dynamic(Dynamic),
        fun(GleamCaptureVariable) ->
            gleam@list:traverse(GleamCaptureVariable, DecoderType)
        end
    ).

field(A, B) ->
    gleam_stdlib:decode_field(A, B).

element(A, B) ->
    gleam_stdlib:decode_element(A, B).
