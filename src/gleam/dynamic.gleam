import gleam/list as list_mod
import gleam/atom
import gleam/result

// `Dynamic` data is data that we don"t know the type of yet.
// We likely get data like this from interop with Erlang, or from
// IO with the outside world.
pub external type Dynamic;

// Convert any Gleam data into `Dynamic` data.
pub external fn from(a) -> Dynamic = "gleam_stdlib" "identity";

// Unsafely cast a Dynamic value into any other type.
//
// This is an escape hatch for the type system that may be useful when wrapping
// native Erlang APIs. It is to be used as a last measure only.
pub external fn unsafe_coerce(a) -> b = "gleam_stdlib" "identity";

pub external fn string(from: Dynamic) -> Result(String, String)
  = "gleam_stdlib" "decode_string"

pub external fn int(from: Dynamic) -> Result(Int, String)
  = "gleam_stdlib" "decode_int"

pub external fn float(from: Dynamic) -> Result(Float, String)
  = "gleam_stdlib" "decode_float"

pub external fn atom(from: Dynamic) -> Result(atom.Atom, String)
  = "gleam_stdlib" "decode_atom"

pub external fn bool(from: Dynamic) -> Result(Bool, String)
  = "gleam_stdlib" "decode_bool"

pub external fn thunk(from: Dynamic) -> Result(fn() -> Dynamic, String)
  = "gleam_stdlib" "decode_thunk"

external fn list_dynamic(from: Dynamic) -> Result(List(Dynamic), String)
  = "gleam_stdlib" "decode_list"

pub fn list(
  from dynamic: Dynamic,
  containing decoder_type: fn(Dynamic) -> Result(inner, String),
) -> Result(List(inner), String) {
  dynamic
  |> list_dynamic
  |> result.then(_, list_mod.traverse(_, decoder_type))
}

pub external fn field(from: Dynamic, named: a) -> Result(Dynamic, String)
  = "gleam_stdlib" "decode_field"

pub external fn element(from: Dynamic, position: Int) -> Result(Dynamic, String)
  = "gleam_stdlib" "decode_element"
