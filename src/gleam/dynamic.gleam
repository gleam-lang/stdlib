/// `Dynamic` data is data that we don"t know the type of yet.
/// We likely get data like this from interop with Erlang, or from
/// IO with the outside world.

import gleam/list as list_mod
import gleam/atom
import gleam/result

pub external type Dynamic;

/// Convert any Gleam data into `Dynamic` data.
///
pub external fn from(a) -> Dynamic = "gleam_stdlib" "identity";

/// Unsafely cast a Dynamic value into any other type.
///
/// This is an escape hatch for the type system that may be useful when wrapping
/// native Erlang APIs. It is to be used as a last measure only.
///
pub external fn unsafe_coerce(a) -> b = "gleam_stdlib" "identity";

/// Safely cast a Dynamic value into a String.
///
/// ## Examples
/// ```gleam
/// string(from("Hello")) == Ok("Hello")
/// string(from(123)) == Error("Expected a String, got `123`")
/// ```
pub external fn string(from: Dynamic) -> Result(String, String)
  = "gleam_stdlib" "decode_string"

/// Safely cast a Dynamic value into a String.
///
/// ## Examples
/// ```gleam
/// int(from(123)) == Ok(123)
/// int(from("Hello")) == Error("Expected an Int, got `\"Hello World\"`")
/// ```
pub external fn int(from: Dynamic) -> Result(Int, String)
  = "gleam_stdlib" "decode_int"

/// Safely cast a Dynamic value into a Float.
///
/// ## Examples
/// ```gleam
/// float(from(2.0)) == Ok(2.0)
/// float(from(123)) == Error("Expected a Float, got `123`")
/// ```
pub external fn float(from: Dynamic) -> Result(Float, String)
  = "gleam_stdlib" "decode_float"

/// Safely cast a Dynamic value into an Atom.
///
/// ## Examples
/// ```gleam
/// import gleam/atom.{create_from_string, to_string}
/// to_string(atom(from(create_from_string("hello")))) == "hello"
/// atom(from(123)) == Error("Expected an Atom, got `123`")
/// ```
pub external fn atom(from: Dynamic) -> Result(atom.Atom, String)
  = "gleam_stdlib" "decode_atom"

/// Safely cast a Dynamic value into a Bool.
///
/// ## Examples
/// ```gleam
/// bool(from(True)) == Ok(True)
/// bool(from(123)) == Error("Expected a Bool, got `123`")
/// ```
pub external fn bool(from: Dynamic) -> Result(Bool, String)
  = "gleam_stdlib" "decode_bool"

/// Safely cast a Dynamic value into a String.
///
/// ## Examples
/// ```gleam
/// import gleam/result
/// let f = fn() { 1 }
/// result.map(thunk(from(f)), fn(v) { v == f}) == True
/// thunk(from(123)) == Error("Expected a zero arity function, got `123`")
/// ```
pub external fn thunk(from: Dynamic) -> Result(fn() -> Dynamic, String)
  = "gleam_stdlib" "decode_thunk"

external fn list_dynamic(from: Dynamic) -> Result(List(Dynamic), String)
  = "gleam_stdlib" "decode_list"

/// Safely cast a Dynamic value into a List of some type.
///
/// ## Examples
/// ```gleam
/// list(from(["a", "b", "c"]), string) == Ok(["a", "b", "c"])
/// list(from([1, 2, 3]), string) == Error("Expected a List, got `[1, 2, 3]`")
/// ```
pub fn list(
  from dynamic: Dynamic,
  containing decoder_type: fn(Dynamic) -> Result(inner, String),
) -> Result(List(inner), String) {
  dynamic
  |> list_dynamic
  |> result.then(_, list_mod.traverse(_, decoder_type))
}

/// Returns a field from a Dynamic value representing a map if it exists.
/// This will not succeed on a record.
///
/// ## Examples
/// ```gleam
/// import gleam/map
/// field(from(map.new("Hello", "World")), "Hello") == Ok(Dynamic)
/// field(from(123)) == Error("Expected a map with key `\"Hello\"`, got `123`")
/// ```
pub external fn field(from: Dynamic, named: a) -> Result(Dynamic, String)
  = "gleam_stdlib" "decode_field"

/// Returns an element of a Dynamic value representing a tuple if it exists.
///
/// ## Examples
/// ```gleam
/// element(from(tuple(1, 2)), 0) == Ok(Dynamic)
/// element(from(tuple(1, 2)), 2) == Error("Element position is out-of-bounds")
/// ```
pub external fn element(from: Dynamic, position: Int) -> Result(Dynamic, String)
  = "gleam_stdlib" "decode_element"
