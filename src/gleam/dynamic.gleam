import gleam/list as list_mod
import gleam/atom
import gleam/result

/// `Dynamic` data is data that we don"t know the type of yet.
/// We likely get data like this from interop with Erlang, or from
/// IO with the outside world.
pub external type Dynamic

/// Convert any Gleam data into `Dynamic` data.
///
pub external fn from(a) -> Dynamic =
  "gleam_stdlib" "identity"

/// Unsafely cast a Dynamic value into any other type.
///
/// This is an escape hatch for the type system that may be useful when wrapping
/// native Erlang APIs. It is to be used as a last measure only!
///
/// If you can avoid using this function, do!
///
pub external fn unsafe_coerce(Dynamic) -> a =
  "gleam_stdlib" "identity"

/// Check to see whether a Dynamic value is a string, and return the string if
/// it is.
///
/// ## Examples
///
///    > string(from("Hello"))
///    Ok("Hello")
///
///    > string(from(123))
///    Error("Expected a String, got `123`")
///
pub external fn string(from: Dynamic) -> Result(String, String) =
  "gleam_stdlib" "decode_string"

/// Check to see whether a Dynamic value is an int, and return the int if it
/// is.
///
/// ## Examples
///
///    > int(from(123))
///    Ok(123)
///
///    > int(from("Hello"))
///    Error("Expected an Int, got `\"Hello World\"`")
///
pub external fn int(from: Dynamic) -> Result(Int, String) =
  "gleam_stdlib" "decode_int"

/// Check to see whether a Dynamic value is an float, and return the float if
/// it is.
///
/// ## Examples
///
///    > float(from(2.0))
///    Ok(2.0)
///
///    > float(from(123))
///    Error("Expected a Float, got `123`")
///
pub external fn float(from: Dynamic) -> Result(Float, String) =
  "gleam_stdlib" "decode_float"

/// Check to see whether a Dynamic value is an atom, and return the atom if
/// it is.
///
/// ## Examples
///
///    > import gleam/atom
///    > atom(from(atom.create_from_string("hello")))
///    OK("hello")
///
///    > atom(from(123))
///    Error("Expected an Atom, got `123`")
///
pub external fn atom(from: Dynamic) -> Result(atom.Atom, String) =
  "gleam_stdlib" "decode_atom"

/// Check to see whether a Dynamic value is an bool, and return the bool if
/// it is.
///
/// ## Examples
///
///    > bool(from(True))
///    Ok(True)
///
///    > bool(from(123))
///    Error("Expected a Bool, got `123`")
///
pub external fn bool(from: Dynamic) -> Result(Bool, String) =
  "gleam_stdlib" "decode_bool"

/// Check to see whether a Dynamic value is a function that takes no arguments,
/// and return the function if it is.
///
/// ## Examples
///
///    > import gleam/result
///    > let f = fn() { 1 }
///    > thunk(from(f)) |> result.is_ok
///    True
///
///    > thunk(from(123))
///    Error("Expected a zero arity function, got `123`")
///
pub external fn thunk(from: Dynamic) -> Result(fn() -> Dynamic, String) =
  "gleam_stdlib" "decode_thunk"

external fn list_dynamic(from: Dynamic) -> Result(List(Dynamic), String) =
  "gleam_stdlib" "decode_list"

/// Check to see whether a Dynamic value is a list, and return the list if it
/// is.
///
/// ## Examples
///
///    > list(from(["a", "b", "c"]), string)
///    Ok(["a", "b", "c"])
///
///    > list(from([1, 2, 3]), string)
///    Error("Expected an Int, got a binary")
///
///    > list(from("ok"), string)
///    Error("Expected a List, got a binary")
///
pub fn list(
  from dynamic: Dynamic,
  containing decoder_type: fn(Dynamic) -> Result(inner, String),
) -> Result(List(inner), String) {
  dynamic
  |> list_dynamic
  |> result.then(list_mod.traverse(_, decoder_type))
}

/// Check to see if a Dynamic value is a map with a specific field, and return
/// the value of the field if it is.
///
/// This will not succeed on a record.
///
/// ## Examples
///
///    > import gleam/map
///    > field(from(map.new("Hello", "World")), "Hello")
///    Ok(Dynamic)
///
///    > field(from(123), "Hello")
///    Error("Expected a map with key `\"Hello\"`, got an Int")
///
pub external fn field(from: Dynamic, named: a) -> Result(Dynamic, String) =
  "gleam_stdlib" "decode_field"

/// Check to see if the Dynamic value is a tuple large enough to have a certain
/// index, and return the value of that index if it is.
///
/// ## Examples
///
///    > element(from(tuple(1, 2)), 0)
///    Ok(Dynamic)
///
///    > element(from(tuple(1, 2)), 2)
///    Error("Expected a tuple of at least 3 size, got a tuple of 2 size")
///
///    > element(from(""), 2)
///    Error("Expected a Tuple, got a binary")
///
pub external fn element(
  from: Dynamic,
  position: Int,
) -> Result(Dynamic, String) =
  "gleam_stdlib" "decode_element"
