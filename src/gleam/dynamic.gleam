import gleam/list as list_mod
import gleam/atom
import gleam/map.{Map}
import gleam/result

/// `Dynamic` data is data that we don"t know the type of yet.
/// We likely get data like this from interop with Erlang, or from
/// IO with the outside world.
pub external type Dynamic

pub type Decoder(t) =
  fn(Dynamic) -> Result(t, String)

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

/// Check to see whether a Dynamic value is a list, and return the list if it
/// is.
///
/// ## Examples
///
///    > opaque_list(from(["a", "b", "c"]))
///    Ok([from("a"), from("b"), from("c")])
///
///    > opaque_list(1)
///    Error("Expected an Int, got a binary")
///
pub external fn opaque_list(from: Dynamic) -> Result(List(Dynamic), String) =
  "gleam_stdlib" "decode_list"

/// Check to see whether a Dynamic value is a list of a particular type, and
/// return the list if it is.
///
/// The second argument is a decoder function used to decode the elements of
/// the list. The list is only decoded if all elements in the list can be
/// successfully decoded using this function.
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
  |> opaque_list
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
///    Ok(from(1))
///
///    > element(from(tuple(1, 2)), 2)
///    Error("Expected a tuple of at least 3 size, got a tuple of 2 size")
///
///    > element(from(""), 2)
///    Error("Expected a tuple, got a binary")
///
pub external fn element(
  from: Dynamic,
  position: Int,
) -> Result(Dynamic, String) =
  "gleam_stdlib" "decode_element"

/// Check to see if the Dynamic value is a 2 element tuple.
///
/// ## Examples
///
///    > tuple2(from(tuple(1, 2)))
///    Ok(tuple(from(1), from(2)))
///
///    > tuple2(from(tuple(1, 2)))
///    Error("Expected a 2 element tuple")
///
///    > tuple2(from(""))
///    Error("Expected a tuple, got a binary")
///
pub external fn tuple2(
  from: Dynamic,
) -> Result(tuple(Dynamic, Dynamic), String) =
  "gleam_stdlib" "decode_tuple2"

/// Check to see if the Dynamic value is a 2 element tuple containing two
/// specifically typed elements.
///
/// ## Examples
///
///    > tuple2_of(from(tuple(1, 2)), int, int)
///    Ok(tuple(1, 2))
///
///    > tuple2_of(from(tuple(1, 2.0)), int, float)
///    Ok(tuple(1, 2.0))
///
///    > tuple2_of(from(tuple(1, 2, 3)), int, float)
///    Error("Expected a 2 element tuple, got a 3 element tuple")
///
///    > tuple2_of(from(""), int, float)
///    Error("Expected a tuple, got a binary")
///
pub fn tuple2_of(
  from tup: Dynamic,
  first decode_first: Decoder(a),
  second decode_second: Decoder(b),
) -> Result(tuple(a, b), String) {
  try tuple(first, second) = tuple2(tup)
  try a = decode_first(first)
  try b = decode_second(second)
  Ok(tuple(a, b))
}

/// Check to see if the Dynamic value is map.
///
/// ## Examples
///
///    > import gleam/map as map_mod
///    > map(from(map_mod.new()))
///    Ok(map_mod.new())
///
///    > map(from(1))
///    Error("Expected a 2 element tuple, got an int")
///
///    > map(from(""))
///    Error("Expected a map, got a binary")
///
pub external fn map(from: Dynamic) -> Result(Map(Dynamic, Dynamic), String) =
  "gleam_stdlib" "decode_map"
