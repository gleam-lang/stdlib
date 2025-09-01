import gleam/dict

/// `Dynamic` data is data that we don't know the type of yet.
/// We likely get data like this from interop with Erlang, or from
/// IO with the outside world.
///
/// This module contains code for forming dynamic data, and the
/// `gleam/dynamic/decode` module contains code for turning dynamic data back
/// into Gleam data with known types. You will likely mostly use the other
/// module in your projects.
///
/// The exact runtime representation of dynamic values will depend on the
/// compilation target used.
///
pub type Dynamic

/// Return a string indicating the type of the dynamic value.
///
/// This function may be useful for constructing error messages or logs. If you
/// want to turn dynamic data into well typed data then you want the
/// `gleam/dynamic/decode` module.
///
/// ```gleam
/// classify(string("Hello"))
/// // -> "String"
/// ```
///
@external(erlang, "gleam_stdlib", "classify_dynamic")
@external(javascript, "../gleam_stdlib.mjs", "classify_dynamic")
pub fn classify(data: Dynamic) -> String

/// Create a dynamic value from a bool.
///
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn bool(a: Bool) -> Dynamic

/// Create a dynamic value from a string.
///
/// On Erlang this will be a binary string rather than a character list.
///
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn string(a: String) -> Dynamic

/// Create a dynamic value from a float.
///
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn float(a: Float) -> Dynamic

/// Create a dynamic value from an int.
///
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn int(a: Int) -> Dynamic

/// Create a dynamic value from a bit array.
///
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn bit_array(a: BitArray) -> Dynamic

/// Create a dynamic value from a list.
///
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn list(a: List(Dynamic)) -> Dynamic

/// Create a dynamic value from a list, converting it to a sequential runtime
/// format rather than the regular list format.
///
/// On Erlang this will be a tuple, on JavaScript this will be an array.
///
@external(erlang, "erlang", "list_to_tuple")
@external(javascript, "../gleam_stdlib.mjs", "list_to_array")
pub fn array(a: List(Dynamic)) -> Dynamic

/// Create a dynamic value made an unordered series of keys and values, where
/// the keys are unique.
///
/// On Erlang this will be a map, on JavaScript this will be a Gleam dict
/// object.
///
pub fn properties(entries: List(#(Dynamic, Dynamic))) -> Dynamic {
  cast(dict.from_list(entries))
}

/// A dynamic value representing nothing.
///
/// On Erlang this will be the atom `nil`, on JavaScript this will be
/// `undefined`.
///
pub fn nil() -> Dynamic {
  cast(Nil)
}

@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn cast(a: anything) -> Dynamic
