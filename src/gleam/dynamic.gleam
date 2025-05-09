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
pub type Dynamic

/// Return a string indicating the type of the dynamic value.
///
/// This function may be useful for constructing error messages or logs. If you
/// want to turn dynamic data into well typed data then you want the
/// `gleam/dynamic/decode` module.
///
/// ```gleam
/// classify(from("Hello"))
/// // -> "String"
/// ```
///
@external(erlang, "gleam_stdlib", "classify_dynamic")
@external(javascript, "../gleam_stdlib.mjs", "classify_dynamic")
pub fn classify(data: Dynamic) -> String

@deprecated("Please use the other functions in the gleam/dynamic module")
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn from(a: anything) -> Dynamic

// TODO: document
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn bool(a: Bool) -> Dynamic

// TODO: document
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn string(a: String) -> Dynamic

// TODO: document
// TODO: test
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn float(a: Float) -> Dynamic

// TODO: document
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn int(a: Int) -> Dynamic

// TODO: document
// TODO: test
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn bit_array(a: BitArray) -> Dynamic

// TODO: document
// TODO: test
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
pub fn list(a: List(Dynamic)) -> Dynamic

// TODO: document
// TODO: test
@external(erlang, "erlang", "list_to_tuple")
@external(javascript, "../gleam_stdlib.mjs", "list_to_array")
pub fn array(a: List(Dynamic)) -> Dynamic

// TODO: document
pub fn object(entries: List(#(Dynamic, Dynamic))) -> Dynamic {
  cast(dict.from_list(entries))
}

// TODO: document
pub fn null() -> Dynamic {
  cast(Nil)
}

@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "identity")
fn cast(a: anything) -> Dynamic
