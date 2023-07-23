//// Working with raw bit string data.
//// The `BitString` type should be used instead of a String type when not utf8
//// encoded.

/// Converts a UTF-8 `String` type into a raw `BitString` type.
///
pub fn from_string(x: String) -> BitString {
  do_from_string(x)
}

@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "bit_string_from_string")
fn do_from_string(a: String) -> BitString

/// Returns an integer which is the number of bytes in the bit string.
///
pub fn byte_size(x: BitString) -> Int {
  do_byte_size(x)
}

@external(erlang, "erlang", "byte_size")
@external(javascript, "../gleam_stdlib.mjs", "length")
fn do_byte_size(a: BitString) -> Int

/// Creates a new bit string by joining two binaries.
///
/// ## Examples
///
/// ```gleam
/// > append(to: from_string("butter"), suffix: from_string("fly"))
/// from_string("butterfly")
/// ```
///
pub fn append(to first: BitString, suffix second: BitString) -> BitString {
  concat([first, second])
}

/// Extracts a sub-section of a bit string.
///
/// The slice will start at given position and continue up to specified
/// length.
/// A negative length can be used to extract bytes at the end of a bit string.
///
/// This function runs in constant time.
///
pub fn slice(
  from string: BitString,
  at position: Int,
  take length: Int,
) -> Result(BitString, Nil) {
  do_slice(string, position, length)
}

@external(erlang, "gleam_stdlib", "bit_string_slice")
@external(javascript, "../gleam_stdlib.mjs", "bit_string_slice")
fn do_slice(
  string string: BitString,
  position position: Int,
  length length: Int,
) -> Result(BitString, Nil)

/// Tests to see whether a bit string is valid UTF-8.
///
pub fn is_utf8(bits: BitString) -> Bool {
  do_is_utf8(bits)
}

@target(erlang)
fn do_is_utf8(bits: BitString) -> Bool {
  case bits {
    <<>> -> True
    <<_:utf8, rest:binary>> -> do_is_utf8(rest)
    _ -> False
  }
}

@target(javascript)
fn do_is_utf8(bits: BitString) -> Bool {
  case to_string(bits) {
    Ok(_) -> True
    _ -> False
  }
}

/// Converts a bit string to a string.
///
/// Returns an error if the bit string is invalid UTF-8 data.
///
pub fn to_string(bits: BitString) -> Result(String, Nil) {
  do_to_string(bits)
}

@target(erlang)
@external(erlang, "gleam_stdlib", "identity")
fn unsafe_to_string(a: BitString) -> String

@target(erlang)
fn do_to_string(bits: BitString) -> Result(String, Nil) {
  case is_utf8(bits) {
    True -> Ok(unsafe_to_string(bits))
    False -> Error(Nil)
  }
}

@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "bit_string_to_string")
fn do_to_string(a: BitString) -> Result(String, Nil)

/// Creates a new bit string by joining multiple binaries.
///
/// ## Examples
///
/// ```gleam
/// > concat([from_string("butter"), from_string("fly")])
/// from_string("butterfly")
/// ```
///
pub fn concat(bit_strings: List(BitString)) -> BitString {
  do_concat(bit_strings)
}

@external(erlang, "gleam_stdlib", "bit_string_concat")
@external(javascript, "../gleam_stdlib.mjs", "bit_string_concat")
fn do_concat(a: List(BitString)) -> BitString
