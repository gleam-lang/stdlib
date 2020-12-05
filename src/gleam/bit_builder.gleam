import gleam/bit_string.{BitString}
import gleam/string_builder.{StringBuilder}

/// BitBuilder is a type used for efficiently concatenating bits to create bit
/// strings.
///
/// If we append one bit string to another the bit strings must be copied to a
/// new location in memory so that they can sit together. This behaviour
/// enables efficient reading of the string but copying can be expensive,
/// especially if we want to join many bit strings together.
///
/// BitBuilder is different in that it can be joined together in constant
/// time using minimal memory, and then can be efficiently converted to a
/// bit string using the `to_bit_string` function.
///
pub external type BitBuilder

/// Prepend a bit string to the start of a builder.
///
/// Runs in constant time.
///
pub external fn prepend(to: BitBuilder, prefix: BitString) -> BitBuilder =
  "gleam_stdlib" "iodata_prepend"

/// Append a bit string to the end of a builder.
///
/// Runs in constant time.
///
pub external fn append(to: BitBuilder, suffix: BitString) -> BitBuilder =
  "gleam_stdlib" "iodata_append"

/// Prepend a builder onto the start of another.
///
/// Runs in constant time.
///
pub external fn prepend_builder(
  to: BitBuilder,
  prefix: BitBuilder,
) -> BitBuilder =
  "gleam_stdlib" "iodata_prepend"

/// Append a builder onto the end of another.
///
/// Runs in constant time.
///
pub external fn append_builder(to: BitBuilder, suffix: BitBuilder) -> BitBuilder =
  "gleam_stdlib" "iodata_append"

/// Prepend a string onto the start of a builder.
///
/// Runs in constant time.
///
pub external fn prepend_string(to: BitBuilder, prefix: String) -> BitBuilder =
  "gleam_stdlib" "iodata_prepend"

/// Append a string onto the end of a builder.
///
/// Runs in constant time.
///
pub external fn append_string(to: BitBuilder, suffix: String) -> BitBuilder =
  "gleam_stdlib" "iodata_append"

/// Joins a list of builders into a single builders.
///
/// Runs in constant time.
///
pub external fn concat(List(BitBuilder)) -> BitBuilder =
  "gleam_stdlib" "identity"

/// Create a new builder from a string.
///
/// Runs in constant time.
///
pub external fn from_string(String) -> BitBuilder =
  "gleam_stdlib" "wrap_list"

/// Create a new builder from a string builder.
///
/// Runs in constant time.
///
pub external fn from_string_builder(StringBuilder) -> BitBuilder =
  "gleam_stdlib" "identity"

/// Create a new builder from a bit string.
///
/// Runs in constant time.
///
pub external fn from_bit_string(BitString) -> BitBuilder =
  "gleam_stdlib" "wrap_list"

/// Turns an builder into a bit string.
///
/// This function is implemented natively by the virtual machine and is highly
/// optimised.
///
pub external fn to_bit_string(BitBuilder) -> BitString =
  "erlang" "list_to_bitstring"

/// Returns the size of the builder's content in bytes.
///
pub external fn byte_size(BitBuilder) -> Int =
  "erlang" "iolist_size"
