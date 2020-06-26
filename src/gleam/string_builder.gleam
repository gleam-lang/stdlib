/// StringBuilder is a type used for efficiently building strings.
///
/// When we append one string to another the strings must be copied to a
/// new location in memory so that they can sit together. This behaviour
/// enables efficient reading of the string but copying can be expensive,
/// especially if we want to join many strings together.
///
/// StringBuilder is different in that it can be joined together in constant time
/// using minimal memory, and then can be efficiently converted to a string
/// using the `to_string` function.
///
pub external type StringBuilder

/// Prepend a String onto the start of some StringBuilder.
///
/// Runs in constant time.
///
pub external fn prepend(to: StringBuilder, prefix: String) -> StringBuilder =
  "gleam_stdlib" "iodata_prepend"

/// Append a String onto the end of some StringBuilder.
///
/// Runs in constant time.
///
pub external fn append(to: StringBuilder, suffix: String) -> StringBuilder =
  "gleam_stdlib" "iodata_append"

/// Prepend some StringBuilder onto the start of another.
///
/// Runs in constant time.
///
pub external fn prepend_builder(
  to: StringBuilder,
  prefix: StringBuilder,
) -> StringBuilder =
  "gleam_stdlib" "iodata_prepend"

/// Append some StringBuilder onto the end of another.
///
/// Runs in constant time.
///
pub external fn append_builder(
  to: StringBuilder,
  suffix: StringBuilder,
) -> StringBuilder =
  "gleam_stdlib" "iodata_append"

/// Convert a list of strings into a builder.
///
/// Runs in constant time.
///
pub external fn from_strings(List(String)) -> StringBuilder =
  "gleam_stdlib" "identity"

/// Joins a list of builders into a single builder.
///
/// Runs in constant time.
///
pub external fn concat(List(StringBuilder)) -> StringBuilder =
  "gleam_stdlib" "identity"

/// Convert a string into a builder.
///
/// Runs in constant time.
///
pub external fn from_string(String) -> StringBuilder =
  "gleam_stdlib" "identity"

/// Turns an `StringBuilder` into a `String`
///
/// This function is implemented natively by the virtual machine and is highly
/// optimised.
///
pub external fn to_string(StringBuilder) -> String =
  "erlang" "iolist_to_binary"

/// Returns the size of the StringBuilder in bytes.
///
pub external fn byte_size(StringBuilder) -> Int =
  "erlang" "iolist_size"

/// Creates a builder containing the textual representation of a given float.
///
pub external fn from_float(Float) -> StringBuilder =
  "io_lib_format" "fwrite_g"

/// Converts a builder to a new builder where the contents have been
/// lowercased.
///
pub external fn lowercase(StringBuilder) -> StringBuilder =
  "string" "lowercase"

/// Converts a builder to a new builder where the contents have been
/// uppercased.
///
pub external fn uppercase(StringBuilder) -> StringBuilder =
  "string" "uppercase"

/// Converts a builder to a new builder with the contents reversed.
///
pub external fn reverse(StringBuilder) -> StringBuilder =
  "string" "reverse"

type Direction {
  All
}

external fn erl_split(StringBuilder, String, Direction) -> List(StringBuilder) =
  "string" "split"

/// Splits a builder on a given pattern into a list of builders.
///
pub fn split(iodata: StringBuilder, on pattern: String) -> List(StringBuilder) {
  erl_split(iodata, pattern, All)
}

external fn erl_replace(
  StringBuilder,
  String,
  String,
  Direction,
) -> StringBuilder =
  "string" "replace"

/// Replaces all instances of a pattern with a given string substitute.
///
pub fn replace(
  in iodata: StringBuilder,
  each pattern: String,
  with substitute: String,
) -> StringBuilder {
  erl_replace(iodata, pattern, substitute, All)
}

/// Compare two builders to determine if they have the same textual content.
///
/// Comparing two iodata using the `==` operator may return False even if they
/// have the same content as they may have been build in different ways, so
/// using this function is often preferred.
///
/// ## Examples
///
///    > from_strings(["a", "b"]) == new("ab")
///    False
///
///    > is_equal(from_strings(["a", "b"]), new("ab"))
///    True
///
///
pub external fn is_equal(StringBuilder, StringBuilder) -> Bool =
  "string" "equal"

/// Inspect a builder to determine if it is equivalent to an empty string.
///
/// ## Examples
///
///    > new("ok") |> is_empty
///    False
///
///    > new("") |> is_empty
///    True
///
///    > from_strings([]) |> is_empty
///    True
///
///
pub external fn is_empty(StringBuilder) -> Bool =
  "string" "is_empty"
