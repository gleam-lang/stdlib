import gleam/string_tree.{type StringTree}

/// `StringBuilder` is a type used for efficiently building text content to be
/// written to a file or a socket. Internally it is represented as tree so to
/// append or prepend to a string builder is a constant time operation that
/// allocates a new node in the tree without copying any of the content. When
/// writing to an output stream the tree is traversed and the content is sent
/// directly rather than copying it into a single buffer beforehand.
///
/// On Erlang this type is compatible with Erlang's iodata. On JavaScript this
/// type is compatible with normal strings.
///
/// The BEAM virtual machine has an optimisation for appending strings, where it
/// will mutate the string buffer when safe to do so, so if you are looking to
/// build a string through appending many small strings then you may get better
/// performance by not using a string builder. Always benchmark your performance
/// sensitive code.
///
@deprecated("The `string_builder` module has been deprecated, use the `string_tree.StringTree` type instead.")
pub type StringBuilder =
  StringTree

/// Create an empty `StringBuilder`. Useful as the start of a pipe chaining many
/// builders together.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.new` instead.")
pub fn new() -> StringTree {
  string_tree.from_strings([])
}

/// Prepends a `String` onto the start of some `StringBuilder`.
///
/// Runs in constant time.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.prepend` instead.")
pub fn prepend(to builder: StringTree, prefix prefix: String) -> StringTree {
  string_tree.append_tree(string_tree.from_string(prefix), builder)
}

/// Appends a `String` onto the end of some `StringBuilder`.
///
/// Runs in constant time.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.append` instead.")
pub fn append(to builder: StringTree, suffix second: String) -> StringTree {
  string_tree.append_tree(builder, string_tree.from_string(second))
}

/// Prepends some `StringBuilder` onto the start of another.
///
/// Runs in constant time.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.prepend_tree` instead.")
pub fn prepend_builder(
  to builder: StringTree,
  prefix prefix: StringTree,
) -> StringTree {
  string_tree.prepend_tree(builder, prefix)
}

/// Appends some `StringBuilder` onto the end of another.
///
/// Runs in constant time.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.append_tree` instead.")
pub fn append_builder(
  to builder: StringTree,
  suffix suffix: StringTree,
) -> StringTree {
  string_tree.append_tree(builder, suffix)
}

/// Converts a list of strings into a builder.
///
/// Runs in constant time.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.from_strings` instead.")
pub fn from_strings(strings: List(String)) -> StringTree {
  string_tree.from_strings(strings)
}

/// Joins a list of builders into a single builder.
///
/// Runs in constant time.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.concat` instead.")
pub fn concat(builders: List(StringTree)) -> StringTree {
  string_tree.concat(builders)
}

/// Converts a string into a builder.
///
/// Runs in constant time.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.from_string` instead.")
pub fn from_string(string: String) -> StringTree {
  string_tree.from_string(string)
}

/// Turns an `StringBuilder` into a `String`
///
/// This function is implemented natively by the virtual machine and is highly
/// optimised.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.to_string` instead.")
pub fn to_string(builder: StringTree) -> String {
  string_tree.to_string(builder)
}

/// Returns the size of the `StringBuilder` in bytes.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.byte_size` instead.")
pub fn byte_size(builder: StringTree) -> Int {
  string_tree.byte_size(builder)
}

/// Joins the given builders into a new builder separated with the given string
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.join` instead.")
pub fn join(builders: List(StringTree), with sep: String) -> StringTree {
  string_tree.join(builders, sep)
}

/// Converts a builder to a new builder where the contents have been
/// lowercased.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.lowercase` instead.")
pub fn lowercase(builder: StringTree) -> StringTree {
  string_tree.lowercase(builder)
}

/// Converts a builder to a new builder where the contents have been
/// uppercased.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.uppercase` instead.")
pub fn uppercase(builder: StringTree) -> StringTree {
  string_tree.uppercase(builder)
}

/// Converts a builder to a new builder with the contents reversed.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.reverse` instead.")
pub fn reverse(builder: StringTree) -> StringTree {
  string_tree.reverse(builder)
}

/// Splits a builder on a given pattern into a list of builders.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.split` instead.")
pub fn split(iodata: StringTree, on pattern: String) -> List(StringTree) {
  string_tree.split(iodata, pattern)
}

/// Replaces all instances of a pattern with a given string substitute.
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.replace` instead.")
@external(erlang, "gleam_stdlib", "string_replace")
@external(javascript, "../gleam_stdlib.mjs", "string_replace")
pub fn replace(
  in builder: StringTree,
  each pattern: String,
  with substitute: String,
) -> StringTree

/// Compares two builders to determine if they have the same textual content.
///
/// Comparing two iodata using the `==` operator may return `False` even if they
/// have the same content as they may have been build in different ways, so
/// using this function is often preferred.
///
/// ## Examples
///
/// ```gleam
/// from_strings(["a", "b"]) == from_string("ab")
/// // -> False
/// ```
///
/// ```gleam
/// is_equal(from_strings(["a", "b"]), from_string("ab"))
/// // -> True
/// ```
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.is_equal` instead.")
@external(erlang, "string", "equal")
pub fn is_equal(a: StringTree, b: StringTree) -> Bool {
  a == b
}

/// Inspects a builder to determine if it is equivalent to an empty string.
///
/// ## Examples
///
/// ```gleam
/// from_string("ok") |> is_empty
/// // -> False
/// ```
///
/// ```gleam
/// from_string("") |> is_empty
/// // -> True
/// ```
///
/// ```gleam
/// from_strings([]) |> is_empty
/// // -> True
/// ```
///
@deprecated("The `string_builder` module has been deprecated, use `string_tree.is_empty` instead.")
@external(erlang, "string", "is_empty")
pub fn is_empty(builder: StringTree) -> Bool {
  string_tree.from_string("") == builder
}
