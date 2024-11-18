//// `BytesBuilder` is a type used for efficiently building binary content to be
//// written to a file or a socket. Internally it is represented as tree so to
//// append or prepend to a bytes builder is a constant time operation that
//// allocates a new node in the tree without copying any of the content. When
//// writing to an output stream the tree is traversed and the content is sent
//// directly rather than copying it into a single buffer beforehand.
////
//// If we append one bit array to another the bit arrays must be copied to a
//// new location in memory so that they can sit together. This behaviour
//// enables efficient reading of the data but copying can be expensive,
//// especially if we want to join many bit arrays together.
////
//// BytesBuilder is different in that it can be joined together in constant
//// time using minimal memory, and then can be efficiently converted to a
//// bit array using the `to_bit_array` function.
////
//// Byte builders are always byte aligned, so that a number of bits that is not
//// divisible by 8 will be padded with 0s.
////
//// On Erlang this type is compatible with Erlang's iolists.

// TODO: pad bit arrays to byte boundaries when adding to a builder.
import gleam/bytes_tree.{type BytesTree}
import gleam/string_tree.{type StringTree}

@deprecated("The `bytes_builder` module has been deprecated, use the `bytes_tree.BytesTree` type instead.")
pub type BytesBuilder =
  BytesTree

/// Create an empty `BytesBuilder`. Useful as the start of a pipe chaining many
/// builders together.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.new` instead.")
pub fn new() -> BytesTree {
  bytes_tree.concat([])
}

/// Prepends a bit array to the start of a builder.
///
/// Runs in constant time.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.prepend` instead.")
pub fn prepend(to second: BytesTree, prefix first: BitArray) -> BytesTree {
  bytes_tree.append_tree(bytes_tree.from_bit_array(first), second)
}

/// Appends a bit array to the end of a builder.
///
/// Runs in constant time.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.append` instead.")
pub fn append(to first: BytesTree, suffix second: BitArray) -> BytesTree {
  bytes_tree.append_tree(first, bytes_tree.from_bit_array(second))
}

/// Prepends a builder onto the start of another.
///
/// Runs in constant time.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.prepend_tree` instead.")
pub fn prepend_builder(
  to second: BytesTree,
  prefix first: BytesTree,
) -> BytesTree {
  bytes_tree.append_tree(first, second)
}

/// Appends a builder onto the end of another.
///
/// Runs in constant time.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.append_tree` instead.")
@external(erlang, "gleam_stdlib", "iodata_append")
pub fn append_builder(
  to first: BytesTree,
  suffix second: BytesTree,
) -> BytesTree {
  bytes_tree.append_tree(first, second)
}

/// Prepends a string onto the start of a builder.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time with the length of the string otherwise.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.prepend_string` instead.")
pub fn prepend_string(to second: BytesTree, prefix first: String) -> BytesTree {
  bytes_tree.append_tree(bytes_tree.from_string(first), second)
}

/// Appends a string onto the end of a builder.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time with the length of the string otherwise.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.append_string` instead.")
pub fn append_string(to first: BytesTree, suffix second: String) -> BytesTree {
  bytes_tree.append_tree(first, bytes_tree.from_string(second))
}

/// Joins a list of builders into a single builder.
///
/// Runs in constant time.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.concat` instead.")
@external(erlang, "gleam_stdlib", "identity")
pub fn concat(builders: List(BytesTree)) -> BytesTree {
  bytes_tree.concat(builders)
}

/// Joins a list of bit arrays into a single builder.
///
/// Runs in constant time.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.concat_bit_arrays` instead.")
@external(erlang, "gleam_stdlib", "identity")
pub fn concat_bit_arrays(bits: List(BitArray)) -> BytesTree {
  bytes_tree.concat_bit_arrays(bits)
}

/// Creates a new builder from a string.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time otherwise.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.from_string` instead.")
@external(erlang, "gleam_stdlib", "wrap_list")
pub fn from_string(string: String) -> BytesTree {
  bytes_tree.from_string(string)
}

/// Creates a new builder from a string builder.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time otherwise.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.from_string_tree` instead.")
@external(erlang, "gleam_stdlib", "wrap_list")
pub fn from_string_builder(builder: StringTree) -> BytesTree {
  bytes_tree.from_string_tree(builder)
}

/// Creates a new builder from a bit array.
///
/// Runs in constant time.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.from_bit_array` instead.")
@external(erlang, "gleam_stdlib", "wrap_list")
pub fn from_bit_array(bits: BitArray) -> BytesTree {
  bytes_tree.from_bit_array(bits)
}

/// Turns an builder into a bit array.
///
/// Runs in linear time.
///
/// When running on Erlang this function is implemented natively by the
/// virtual machine and is highly optimised.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.to_bit_array` instead.")
@external(erlang, "erlang", "list_to_bitstring")
pub fn to_bit_array(builder: BytesTree) -> BitArray {
  bytes_tree.to_bit_array(builder)
}

/// Returns the size of the builder's content in bytes.
///
/// Runs in linear time.
///
@deprecated("The `bytes_builder` module has been deprecated, use `bytes_tree.byte_size` instead.")
@external(erlang, "erlang", "iolist_size")
pub fn byte_size(builder: BytesTree) -> Int {
  bytes_tree.byte_size(builder)
}
