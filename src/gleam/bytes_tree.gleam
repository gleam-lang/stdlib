//// `BytesTree` is a type used for efficiently building binary content to be
//// written to a file or a socket. Internally it is represented as tree so to
//// append or prepend to a bytes tree is a constant time operation that
//// allocates a new node in the tree without copying any of the content. When
//// writing to an output stream the tree is traversed and the content is sent
//// directly rather than copying it into a single buffer beforehand.
////
//// If we append one bit array to another the bit arrays must be copied to a
//// new location in memory so that they can sit together. This behaviour
//// enables efficient reading of the data but copying can be expensive,
//// especially if we want to join many bit arrays together.
////
//// BytesTree is different in that it can be joined together in constant
//// time using minimal memory, and then can be efficiently converted to a
//// bit array using the `to_bit_array` function.
////
//// Byte trees are always byte aligned, so that a number of bits that is not
//// divisible by 8 will be padded with 0s.
////
//// On Erlang this type is compatible with Erlang's iolists.

import gleam/bit_array
import gleam/list
import gleam/string_tree.{type StringTree}

pub opaque type BytesTree {
  Bytes(BitArray)
  Text(StringTree)
  Many(List(BytesTree))
}

/// Create an empty `BytesTree`. Useful as the start of a pipe chaining many
/// trees together.
///
pub fn new() -> BytesTree {
  concat([])
}

/// Prepends a bit array to the start of a bytes tree.
///
/// Runs in constant time.
///
pub fn prepend(to second: BytesTree, prefix first: BitArray) -> BytesTree {
  append_tree(from_bit_array(first), second)
}

/// Appends a bit array to the end of a bytes tree.
///
/// Runs in constant time.
///
pub fn append(to first: BytesTree, suffix second: BitArray) -> BytesTree {
  append_tree(first, from_bit_array(second))
}

/// Prepends a bytes tree onto the start of another.
///
/// Runs in constant time.
///
pub fn prepend_tree(to second: BytesTree, prefix first: BytesTree) -> BytesTree {
  append_tree(first, second)
}

/// Appends a bytes tree onto the end of another.
///
/// Runs in constant time.
///
@external(erlang, "gleam_stdlib", "iodata_append")
pub fn append_tree(to first: BytesTree, suffix second: BytesTree) -> BytesTree {
  case second {
    Many(trees) -> Many([first, ..trees])
    Text(_) | Bytes(_) -> Many([first, second])
  }
}

/// Prepends a string onto the start of a bytes tree.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time with the length of the string otherwise.
///
pub fn prepend_string(to second: BytesTree, prefix first: String) -> BytesTree {
  append_tree(from_string(first), second)
}

/// Appends a string onto the end of a bytes tree.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time with the length of the string otherwise.
///
pub fn append_string(to first: BytesTree, suffix second: String) -> BytesTree {
  append_tree(first, from_string(second))
}

/// Joins a list of bytes trees into a single one.
///
/// Runs in constant time.
///
@external(erlang, "gleam_stdlib", "identity")
pub fn concat(trees: List(BytesTree)) -> BytesTree {
  Many(trees)
}

/// Joins a list of bit arrays into a single bytes tree.
///
/// Runs in constant time.
///
pub fn concat_bit_arrays(bits: List(BitArray)) -> BytesTree {
  bits
  |> list.map(from_bit_array)
  |> concat()
}

/// Creates a new bytes tree from a string.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time otherwise.
///
@external(erlang, "gleam_stdlib", "wrap_list")
pub fn from_string(string: String) -> BytesTree {
  Text(string_tree.from_string(string))
}

/// Creates a new bytes tree from a string tree.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time otherwise.
///
@external(erlang, "gleam_stdlib", "wrap_list")
pub fn from_string_tree(tree: string_tree.StringTree) -> BytesTree {
  Text(tree)
}

/// Creates a new bytes tree from a bit array.
///
/// Runs in constant time.
///
pub fn from_bit_array(bits: BitArray) -> BytesTree {
  bits
  |> bit_array.pad_to_bytes
  |> wrap_list
}

@external(erlang, "gleam_stdlib", "wrap_list")
fn wrap_list(bits: BitArray) -> BytesTree {
  Bytes(bits)
}

/// Turns a bytes tree into a bit array.
///
/// Runs in linear time.
///
/// When running on Erlang this function is implemented natively by the
/// virtual machine and is highly optimised.
///
@external(erlang, "erlang", "list_to_bitstring")
pub fn to_bit_array(tree: BytesTree) -> BitArray {
  [[tree]]
  |> to_list([])
  |> list.reverse
  |> bit_array.concat
}

fn to_list(stack: List(List(BytesTree)), acc: List(BitArray)) -> List(BitArray) {
  case stack {
    [] -> acc

    [[], ..remaining_stack] -> to_list(remaining_stack, acc)

    [[Bytes(bits), ..rest], ..remaining_stack] ->
      to_list([rest, ..remaining_stack], [bits, ..acc])

    [[Text(tree), ..rest], ..remaining_stack] -> {
      let bits = bit_array.from_string(string_tree.to_string(tree))
      to_list([rest, ..remaining_stack], [bits, ..acc])
    }

    [[Many(trees), ..rest], ..remaining_stack] ->
      to_list([trees, rest, ..remaining_stack], acc)
  }
}

/// Returns the size of the bytes tree's content in bytes.
///
/// Runs in linear time.
///
@external(erlang, "erlang", "iolist_size")
pub fn byte_size(tree: BytesTree) -> Int {
  [[tree]]
  |> to_list([])
  |> list.fold(0, fn(acc, bits) { bit_array.byte_size(bits) + acc })
}
