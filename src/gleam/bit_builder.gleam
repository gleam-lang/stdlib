//// BitBuilder is a type used for efficiently concatenating bits to create bit
//// strings.
////
//// If we append one bit string to another the bit strings must be copied to a
//// new location in memory so that they can sit together. This behaviour
//// enables efficient reading of the string but copying can be expensive,
//// especially if we want to join many bit strings together.
////
//// BitBuilder is different in that it can be joined together in constant
//// time using minimal memory, and then can be efficiently converted to a
//// bit string using the `to_bit_string` function.
////
//// On Erlang this type is compatible with Erlang's iolists.

import gleam/string_builder.{type StringBuilder}
@target(javascript)
import gleam/list
@target(javascript)
import gleam/bit_string

@target(erlang)
pub type BitBuilder

@target(javascript)
pub opaque type BitBuilder {
  Bits(BitArray)
  Text(StringBuilder)
  Many(List(BitBuilder))
}

/// Create an empty `BitBuilder`. Useful as the start of a pipe chaining many
/// builders together.
///
pub fn new() -> BitBuilder {
  do_concat([])
}

/// Prepends a bit string to the start of a builder.
///
/// Runs in constant time.
///
pub fn prepend(to: BitBuilder, prefix: BitArray) -> BitBuilder {
  append_builder(from_bit_string(prefix), to)
}

/// Appends a bit string to the end of a builder.
///
/// Runs in constant time.
///
pub fn append(to: BitBuilder, suffix: BitArray) -> BitBuilder {
  append_builder(to, from_bit_string(suffix))
}

/// Prepends a builder onto the start of another.
///
/// Runs in constant time.
///
pub fn prepend_builder(to: BitBuilder, prefix: BitBuilder) -> BitBuilder {
  append_builder(prefix, to)
}

/// Appends a builder onto the end of another.
///
/// Runs in constant time.
///
pub fn append_builder(
  to first: BitBuilder,
  suffix second: BitBuilder,
) -> BitBuilder {
  do_append_builder(first, second)
}

@target(erlang)
@external(erlang, "gleam_stdlib", "iodata_append")
fn do_append_builder(to to: BitBuilder, suffix suffix: BitBuilder) -> BitBuilder

@target(javascript)
fn do_append_builder(first: BitBuilder, second: BitBuilder) -> BitBuilder {
  case second {
    Many(builders) -> Many([first, ..builders])
    _ -> Many([first, second])
  }
}

/// Prepends a string onto the start of a builder.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time with the length of the string otherwise.
///
pub fn prepend_string(to: BitBuilder, prefix: String) -> BitBuilder {
  append_builder(from_string(prefix), to)
}

/// Appends a string onto the end of a builder.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time with the length of the string otherwise.
///
pub fn append_string(to: BitBuilder, suffix: String) -> BitBuilder {
  append_builder(to, from_string(suffix))
}

/// Joins a list of builders into a single builder.
///
/// Runs in constant time.
///
pub fn concat(builders: List(BitBuilder)) -> BitBuilder {
  do_concat(builders)
}

@target(erlang)
@external(erlang, "gleam_stdlib", "identity")
fn do_concat(a: List(BitBuilder)) -> BitBuilder

@target(javascript)
fn do_concat(builders: List(BitBuilder)) -> BitBuilder {
  Many(builders)
}

/// Joins a list of bit strings into a single builder.
///
/// Runs in constant time.
///
pub fn concat_bit_strings(bits: List(BitArray)) -> BitBuilder {
  do_concat_bit_strings(bits)
}

@target(erlang)
@external(erlang, "gleam_stdlib", "identity")
fn do_concat_bit_strings(a: List(BitArray)) -> BitBuilder

@target(javascript)
fn do_concat_bit_strings(bits: List(BitArray)) -> BitBuilder {
  bits
  |> list.map(fn(b) { from_bit_string(b) })
  |> concat()
}

/// Creates a new builder from a string.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time otherwise.
///
pub fn from_string(string: String) -> BitBuilder {
  do_from_string(string)
}

@target(erlang)
@external(erlang, "gleam_stdlib", "wrap_list")
fn do_from_string(a: String) -> BitBuilder

@target(javascript)
fn do_from_string(string: String) -> BitBuilder {
  Text(string_builder.from_string(string))
}

/// Creates a new builder from a string builder.
///
/// Runs in constant time when running on Erlang.
/// Runs in linear time otherwise.
///
pub fn from_string_builder(builder: StringBuilder) -> BitBuilder {
  do_from_string_builder(builder)
}

@target(erlang)
@external(erlang, "gleam_stdlib", "wrap_list")
fn do_from_string_builder(a: StringBuilder) -> BitBuilder

@target(javascript)
fn do_from_string_builder(builder: StringBuilder) -> BitBuilder {
  Text(builder)
}

/// Creates a new builder from a bit string.
///
/// Runs in constant time.
///
pub fn from_bit_string(bits: BitArray) -> BitBuilder {
  do_from_bit_string(bits)
}

@target(erlang)
@external(erlang, "gleam_stdlib", "wrap_list")
fn do_from_bit_string(a: BitArray) -> BitBuilder

@target(javascript)
fn do_from_bit_string(bits: BitArray) -> BitBuilder {
  Bits(bits)
}

/// Turns an builder into a bit string.
///
/// Runs in linear time.
///
/// When running on Erlang this function is implemented natively by the
/// virtual machine and is highly optimised.
///
pub fn to_bit_string(builder: BitBuilder) -> BitArray {
  do_to_bit_string(builder)
}

@target(erlang)
@external(erlang, "erlang", "list_to_bitstring")
fn do_to_bit_string(a: BitBuilder) -> BitArray

@target(javascript)
fn do_to_bit_string(builder: BitBuilder) -> BitArray {
  [[builder]]
  |> to_list([])
  |> list.reverse
  |> bit_string.concat
}

@target(javascript)
fn to_list(stack: List(List(BitBuilder)), acc: List(BitArray)) -> List(BitArray) {
  case stack {
    [] -> acc

    [[], ..remaining_stack] -> to_list(remaining_stack, acc)

    [[Bits(bits), ..rest], ..remaining_stack] ->
      to_list([rest, ..remaining_stack], [bits, ..acc])

    [[Text(builder), ..rest], ..remaining_stack] -> {
      let bits = bit_string.from_string(string_builder.to_string(builder))
      to_list([rest, ..remaining_stack], [bits, ..acc])
    }

    [[Many(builders), ..rest], ..remaining_stack] ->
      to_list([builders, rest, ..remaining_stack], acc)
  }
}

/// Returns the size of the builder's content in bytes.
///
/// Runs in linear time.
///
pub fn byte_size(builder: BitBuilder) -> Int {
  do_byte_size(builder)
}

@target(erlang)
@external(erlang, "erlang", "iolist_size")
fn do_byte_size(a: BitBuilder) -> Int

@target(javascript)
fn do_byte_size(builder: BitBuilder) -> Int {
  [[builder]]
  |> to_list([])
  |> list.fold(0, fn(acc, builder) { bit_string.byte_size(builder) + acc })
}
