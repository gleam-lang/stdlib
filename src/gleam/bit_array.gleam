//// BitArrays are a sequence of binary data of any length.

import gleam/int
import gleam/order
import gleam/string

/// Converts a UTF-8 `String` type into a `BitArray`.
///
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_from_string")
pub fn from_string(x: String) -> BitArray

/// Returns an integer which is the number of bits in the bit array.
///
@external(erlang, "erlang", "bit_size")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_bit_size")
pub fn bit_size(x: BitArray) -> Int

/// Returns an integer which is the number of bytes in the bit array.
///
@external(erlang, "erlang", "byte_size")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_byte_size")
pub fn byte_size(x: BitArray) -> Int

/// Pads a bit array with zeros so that it is a whole number of bytes.
///
@external(erlang, "gleam_stdlib", "bit_array_pad_to_bytes")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_pad_to_bytes")
pub fn pad_to_bytes(x: BitArray) -> BitArray

/// Creates a new bit array by joining two bit arrays.
///
/// ## Examples
///
/// ```gleam
/// assert append(to: from_string("butter"), suffix: from_string("fly"))
///   == from_string("butterfly")
/// ```
///
pub fn append(to first: BitArray, suffix second: BitArray) -> BitArray {
  concat([first, second])
}

/// Extracts a sub-section of a bit array.
///
/// The slice will start at given position and continue up to specified
/// length.
/// A negative length can be used to extract bytes at the end of a bit array.
///
/// This function runs in constant time.
///
@external(erlang, "gleam_stdlib", "bit_array_slice")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_slice")
pub fn slice(
  from string: BitArray,
  at position: Int,
  take length: Int,
) -> Result(BitArray, Nil)

/// Splits a bit array into two parts at the location of the pattern.
/// 
/// The result will not include the pattern, and returns an error if the
/// pattern is not found.
/// 
/// This function runs in linear time.
///
/// ## Examples
/// 
/// ```gleam
/// split_once(from: <<1, 2, 3>>, on: <<2>>)
/// // -> Ok(#(<<1>>, <<3>>))
/// 
/// split_once(from: <<0>>, on: <<1>>)
/// // -> Error(Nil)
/// ```
@external(erlang, "gleam_stdlib", "bit_array_split_once")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_split_once")
pub fn split_once(
  from bits: BitArray,
  on pattern: BitArray,
) -> Result(#(BitArray, BitArray), Nil)

/// Splits a bit array into parts at the locations of the pattern.
///
/// The result will not include the pattern, and returns the input
/// as is if the pattern is not found.
///
/// This function runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// split(from: <<0, 1, 0, 2, 0, 3>>, on: <<0>>)
/// // -> Ok([<<1>>, <<2>>, <<3>>])
/// 
/// split(from: <<0>>, on: <<1>>)
/// // -> Ok([<<0>>])
/// ```
@external(erlang, "gleam_stdlib", "bit_array_split")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_split")
pub fn split(
  from bits: BitArray,
  on pattern: BitArray,
) -> Result(List(BitArray), Nil)

/// Tests to see whether a bit array is valid UTF-8.
///
pub fn is_utf8(bits: BitArray) -> Bool {
  is_utf8_loop(bits)
}

@target(erlang)
fn is_utf8_loop(bits: BitArray) -> Bool {
  case bits {
    <<>> -> True
    <<_:utf8, rest:bytes>> -> is_utf8_loop(rest)
    _ -> False
  }
}

@target(javascript)
fn is_utf8_loop(bits: BitArray) -> Bool {
  case to_string(bits) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Converts a bit array to a string.
///
/// Returns an error if the bit array is invalid UTF-8 data.
///
@external(javascript, "../gleam_stdlib.mjs", "bit_array_to_string")
pub fn to_string(bits: BitArray) -> Result(String, Nil) {
  case is_utf8(bits) {
    True -> Ok(unsafe_to_string(bits))
    False -> Error(Nil)
  }
}

@external(erlang, "gleam_stdlib", "identity")
fn unsafe_to_string(a: BitArray) -> String

/// Creates a new bit array by joining multiple binaries.
///
/// ## Examples
///
/// ```gleam
/// assert concat([from_string("butter"), from_string("fly")])
///   == from_string("butterfly")
/// ```
///
@external(erlang, "gleam_stdlib", "bit_array_concat")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_concat")
pub fn concat(bit_arrays: List(BitArray)) -> BitArray

/// Encodes a BitArray into a base 64 encoded string.
///
/// If the bit array does not contain a whole number of bytes then it is padded
/// with zero bits prior to being encoded.
///
@external(erlang, "gleam_stdlib", "base64_encode")
@external(javascript, "../gleam_stdlib.mjs", "base64_encode")
pub fn base64_encode(input: BitArray, padding: Bool) -> String

/// Decodes a base 64 encoded string into a `BitArray`.
///
pub fn base64_decode(encoded: String) -> Result(BitArray, Nil) {
  let padded = case byte_size(from_string(encoded)) % 4 {
    0 -> encoded
    n -> string.append(encoded, string.repeat("=", 4 - n))
  }
  decode64(padded)
}

@external(erlang, "gleam_stdlib", "base64_decode")
@external(javascript, "../gleam_stdlib.mjs", "base64_decode")
fn decode64(a: String) -> Result(BitArray, Nil)

/// Encodes a `BitArray` into a base 64 encoded string with URL and filename
/// safe alphabet.
///
/// If the bit array does not contain a whole number of bytes then it is padded
/// with zero bits prior to being encoded.
///
pub fn base64_url_encode(input: BitArray, padding: Bool) -> String {
  input
  |> base64_encode(padding)
  |> string.replace("+", "-")
  |> string.replace("/", "_")
}

/// Decodes a base 64 encoded string with URL and filename safe alphabet into a
/// `BitArray`.
///
pub fn base64_url_decode(encoded: String) -> Result(BitArray, Nil) {
  encoded
  |> string.replace("-", "+")
  |> string.replace("_", "/")
  |> base64_decode()
}

/// Encodes a `BitArray` into a base 16 encoded string.
///
/// If the bit array does not contain a whole number of bytes then it is padded
/// with zero bits prior to being encoded.
///
@external(erlang, "gleam_stdlib", "base16_encode")
@external(javascript, "../gleam_stdlib.mjs", "base16_encode")
pub fn base16_encode(input: BitArray) -> String

/// Decodes a base 16 encoded string into a `BitArray`.
///
@external(erlang, "gleam_stdlib", "base16_decode")
@external(javascript, "../gleam_stdlib.mjs", "base16_decode")
pub fn base16_decode(input: String) -> Result(BitArray, Nil)

/// Converts a bit array to a string containing the decimal value of each byte.
///
/// Use this over `string.inspect` when you have a bit array you want printed
/// in the array syntax even if it is valid UTF-8.
///
/// ## Examples
///
/// ```gleam
/// assert inspect(<<0, 20, 0x20, 255>>) == "<<0, 20, 32, 255>>"
/// ```
///
/// ```gleam
/// assert inspect(<<100, 5:3>>) == "<<100, 5:size(3)>>"
/// ```
///
pub fn inspect(input: BitArray) -> String {
  inspect_loop(input, "<<") <> ">>"
}

fn inspect_loop(input: BitArray, accumulator: String) -> String {
  case input {
    <<>> -> accumulator

    <<x:size(1)>> -> accumulator <> int.to_string(x) <> ":size(1)"
    <<x:size(2)>> -> accumulator <> int.to_string(x) <> ":size(2)"
    <<x:size(3)>> -> accumulator <> int.to_string(x) <> ":size(3)"
    <<x:size(4)>> -> accumulator <> int.to_string(x) <> ":size(4)"
    <<x:size(5)>> -> accumulator <> int.to_string(x) <> ":size(5)"
    <<x:size(6)>> -> accumulator <> int.to_string(x) <> ":size(6)"
    <<x:size(7)>> -> accumulator <> int.to_string(x) <> ":size(7)"

    <<x, rest:bits>> -> {
      let suffix = case rest {
        <<>> -> ""
        _ -> ", "
      }

      let accumulator = accumulator <> int.to_string(x) <> suffix
      inspect_loop(rest, accumulator)
    }

    _ -> accumulator
  }
}

/// Compare two bit arrays as sequences of bytes.
///
/// ## Examples
///
/// ```gleam
/// assert compare(<<1>>, <<2>>) == Lt
/// ```
///
/// ```gleam
/// assert compare(<<"AB":utf8>>, <<"AA":utf8>>) == Gt
/// ```
///
/// ```gleam
/// assert compare(<<1, 2:size(2)>>, with: <<1, 2:size(2)>>) == Eq
/// ```
///
pub fn compare(a: BitArray, with b: BitArray) -> order.Order {
  case a, b {
    <<first_byte, first_rest:bits>>, <<second_byte, second_rest:bits>> ->
      case first_byte, second_byte {
        f, s if f > s -> order.Gt
        f, s if f < s -> order.Lt
        _, _ -> compare(first_rest, second_rest)
      }

    <<>>, <<>> -> order.Eq
    // First has more items, example: "AB" > "A":
    _, <<>> -> order.Gt
    // Second has more items, example: "A" < "AB":
    <<>>, _ -> order.Lt
    // This happens when there's unusually sized elements.
    // Handle these special cases via custom erlang function.
    first, second ->
      case bit_array_to_int_and_size(first), bit_array_to_int_and_size(second) {
        #(a, _), #(b, _) if a > b -> order.Gt
        #(a, _), #(b, _) if a < b -> order.Lt
        #(_, size_a), #(_, size_b) if size_a > size_b -> order.Gt
        #(_, size_a), #(_, size_b) if size_a < size_b -> order.Lt
        _, _ -> order.Eq
      }
  }
}

@external(erlang, "gleam_stdlib", "bit_array_to_int_and_size")
@external(javascript, "../gleam_stdlib.mjs", "bit_array_to_int_and_size")
fn bit_array_to_int_and_size(a: BitArray) -> #(Int, Int)

/// Checks whether the first `BitArray` starts with the second one.
///
/// ## Examples
///
/// ```gleam
/// assert starts_with(<<1, 2, 3, 4>>, <<1, 2>>)
/// ```
///
@external(javascript, "../gleam_stdlib.mjs", "bit_array_starts_with")
pub fn starts_with(bits: BitArray, prefix: BitArray) -> Bool {
  let prefix_size = bit_size(prefix)

  case bits {
    <<pref:bits-size(prefix_size), _:bits>> if pref == prefix -> True
    _ -> False
  }
}
