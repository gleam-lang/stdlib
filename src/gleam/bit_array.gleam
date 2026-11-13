//// BitArrays are a sequence of binary data of any length.

import gleam/int
import gleam/order
import gleam/string

/// Converts a string type into a bit array of UTF8 encoded data.
///
/// This function runs in constant time on Erlang and in linear time on
/// JavaScript.
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
pub fn pad_to_bytes(data: BitArray) -> BitArray {
  case bit_size(data) % 8 {
    0 -> data
    trailing_bit_count -> {
      let padding_bits = 8 - trailing_bit_count
      <<data:bits, 0:size(padding_bits)>>
    }
  }
}

/// Creates a new bit array by joining two bit arrays.
///
/// ## Examples
///
/// ```gleam
/// assert bit_array.append(
///     to: bit_array.from_string("butter"),
///     suffix: bit_array.from_string("fly"),
///   )
///   == bit_array.from_string("butterfly")
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

/// Tests to see whether a bit array is valid UTF-8.
@external(javascript, "../gleam_stdlib.mjs", "bit_array_is_utf8")
pub fn is_utf8(bits: BitArray) -> Bool {
  case bits {
    // Try to consume a BOM mark before trying to consume ascii characters.
    <<0xEF, 0xBB, 0xBF, rest:bytes>> -> is_utf8_loop(rest)
    <<rest:bytes>> -> is_utf8_loop(rest)
    _ -> False
  }
}

fn is_utf8_loop(bits: BitArray) -> Bool {
  case bits {
    <<>> -> True
    // Here, we consume bytes in 56-bit slices, the biggest number that fits
    // into an Erlang 60-bit small integer. This allows us to test 7 bytes
    // at once, considerably speeding up this loop comparing to checking each
    // byte one-by-one.
    <<word:size(56), rest:bits>> ->
      // The 0x80..80 pattern represents 0b10000000 repeated, or 128 in decimal.
      // Since all ASCII characters only use the lower 7 bits of a byte, this
      // allows us to check all 7 characters at once to determine if all of
      // them are ASCII characters.
      // If they are, we continue in this fast loop; if one of those characters
      // has its high bit set, that means it's a non-ascii character and we fall
      // back to the slower codepoint-based loop.
      case int.bitwise_and(word, 0x80808080808080) {
        0 -> is_utf8_loop(rest)
        _ -> is_utf8_codepoints(bits)
      }
    // These branches handle the end of the string.
    <<word:size(48)>> ->
      int.bitwise_and(word, 0x808080808080) == 0 || is_utf8_codepoints(bits)
    <<word:size(40)>> ->
      int.bitwise_and(word, 0x8080808080) == 0 || is_utf8_codepoints(bits)
    <<word:size(32)>> ->
      int.bitwise_and(word, 0x80808080) == 0 || is_utf8_codepoints(bits)
    <<word:size(24)>> ->
      int.bitwise_and(word, 0x808080) == 0 || is_utf8_codepoints(bits)
    <<word:size(16)>> ->
      int.bitwise_and(word, 0x8080) == 0 || is_utf8_codepoints(bits)
    <<byte>> -> int.bitwise_and(byte, 0x80) == 0
    _ -> is_utf8_codepoints(bits)
  }
}

fn is_utf8_codepoints(bits: BitArray) -> Bool {
  case bits {
    <<>> -> True
    // We are definitely at a non-ASCII codepoint now. If we found one and we
    // have at least 4KiB of text left, we call the natively implemented
    // function to check this entire chunk at once.
    // We have to look-ahead for one codepoint though, since we don't know where
    // the next codepoint boundary is in the utf8 binary data. We try up to 4
    // times, as longer UTF8 encodings are always invalid.
    <<_:utf8, chunk:bytes-size(4096), _:utf8, rest:bits>>
    | <<_:utf8, chunk:bytes-size(4095), _:utf8, rest:bits>>
    | <<_:utf8, chunk:bytes-size(4094), _:utf8, rest:bits>>
    | <<_:utf8, chunk:bytes-size(4093), _:utf8, rest:bits>> ->
      erlang_is_utf8(chunk) && is_utf8_loop(rest)

    // The most we get called with is 7 codepoints. If those are all valid,
    // we swap to the native check, as it's likely that we will encounter
    // more non-ascii codepoints in this string.
    <<_:utf8, _:utf8, _:utf8, _:utf8, _:utf8, _:utf8, _:utf8>> -> True
    <<_:utf8, _:utf8, _:utf8, _:utf8, _:utf8, _:utf8, _:utf8, rest:bits>> ->
      erlang_is_utf8(rest)

    // Handle all cases where we have less than 7 codepoints left.
    <<_:utf8, _:utf8, _:utf8, _:utf8, _:utf8, _:utf8>>
    | <<_:utf8, _:utf8, _:utf8, _:utf8, _:utf8>>
    | <<_:utf8, _:utf8, _:utf8, _:utf8>>
    | <<_:utf8, _:utf8, _:utf8>>
    | <<_:utf8, _:utf8>>
    | <<_:utf8>> -> True

    _ -> False
  }
}

@external(erlang, "gleam_stdlib", "is_utf8")
fn erlang_is_utf8(bits: BitArray) -> Bool

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
/// assert bit_array.concat([
///     bit_array.from_string("butter"),
///     bit_array.from_string("fly"),
///   ])
///   == bit_array.from_string("butterfly")
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
/// assert bit_array.inspect(<<0, 20, 0x20, 255>>) == "<<0, 20, 32, 255>>"
/// ```
///
/// ```gleam
/// assert bit_array.inspect(<<100, 5:3>>) == "<<100, 5:size(3)>>"
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
/// assert bit_array.compare(<<1>>, <<2>>) == Lt
/// ```
///
/// ```gleam
/// assert bit_array.compare(<<"AB":utf8>>, <<"AA":utf8>>) == Gt
/// ```
///
/// ```gleam
/// assert bit_array.compare(<<1, 2:size(2)>>, with: <<1, 2:size(2)>>) == Eq
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
/// assert bit_array.starts_with(<<1, 2, 3, 4>>, <<1, 2>>)
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
