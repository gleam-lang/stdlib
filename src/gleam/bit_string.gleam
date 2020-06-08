//// Working with raw bit string data.
//// The BitString type should be used instead of a String type when not utf8
//// encoded.

// TODO: determine which of these functions once we have bit string syntax
pub external type BitString

/// Convert a utf8 String type into a raw Bitstring type.
///
pub external fn from_string(String) -> Bitstring =
  "gleam_stdlib" "identity"

/// Returns an integer which is the number of bytes in the bit string.
///
pub external fn byte_size(Bitstring) -> Int =
  "erlang" "byte_size"

/// Create a new bit string by joining two binaries.
///
/// ## Examples
///
///    > append(to: from_string("butter"), suffix: from_string("fly"))
///    from_string("butterfly")
///
pub external fn append(first: Bitstring, second: Bitstring) -> Bitstring =
  "gleam_stdlib" "bit_string_append"

/// Extracts part of a bit string.
///
/// Bitstring part will start at given position and continue up to specified
/// length.
/// A negative length can be used to extract bytes at the end of a bit string.
///
pub external fn part(
  string: Bitstring,
  position: Int,
  length: Int,
) -> Result(Bitstring, Nil) =
  "gleam_stdlib" "bit_string_part_"

/// Convert an integer to unsigned 32 bits.
///
/// Returns an error if integer is less than zero or equal to or larger than
/// 2^32.
///
pub external fn int_to_u32(Int) -> Result(Bitstring, Nil) =
  "gleam_stdlib" "bit_string_int_to_u32"

/// Convert unsigned 32 bits to an integer.
///
/// Returns an error if the bit string is not 32 bits in length.
///
pub external fn int_from_u32(Bitstring) -> Result(Int, Nil) =
  "gleam_stdlib" "bit_string_int_from_u32"
