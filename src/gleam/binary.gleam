//// Working with raw binary data.
//// The Binary type should be used instead of a String type when not utf8 encoded.

pub external type Binary

/// Convert a utf8 String type into a raw Binary type.
pub external fn from_string(String) -> Binary =
  "gleam_stdlib" "identity"

/// Returns an integer which is the number of bytes in the binary.
pub external fn byte_size(Binary) -> Int =
"erlang" "byte_size"

/// Extracts the part of a binary.
///
/// Binary part will start at given position and continue up to specified length.
/// A negative length can be used to extract bytes at the end of a binary:
pub external fn part(string: Binary, position: Int, length: Int) -> Result(Binary, Nil) =
"binary_native" "part"

/// Convert integer to unsigned 32 bits.
///
/// Returns an error if integer is less than zero or equal to or larger than 2^32.
pub external fn int_to_u32(Int) -> Result(Binary, Nil) =
  "binary_native" "int_to_u32"

/// Convert unsigned 32 bits to Integer
///
/// Returns an error if the binary is not 32 bits in length.
pub external fn int_from_u32(Binary) -> Result(Int, Nil) =
  "binary_native" "int_from_u32"
