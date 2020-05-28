//// Working with raw binary data.
//// The Binary type should be used instead of a String type when not utf8 encoded.

pub external type Binary

/// Convert a utf8 String type into a raw Binary type.
pub external fn from_string(String) -> Binary =
  "gleam_stdlib" "identity"

/// Extracts the part of a binary.
///
/// Binary part will start at given position and continue up to specified length.
/// A negative length can be used to extract bytes at the end of a binary:
pub external fn part(string: Binary, position: Int, length: Int) -> Binary =
  "binary" "part"

/// Returns an integer which is the number of bytes in the binary.
pub external fn byte_size(Binary) -> Int =
  "erlang" "byte_size"

// Not sure these will be necessary with bit syntax for for now I think they are
pub external fn int_to_u32(Int) -> Result(Binary, Nil) =
  "binary_native" "int_to_u32"

pub external fn int_from_u32(Binary) -> Result(Int, Nil) =
  "binary_native" "int_from_u32"
