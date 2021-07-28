//// Working with raw bit string data.
//// The BitString type should be used instead of a String type when not utf8
//// encoded.

/// Converts a UTF-8 String type into a raw BitString type.
///
pub fn from_string(x: String) -> BitString {
  do_from_string(x)
}

if erlang {
  external fn do_from_string(String) -> BitString =
    "gleam_stdlib" "identity"
}

if javascript {
  external fn do_from_string(String) -> BitString =
    "../gleam_stdlib.js" "bit_string_from_string"
}

/// Returns an integer which is the number of bytes in the bit string.
///
pub fn byte_size(x: BitString) -> Int {
  do_byte_size(x)
}

if erlang {
  external fn do_byte_size(BitString) -> Int =
    "erlang" "byte_size"
}

if javascript {
  external fn do_byte_size(BitString) -> Int =
    "../gleam_stdlib.js" "byte_size"
}

/// Creates a new bit string by joining two binaries.
///
/// ## Examples
///
///    > append(to: from_string("butter"), suffix: from_string("fly"))
///    from_string("butterfly")
///
pub fn append(to first: BitString, suffix second: BitString) -> BitString {
  do_append(first, second)
}

if erlang {
  external fn do_append(BitString, BitString) -> BitString =
    "gleam_stdlib" "bit_string_append"
}

if javascript {
  external fn do_append(BitString, BitString) -> BitString =
    "../gleam_stdlib.js" "bit_string_append"
}

if erlang {
  /// Extracts part of a bit string.
  ///
  /// BitString part will start at given position and continue up to specified
  /// length.
  /// A negative length can be used to extract bytes at the end of a bit string.
  ///
  pub external fn part(
    string: BitString,
    position: Int,
    length: Int,
  ) -> Result(BitString, Nil) =
    "gleam_stdlib" "bit_string_part_"

  /// Converts an integer to unsigned 32 bits.
  ///
  /// Returns an error if integer is less than zero or equal to or larger than
  /// 2^32.
  ///
  pub external fn int_to_u32(Int) -> Result(BitString, Nil) =
    "gleam_stdlib" "bit_string_int_to_u32"

  /// Converts unsigned 32 bits to an integer.
  ///
  /// Returns an error if the bit string is not 32 bits in length.
  ///
  pub external fn int_from_u32(BitString) -> Result(Int, Nil) =
    "gleam_stdlib" "bit_string_int_from_u32"

  /// Tests to see whether a bit string is valid UTF-8.
  ///
  pub fn is_utf8(bits: BitString) -> Bool {
    case bits {
      <<>> -> True
      <<_:utf8, rest:binary>> -> is_utf8(rest)
      _ -> False
    }
  }

  external fn unsafe_to_string(BitString) -> String =
    "gleam_stdlib" "identity"

  /// Converts a bit string to a string.
  ///
  /// Returns an error if the bit string is invalid UTF-8 data.
  ///
  pub fn to_string(bits: BitString) -> Result(String, Nil) {
    case is_utf8(bits) {
      True -> Ok(unsafe_to_string(bits))
      False -> Error(Nil)
    }
  }
}
