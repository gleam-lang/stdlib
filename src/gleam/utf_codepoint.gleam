import gleam/result

/// A UtfCodepoint is the integer representation of a valid UTF codepoint
pub type UtfCodepoint = UtfCodepoint

external fn int_to_utf8_codepoint(Int) -> UtfCodepoint =
  "gleam_stdlib" "identity"

/// Convert an integer to a UtfCodepoint
///
/// Returns an error if the integer does not represent a valid UTF codepoint.
///
pub fn from_int(value: Int) -> Result(UtfCodepoint, Nil) {
  case value {
    i if i > 1114111 -> Error(Nil)
    i if i == 65534 -> Error(Nil)
    i if i == 65535 -> Error(Nil)
    i if i >= 55296 && i <= 57343 -> Error(Nil)
    i -> Ok(int_to_utf8_codepoint(i))
  }
}
