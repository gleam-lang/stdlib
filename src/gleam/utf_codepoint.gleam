import gleam/result

pub type UtfCodepoint = UtfCodepoint

pub type Error {
  Invalid
}

external fn int_to_utf8_codepoint(Int) -> UtfCodepoint =
  "gleam_stdlib" "identity"

pub fn from_int(value: Int) -> Result(UtfCodepoint, Error) {
  case value {
    i if i > 1114111 -> Error(Invalid)
    i if i == 65534 -> Error(Invalid)
    i if i == 65535 -> Error(Invalid)
    i if i >= 55296 && i <= 57343 -> Error(Invalid)
    i -> Ok(int_to_utf8_codepoint(i))
  }
}
