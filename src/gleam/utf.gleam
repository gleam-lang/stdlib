/// Splits a non-empty `String` into its head and tail. This lets you
/// pattern match on `String`s exactly as you would with lists.
///
/// ## Examples
/// ```gleam
/// > pop_grapheme("gleam")
/// Ok(#("g", "leam"))
///
/// > pop_grapheme("")
/// Error(Nil)
/// ```
///
pub fn pop_grapheme(string: String) -> Result(#(String, String), Nil) {
  do_pop_grapheme(string)
}

if erlang {
  external fn do_pop_grapheme(string: String) -> Result(#(String, String), Nil) =
    "gleam_stdlib" "string_pop_grapheme"
}

if javascript {
  external fn do_pop_grapheme(string: String) -> Result(#(String, String), Nil) =
    "../gleam_stdlib.mjs" "pop_grapheme"
}

/// Converts a `String` to a list of graphemes.
///
/// ```gleam
/// > graphemes("abc")
/// ["a", "b", "c"]
/// ```
///
pub fn graphemes(string: String) -> List(String) {
  case pop_grapheme(string) {
    Ok(#(grapheme, rest)) -> [grapheme, ..graphemes(rest)]
    _ -> []
  }
}

if erlang {
  external fn unsafe_int_to_utf_codepoint(Int) -> UtfCodepoint =
    "gleam_stdlib" "identity"
}

if javascript {
  external fn unsafe_int_to_utf_codepoint(Int) -> UtfCodepoint =
    "../gleam_stdlib.mjs" "codepoint"
}

/// Converts an integer to a `UtfCodepoint`.
///
/// Returns an `Error` if the integer does not represent a valid UTF codepoint.
///
pub fn codepoint(value: Int) -> Result(UtfCodepoint, Nil) {
  case value {
    i if i > 1114111 -> Error(Nil)
    65534 | 65535 -> Error(Nil)
    i if i >= 55296 && i <= 57343 -> Error(Nil)
    i -> Ok(unsafe_int_to_utf_codepoint(i))
  }
}
