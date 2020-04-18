/// A built-in representation for efficient string manipulation. String literals
/// are enclosed in `"double quotes"`.
///
import gleam/iodata
import gleam/list
import gleam/order
import gleam/result.{Option}

pub type String =
  String

/// ## Basics

/// Determine if a string is empty.
///
/// ## Examples
/// ```
/// is_empty("") == True
/// is_empty("the world") == False
/// ```
///
pub fn is_empty(str: String) -> Bool {
  str == ""
}

/// Get the length of a
///
/// ## Examples
/// ```gleam
/// length("") == 0
/// ```
///
pub external fn length(String) -> Int = "string" "length"

/// Reverse a string.
///
/// ## Examples
/// ```gleam
/// reverse("stressed") == "desserts"
/// ```
///
pub fn reverse(string: String) -> String {
  string
  |> iodata.new
  |> iodata.reverse
  |> iodata.to_string
}

/// Replace all occurrences of some substring.
///
/// ## Examples
/// ```gleam
/// replace("Json.Decode.succeed", each: ".", with: "-") == "Json-Decode-succeed"
/// replace("a,b,c,d,e", each: ",", with: "/")           == "a/b/c/d/e"
/// ```
///
pub fn replace(
  in string: String,
  each pattern: String,
  with substitute: String,
) -> String {
  string
  |> iodata.new
  |> iodata.replace(_, each: pattern, with: substitute)
  |> iodata.to_string
}

/// Convert a string to all lower case. Useful for case-insensitive comparisons.
///
/// ## Examples
/// ```gleam
/// lowercase("X-FILES") == "x-files"
/// ```
///
pub external fn lowercase(String) -> String = "string" "lowercase"

/// Convert a string to all upper case. Useful for case-insensitive comparisons
/// and VIRTUAL YELLING.
///
/// ## Examples
/// ```gleam
/// uppercase("skinner") == "SKINNER"
/// ```
///
pub external fn uppercase(String) -> String = "string" "uppercase"

/// Determines the order of the two strings.
///
/// ## Examples
/// ```gleam
/// compare("Anthony", "Anthony") == order.Eq
/// compare("A", "B") == order.Gt
/// ```
///
pub external fn compare(String, String) -> order.Order =
  "gleam_stdlib" "compare_strings"

// slice helper
external fn erl_string_slice(string, start, length) -> String =
  "string" "slice"

/// ## Get Substrings
///
/// Take a substring given a starting index and length.
/// A negative start or length will return an empty string.
/// Works on Unicode Graphemes.
///
/// ## Examples
/// ```gleam
/// slice("gleam", start: 1, length: 3) == "lea"
/// slice("gleam", start: 1, length: 10) == "leam"
/// slice("snakes on a plane!", start: -6, to: -1) == ""
/// ```
///
pub fn slice(string: String, start start: Int, length length: Int) -> String {
  case start < 0 || length <= 0 {
    True ->
      ""

    False ->
      erl_string_slice(string, start, length)
  }

}

// drop helpers
external fn erl_string_slice_to_infinity(string: String, start: Int) -> String =
    "string" "slice"

/// Drop *n* Graphemes from the start of a string
///
/// If `up_to` is <= 0 the input string will be returned unchanged.
/// If `up_to` is >= the input strings length, "" will be returned.
/// Works on Unicode Graphemes.
///
/// ## Examples
/// ```gleam
/// drop_left(from: "The Lone Gunmen", up_to: 2) == "e Lone Gunmen"
/// ```
///
pub fn drop_left(from from: String, up_to up_to: Int) -> String {
  case up_to <= 0 {
      True ->
        from

      False ->
        erl_string_slice_to_infinity(from, up_to)
  }
}

/// Drop *n* Graphemes from the end of a string
///
/// If `drop` is <= 0 the input string will be returned unchanged.
/// If `drop` is >= the input strings length, "" will be returned.
/// Works on Unicode Graphemes.
///
///
/// ## Examples
/// ```gleam
/// drop_right(from: "Cigarette Smoking Man", up_to: 2) == "Cigarette Smoking M"
/// ```
///
pub fn drop_right(from from: String, drop drop: Int) -> String {
    case drop <= 0 {
        True ->
          from

        False -> {
          let start = length(from) - drop
          case start <= 0 {
             True ->
               ""
             False ->
               erl_string_slice(from, 0, start)
          }
        }
    }
}

/// ## Check for Substrings

/// Check if the first string contains the second.
///
/// ## Examples
/// ```gleam
/// contains(does: "theory", contain: "ory") == True
/// contains(does: "theory", contain: "the") == True
/// contains(does: "theory", contain: "THE") == False
/// ```
///
pub external fn contains(does: String, contain: String) -> Bool =
  "gleam_stdlib" "string_contains"

/// See if the second string starts with the first one.
///
/// ## Examples
/// ```gleam
/// starts_with(does: "theory", start_with: "ory") == False
/// ```
///
pub external fn starts_with(does: String, start_with: String) -> Bool =
  "gleam_stdlib" "string_starts_with"

/// See if the second string ends with the first one.
///
/// ## Examples
/// ```gleam
/// ends_with(does: "theory", end_with: "ory") == True
/// ```
///
pub external fn ends_with(does: String, end_with: String) -> Bool =
  "gleam_stdlib" "string_ends_with"

/// ## Building and Splitting

/// Split a string using a given separator.
///
/// ## Examples
/// ```gleam
/// split("home/evan/Desktop/", on: "/") == ["home","evan","Desktop", ""]
/// ```
///
pub fn split(x: String, on substring: String) -> List(String) {
  x
  |> iodata.new
  |> iodata.split(_, on: substring)
  |> list.map(_, with: iodata.to_string)
}


/// Append two strings.
///
/// ## Examples
/// ```gleam
/// append(to: "butter", suffix: "fly") == "butterfly"
/// ```
///
pub fn append(to first: String, suffix second: String) -> String {
  first
  |> iodata.new
  |> iodata.append(_, second)
  |> iodata.to_string
}

/// Concatenate many strings into one.
///
/// ## Examples
/// ```gleam
/// concat(["never", "the", "less"]) == "nevertheless"
/// ```
///
pub external fn concat(strings: List(String)) -> String =
  "unicode" "characters_to_binary"

// repeat helper
fn repeat_help(chunk: String, result: List(String), repeats: Int) -> String {
  case repeats <= 0 {
     True -> concat(result)
     False -> repeat_help(chunk, [chunk | result], repeats - 1)
  }
}

/// Repeat a string `n` times.
///
/// ## Examples
/// ```gleam
/// repeat("ha", times: 3) == "hahaha"
/// ```
///
pub fn repeat(string: String, times times: Int) -> String {
  repeat_help(string, [], times)
}

/// Join many strings together with a given separator.
///
/// ## Examples
/// ```gleam
/// join(["super", "mega", "hyper", "bon"], with: "-") == "super-mega-hyper-bon"
/// ```
///
pub fn join(strings: List(String), with separator: String) -> String {
  strings
  |> list.intersperse(_, with: separator)
  |> iodata.from_strings
  |> iodata.to_string
}

/// ## Formatting

// pad helpers
type PadSide {
  Left
  Right
}

fn pad_fill_help(result: List(String), chunk: String, chunk_length: Int, to_fill: Int) -> String {
    let one = 1
    case True {
        True if one > to_fill->
          result
          |> concat

        True if to_fill > chunk_length ->
          pad_fill_help([chunk | result], chunk, chunk_length, to_fill - chunk_length)

        True ->
          [slice(chunk, 0, to_fill) | result ]
          |> list.reverse
          |> concat
    }
}

fn pad_help(pad: String, to_length: Int, with: String, pad_side: PadSide) -> String {
    let one = 1
    let pad_len = length(pad)
    let with_len = length(with)
    case True {
        True if pad_len > with_len ->
          pad

        True if one > with_len ->
          pad

        True ->
          case pad_side {
            Left ->
              [ pad_fill_help([], with, with_len, to_length - pad_len), pad ]
              |> concat

            Right ->
              [ pad, pad_fill_help([], with, with_len, to_length - pad_len) ]
              |> concat
          }
    }
}

/// Pad a string on the left until it has at least given number of Graphemes.
///
/// ## Examples
/// ```gleam
/// pad_left("121", to: 5, with: ".") == "..121"
/// pad_left("121", to: 3, with: ".") == "121"
/// pad_left("121", to: 2, with: ".") == "121"
/// ```
///
pub fn pad_left(pad pad: String, to_length to_length: Int, with with: String) {
    pad_help(pad, to_length, with, Left)
}

/// Pad a string on the right until it has a given length.
///
/// ## Examples
/// ```gleam
/// pad_right("121", to: 5, with: ".") == "121.."
/// pad_right("121", to: 3, with: ".") == "121"
/// pad_right("121", to: 2, with: ".") == "121"
/// ```
///
pub fn pad_right(pad pad: String, to_length to_length: Int, with with: String) {
    pad_help(pad, to_length, with, Right)
}

// trim helpers
type TrimDirection {
  Leading
  Trailing
  Both
}

external fn erl_string_trim(String, TrimDirection) -> String =
  "string" "trim"

/// Remove whitespace on both sides of a String.
/// All breaking Unicode characters with the White Space property will be removed.
/// See: https://en.wikipedia.org/wiki/Whitespace_character#Unicode
///
/// ## Examples
/// ```gleam
/// trim("  hats  \n") == "hats"
/// ```
///
pub fn trim(string: String) -> String {
    erl_string_trim(string, Both)
}

/// Remove whitespace on the left of a String.
///
/// ## Examples
/// ```gleam
/// trim_left("  hats  \n") == "hats  \n"
/// ```
///
pub fn trim_left(string: String) -> String {
    erl_string_trim(string, Leading)
}

/// Remove whitespace on the right of a String.
///
/// ## Examples
/// ```gleam
/// trim_right("  hats  \n") == "  hats"
/// ```
///
pub fn trim_right(string: String) -> String {
    erl_string_trim(string, Trailing)
}

/// ## Grapheme Conversions

/// Convert a string to a list of Graphemes.
///
/// to_graphemes("abc") == ["a","b","c"]
///
pub external fn to_graphemes(String) -> List(String) =
  "gleam_stdlib" "string_to_graphemes"

/// Split a non-empty string into its head and tail. This lets you
/// pattern match on strings exactly as you would with lists.
///
/// ## Examples
/// ```gleam
/// next_grapheme("ab") == Ok("a", "b")
/// next_grapheme("") == Error(Nil)
/// ```
///
pub external fn next_grapheme(String) -> Option(tuple(String, String)) =
  "gleam_stdlib" "string_next_grapheme"
