//// Strings in Gleam are UTF-8 binaries. They can be written in your code a
//// text surrounded by `"double quotes"`.

import gleam/string_builder
import gleam/dynamic.{Dynamic}
import gleam/iterator
import gleam/list
import gleam/order
import gleam/result

pub type String =
  String

/// A UtfCodepoint is the integer representation of a valid UTF codepoint
pub type UtfCodepoint =
  UtfCodepoint

/// Determine if a string is empty.
///
/// ## Examples
///
///    > is_empty("")
///    True
///
///    > is_empty("the world")
///    False
///
pub fn is_empty(str: String) -> Bool {
  str == ""
}

/// Get the number of grapheme clusters in a given string.
///
/// This function has to iterate across the whole string to count the number of
/// graphemes, so it runs in linear time.
///
/// ## Examples
///
///    > length("Gleam")
///    5
///
///    > length("ß↑e̊")
///    3
///
///    > length("")
///    0
///
pub external fn length(String) -> Int =
  "string" "length"

///
/// Reverse a string.
///
/// This function has to iterate across the whole string so it runs in linear
/// time.
///
/// ## Examples
///
///    > reverse("stressed")
///    "desserts"
///
pub fn reverse(string: String) -> String {
  string
  |> string_builder.from_string
  |> string_builder.reverse
  |> string_builder.to_string
}

/// Create a new string by replacing all occurrences of a given substring.
///
/// ## Examples
///
///    > replace("www.example.com", each: ".", with: "-")
///    "www-example-com"
///
///    > replace("a,b,c,d,e", each: ",", with: "/")
///    "a/b/c/d/e"
///
pub fn replace(
  in string: String,
  each pattern: String,
  with substitute: String,
) -> String {
  string
  |> string_builder.from_string
  |> string_builder.replace(each: pattern, with: substitute)
  |> string_builder.to_string
}

/// Create a new string with all the graphemes in the input string converted to
/// lowercase.
///
/// Useful for case-insensitive comparisons.
///
/// ## Examples
///
///    > lowercase("X-FILES")
///    "x-files"
///
pub external fn lowercase(String) -> String =
  "string" "lowercase"

/// Create a new string with all the graphemes in the input string converted to
/// uppercase.
///
/// Useful for case-insensitive comparisons and VIRTUAL YELLING.
///
/// ## Examples
///
///    > uppercase("skinner")
///    "SKINNER"
///
pub external fn uppercase(String) -> String =
  "string" "uppercase"

/// Compares two strings to see which is "larger" by comparing their graphemes.
///
/// This does not compare the size or length of the given strings.
///
/// ## Examples
///
///    > compare("Anthony", "Anthony")
///    order.Eq
///
///    > compare("A", "B")
///    order.Lt
///
pub external fn compare(String, String) -> order.Order =
  "gleam_stdlib" "compare_strings"

external fn erl_slice(String, Int, Int) -> String =
  "string" "slice"

/// Take a substring given a start and end Grapheme indexes. Negative indexes
/// are taken starting from the *end* of the list.
///
/// ## Examples
///    > slice(from: "gleam", at_index: 1, length: 2)
///    "le"
///
///    > slice(from: "gleam", at_index: 1, length: 10)
///    "leam"
///
///    > slice(from: "gleam", at_index: 10, length: 3)
///    ""
///
///    > slice(from: "gleam", at_index: -2, length: 2)
///    "am"
///
///    > slice(from: "gleam", at_index: -12, length: 2)
///    ""
///
pub fn slice(from string: String, at_index idx: Int, length len: Int) -> String {
  case len < 0 {
    True -> ""
    False ->
      case idx < 0 {
        True -> {
          let translated_idx = length(string) + idx
          case translated_idx < 0 {
            True -> ""
            False -> erl_slice(string, translated_idx, len)
          }
        }
        False -> erl_slice(string, idx, len)
      }
  }
}

/// Drop *n* Graphemes from the left side of a string.
///
/// ## Examples
///    > drop_left(from: "The Lone Gunmen", up_to: 2)
///    "e Lone Gunmen"
///
pub fn drop_left(from string: String, up_to num_graphemes: Int) -> String {
  case num_graphemes < 0 {
    True -> string
    False -> slice(string, num_graphemes, length(string) - num_graphemes)
  }
}

/// Drop *n* Graphemes from the right side of a string.
///
/// ## Examples
///    > drop_right(from: "Cigarette Smoking Man", up_to: 2)
///    "Cigarette Smoking M"
///
pub fn drop_right(from string: String, up_to num_graphemes: Int) -> String {
  case num_graphemes < 0 {
    True -> string
    False -> slice(string, 0, length(string) - num_graphemes)
  }
}

external fn erl_contains(String, String) -> Dynamic =
  "string" "find"

/// Check if the first string contains the second.
///
/// ## Examples
///
///    > contains(does: "theory", contain: "ory")
///    True
///
///    > contains(does: "theory", contain: "the")
///    True
///
///    > contains(does: "theory", contain: "THE")
///    False
///
pub fn contains(does haystack: String, contain needle: String) -> Bool {
  haystack
  |> erl_contains(needle)
  |> dynamic.atom
  |> result.is_error
}

/// See if the first string starts with the second one.
///
/// ## Examples
///
///    > starts_with("theory", "ory")
///    False
///
pub external fn starts_with(String, String) -> Bool =
  "gleam_stdlib" "string_starts_with"

/// See if the first string ends with the second one.
///
/// ## Examples
///
///    > ends_with("theory", "ory")
///    True
///
pub external fn ends_with(String, String) -> Bool =
  "gleam_stdlib" "string_ends_with"

/// Create a list of strings by splitting a given string on a given substring.
///
/// ## Examples
///
///    > split("home/gleam/desktop/", on: "/")
///    ["home", "gleam", "desktop", ""]
///
pub fn split(x: String, on substring: String) -> List(String) {
  x
  |> string_builder.from_string
  |> string_builder.split(on: substring)
  |> list.map(with: string_builder.to_string)
}

external fn erl_split(String, String) -> List(String) =
  "string" "split"

/// Splits a string a single time on the given substring.
///
/// Returns an error if substring not present.
///
/// ## Examples
///
///    > split_once("home/gleam/desktop/", on: "/")
///    Ok(tuple("home", "gleam/desktop/"))
///
///    > split_once("home/gleam/desktop/", on: "?")
///    Error(Nil)
///
pub fn split_once(
  x: String,
  on substring: String,
) -> Result(tuple(String, String), Nil) {
  case erl_split(x, substring) {
    [first, rest] -> Ok(tuple(first, rest))
    _ -> Error(Nil)
  }
}

/// Create a new string by joining two strings together.
///
/// This function copies both strings and runs in linear time. If you find
/// yourself joining strings frequently consider using the [string_builder](../string_builder)
/// module as it can append strings much faster!
///
/// ## Examples
///
///    > append(to: "butter", suffix: "fly")
///    "butterfly"
///
pub fn append(to first: String, suffix second: String) -> String {
  first
  |> string_builder.from_string
  |> string_builder.append(second)
  |> string_builder.to_string
}

/// Create a new string by joining many strings together.
///
/// This function copies both strings and runs in linear time. If you find
/// yourself joining strings frequently consider using the [string_builder](../string_builder)
/// module as it can append strings much faster!
///
/// ## Examples
///
///    > concat(["never", "the", "less"])
///    "nevertheless"
///
pub fn concat(strings: List(String)) -> String {
  strings
  |> string_builder.from_strings
  |> string_builder.to_string
}

/// Create a new string by repeating a string a given number of times.
///
/// This function runs in linear time.
///
/// ## Examples
///
///    > repeat("ha", times: 3)
///    "hahaha"
///
pub fn repeat(string: String, times times: Int) -> String {
  iterator.repeat(string)
  |> iterator.take(times)
  |> concat
}

/// Join many strings together with a given separator.
///
/// This function runs in linear time.
///
/// ## Examples
///
///    > join(["home","evan","Desktop"], with: "/")
///    "home/evan/Desktop"
///
pub fn join(strings: List(String), with separator: String) -> String {
  strings
  |> list.intersperse(with: separator)
  |> concat
}

type Direction {
  Leading
  Trailing
  Both
}

external fn erl_pad(String, Int, Direction, String) -> String =
  "gleam_stdlib" "string_pad"

/// Pad a string on the left until it has at least given number of Graphemes.
///
/// ## Examples
///
///    > pad_left("121", to: 5, with: ".")
///    "..121"
///
///    > pad_left("121", to: 3, with: ".")
///    "121"
///
///    > pad_left("121", to: 2, with: ".")
///    "121"
///
pub fn pad_left(string: String, to length: Int, with pad_string: String) {
  erl_pad(string, length, Leading, pad_string)
}

/// Pad a string on the right until it has a given length.
///
/// ## Examples
///
///    > pad_right("121", to: 5, with: ".")
///    "121.."
///
///    > pad_right("121", to: 3, with: ".")
///    "121"
///
///    > pad_right("121", to: 2, with: ".")
///    "121"
///
pub fn pad_right(string: String, to length: Int, with pad_string: String) {
  erl_pad(string, length, Trailing, pad_string)
}

external fn erl_trim(String, Direction) -> String =
  "string" "trim"

/// Get rid of whitespace on both sides of a String.
///
/// ## Examples
///
///    > trim("  hats  \n")
///    "hats"
///
pub fn trim(string: String) -> String {
  erl_trim(string, Both)
}

/// Get rid of whitespace on the left of a String.
///
/// ## Examples
///
///    > trim_left("  hats  \n")
///    "hats  \n"
///
pub fn trim_left(string: String) -> String {
  erl_trim(string, Leading)
}

/// Get rid of whitespace on the right of a String.
///
/// ## Examples
///
///    > trim_right("  hats  \n")
///    "  hats"
///
pub fn trim_right(string: String) -> String {
  erl_trim(string, Trailing)
}

/// Split a non-empty string into its head and tail. This lets you
/// pattern match on strings exactly as you would with lists.
///
/// ## Examples
///    > pop_grapheme("gleam")
///    Ok(tuple("g", "leam"))
///
///    > pop_grapheme("")
///    Error(Nil)
///
pub external fn pop_grapheme(
  string: String,
) -> Result(tuple(String, String), Nil) =
  "gleam_stdlib" "string_pop_grapheme"

/// Convert a string to a list of Graphemes.
///
///    > to_graphemes("abc")
///    ["a", "b", "c"]
///
pub fn to_graphemes(string: String) -> List(String) {
  case pop_grapheme(string) {
    Ok(tuple(grapheme, rest)) -> [grapheme, ..to_graphemes(rest)]
    _ -> []
  }
}

external fn int_to_utf_codepoint(Int) -> UtfCodepoint =
  "gleam_stdlib" "identity"

/// Convert an integer to a UtfCodepoint
///
/// Returns an error if the integer does not represent a valid UTF codepoint.
///
pub fn utf_codepoint(value: Int) -> Result(UtfCodepoint, Nil) {
  case value {
    i if i > 1114111 -> Error(Nil)
    65534 | 65535 -> Error(Nil)
    i if i >= 55296 && i <= 57343 -> Error(Nil)
    i -> Ok(int_to_utf_codepoint(i))
  }
}
