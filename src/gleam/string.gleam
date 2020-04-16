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

/// ## Get Substrings

// TODO
/// Take a substring given a start and end Grapheme indexes. Negative indexes
/// are taken starting from the *end* of the list.
///
/// ## Examples
/// ```gleam
/// slice("gleam", from: 1, to: 3) == "lea"
/// slice("gleam", from: 1, to: 10) == "leam"
/// slice("snakes on a plane!", from: -6, to: -1) == "plane"
/// ```
///
// pub fn slice(out_of string: String, from start: Int, end: Int) -> String {}

// TODO
/// Drop *n* Graphemes from the left side of a
///
/// ## Examples
/// ```gleam
/// drop_left(from: "The Lone Gunmen", up_to: 2) == "e Lone Gunmen"
/// ```
///
// pub fn drop_left(from string: String, up_to num_graphemes: Int) -> String {}

// TODO
/// Drop *n* Graphemes from the right side of a
///
/// ## Examples
/// ```gleam
/// drop_right(from: "Cigarette Smoking Man", up_to: 2) == "Cigarette Smoking M"
/// ```
///
// pub fn drop_right(from string: String, up_to num_graphemes: Int) -> String {}

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
external fn erl_contains(String, String) -> Bool =
  "gleam_stdlib" "string_contains"

pub fn contains(does haystack: String, contain needle: String) -> Bool {
    erl_contains(haystack, needle)
}

// TODO
// TODO: Not sure about the name and labels here
/// See if the second string starts with the first one.
///
/// ## Examples
/// ```gleam
/// starts_with(does: "theory", start_with: "ory") == False
/// ```
///
// pub fn starts_with(does string: String, start_with prefix: String) -> String {}

// TODO
// TODO: Not sure about the name and labels here
/// See if the second string ends with the first one.
///
/// ## Examples
/// ```gleam
/// ends_with(does: "theory", end_with: "ory") == True
/// ```
///
// pub fn ends_with(does string: String, end_with suffix: String) -> String {}

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
pub fn concat(strings: List(String)) -> String {
  strings
  |> iodata.from_strings
  |> iodata.to_string
}

/// Repeat a string `n` times.
///
/// ## Examples
/// ```gleam
/// repeat("ha", times: 3) == "hahaha"
/// ```
///
fn repeat_help(chunk: String, result: List(String), repeats: Int) -> String {
  case repeats <= 0 {
     True -> concat(result)
     False -> repeat_help(chunk, [chunk | result], repeats - 1)
  }
}

pub fn repeat(string: String, times times: Int) -> String {
  repeat_help(string, [], times)
}

/// Join many strings together with a given separator.
///
/// ## Examples
/// ```gleam
/// join(["home","evan","Desktop"], with: "/") == "home/evan/Desktop"
/// ```
///
pub fn join(strings: List(String), with separator: String) -> String {
  strings
  |> list.intersperse(_, with: separator)
  |> iodata.from_strings
  |> iodata.to_string
}

/// ## Formatting

// TODO
/// Pad a string on the left until it has at least given number of Graphemes.
///
/// ## Examples
/// ```gleam
/// pad_left("121", to: 5, with: ".") == "..121"
/// pad_left("121", to: 3, with: ".") == "121"
/// pad_left("121", to: 2, with: ".") == "121"
/// ```
///
// pub fn pad_left(string: String, to size: Int, with: String) {}

// TODO
/// Pad a string on the right until it has a given length.
///
/// ## Examples
/// ```gleam
/// pad_right("121", to: 5, with: ".") == "121.."
/// pad_right("121", to: 3, with: ".") == "121"
/// pad_right("121", to: 2, with: ".") == "121"
/// ```
///
// pub fn pad_right(string: String, to size: Int, with: String) {}

// TODO
/// Get rid of whitespace on both sides of a String.
///
/// ## Examples
/// ```gleam
/// trim("  hats  \n") == "hats"
/// ```
///
// pub fn trim(string: String) -> String {}

// TODO
/// Get rid of whitespace on the left of a String.
///
/// ## Examples
/// ```gleam
/// trim_left("  hats  \n") == "hats  \n"
/// ```
///
// pub fn trim_left(string: String) -> String {}

// TODO
/// Get rid of whitespace on the right of a String.
///
/// ## Examples
/// ```gleam
/// trim_right("  hats  \n") == "  hats"
/// ```
///
// pub fn trim_right(string: String) -> String {}

/// ## Grapheme Conversions

// These functions convert to and from Grapheme, which currently
// does not exist as a type in Gleam.

// TODO
// /// Convert a string to a list of Graphemes.
// ///
// /// to_graphemes("abc") == ['a','b','c']
// ///
// pub fn to_graphemes(string: String) -> List(String) {}

// TODO
// /// Convert a list of characters into a String. Can be useful if you
// /// want to create a string primarily by consing, perhaps for decoding
// /// something.
// ///
// /// from_list(['a','b','c']) == "abc"
// ///
// // pub fn from_graphemes(graphemes: List(Grapheme)) -> String {}

// TODO
/// Split a non-empty string into its head and tail. This lets you
/// pattern match on strings exactly as you would with lists.
///
/// ## Examples
/// ```gleam
/// next_grapheme("") == Error(Nil)
/// ```
///
// pub fn next_grapheme(string: String) -> Option(tuple(Grapheme, String)) {}
