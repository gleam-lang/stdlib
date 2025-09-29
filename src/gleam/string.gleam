//// Strings in Gleam are UTF-8 binaries. They can be written in your code as
//// text surrounded by `"double quotes"`.

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/string_tree.{type StringTree}

/// Determines if a `String` is empty.
///
/// ## Examples
///
/// ```gleam
/// is_empty("")
/// // -> True
/// ```
///
/// ```gleam
/// is_empty("the world")
/// // -> False
/// ```
///
pub fn is_empty(str: String) -> Bool {
  str == ""
}

/// Gets the number of grapheme clusters in a given `String`.
///
/// This function has to iterate across the whole string to count the number of
/// graphemes, so it runs in linear time. Avoid using this in a loop.
///
/// ## Examples
///
/// ```gleam
/// length("Gleam")
/// // -> 5
/// ```
///
/// ```gleam
/// length("ß↑e̊")
/// // -> 3
/// ```
///
/// ```gleam
/// length("")
/// // -> 0
/// ```
///
@external(erlang, "string", "length")
@external(javascript, "../gleam_stdlib.mjs", "string_length")
pub fn length(string: String) -> Int

/// Reverses a `String`.
///
/// This function has to iterate across the whole `String` so it runs in linear
/// time. Avoid using this in a loop.
///
/// ## Examples
///
/// ```gleam
/// reverse("stressed")
/// // -> "desserts"
/// ```
///
pub fn reverse(string: String) -> String {
  string
  |> string_tree.from_string
  |> string_tree.reverse
  |> string_tree.to_string
}

/// Creates a new `String` by replacing all occurrences of a given substring.
///
/// ## Examples
///
/// ```gleam
/// replace("www.example.com", each: ".", with: "-")
/// // -> "www-example-com"
/// ```
///
/// ```gleam
/// replace("a,b,c,d,e", each: ",", with: "/")
/// // -> "a/b/c/d/e"
/// ```
///
pub fn replace(
  in string: String,
  each pattern: String,
  with substitute: String,
) -> String {
  string
  |> string_tree.from_string
  |> string_tree.replace(each: pattern, with: substitute)
  |> string_tree.to_string
}

/// Creates a new `String` with all the graphemes in the input `String` converted to
/// lowercase.
///
/// Useful for case-insensitive comparisons.
///
/// ## Examples
///
/// ```gleam
/// lowercase("X-FILES")
/// // -> "x-files"
/// ```
///
@external(erlang, "string", "lowercase")
@external(javascript, "../gleam_stdlib.mjs", "lowercase")
pub fn lowercase(string: String) -> String

/// Creates a new `String` with all the graphemes in the input `String` converted to
/// uppercase.
///
/// Useful for case-insensitive comparisons and VIRTUAL YELLING.
///
/// ## Examples
///
/// ```gleam
/// uppercase("skinner")
/// // -> "SKINNER"
/// ```
///
@external(erlang, "string", "uppercase")
@external(javascript, "../gleam_stdlib.mjs", "uppercase")
pub fn uppercase(string: String) -> String

/// Compares two `String`s to see which is "larger" by comparing their graphemes.
///
/// This does not compare the size or length of the given `String`s.
///
/// ## Examples
///
/// ```gleam
/// compare("Anthony", "Anthony")
/// // -> order.Eq
/// ```
///
/// ```gleam
/// compare("A", "B")
/// // -> order.Lt
/// ```
///
pub fn compare(a: String, b: String) -> order.Order {
  case a == b {
    True -> order.Eq
    _ ->
      case less_than(a, b) {
        True -> order.Lt
        False -> order.Gt
      }
  }
}

@external(erlang, "gleam_stdlib", "less_than")
@external(javascript, "../gleam_stdlib.mjs", "less_than")
fn less_than(a: String, b: String) -> Bool

/// Takes a substring given a start grapheme index and a length. Negative indexes
/// are taken starting from the *end* of the list.
///
/// This function runs in linear time with the size of the index and the
/// length. Negative indexes are linear with the size of the input string in
/// addition to the other costs.
///
/// ## Examples
///
/// ```gleam
/// slice(from: "gleam", at_index: 1, length: 2)
/// // -> "le"
/// ```
///
/// ```gleam
/// slice(from: "gleam", at_index: 1, length: 10)
/// // -> "leam"
/// ```
///
/// ```gleam
/// slice(from: "gleam", at_index: 10, length: 3)
/// // -> ""
/// ```
///
/// ```gleam
/// slice(from: "gleam", at_index: -2, length: 2)
/// // -> "am"
/// ```
///
/// ```gleam
/// slice(from: "gleam", at_index: -12, length: 2)
/// // -> ""
/// ```
///
pub fn slice(from string: String, at_index idx: Int, length len: Int) -> String {
  case len <= 0 {
    True -> ""
    False ->
      case idx < 0 {
        True -> {
          let translated_idx = length(string) + idx
          case translated_idx < 0 {
            True -> ""
            False -> grapheme_slice(string, translated_idx, len)
          }
        }
        False -> grapheme_slice(string, idx, len)
      }
  }
}

@external(erlang, "gleam_stdlib", "slice")
@external(javascript, "../gleam_stdlib.mjs", "string_grapheme_slice")
fn grapheme_slice(string: String, index: Int, length: Int) -> String

@external(erlang, "binary", "part")
@external(javascript, "../gleam_stdlib.mjs", "string_byte_slice")
fn unsafe_byte_slice(string: String, index: Int, length: Int) -> String

/// Drops contents of the first `String` that occur before the second `String`.
/// If the `from` string does not contain the `before` string, `from` is
/// returned unchanged.
///
/// ## Examples
///
/// ```gleam
/// crop(from: "The Lone Gunmen", before: "Lone")
/// // -> "Lone Gunmen"
/// ```
///
@external(erlang, "gleam_stdlib", "crop_string")
@external(javascript, "../gleam_stdlib.mjs", "crop_string")
pub fn crop(from string: String, before substring: String) -> String

/// Drops *n* graphemes from the start of a `String`.
///
/// This function runs in linear time with the number of graphemes to drop.
///
/// ## Examples
///
/// ```gleam
/// drop_start(from: "The Lone Gunmen", up_to: 2)
/// // -> "e Lone Gunmen"
/// ```
///
pub fn drop_start(from string: String, up_to num_graphemes: Int) -> String {
  case num_graphemes <= 0 {
    True -> string
    False -> {
      let prefix = grapheme_slice(string, 0, num_graphemes)
      let prefix_size = byte_size(prefix)
      unsafe_byte_slice(string, prefix_size, byte_size(string) - prefix_size)
    }
  }
}

/// Drops *n* graphemes from the end of a `String`.
///
/// This function traverses the full string, so it runs in linear time with the
/// size of the string. Avoid using this in a loop.
///
/// ## Examples
///
/// ```gleam
/// drop_end(from: "Cigarette Smoking Man", up_to: 2)
/// // -> "Cigarette Smoking M"
/// ```
///
pub fn drop_end(from string: String, up_to num_graphemes: Int) -> String {
  case num_graphemes <= 0 {
    True -> string
    False -> slice(string, 0, length(string) - num_graphemes)
  }
}

/// Checks if the first `String` contains the second.
///
/// ## Examples
///
/// ```gleam
/// contains(does: "theory", contain: "ory")
/// // -> True
/// ```
///
/// ```gleam
/// contains(does: "theory", contain: "the")
/// // -> True
/// ```
///
/// ```gleam
/// contains(does: "theory", contain: "THE")
/// // -> False
/// ```
///
@external(erlang, "gleam_stdlib", "contains_string")
@external(javascript, "../gleam_stdlib.mjs", "contains_string")
pub fn contains(does haystack: String, contain needle: String) -> Bool

/// Checks whether the first `String` starts with the second one.
///
/// ## Examples
///
/// ```gleam
/// starts_with("theory", "ory")
/// // -> False
/// ```
///
@external(erlang, "gleam_stdlib", "string_starts_with")
@external(javascript, "../gleam_stdlib.mjs", "starts_with")
pub fn starts_with(string: String, prefix: String) -> Bool

/// Checks whether the first `String` ends with the second one.
///
/// ## Examples
///
/// ```gleam
/// ends_with("theory", "ory")
/// // -> True
/// ```
///
@external(erlang, "gleam_stdlib", "string_ends_with")
@external(javascript, "../gleam_stdlib.mjs", "ends_with")
pub fn ends_with(string: String, suffix: String) -> Bool

/// Creates a list of `String`s by splitting a given string on a given substring.
///
/// ## Examples
///
/// ```gleam
/// split("home/gleam/desktop/", on: "/")
/// // -> ["home", "gleam", "desktop", ""]
/// ```
///
pub fn split(x: String, on substring: String) -> List(String) {
  case substring {
    "" -> to_graphemes(x)
    _ ->
      x
      |> string_tree.from_string
      |> string_tree.split(on: substring)
      |> list.map(with: string_tree.to_string)
  }
}

/// Splits a `String` a single time on the given substring.
///
/// Returns an `Error` if substring not present.
///
/// ## Examples
///
/// ```gleam
/// split_once("home/gleam/desktop/", on: "/")
/// // -> Ok(#("home", "gleam/desktop/"))
/// ```
///
/// ```gleam
/// split_once("home/gleam/desktop/", on: "?")
/// // -> Error(Nil)
/// ```
///
@external(javascript, "../gleam_stdlib.mjs", "split_once")
pub fn split_once(
  string: String,
  on substring: String,
) -> Result(#(String, String), Nil) {
  case erl_split(string, substring) {
    [first, rest] -> Ok(#(first, rest))
    _ -> Error(Nil)
  }
}

@external(erlang, "string", "split")
fn erl_split(a: String, b: String) -> List(String)

/// Creates a new `String` by joining two `String`s together.
///
/// This function typically copies both `String`s and runs in linear time, but
/// the exact behaviour will depend on how the runtime you are using optimises
/// your code. Benchmark and profile your code if you need to understand its
/// performance better.
///
/// If you are joining together large string and want to avoid copying any data
/// you may want to investigate using the [`string_tree`](../gleam/string_tree.html)
/// module.
///
/// ## Examples
///
/// ```gleam
/// append(to: "butter", suffix: "fly")
/// // -> "butterfly"
/// ```
///
pub fn append(to first: String, suffix second: String) -> String {
  first <> second
}

/// Creates a new `String` by joining many `String`s together.
///
/// This function copies all the `String`s and runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// concat(["never", "the", "less"])
/// // -> "nevertheless"
/// ```
///
@external(erlang, "erlang", "list_to_binary")
pub fn concat(strings: List(String)) -> String {
  concat_loop(strings, "")
}

fn concat_loop(strings: List(String), accumulator: String) -> String {
  case strings {
    [string, ..strings] -> concat_loop(strings, accumulator <> string)
    [] -> accumulator
  }
}

/// Creates a new `String` by repeating a `String` a given number of times.
///
/// This function runs in loglinear time.
///
/// ## Examples
///
/// ```gleam
/// repeat("ha", times: 3)
/// // -> "hahaha"
/// ```
///
pub fn repeat(string: String, times times: Int) -> String {
  case times <= 0 {
    True -> ""
    False -> repeat_loop(times, string, "")
  }
}

fn repeat_loop(times: Int, doubling_acc: String, acc: String) -> String {
  let acc = case times % 2 {
    0 -> acc
    _ -> acc <> doubling_acc
  }
  let times = times / 2
  case times <= 0 {
    True -> acc
    False -> repeat_loop(times, doubling_acc <> doubling_acc, acc)
  }
}

/// Joins many `String`s together with a given separator.
///
/// This function runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// join(["home","evan","Desktop"], with: "/")
/// // -> "home/evan/Desktop"
/// ```
///
pub fn join(strings: List(String), with separator: String) -> String {
  case strings {
    [] -> ""
    [first, ..rest] -> join_loop(rest, separator, first)
  }
}

fn join_loop(
  strings: List(String),
  separator: String,
  accumulator: String,
) -> String {
  case strings {
    [] -> accumulator
    [string, ..strings] ->
      join_loop(strings, separator, accumulator <> separator <> string)
  }
}

/// Pads the start of a `String` until it has a given length.
///
/// ## Examples
///
/// ```gleam
/// pad_start("121", to: 5, with: ".")
/// // -> "..121"
/// ```
///
/// ```gleam
/// pad_start("121", to: 3, with: ".")
/// // -> "121"
/// ```
///
/// ```gleam
/// pad_start("121", to: 2, with: ".")
/// // -> "121"
/// ```
///
pub fn pad_start(
  string: String,
  to desired_length: Int,
  with pad_string: String,
) -> String {
  let current_length = length(string)
  let to_pad_length = desired_length - current_length

  case to_pad_length <= 0 {
    True -> string
    False -> padding(to_pad_length, pad_string) <> string
  }
}

/// Pads the end of a `String` until it has a given length.
///
/// ## Examples
///
/// ```gleam
/// pad_end("123", to: 5, with: ".")
/// // -> "123.."
/// ```
///
/// ```gleam
/// pad_end("123", to: 3, with: ".")
/// // -> "123"
/// ```
///
/// ```gleam
/// pad_end("123", to: 2, with: ".")
/// // -> "123"
/// ```
///
pub fn pad_end(
  string: String,
  to desired_length: Int,
  with pad_string: String,
) -> String {
  let current_length = length(string)
  let to_pad_length = desired_length - current_length

  case to_pad_length <= 0 {
    True -> string
    False -> string <> padding(to_pad_length, pad_string)
  }
}

fn padding(size: Int, pad_string: String) -> String {
  let pad_string_length = length(pad_string)
  let num_pads = size / pad_string_length
  let extra = size % pad_string_length

  repeat(pad_string, num_pads) <> slice(pad_string, 0, extra)
}

/// Removes whitespace on both sides of a `String`.
///
/// Whitespace in this function is the set of nonbreakable whitespace
/// codepoints, defined as Pattern_White_Space in [Unicode Standard Annex #31][1].
///
/// [1]: https://unicode.org/reports/tr31/
///
/// ## Examples
///
/// ```gleam
/// trim("  hats  \n")
/// // -> "hats"
/// ```
///
pub fn trim(string: String) -> String {
  string |> trim_start |> trim_end
}

@external(erlang, "string", "trim")
fn erl_trim(a: String, b: Direction) -> String

type Direction {
  Leading
  Trailing
}

/// Removes whitespace at the start of a `String`.
///
/// ## Examples
///
/// ```gleam
/// trim_start("  hats  \n")
/// // -> "hats  \n"
/// ```
///
@external(javascript, "../gleam_stdlib.mjs", "trim_start")
pub fn trim_start(string: String) -> String {
  erl_trim(string, Leading)
}

/// Removes whitespace at the end of a `String`.
///
/// ## Examples
///
/// ```gleam
/// trim_end("  hats  \n")
/// // -> "  hats"
/// ```
///
@external(javascript, "../gleam_stdlib.mjs", "trim_end")
pub fn trim_end(string: String) -> String {
  erl_trim(string, Trailing)
}

/// Splits a non-empty `String` into its first element (head) and rest (tail).
/// This lets you pattern match on `String`s exactly as you would with lists.
///
/// ## Performance
///
/// There is a notable overhead to using this function, so you may not want to
/// use it in a tight loop. If you wish to efficiently parse a string you may
/// want to use alternatives such as the [splitter package](https://hex.pm/packages/splitter).
///
/// ## Examples
///
/// ```gleam
/// pop_grapheme("gleam")
/// // -> Ok(#("g", "leam"))
/// ```
///
/// ```gleam
/// pop_grapheme("")
/// // -> Error(Nil)
/// ```
///
@external(erlang, "gleam_stdlib", "string_pop_grapheme")
@external(javascript, "../gleam_stdlib.mjs", "pop_grapheme")
pub fn pop_grapheme(string: String) -> Result(#(String, String), Nil)

/// Converts a `String` to a list of
/// [graphemes](https://en.wikipedia.org/wiki/Grapheme).
///
/// ```gleam
/// to_graphemes("abc")
/// // -> ["a", "b", "c"]
/// ```
///
@external(javascript, "../gleam_stdlib.mjs", "graphemes")
pub fn to_graphemes(string: String) -> List(String) {
  string
  |> to_graphemes_loop([])
  |> list.reverse
}

fn to_graphemes_loop(string: String, acc: List(String)) -> List(String) {
  case pop_grapheme(string) {
    Ok(#(grapheme, rest)) -> to_graphemes_loop(rest, [grapheme, ..acc])
    Error(_) -> acc
  }
}

@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "codepoint")
fn unsafe_int_to_utf_codepoint(a: Int) -> UtfCodepoint

/// Converts a `String` to a `List` of `UtfCodepoint`.
///
/// See <https://en.wikipedia.org/wiki/Code_point> and
/// <https://en.wikipedia.org/wiki/Unicode#Codespace_and_Code_Points> for an
/// explanation on code points.
///
/// ## Examples
///
/// ```gleam
/// "a" |> to_utf_codepoints
/// // -> [UtfCodepoint(97)]
/// ```
///
/// ```gleam
/// // Semantically the same as:
/// // ["🏳", "️", "‍", "🌈"] or:
/// // [waving_white_flag, variant_selector_16, zero_width_joiner, rainbow]
/// "🏳️‍🌈" |> to_utf_codepoints
/// // -> [
/// //   UtfCodepoint(127987),
/// //   UtfCodepoint(65039),
/// //   UtfCodepoint(8205),
/// //   UtfCodepoint(127752),
/// // ]
/// ```
///
pub fn to_utf_codepoints(string: String) -> List(UtfCodepoint) {
  do_to_utf_codepoints(string)
}

@target(erlang)
fn do_to_utf_codepoints(string: String) -> List(UtfCodepoint) {
  to_utf_codepoints_loop(<<string:utf8>>, [])
}

@target(erlang)
fn to_utf_codepoints_loop(
  bit_array: BitArray,
  acc: List(UtfCodepoint),
) -> List(UtfCodepoint) {
  case bit_array {
    <<first:utf8_codepoint, rest:bytes>> ->
      to_utf_codepoints_loop(rest, [first, ..acc])
    _ -> list.reverse(acc)
  }
}

@target(javascript)
fn do_to_utf_codepoints(string: String) -> List(UtfCodepoint) {
  string
  |> string_to_codepoint_integer_list
  |> list.map(unsafe_int_to_utf_codepoint)
}

@target(javascript)
@external(javascript, "../gleam_stdlib.mjs", "string_to_codepoint_integer_list")
fn string_to_codepoint_integer_list(string: String) -> List(Int)

/// Converts a `List` of `UtfCodepoint`s to a `String`.
///
/// See <https://en.wikipedia.org/wiki/Code_point> and
/// <https://en.wikipedia.org/wiki/Unicode#Codespace_and_Code_Points> for an
/// explanation on code points.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(a) = utf_codepoint(97)
/// let assert Ok(b) = utf_codepoint(98)
/// let assert Ok(c) = utf_codepoint(99)
/// from_utf_codepoints([a, b, c])
/// // -> "abc"
/// ```
///
@external(erlang, "gleam_stdlib", "utf_codepoint_list_to_string")
@external(javascript, "../gleam_stdlib.mjs", "utf_codepoint_list_to_string")
pub fn from_utf_codepoints(utf_codepoints: List(UtfCodepoint)) -> String

/// Converts an integer to a `UtfCodepoint`.
///
/// Returns an `Error` if the integer does not represent a valid UTF codepoint.
///
pub fn utf_codepoint(value: Int) -> Result(UtfCodepoint, Nil) {
  case value {
    i if i > 1_114_111 -> Error(Nil)
    i if i >= 55_296 && i <= 57_343 -> Error(Nil)
    i if i < 0 -> Error(Nil)
    i -> Ok(unsafe_int_to_utf_codepoint(i))
  }
}

/// Converts an UtfCodepoint to its ordinal code point value.
///
/// ## Examples
///
/// ```gleam
/// let assert [utf_codepoint, ..] = to_utf_codepoints("💜")
/// utf_codepoint_to_int(utf_codepoint)
/// // -> 128156
/// ```
///
@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "../gleam_stdlib.mjs", "utf_codepoint_to_int")
pub fn utf_codepoint_to_int(cp: UtfCodepoint) -> Int

/// Converts a `String` into `Option(String)` where an empty `String` becomes
/// `None`.
///
/// ## Examples
///
/// ```gleam
/// to_option("")
/// // -> None
/// ```
///
/// ```gleam
/// to_option("hats")
/// // -> Some("hats")
/// ```
///
pub fn to_option(string: String) -> Option(String) {
  case string {
    "" -> None
    _ -> Some(string)
  }
}

/// Returns the first grapheme cluster in a given `String` and wraps it in a
/// `Result(String, Nil)`. If the `String` is empty, it returns `Error(Nil)`.
/// Otherwise, it returns `Ok(String)`.
///
/// ## Examples
///
/// ```gleam
/// first("")
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// first("icecream")
/// // -> Ok("i")
/// ```
///
pub fn first(string: String) -> Result(String, Nil) {
  case pop_grapheme(string) {
    Ok(#(first, _)) -> Ok(first)
    Error(e) -> Error(e)
  }
}

/// Returns the last grapheme cluster in a given `String` and wraps it in a
/// `Result(String, Nil)`. If the `String` is empty, it returns `Error(Nil)`.
/// Otherwise, it returns `Ok(String)`.
///
/// This function traverses the full string, so it runs in linear time with the
/// length of the string. Avoid using this in a loop.
///
/// ## Examples
///
/// ```gleam
/// last("")
/// // -> Error(Nil)
/// ```
///
/// ```gleam
/// last("icecream")
/// // -> Ok("m")
/// ```
///
pub fn last(string: String) -> Result(String, Nil) {
  case pop_grapheme(string) {
    Ok(#(first, "")) -> Ok(first)
    Ok(#(_, rest)) -> Ok(slice(rest, -1, 1))
    Error(e) -> Error(e)
  }
}

/// Creates a new `String` with the first grapheme in the input `String`
/// converted to uppercase and the remaining graphemes to lowercase.
///
/// ## Examples
///
/// ```gleam
/// capitalise("mamouna")
/// // -> "Mamouna"
/// ```
///
pub fn capitalise(string: String) -> String {
  case pop_grapheme(string) {
    Ok(#(first, rest)) -> append(to: uppercase(first), suffix: lowercase(rest))
    Error(_) -> ""
  }
}

/// Returns a `String` representation of a term in Gleam syntax.
///
/// This may be occasionally useful for quick-and-dirty printing of values in
/// scripts. For error reporting and other uses prefer constructing strings by
/// pattern matching on the values.
///
/// ## Limitations
///
/// The output format of this function is not stable and could change at any
/// time. The output is not suitable for parsing.
///
/// This function works using runtime reflection, so the output may not be
/// perfectly accurate for data structures where the runtime structure doesn't
/// hold enough information to determine the original syntax. For example,
/// tuples with an Erlang atom in the first position will be mistaken for Gleam
/// records.
///
/// ## Security and safety
///
/// There is no limit to how large the strings that this function can produce.
/// Be careful not to call this function with large data structures or you
/// could use very large amounts of memory, potentially causing runtime
/// problems.
///
pub fn inspect(term: anything) -> String {
  term
  |> do_inspect
  |> string_tree.to_string
}

@external(erlang, "gleam_stdlib", "inspect")
@external(javascript, "../gleam_stdlib.mjs", "inspect")
fn do_inspect(term: anything) -> StringTree

/// Returns the number of bytes in a `String`.
///
/// This function runs in constant time on Erlang and in linear time on
/// JavaScript.
///
/// ## Examples
///
/// ```gleam
/// byte_size("🏳️‍⚧️🏳️‍🌈👩🏾‍❤️‍👨🏻")
/// // -> 58
/// ```
///
@external(erlang, "erlang", "byte_size")
@external(javascript, "../gleam_stdlib.mjs", "byte_size")
pub fn byte_size(string: String) -> Int
