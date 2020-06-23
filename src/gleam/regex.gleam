//// This module contains regular expression matching functions for strings.
//// The matching algorithms of the library are based on the PCRE library, but not
//// all of the PCRE library is interfaced and some parts of the library go beyond
//// what PCRE offers. Currently PCRE version 8.40 (release date 2017-01-11) is used.

import gleam/option.{Option}

pub external type Regex

/// The details about a particular match:
///
/// - match — the full string of the match.
/// - index — the index of the match in the original string.
/// - number — each match is numbered, starting from 1
/// - submatches — a Regex can have subpatterns, sup-parts that are in parentheses.
///
pub type Match {
  Match(
    match: String,
    index: Int,
    number: Int,
    submatches: List(Option(String)),
  )
}

/// When a regular expression fails to compile:
///
/// - error — a descriptive error message
/// - index — the index of the cause in the regex string
///
pub type FromStringError {
  FromStringError(error: String, index: Int)
}

/// Create a new Regex.
///
/// ## Examples
///
///    > let Ok(re) = from_string("[0-9]")
///    > match(re, "abc123")
///    True
///
///    > match(re, "abcxyz")
///    False
///
///    > from_string("[0-9")
///    Error(
///      FromStringError(
///        error: "missing terminating ] for character class",
///        index: 4
///      )
///    )
///
pub external fn from_string(String) -> Result(Regex, FromStringError) =
  "gleam_stdlib" "regex_from_string"

pub type Options {
  Options(case_insensitive: Bool, multi_line: Bool)
}

/// Create a Regex with some additional options.
///
/// ## Examples
///
///    > let options = Options(case_insensitive: False, multi_line: True)
///    > let Ok(re) = from_string_with(options, "^[0-9]")
///    > match(re, "abc\n123")
///    True
///
///    > let options = Options(case_insensitive: True, multi_line: False)
///    > let Ok(re) = from_string_with(options, "[A-Z]")
///    > match(re, "abc123")
///    True
///
pub external fn from_string_with(
  Options,
  String,
) -> Result(Regex, FromStringError) =
  "gleam_stdlib" "regex_from_string_with"

/// Returns a boolean indicating whether there was a match or not.
///
/// ## Examples
///
///    > let Ok(re) = from_string("^f.o.?")
///    > match(re, "foo")
///    True
///
///    > match(re, "boo")
///    False
///
pub external fn match(Regex, String) -> Bool =
  "gleam_stdlib" "regex_match"

/// Split a string
///
/// ## Examples
///
///    > let Ok(re) = from_string(" *, *")
///    > match(re, "foo,32, 4, 9  ,0")
///    ["foo", "32", "4", "9", "0"]
///
pub external fn split(Regex, String) -> List(String) =
  "gleam_stdlib" "regex_split"

/// Collects all matches of the regular expression.
///
/// ## Examples
///
///    > let Ok(re) = regex.from_string("[oi]n a (\\w+)")
///    > regex.scan(re, "I am on a boat in a lake.")
///    [
///      Match(
///        match: "on a boat",
///        index: 5,
///        number: 1,
///        submatches: [Some("boat")]
///      ),
///      Match(
///        match: "in a lake",
///        index: 15,
///        number: 2,
///        submatches: [Some("lake")]
///      )
///    ]
///
pub external fn scan(Regex, String) -> List(Match) =
  "gleam_stdlib" "regex_scan"
