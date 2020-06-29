//// This module contains regular expression matching functions for strings.
//// The matching algorithms of the library are based on the PCRE library, but not
//// all of the PCRE library is interfaced and some parts of the library go beyond
//// what PCRE offers. Currently PCRE version 8.40 (release date 2017-01-11) is used.

import gleam/option.{Option}

pub external type Regex

/// The details about a particular match:
///
pub type Match {
  Match(
    /// The full string of the match.
    content: String,
    /// The byte index of the match in the original string.
    byte_index: Int,
    /// A Regex can have subpatterns, sup-parts that are in parentheses.
    submatches: List(Option(String)),
  )
}

/// When a regular expression fails to compile:
///
pub type CompileError {
  CompileError(
    /// The problem encountered that caused the compilation to fail
    error: String,
    /// The byte index into the string to where the problem was found
    byte_index: Int,
  )
}

pub type Options {
  Options(case_insensitive: Bool, multi_line: Bool)
}

/// Create a Regex with some additional options.
///
/// ## Examples
///
///    > let options = Options(case_insensitive: False, multi_line: True)
///    > assert Ok(re) = compile("^[0-9]", with: options)
///    > match(re, "abc\n123")
///    True
///
///    > let options = Options(case_insensitive: True, multi_line: False)
///    > assert Ok(re) = compile("[A-Z]", with: options)
///    > match(re, "abc123")
///    True
///
pub external fn compile(String, with: Options) -> Result(Regex, CompileError) =
  "gleam_stdlib" "compile_regex"

/// Create a new Regex.
///
/// ## Examples
///
///    > assert Ok(re) = from_string("[0-9]")
///    > match(re, "abc123")
///    True
///
///    > match(re, "abcxyz")
///    False
///
///    > from_string("[0-9")
///    Error(
///      CompileError(
///        error: "missing terminating ] for character class",
///        byte_index: 4
///      )
///    )
///
pub fn from_string(pattern: String) -> Result(Regex, CompileError) {
  compile(pattern, Options(case_insensitive: False, multi_line: False))
}

/// Returns a boolean indicating whether there was a match or not.
///
/// ## Examples
///
///    > assert Ok(re) = from_string("^f.o.?")
///    > check(with: re, content: "foo")
///    True
///
///    > check(with: re, content: "boo")
///    False
///
pub external fn check(with: Regex, content: String) -> Bool =
  "gleam_stdlib" "regex_match"

/// Split a string
///
/// ## Examples
///
///    > assert Ok(re) = from_string(" *, *")
///    > split(with: re, content: "foo,32, 4, 9  ,0")
///    ["foo", "32", "4", "9", "0"]
///
pub external fn split(with: Regex, content: String) -> List(String) =
  "gleam_stdlib" "regex_split"

/// Collects all matches of the regular expression.
///
/// ## Examples
///
///    > assert Ok(re) = regex.from_string("[oi]n a (\\w+)")
///    > regex.scan(with: re, content: "I am on a boat in a lake.")
///    [
///      Match(
///        content: "on a boat",
///        byte_index: 5,
///        submatches: [Some("boat")]
///      ),
///      Match(
///        content: "in a lake",
///        byte_index: 15,
///        submatches: [Some("lake")]
///      )
///    ]
///
pub external fn scan(with: Regex, content: String) -> List(Match) =
  "gleam_stdlib" "regex_scan"
