//// Utilities for working with URIs
////
//// This module provides functions for working with URIs (for example, parsing
//// URIs or encoding query strings). The functions in this module are implemented
//// according to [RFC 3986](https://tools.ietf.org/html/rfc3986).
////
//// Query encoding (Form encoding) is defined in the
//// [W3C specification](https://www.w3.org/TR/html52/sec-forms.html#urlencoded-form-data).

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam/string_tree.{type StringTree}

/// Type representing holding the parsed components of an URI.
/// All components of a URI are optional, except the path.
///
pub type Uri {
  Uri(
    scheme: Option(String),
    userinfo: Option(String),
    host: Option(String),
    port: Option(Int),
    path: String,
    query: Option(String),
    fragment: Option(String),
  )
}

/// Constant representing an empty URI, equivalent to "".
///
/// ## Examples
///
/// ```gleam
/// let uri = Uri(..empty, scheme: Some("https"), host: Some("example.com"))
/// // -> Uri(
/// //   scheme: Some("https"),
/// //   userinfo: None,
/// //   host: Some("example.com"),
/// //   port: None,
/// //   path: "",
/// //   query: None,
/// //   fragment: None,
/// // )
/// ```
///
pub const empty = Uri(
  scheme: None,
  userinfo: None,
  host: None,
  port: None,
  path: "",
  query: None,
  fragment: None,
)

/// Parses a compliant URI string into the `Uri` Type.
/// If the string is not a valid URI string then an error is returned.
///
/// The opposite operation is `uri.to_string`.
///
/// ## Examples
///
/// ```gleam
/// parse("https://example.com:1234/a/b?query=true#fragment")
/// // -> Ok(
/// //   Uri(
/// //     scheme: Some("https"),
/// //     userinfo: None,
/// //     host: Some("example.com"),
/// //     port: Some(1234),
/// //     path: "/a/b",
/// //     query: Some("query=true"),
/// //     fragment: Some("fragment")
/// //   )
/// // )
/// ```
///
@external(erlang, "gleam_stdlib", "uri_parse")
pub fn parse(uri_string: String) -> Result(Uri, Nil) {
  // This parses a uri_string following the regex defined in
  // https://tools.ietf.org/html/rfc3986#appendix-B
  //
  // TODO: This is not perfect and will be more permissive than its Erlang
  // counterpart, ideally we want to replicate Erlang's implementation on the js
  // target as well.
  parse_scheme_loop(uri_string, uri_string, empty, 0)
}

fn parse_scheme_loop(
  original: String,
  uri_string: String,
  pieces: Uri,
  size: Int,
) -> Result(Uri, Nil) {
  case uri_string {
    // `/` is not allowed to appear in a scheme so we know it's over and we can
    // start parsing the authority with slashes.
    "/" <> _ if size == 0 -> parse_authority_with_slashes(uri_string, pieces)
    "/" <> _ -> {
      let scheme = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, scheme: Some(string.lowercase(scheme)))
      parse_authority_with_slashes(uri_string, pieces)
    }

    // `?` is not allowed to appear in a schemem, in an authority, or in a path;
    // so if we see it we know it marks the beginning of the query part.
    "?" <> rest if size == 0 -> parse_query_with_question_mark(rest, pieces)
    "?" <> rest -> {
      let scheme = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, scheme: Some(string.lowercase(scheme)))
      parse_query_with_question_mark(rest, pieces)
    }

    // `#` is not allowed to appear in a scheme, in an authority, in a path or
    // in a query; so if we see it we know it marks the beginning of the final
    // fragment.
    "#" <> rest if size == 0 -> parse_fragment(rest, pieces)
    "#" <> rest -> {
      let scheme = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, scheme: Some(string.lowercase(scheme)))
      parse_fragment(rest, pieces)
    }

    // A colon marks the end of a uri scheme, but if it is not preceded by any
    // character then it's not a valid URI.
    ":" <> _ if size == 0 -> Error(Nil)
    ":" <> rest -> {
      let scheme = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, scheme: Some(string.lowercase(scheme)))
      parse_authority_with_slashes(rest, pieces)
    }

    // If we could get to the end of the string and we've met no special
    // chars whatsoever, that means the entire string is just a long path.
    "" -> Ok(Uri(..pieces, path: original))

    // In all other cases the first character is just a valid URI scheme
    // character and we just keep munching characters until we reach the end of
    // the uri scheme (or the end of the string and that would mean this is not
    // a valid uri scheme since we found no `:`).
    _ -> {
      let #(_, rest) = pop_codeunit(uri_string)
      parse_scheme_loop(original, rest, pieces, size + 1)
    }
  }
}

fn parse_authority_with_slashes(
  uri_string: String,
  pieces: Uri,
) -> Result(Uri, Nil) {
  case uri_string {
    // To be a valid authority the string must start with a `//`, otherwise
    // there's no authority and we just skip ahead to parsing the path.
    "//" -> Ok(Uri(..pieces, host: Some("")))
    "//" <> rest -> parse_authority_pieces(rest, pieces)
    _ -> parse_path(uri_string, pieces)
  }
}

fn parse_authority_pieces(string: String, pieces: Uri) -> Result(Uri, Nil) {
  parse_userinfo_loop(string, string, pieces, 0)
}

fn parse_userinfo_loop(
  original: String,
  uri_string: String,
  pieces: Uri,
  size: Int,
) -> Result(Uri, Nil) {
  case uri_string {
    // `@` marks the end of the userinfo and the start of the host part in the
    // authority string.
    "@" <> rest if size == 0 -> parse_host(rest, pieces)
    "@" <> rest -> {
      let userinfo = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, userinfo: Some(userinfo))
      parse_host(rest, pieces)
    }

    // If we reach the end of the authority string without finding an `@`
    // special character, then we know that the authority doesn't actually
    // contain the userinfo part.
    // The entire string we just went through was a host! So we parse it as
    // such.
    "" | "/" <> _ | "?" <> _ | "#" <> _ -> parse_host(original, pieces)

    // In all other cases we just keep munching characters increasing the size
    // of the userinfo bit.
    _ -> {
      let #(_, rest) = pop_codeunit(uri_string)
      parse_userinfo_loop(original, rest, pieces, size + 1)
    }
  }
}

fn parse_host(uri_string: String, pieces: Uri) -> Result(Uri, Nil) {
  // A host string can be in two formats:
  // - \[[:.a-zA-Z0-9]*\]
  // - [^:]
  case uri_string {
    // If we find an opening bracket we know it's the first format.
    "[" <> _ -> parse_host_within_brackets(uri_string, pieces)

    // A `:` marks the beginning of the port part of the authority string.
    ":" <> _ -> {
      let pieces = Uri(..pieces, host: Some(""))
      parse_port(uri_string, pieces)
    }

    // If the string is empty then there's no need to keep going. The host is
    // empty.
    "" -> Ok(Uri(..pieces, host: Some("")))

    // Otherwise it's the second format
    _ -> parse_host_outside_of_brackets(uri_string, pieces)
  }
}

fn parse_host_within_brackets(
  uri_string: String,
  pieces: Uri,
) -> Result(Uri, Nil) {
  parse_host_within_brackets_loop(uri_string, uri_string, pieces, 0)
}

fn parse_host_within_brackets_loop(
  original: String,
  uri_string: String,
  pieces: Uri,
  size: Int,
) -> Result(Uri, Nil) {
  case uri_string {
    // If the string is over the entire string we were iterating through is the
    // host part.
    "" -> Ok(Uri(..pieces, host: Some(uri_string)))

    // A `]` marks the end of the host and the start of the port part.
    "]" <> rest if size == 0 -> parse_port(rest, pieces)
    "]" <> rest -> {
      let host = codeunit_slice(original, at_index: 0, length: size + 1)
      let pieces = Uri(..pieces, host: Some(host))
      parse_port(rest, pieces)
    }

    // `/` marks the beginning of a path.
    "/" <> _ if size == 0 -> parse_path(uri_string, pieces)
    "/" <> _ -> {
      let host = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, host: Some(host))
      parse_path(uri_string, pieces)
    }

    // `?` marks the beginning of the query with question mark.
    "?" <> rest if size == 0 -> parse_query_with_question_mark(rest, pieces)
    "?" <> rest -> {
      let host = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, host: Some(host))
      parse_query_with_question_mark(rest, pieces)
    }

    // `#` marks the beginning of the fragment part.
    "#" <> rest if size == 0 -> parse_fragment(rest, pieces)
    "#" <> rest -> {
      let host = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, host: Some(host))
      parse_fragment(rest, pieces)
    }

    // In all other cases we just keep iterating.
    _ -> {
      let #(char, rest) = pop_codeunit(uri_string)
      // Inside `[...]` there can only be some characters, if we find a special
      // one then we know that we're actually parsing the other format for the
      // host and we switch to that!
      case is_valid_host_within_brackets_char(char) {
        True ->
          parse_host_within_brackets_loop(original, rest, pieces, size + 1)

        False ->
          parse_host_outside_of_brackets_loop(original, original, pieces, 0)
      }
    }
  }
}

fn is_valid_host_within_brackets_char(char: Int) -> Bool {
  // [0-9]
  { 48 >= char && char <= 57 }
  // [A-Z]
  || { 65 >= char && char <= 90 }
  // [a-z]
  || { 97 >= char && char <= 122 }
  // :
  || char == 58
  // .
  || char == 46
}

fn parse_host_outside_of_brackets(
  uri_string: String,
  pieces: Uri,
) -> Result(Uri, Nil) {
  parse_host_outside_of_brackets_loop(uri_string, uri_string, pieces, 0)
}

fn parse_host_outside_of_brackets_loop(
  original: String,
  uri_string: String,
  pieces: Uri,
  size: Int,
) -> Result(Uri, Nil) {
  case uri_string {
    "" -> Ok(Uri(..pieces, host: Some(original)))

    // `:` marks the beginning of the port.
    ":" <> _ -> {
      let host = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, host: Some(host))
      parse_port(uri_string, pieces)
    }

    // `/` marks the beginning of a path.
    "/" <> _ -> {
      let host = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, host: Some(host))
      parse_path(uri_string, pieces)
    }

    // `?` marks the beginning of the query with question mark.
    "?" <> rest -> {
      let host = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, host: Some(host))
      parse_query_with_question_mark(rest, pieces)
    }

    // `#` marks the beginning of the fragment part.
    "#" <> rest -> {
      let host = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, host: Some(host))
      parse_fragment(rest, pieces)
    }

    _ -> {
      let #(_, rest) = pop_codeunit(uri_string)
      parse_host_outside_of_brackets_loop(original, rest, pieces, size + 1)
    }
  }
}

fn parse_port(uri_string: String, pieces: Uri) -> Result(Uri, Nil) {
  case uri_string {
    ":0" <> rest -> parse_port_loop(rest, pieces, 0)
    ":1" <> rest -> parse_port_loop(rest, pieces, 1)
    ":2" <> rest -> parse_port_loop(rest, pieces, 2)
    ":3" <> rest -> parse_port_loop(rest, pieces, 3)
    ":4" <> rest -> parse_port_loop(rest, pieces, 4)
    ":5" <> rest -> parse_port_loop(rest, pieces, 5)
    ":6" <> rest -> parse_port_loop(rest, pieces, 6)
    ":7" <> rest -> parse_port_loop(rest, pieces, 7)
    ":8" <> rest -> parse_port_loop(rest, pieces, 8)
    ":9" <> rest -> parse_port_loop(rest, pieces, 9)

    // The port could be empty and be followed by any of the next delimiters.
    // Like `:#`, `:?` or `:/`
    ":" | "" -> Ok(pieces)

    // `?` marks the beginning of the query with question mark.
    "?" <> rest | ":?" <> rest -> parse_query_with_question_mark(rest, pieces)

    // `#` marks the beginning of the fragment part.
    "#" <> rest | ":#" <> rest -> parse_fragment(rest, pieces)

    // `/` marks the beginning of a path.
    "/" <> _ -> parse_path(uri_string, pieces)
    ":" <> rest ->
      case rest {
        "/" <> _ -> parse_path(rest, pieces)
        _ -> Error(Nil)
      }

    _ -> Error(Nil)
  }
}

fn parse_port_loop(
  uri_string: String,
  pieces: Uri,
  port: Int,
) -> Result(Uri, Nil) {
  case uri_string {
    // As long as we find port numbers we keep accumulating those.
    "0" <> rest -> parse_port_loop(rest, pieces, port * 10)
    "1" <> rest -> parse_port_loop(rest, pieces, port * 10 + 1)
    "2" <> rest -> parse_port_loop(rest, pieces, port * 10 + 2)
    "3" <> rest -> parse_port_loop(rest, pieces, port * 10 + 3)
    "4" <> rest -> parse_port_loop(rest, pieces, port * 10 + 4)
    "5" <> rest -> parse_port_loop(rest, pieces, port * 10 + 5)
    "6" <> rest -> parse_port_loop(rest, pieces, port * 10 + 6)
    "7" <> rest -> parse_port_loop(rest, pieces, port * 10 + 7)
    "8" <> rest -> parse_port_loop(rest, pieces, port * 10 + 8)
    "9" <> rest -> parse_port_loop(rest, pieces, port * 10 + 9)

    // `?` marks the beginning of the query with question mark.
    "?" <> rest -> {
      let pieces = Uri(..pieces, port: Some(port))
      parse_query_with_question_mark(rest, pieces)
    }

    // `#` marks the beginning of the fragment part.
    "#" <> rest -> {
      let pieces = Uri(..pieces, port: Some(port))
      parse_fragment(rest, pieces)
    }

    // `/` marks the beginning of a path.
    "/" <> _ -> {
      let pieces = Uri(..pieces, port: Some(port))
      parse_path(uri_string, pieces)
    }

    // The string (and so the port) is over, we return what we parsed so far.
    "" -> Ok(Uri(..pieces, port: Some(port)))

    // In all other cases we've ran into some invalid character inside the port
    // so the uri is invalid!
    _ -> Error(Nil)
  }
}

fn parse_path(uri_string: String, pieces: Uri) -> Result(Uri, Nil) {
  parse_path_loop(uri_string, uri_string, pieces, 0)
}

fn parse_path_loop(
  original: String,
  uri_string: String,
  pieces: Uri,
  size: Int,
) -> Result(Uri, Nil) {
  case uri_string {
    // `?` marks the beginning of the query with question mark.
    "?" <> rest -> {
      let path = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, path: path)
      parse_query_with_question_mark(rest, pieces)
    }

    // `#` marks the beginning of the fragment part.
    "#" <> rest -> {
      let path = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, path: path)
      parse_fragment(rest, pieces)
    }

    // If the string is over that means the entirety of the string was the path
    // and it has an empty query and fragment.
    "" -> Ok(Uri(..pieces, path: original))

    // In all other cases the character is allowed to be part of the path so we
    // just keep munching until we reach to its end.
    _ -> {
      let #(_, rest) = pop_codeunit(uri_string)
      parse_path_loop(original, rest, pieces, size + 1)
    }
  }
}

fn parse_query_with_question_mark(
  uri_string: String,
  pieces: Uri,
) -> Result(Uri, Nil) {
  parse_query_with_question_mark_loop(uri_string, uri_string, pieces, 0)
}

fn parse_query_with_question_mark_loop(
  original: String,
  uri_string: String,
  pieces: Uri,
  size: Int,
) -> Result(Uri, Nil) {
  case uri_string {
    // `#` marks the beginning of the fragment part.
    "#" <> rest if size == 0 -> parse_fragment(rest, pieces)
    "#" <> rest -> {
      let query = codeunit_slice(original, at_index: 0, length: size)
      let pieces = Uri(..pieces, query: Some(query))
      parse_fragment(rest, pieces)
    }

    // If the string is over that means the entirety of the string was the query
    // and it has an empty fragment.
    "" -> Ok(Uri(..pieces, query: Some(original)))

    // In all other cases the character is allowed to be part of the query so we
    // just keep munching until we reach to its end.
    _ -> {
      let #(_, rest) = pop_codeunit(uri_string)
      parse_query_with_question_mark_loop(original, rest, pieces, size + 1)
    }
  }
}

fn parse_fragment(rest: String, pieces: Uri) -> Result(Uri, Nil) {
  Ok(Uri(..pieces, fragment: Some(rest)))
}

// WARN: this function returns invalid strings!
// We need to return a String anyways to have this as the representation on the
// JavaScript target.
// Alternatively, we could rewrite the entire code to use a single
// `fold_codeunits`-style loop and a state machine.
@external(erlang, "gleam_stdlib", "string_pop_codeunit")
@external(javascript, "../gleam_stdlib.mjs", "pop_codeunit")
fn pop_codeunit(str: String) -> #(Int, String)

@external(erlang, "binary", "part")
@external(javascript, "../gleam_stdlib.mjs", "string_codeunit_slice")
fn codeunit_slice(str: String, at_index from: Int, length length: Int) -> String

/// Parses an urlencoded query string into a list of key value pairs.
/// Returns an error for invalid encoding.
///
/// The opposite operation is `uri.query_to_string`.
///
/// ## Examples
///
/// ```gleam
/// parse_query("a=1&b=2")
/// // -> Ok([#("a", "1"), #("b", "2")])
/// ```
///
@external(erlang, "gleam_stdlib", "parse_query")
@external(javascript, "../gleam_stdlib.mjs", "parse_query")
pub fn parse_query(query: String) -> Result(List(#(String, String)), Nil)

/// Encodes a list of key value pairs as a URI query string.
///
/// The opposite operation is `uri.parse_query`.
///
/// ## Examples
///
/// ```gleam
/// query_to_string([#("a", "1"), #("b", "2")])
/// // -> "a=1&b=2"
/// ```
///
pub fn query_to_string(query: List(#(String, String))) -> String {
  query
  |> list.map(query_pair)
  |> list.intersperse(string_tree.from_string("&"))
  |> string_tree.concat
  |> string_tree.to_string
}

fn query_pair(pair: #(String, String)) -> StringTree {
  string_tree.from_strings([percent_encode(pair.0), "=", percent_encode(pair.1)])
}

/// Encodes a string into a percent encoded representation.
///
/// ## Examples
///
/// ```gleam
/// percent_encode("100% great")
/// // -> "100%25%20great"
/// ```
///
@external(erlang, "gleam_stdlib", "percent_encode")
@external(javascript, "../gleam_stdlib.mjs", "percent_encode")
pub fn percent_encode(value: String) -> String

/// Decodes a percent encoded string.
///
/// ## Examples
///
/// ```gleam
/// percent_decode("100%25%20great+fun")
/// // -> Ok("100% great+fun")
/// ```
///
@external(erlang, "gleam_stdlib", "percent_decode")
@external(javascript, "../gleam_stdlib.mjs", "percent_decode")
pub fn percent_decode(value: String) -> Result(String, Nil)

/// Splits the path section of a URI into it's constituent segments.
///
/// Removes empty segments and resolves dot-segments as specified in
/// [section 5.2](https://www.ietf.org/rfc/rfc3986.html#section-5.2) of the RFC.
///
/// ## Examples
///
/// ```gleam
/// path_segments("/users/1")
/// // -> ["users" ,"1"]
/// ```
///
pub fn path_segments(path: String) -> List(String) {
  remove_dot_segments(string.split(path, "/"))
}

fn remove_dot_segments(input: List(String)) -> List(String) {
  remove_dot_segments_loop(input, [])
}

fn remove_dot_segments_loop(
  input: List(String),
  accumulator: List(String),
) -> List(String) {
  case input {
    [] -> list.reverse(accumulator)
    [segment, ..rest] -> {
      let accumulator = case segment, accumulator {
        "", accumulator -> accumulator
        ".", accumulator -> accumulator
        "..", [] -> []
        "..", [_, ..accumulator] -> accumulator
        segment, accumulator -> [segment, ..accumulator]
      }
      remove_dot_segments_loop(rest, accumulator)
    }
  }
}

/// Encodes a `Uri` value as a URI string.
///
/// The opposite operation is `uri.parse`.
///
/// ## Examples
///
/// ```gleam
/// let uri = Uri(..empty, scheme: Some("https"), host: Some("example.com"))
/// to_string(uri)
/// // -> "https://example.com"
/// ```
///
pub fn to_string(uri: Uri) -> String {
  let parts = case uri.fragment {
    Some(fragment) -> ["#", fragment]
    None -> []
  }
  let parts = case uri.query {
    Some(query) -> ["?", query, ..parts]
    None -> parts
  }
  let parts = [uri.path, ..parts]
  let parts = case uri.host, string.starts_with(uri.path, "/") {
    Some(host), False if host != "" -> ["/", ..parts]
    _, _ -> parts
  }
  let parts = case uri.host, uri.port {
    Some(_), Some(port) -> [":", int.to_string(port), ..parts]
    _, _ -> parts
  }
  let parts = case uri.scheme, uri.userinfo, uri.host {
    Some(s), Some(u), Some(h) -> [s, "://", u, "@", h, ..parts]
    Some(s), None, Some(h) -> [s, "://", h, ..parts]
    Some(s), Some(_), None | Some(s), None, None -> [s, ":", ..parts]
    None, None, Some(h) -> ["//", h, ..parts]
    _, _, _ -> parts
  }
  string.concat(parts)
}

/// Fetches the origin of a URI.
///
/// Returns the origin of a uri as defined in
/// [RFC 6454](https://tools.ietf.org/html/rfc6454)
///
/// The supported URI schemes are `http` and `https`.
/// URLs without a scheme will return `Error`.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(uri) = parse("https://example.com/path?foo#bar")
/// origin(uri)
/// // -> Ok("https://example.com")
/// ```
///
pub fn origin(uri: Uri) -> Result(String, Nil) {
  let Uri(scheme: scheme, host: host, port: port, ..) = uri
  case host, scheme {
    Some(h), Some("https") if port == Some(443) ->
      Ok(string.concat(["https://", h]))
    Some(h), Some("http") if port == Some(80) ->
      Ok(string.concat(["http://", h]))
    Some(h), Some(s) if s == "http" || s == "https" -> {
      case port {
        Some(p) -> Ok(string.concat([s, "://", h, ":", int.to_string(p)]))
        None -> Ok(string.concat([s, "://", h]))
      }
    }
    _, _ -> Error(Nil)
  }
}

/// Resolves a URI with respect to the given base URI.
///
/// The base URI must be an absolute URI or this function will return an error.
/// The algorithm for merging uris is described in
/// [RFC 3986](https://tools.ietf.org/html/rfc3986#section-5.2).
///
pub fn merge(base: Uri, relative: Uri) -> Result(Uri, Nil) {
  case base {
    Uri(scheme: Some(_), host: Some(_), ..) ->
      case relative {
        Uri(host: Some(_), ..) -> {
          let path =
            string.split(relative.path, "/")
            |> remove_dot_segments()
            |> join_segments()
          let resolved =
            Uri(
              option.or(relative.scheme, base.scheme),
              None,
              relative.host,
              option.or(relative.port, base.port),
              path,
              relative.query,
              relative.fragment,
            )
          Ok(resolved)
        }
        _ -> {
          let #(new_path, new_query) = case relative.path {
            "" -> #(base.path, option.or(relative.query, base.query))
            _ -> {
              let path_segments = case string.starts_with(relative.path, "/") {
                True -> string.split(relative.path, "/")
                False ->
                  string.split(base.path, "/")
                  |> drop_last()
                  |> list.append(string.split(relative.path, "/"))
              }
              let path =
                path_segments
                |> remove_dot_segments()
                |> join_segments()
              #(path, relative.query)
            }
          }
          let resolved =
            Uri(
              base.scheme,
              None,
              base.host,
              base.port,
              new_path,
              new_query,
              relative.fragment,
            )
          Ok(resolved)
        }
      }
    _ -> Error(Nil)
  }
}

fn drop_last(elements: List(a)) -> List(a) {
  list.take(from: elements, up_to: list.length(elements) - 1)
}

fn join_segments(segments: List(String)) -> String {
  string.join(["", ..segments], "/")
}
