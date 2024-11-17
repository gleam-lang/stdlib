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
import gleam/string_builder.{type StringBuilder}

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
pub fn parse(uri_string: String) -> Result(Uri, Nil) {
  do_parse(uri_string)
}

@external(erlang, "gleam_stdlib", "uri_parse")
pub fn do_parse(uri_string: String) -> Result(Uri, Nil) {
  case parse_uri_pieces(uri_string) {
    Error(Nil) -> Error(Nil)
    Ok(UriPieces(
      scheme: scheme,
      authority_with_slashes: authority_with_slashes,
      path: path,
      query_with_question_mark: query_with_question_mark,
      fragment: fragment,
    )) -> {
      let scheme = noneify_empty_string(scheme)
      let query = noneify_query(query_with_question_mark)
      let #(userinfo, host, port) = split_authority(authority_with_slashes)
      let scheme =
        scheme
        |> noneify_empty_string
        |> option.map(string.lowercase)
      Ok(Uri(
        scheme: scheme,
        userinfo: userinfo,
        host: host,
        port: port,
        path: path,
        query: query,
        fragment: fragment,
      ))
    }
  }
}

type UriPieces {
  UriPieces(
    scheme: Option(String),
    authority_with_slashes: Option(String),
    path: String,
    query_with_question_mark: Option(String),
    fragment: Option(String),
  )
}

fn parse_uri_pieces(uri_string: String) -> Result(UriPieces, Nil) {
  // This parses a uri_string following the regex defined in
  // https://tools.ietf.org/html/rfc3986#appendix-B
  //
  // TODO: This is not perfect and will be more permissive than its Erlang
  // counterpart, ideally we want to replicate Erlang's implementation on the js
  // target as well.
  let default_pieces =
    UriPieces(
      scheme: None,
      authority_with_slashes: None,
      path: "",
      query_with_question_mark: None,
      fragment: None,
    )

  parse_scheme_loop(uri_string, uri_string, default_pieces, 0)
}

fn parse_scheme_loop(
  original: String,
  uri_string: String,
  pieces: UriPieces,
  size: Int,
) -> Result(UriPieces, Nil) {
  case string.pop_grapheme(uri_string) {
    // `/` is not allowed to appear in a scheme so we know it's over and we can
    // start parsing the authority with slashes.
    Ok(#("/", _)) if size == 0 ->
      parse_authority_with_slashes(uri_string, pieces)
    Ok(#("/", _)) -> {
      let scheme = string.slice(original, at_index: 0, length: size)
      let pieces = UriPieces(..pieces, scheme: Some(scheme))
      parse_authority_with_slashes(uri_string, pieces)
    }

    // `?` is not allowed to appear in a schemem, in an authority, or in a path;
    // so if we see it we know it marks the beginning of the query part.
    Ok(#("?", _)) if size == 0 ->
      parse_query_with_question_mark(uri_string, pieces)
    Ok(#("?", _)) -> {
      let scheme = string.slice(original, at_index: 0, length: size)
      let pieces = UriPieces(..pieces, scheme: Some(scheme))
      parse_query_with_question_mark(uri_string, pieces)
    }

    // `#` is not allowed to appear in a scheme, in an authority, in a path or
    // in a query; so if we see it we know it marks the beginning of the final
    // fragment.
    Ok(#("#", rest)) if size == 0 -> parse_fragment(rest, pieces)
    Ok(#("#", rest)) -> {
      let scheme = string.slice(original, at_index: 0, length: size)
      let pieces = UriPieces(..pieces, scheme: Some(scheme))
      parse_fragment(rest, pieces)
    }

    // A colon marks the end of a uri scheme, but if it is not preceded by any
    // character then it's not a valid URI.
    Ok(#(":", _)) if size == 0 -> Error(Nil)
    Ok(#(":", rest)) -> {
      let scheme = string.slice(original, at_index: 0, length: size)
      let pieces = UriPieces(..pieces, scheme: Some(scheme))
      parse_authority_with_slashes(rest, pieces)
    }

    // In all other cases the first character is just a valid URI scheme
    // character and we just keep munching characters until we reach the end of
    // the uri scheme (or the end of the string and that would mean this is not
    // a valid uri scheme since we found no `:`).
    Ok(#(_, rest)) -> parse_scheme_loop(original, rest, pieces, size + 1)

    // If we could get to the end of the string and we've met no special
    // chars whatsoever, that means the entire string is just a long path.
    Error(_) -> Ok(UriPieces(..pieces, path: original))
  }
}

fn parse_authority_with_slashes(
  uri_string: String,
  pieces: UriPieces,
) -> Result(UriPieces, Nil) {
  case uri_string {
    // To be a valid authority the string must start with a `//`, otherwise
    // there's no authority and we just skip ahead to parsing the path.
    "//" <> rest ->
      parse_authority_with_slashes_loop(uri_string, rest, pieces, 2)
    _ -> parse_path(uri_string, pieces)
  }
}

fn parse_authority_with_slashes_loop(
  original: String,
  uri_string: String,
  pieces: UriPieces,
  size: Int,
) -> Result(UriPieces, Nil) {
  case string.pop_grapheme(uri_string) {
    // `/` marks the beginning of a path.
    Ok(#("/", _)) -> {
      let authority = string.slice(original, at_index: 0, length: size)
      let pieces = UriPieces(..pieces, authority_with_slashes: Some(authority))
      parse_path(uri_string, pieces)
    }

    // `?` marks the beginning of the query with question mark.
    Ok(#("?", _)) -> {
      let authority = string.slice(original, at_index: 0, length: size)
      let pieces = UriPieces(..pieces, authority_with_slashes: Some(authority))
      parse_query_with_question_mark(uri_string, pieces)
    }

    // `#` marks the beginning of the fragment part.
    Ok(#("#", rest)) -> {
      let authority = string.slice(original, at_index: 0, length: size)
      let pieces = UriPieces(..pieces, authority_with_slashes: Some(authority))
      parse_fragment(rest, pieces)
    }

    // In all other cases the character is allowed to be part of the authority
    // so we just keep munching until we reach to its end.
    Ok(#(_, rest)) ->
      parse_authority_with_slashes_loop(original, rest, pieces, size + 1)

    // If the string is over that means the entirety of the string was the
    // authority and it has an empty path, query and fragment.
    Error(_) -> Ok(UriPieces(..pieces, authority_with_slashes: Some(original)))
  }
}

fn parse_path(uri_string: String, pieces: UriPieces) -> Result(UriPieces, Nil) {
  parse_path_loop(uri_string, uri_string, pieces, 0)
}

fn parse_path_loop(
  original: String,
  uri_string: String,
  pieces: UriPieces,
  size: Int,
) -> Result(UriPieces, Nil) {
  case string.pop_grapheme(uri_string) {
    // `?` marks the beginning of the query with question mark.
    Ok(#("?", _)) -> {
      let path = string.slice(original, at_index: 0, length: size)
      let pieces = UriPieces(..pieces, path: path)
      parse_query_with_question_mark(uri_string, pieces)
    }

    // `#` marks the beginning of the fragment part.
    Ok(#("#", rest)) -> {
      let path = string.slice(original, at_index: 0, length: size)
      let pieces = UriPieces(..pieces, path: path)
      parse_fragment(rest, pieces)
    }

    // In all other cases the character is allowed to be part of the path so we
    // just keep munching until we reach to its end.
    Ok(#(_, rest)) -> parse_path_loop(original, rest, pieces, size + 1)

    // If the string is over that means the entirety of the string was the path
    // and it has an empty query and fragment.
    Error(_) -> Ok(UriPieces(..pieces, path: original))
  }
}

fn parse_query_with_question_mark(
  uri_string: String,
  pieces: UriPieces,
) -> Result(UriPieces, Nil) {
  parse_query_with_question_mark_loop(uri_string, uri_string, pieces, 0)
}

fn parse_query_with_question_mark_loop(
  original: String,
  uri_string: String,
  pieces: UriPieces,
  size: Int,
) -> Result(UriPieces, Nil) {
  case string.pop_grapheme(uri_string) {
    // `#` marks the beginning of the fragment part.
    Ok(#("#", rest)) -> {
      let query = string.slice(original, at_index: 0, length: size)
      let pieces = UriPieces(..pieces, query_with_question_mark: Some(query))
      parse_fragment(rest, pieces)
    }

    // In all other cases the character is allowed to be part of the query so we
    // just keep munching until we reach to its end.
    Ok(#(_, rest)) ->
      parse_query_with_question_mark_loop(original, rest, pieces, size + 1)

    // If the string is over that means the entirety of the string was the query
    // and it has an empty fragment.
    Error(_) ->
      Ok(UriPieces(..pieces, query_with_question_mark: Some(original)))
  }
}

fn parse_fragment(rest: String, pieces: UriPieces) -> Result(UriPieces, Nil) {
  Ok(UriPieces(..pieces, fragment: Some(rest)))
}

fn noneify_query(x: Option(String)) -> Option(String) {
  case x {
    None -> None
    Some(x) ->
      case string.pop_grapheme(x) {
        Ok(#("?", query)) -> Some(query)
        _ -> None
      }
  }
}

fn noneify_empty_string(x: Option(String)) -> Option(String) {
  case x {
    Some("") | None -> None
    Some(_) -> x
  }
}

// Split an authority into its userinfo, host and port parts.
fn split_authority(
  authority: Option(String),
) -> #(Option(String), Option(String), Option(Int)) {
  case option.unwrap(authority, "") {
    "" -> #(None, None, None)
    "//" -> #(None, Some(""), None)
    "//" <> authority | authority -> {
      let AuthorityPieces(userinfo: userinfo, host: host, port: port) =
        parse_authority_pieces(authority)

      let userinfo = noneify_empty_string(userinfo)
      let host = noneify_empty_string(host)
      #(userinfo, host, port)
    }
  }
}

type AuthorityPieces {
  AuthorityPieces(
    userinfo: Option(String),
    host: Option(String),
    port: Option(Int),
  )
}

fn parse_authority_pieces(string: String) -> AuthorityPieces {
  let pieces = AuthorityPieces(userinfo: None, host: None, port: None)
  parse_userinfo_loop(string, string, pieces, 0)
}

fn parse_userinfo_loop(original, string, pieces, size) {
  case string.pop_grapheme(string) {
    Error(_) -> parse_host(original, pieces)

    Ok(#("@", rest)) -> {
      let userinfo = string.slice(original, at_index: 0, length: size)
      let pieces = AuthorityPieces(..pieces, userinfo: Some(userinfo))
      parse_host(rest, pieces)
    }
    Ok(#(_, rest)) -> parse_userinfo_loop(original, rest, pieces, size + 1)
  }
}

fn parse_host(string, pieces) {
  case string.pop_grapheme(string) {
    Ok(#("[", _)) -> parse_host_within_brackets(string, pieces)
    Ok(#(":", rest)) -> {
      let pieces = AuthorityPieces(..pieces, host: Some(""))
      parse_port(rest, pieces)
    }
    Ok(#(_, _)) -> parse_host_outside_of_brackets(string, pieces)
    Error(_) -> AuthorityPieces(..pieces, host: Some(""))
  }
}

fn parse_host_within_brackets(string, pieces) {
  parse_host_within_brackets_loop(string, string, pieces, 0)
}

fn parse_host_within_brackets_loop(original, string, pieces, size) {
  case string.pop_grapheme(string) {
    Error(_) -> AuthorityPieces(..pieces, host: Some(string))
    Ok(#("]", rest)) -> {
      let host = string.slice(original, at_index: 0, length: size + 1)
      let pieces = AuthorityPieces(..pieces, host: Some(host))
      parse_port(rest, pieces)
    }
    Ok(#(char, rest)) ->
      case is_valid_host_withing_brackets_char(char) {
        True ->
          parse_host_within_brackets_loop(original, rest, pieces, size + 1)
        False ->
          parse_host_outside_of_brackets_loop(original, rest, pieces, size + 1)
      }
  }
}

fn is_valid_host_withing_brackets_char(char: String) -> Bool {
  case char {
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" -> True
    "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" -> True
    "w" | "x" | "y" | "z" -> True
    "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" -> True
    "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" -> True
    "W" | "X" | "Y" | "Z" -> True
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    "." | ":" -> True
    _ -> False
  }
}

fn parse_host_outside_of_brackets(string, pieces) {
  parse_host_outside_of_brackets_loop(string, string, pieces, 0)
}

fn parse_host_outside_of_brackets_loop(original, string, pieces, size) {
  case string.pop_grapheme(string) {
    Error(_) -> AuthorityPieces(..pieces, host: Some(original))
    Ok(#(":", rest)) -> {
      let host = string.slice(original, at_index: 0, length: size)
      let pieces = AuthorityPieces(..pieces, host: Some(host))
      parse_port(rest, pieces)
    }
    Ok(#(_, rest)) ->
      parse_host_outside_of_brackets_loop(original, rest, pieces, size + 1)
  }
}

fn parse_port(string, pieces) {
  case int.parse(string) {
    Ok(port) -> AuthorityPieces(..pieces, port: Some(port))
    Error(_) -> pieces
  }
}

fn extra_required(list: List(a), remaining: Int) -> Int {
  case list {
    _ if remaining == 0 -> 0
    [] -> remaining
    [_, ..rest] -> extra_required(rest, remaining - 1)
  }
}

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
pub fn parse_query(query: String) -> Result(List(#(String, String)), Nil) {
  do_parse_query(query)
}

@external(erlang, "gleam_stdlib", "parse_query")
@external(javascript, "../gleam_stdlib.mjs", "parse_query")
fn do_parse_query(a: String) -> Result(List(#(String, String)), Nil)

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
  |> list.intersperse(string_builder.from_string("&"))
  |> string_builder.concat
  |> string_builder.to_string
}

fn query_pair(pair: #(String, String)) -> StringBuilder {
  string_builder.from_strings([
    percent_encode(pair.0),
    "=",
    percent_encode(pair.1),
  ])
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
pub fn percent_encode(value: String) -> String {
  do_percent_encode(value)
}

@external(erlang, "gleam_stdlib", "percent_encode")
@external(javascript, "../gleam_stdlib.mjs", "percent_encode")
fn do_percent_encode(a: String) -> String

/// Decodes a percent encoded string.
///
/// ## Examples
///
/// ```gleam
/// percent_decode("100%25%20great+fun")
/// // -> Ok("100% great+fun")
/// ```
///
pub fn percent_decode(value: String) -> Result(String, Nil) {
  do_percent_decode(value)
}

@external(erlang, "gleam_stdlib", "percent_decode")
@external(javascript, "../gleam_stdlib.mjs", "percent_decode")
fn do_percent_decode(a: String) -> Result(String, Nil)

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
/// let uri = Uri(Some("http"), None, Some("example.com"), ...)
/// to_string(uri)
/// // -> "http://example.com"
/// ```
///
pub fn to_string(uri: Uri) -> String {
  let parts = case uri.fragment {
    Some(fragment) -> ["#", fragment]
    _ -> []
  }
  let parts = case uri.query {
    Some(query) -> ["?", query, ..parts]
    _ -> parts
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
/// let assert Ok(uri) = parse("http://example.com/path?foo#bar")
/// origin(uri)
/// // -> Ok("http://example.com")
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
